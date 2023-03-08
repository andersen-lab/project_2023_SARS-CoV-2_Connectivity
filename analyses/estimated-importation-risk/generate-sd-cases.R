library(tidyverse)
library(lubridate)
library(epidemia)
library(ggplot2)
library(reshape2)
library(zoo)
library(httr)
library(jsonlite)
library(progress)
library( outbreakinfo )


getEpiDataNoProgress <- function(name = NULL, location_id = NULL, wb_region = NULL, country_name = NULL, state_name = NULL, admin_level = NULL, date = NULL, mostRecent = NULL, fields = NULL, sort = NULL, size = 1000) {
  q <- c()
  api.url <- "https://api.outbreak.info/covid19/"
  if (!is.null(name)) {
    q <- c(q, paste0("(name:\"", paste(name, collapse = "\" OR name:\""), "\") AND "))
  }
  if (!is.null(location_id)) {
    q <- c(q, paste0("(location_id:\"", paste(location_id, collapse = "\" OR location_id:\""), "\") AND "))
  }
  if (!is.null(wb_region)) {
    q <- c(q, paste0("(wb_region:\"", paste(wb_region, collapse = "\" OR wb_region:\""), "\") AND "))
  }
  if (!is.null(country_name)) {
    q <- c(q, paste0("(country_name:\"", paste(country_name, collapse = "\" OR country_name:\""), "\") AND "))
  }
  if (!is.null(state_name)) {
    q <- c(q, paste0("(admin1:\"", paste(state_name, collapse = "\" OR admin1:\""), "\") AND "))
  }
  if (!is.null(admin_level)) {
    q <- c(q, paste0("(admin_level:\"", paste(admin_level, collapse = "\" OR admin_level:\""), "\") AND "))
  }
  if (!is.null(date)) {
    if (!is.character(date)) {
      stop("Date must be in string format")
    }else {
      q <- c(q, paste0("(date:\"", paste(date, collapse = "\" OR date:\""), "\") AND "))
    }
  }
  q <- paste(q, sep = "", collapse = "")
  q <- substr(q, 1, nchar(q) - 5)
  if (!is.null(mostRecent)) {
    if (!is.logical(mostRecent)) {
      stop("mostRecent must be in Boolean format")
    }else {
      q <- c(q, paste0(" AND ", "mostRecent:", tolower(mostRecent)))
    }
  }
  q <- paste(q, sep = "", collapse = "")
  if (!is.null(fields)) {
    q <- c(q, paste0("&fields=", paste(fields, collapse = ",")))
  }
  if (!is.null(sort)) {
    q <- c(q, paste0("&sort=", paste(sort)))
  }

  q <- c(q, paste0("&size=", paste(size)))
  q <- paste(q, sep = "", collapse = "")
  q <- paste0(q, "&fetch_all=true")

  scroll.id <- NULL
  results <- list()
  success <- NULL
  firstQuery = TRUE
  while (is.null(success)) {
    dataurl <- paste0(api.url, "query?q=", q)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    dataurl <- URLencode(dataurl)
    resp <- GET(
      dataurl
    )
    if (resp$status_code == 200) {
      resp <- fromJSON(content(resp, "text"), flatten = TRUE)
      scroll.id <- resp$'_scroll_id'
      if (!is.null(resp$hits)) {
        if (class(resp$hits) == "data.frame") {
          results[[length(results) + 1]] <- resp$hits
        }
      }
      success <- resp$success
    } else if (resp$status_code == 400) {
      resp <- fromJSON(content(resp, "text"), flatten = TRUE)
      success <- resp$success
    } else {
      stop("Could not connect to API. Check internet connection and try again. If the problem persists please contact help@outbreak.info.")
    }
    if (firstQuery) {
      max = resp$total
      if (max == 0)
        return(data.frame())
      firstQuery = FALSE
    }
  }
  if (length(results) > 1) {
    hits <- rbind_pages(results)
  }else {
    hits <- data.frame(results)
  }
  if ("date" %in% colnames(hits)) {
    hits$date = as.Date(hits$date, "%Y-%m-%d")
    hits <- hits[order(as.Date(hits$date, format = "%Y-%m-%d")),]
  }
  return(hits)
}

format_epi_data <- function(entry) {
  entry %>%
    group_by(name) %>%
    complete( date = seq.Date(min(date), max(date), by="day") ) %>%
    fill( name, population ) %>%
    replace_na( list( dead=0, confirmed=0 ) ) %>%
    mutate(dead = cummax(dead), dead_numIncrease = dead - lag(dead)) %>%
    mutate(confirmed = cummax(confirmed), confirmed_numIncrease = confirmed - lag(confirmed)) %>%
    ungroup() %>%
    select(-c(confirmed, dead)) %>%
    rename(deaths = dead_numIncrease, cases = confirmed_numIncrease, location=name) %>%
    replace_na(list(deaths = 0, cases = 0))
}

fields <- c("name", "date", "dead", "confirmed", "population")
sd <- getEpiDataNoProgress(state_name = searchLocations("California", admin_level = 1), admin_level = 2, fields=fields) %>%
  select(name, date, dead, confirmed, population) %>%
  mutate(name = str_c(name, " County")) %>%
  format_epi_data() %>%
  filter( location=="San Diego County" )
sd$deaths[sd$deaths < 0] <- 0
sd$cases[sd$cases < 0] <- 0
write.csv( sd, "sd-cases.csv" )