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

age_of_infection_matrix <- function(inf_to_death_days, deaths_vector) {
  num_days_in_matrix <- length(deaths_vector) + inf_to_death_days
  result <- matrix(data = NA,
                   nrow = num_days_in_matrix,
                   ncol = inf_to_death_days + 1)

  padded_deaths <- c(rep(0, inf_to_death_days),
                     deaths_vector,
                     rep(NA, inf_to_death_days))

  for (i in 1:num_days_in_matrix) {
    result[i,] <- rev(padded_deaths[i + (0:inf_to_death_days)])
  }

  return(result)
}

## infers infections by back calculating from deaths. Function is modified from https://github.com/COG-UK/UK-lineage-dynamics-analysis/blob/main/analyses/epidemiological/estimate-potential-seeders.R
infer_infections <- function(deaths_df, location_str, smooth = FALSE) {
  ## model-parameters
  days_latent <- 3
  days_incubating <- 5
  days_infectious <- 7
  prop_asymptomatic <- 0.31  # default is 0.31
  days_infection_to_death <- 23
  infection_fatality_ratio <- 100


  location_df <- deaths_df %>%
    filter(location == location_str) %>%
    select(date, deaths, cases, population ) %>%
    mutate(naive_infections = deaths * infection_fatality_ratio)

  if (smooth) {
    location_df$naive_infections <- rollapply(location_df$naive_infections, 7, mean, fill = 0, partial = TRUE)
  }

  age_matrix <- age_of_infection_matrix(days_infection_to_death, location_df$naive_infections)

  presymptomatic_cases <- rowSums(age_matrix[, 0:days_incubating + 1])
  asymptomatic_cases <- rowSums(prop_asymptomatic * age_matrix[, (days_incubating + 1):(days_latent + days_infectious) + 1])

  padded_total_cases <- rowSums(cbind(presymptomatic_cases, asymptomatic_cases))

  start_date <- min(location_df$date)
  padding_dates <- seq(from = start_date - days_infection_to_death,
                       to = start_date - 1,
                       by = 1)
  total_dates <- c(padding_dates, location_df$date)

  return_df <- data.frame(date = total_dates,
                          presymptomatic_cases = presymptomatic_cases,
                          asymptomatic_cases = asymptomatic_cases,
                          estimated_infections = padded_total_cases)

  return_df %>%
    mutate(location = location_str, .after = date) %>%
    left_join(location_df, by = c("date"), keep = FALSE) %>%
    replace(is.na(.), 0)
}


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
    replace_na(list(deaths = 0, cases = 0)) %>%
    filter(date < "2021-10-30")
}

download_deaths <- function() {
  fields <- c("name", "date", "dead", "confirmed", "population")

  countries <- getAdmn0( fields=fields ) %>%
    select(name, date, dead, confirmed, population) %>%
    filter(!name %in% c("United States of America", "Georgia", "Mexico")) %>%
    mutate(name = recode(name,
                         "Bolivia (Plurinational State of)" = "Bolivia",
                         "Cabo Verde" = "Cape Verde",
                         "Iran (Islamic Republic of)" = "Iran",
                         "Republic of Moldova" = "Moldova",
                         "Republic of Korea" = "South Korea",
                         "Taiwan (Province of China)" = "Taiwan",
                         "United States Virgin Islands" = "U.S. Virgin Islands",
                         "Venezuela (Bolivarian Republic of)" = "Venezuela",
                         "Viet Nam" = "Vietnam")) %>%
    format_epi_data()

  # getAdmin1ByCountry doesn't seem to want to return more than 26,000 entries so a lot of locations have missing data.
  # This takes longer, but yields all data.
  states <- getAdmn1ByCountry( "United States of America", fields=c("name"), date="2021-01-01" ) %>%
    pull( name )
  usstates <- lapply( states, getEpiData, admin_level=1, fields=fields ) %>%
    bind_rows() %>%
    select(name, date, dead, confirmed, population) %>%
    filter(name != "California") %>%
    format_epi_data()

  cacounties <- getEpiDataNoProgress(state_name = searchLocations("California", admin_level = 1), admin_level = 2, fields=fields) %>%
    select(name, date, dead, confirmed, population) %>%
    mutate(name = str_c(name, " County")) %>%
    format_epi_data()

  mx <- read_csv( "../../data/deaths_mexico.csv" ) %>%
    select( -c( cve_ent, poblacion ) ) %>%
    mutate( nombre = str_to_title( nombre) ) %>%
    gather(var, val, 2:ncol(.)) %>%
    spread(nombre, val) %>%
    mutate( var=as.Date( var, format="%d-%m-%Y") ) %>%
    rename( date=var ) %>%
    arrange( date ) %>%
    select( date, `Baja California` ) %>%
    rename( deaths = `Baja California` ) %>%
    mutate( population = 3634868, cases=0, location="Mexico" ) %>%
    filter(date < "2021-10-30")

  return_df <- bind_rows( countries, usstates, cacounties, mx )
  return_df$deaths[return_df$deaths < 0] <- 0
  return_df$cases[return_df$cases < 0] <- 0

  return( return_df )
}

total <- download_deaths()

# For each location infer infections.
results <- map(.x = unique(total$location),
               .f = ~infer_infections(deaths_df = total, location_str = .x, smooth = TRUE)) %>%
  bind_rows()

write.csv( results, "back-calculated-infections.csv")