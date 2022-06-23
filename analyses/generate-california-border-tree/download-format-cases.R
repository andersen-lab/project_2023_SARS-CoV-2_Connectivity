library( outbreakinfo)
library( tidyverse )
library( lubridate )
library( reshape2 )

countries <- getAdmn0() %>%
  select( name, date, confirmed_numIncrease ) %>%
  filter( !name %in% c("United States of America", "Georgia", "Guam", "Mexico", "Canada") ) %>%
  mutate( week = floor_date( date, unit="week" ), name=replace( name, name=="Czechia", "Czech Republic" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

usstates <- getAdmn1ByCountry( "United States of America" ) %>%
  select( name, date, confirmed_numIncrease ) %>%
  mutate( week =  floor_date( date, unit="week" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

canstates <- getAdmn1ByCountry( "Canada" ) %>%
  select( name, date, confirmed_numIncrease ) %>%
  mutate( week =  floor_date( date, unit="week" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

sdcases <- getEpiData( name="San Diego" ) %>%
  select( name, date, confirmed_numIncrease ) %>%
  mutate( week =  floor_date( date, unit="week" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

lacases <- getEpiData( name="Los Angeles" ) %>%
  select( name, date, confirmed_numIncrease ) %>%
  mutate( week =  floor_date( date, unit="week" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

mexcases <- read_csv( "/Users/natem/Dropbox (Scripps Research)/Personal/Code/Projects/project_2021_california-hcov-genomics/data/cases_mexico.csv" ) %>%
  select( -c( "cve_ent", "poblacion" ) ) %>%
  melt( id.vars="nombre", variable.name="date", value.name="new_cases") %>%
  mutate( nombre = str_to_title( nombre ), date=as.Date( date, format="%d-%m-%Y" ), week=floor_date( date, unit="week" ) ) %>%
  group_by( nombre, week ) %>%
  summarize( new_cases = sum( new_cases ) ) %>%
  ungroup() %>%
  rename( name=nombre ) %>%
  mutate( name=replace( name, name=="Mexico", "Estado de Mexico") ) %>%
  mutate( name=replace( name, name=="Distrito Federal", "Mexico City") )

total <- rbind( usstates, mexcases, canstates, countries, sdcases, lacases )
total$week <- as.character( total$week )

total %>% write_csv( "cases.csv" )