library( outbreakinfo)
library( tidyverse )
library( lubridate )
library( reshape2 )

country_names <- getAdmn0(fields=c("name"), date="2021-01-01" ) %>%
  pull( name )
countries <- lapply( country_names, getEpiData, admin_level=0, fields=fields ) %>%
  bind_rows() %>%
  select( name, date, confirmed_numIncrease ) %>%
  filter( !name %in% c("United States of America", "Georgia", "Guam", "Mexico", "Canada") ) %>%
  mutate( week = floor_date( date, unit="week" ), name=replace( name, name=="Czechia", "Czech Republic" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

states <- getAdmn1ByCountry( "United States of America", fields=c("name"), date="2021-01-01" ) %>%
  pull( name )
usstates <- lapply( states, getEpiData, admin_level=1, fields=fields ) %>%
  bind_rows() %>%
  select( name, date, confirmed_numIncrease ) %>%
  mutate( week =  floor_date( date, unit="week" ) ) %>%
  group_by( name, week ) %>%
  summarize( new_cases = sum( confirmed_numIncrease ) )

states <- getAdmn1ByCountry( "Canada", fields=c("name"), date="2021-01-01" ) %>%
  pull( name )
canstates <- lapply( states, getEpiData, admin_level=1, fields=fields ) %>%
  bind_rows() %>%
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

mexcases <- read_csv( "../../data/cases_mexico.csv" ) %>%
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