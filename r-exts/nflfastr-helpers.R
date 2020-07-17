library(data.table)
library(glue)

get_pbp_from_db <- function(start_yr=1999, end_year=2020, dbpath='./db/pbp_db'){
  connection <- DBI::dbConnect(RSQLite::SQLite(), dbpath)
  res <- DBI::dbSendQuery(connection, glue_sql('SELECT * FROM nflfastR_pbp WHERE season BETWEEN {start_yr} AND {end_year}'))
  pbp <- as.data.table(DBI::dbFetch(res))
  DBI::dbClearResult(res)
  DBI::dbDisconnect(connection)
  return(pbp)
}

hex_to_rgba <- function(hex, opacity=1) {
  glue("rgba({paste(col2rgb(hex), collapse=',')},{opacity})")
}

revert_to_old_abbr <- function(abbr, yr){
  dplyr::case_when(
    abbr=="LV" & yr < 2020 ~ "OAK",
    abbr=="LA" & yr < 2016 ~ "STL",
    abbr=="LAC" & yr < 2017 ~ "SD",
    TRUE ~ abbr
  )
}

nflfastr_source <- "Chart by @awgymer | Data from @nflfastR"

add_playtimes <- function(pbp){
  playtimes <- fread('data/play_realtimes.csv')
  pbp[playtimes, real_time:=i.real_time,on=c(nfl_api_id='game_id', play_id='play_id')]
} 

clean_weatherdata <- function(){
  gms <- fread('http://www.habitatring.com/games.csv')
  weatherdat <- fread('https://raw.githubusercontent.com/ThompsonJamesBliss/WeatherData/master/data/games_weather.csv')
  weathergms <- fread('https://raw.githubusercontent.com/ThompsonJamesBliss/WeatherData/master/data/games.csv')
  #Add TZoffset to the full weather data set
  weatherdat[weathergms, tzoffset:=i.TZOffset,on='game_id']
  #Join with Lee Sharpe's games CSV to get the new style game_id used by nflfastR
  weatherdat[gms, new_game_id:=i.game_id, on=c(game_id='old_game_id')]
  #Create a proper datetime column with the correct UTC offset adjustment 
  weatherdat[, utc_time:=lubridate::as_datetime(TimeMeasure, format='%m/%d/%Y %H:%M', tz='UTC')]
  # Closing brackets help with returning the correct result
  # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.html#why-do-i-have-to-type-dt-sometimes-twice-after-using-to-print-the-result-to-console
  weatherdat[, utc_time:=utc_time - lubridate::hours(tzoffset)][]
  return(weatherdat)
}

add_realtime_dt <- function(pbp){
  pbp[,utc_start:=lubridate::as_datetime(paste(game_date, start_time), format="%Y-%m-%d %H:%M:%S", tz='America/New_York')]
  # Convert valid real times to actual datetimes
  pbp[
    !is.na(real_time) & real_time != '', 
    real_datetime:=lubridate::as_datetime(paste(game_date, real_time), format="%Y-%m-%d %H:%M:%S", tz='UTC')
  ]
  # Fill the missing datetimes from the next or last available.
  # Seems to affect first and last plays of games so need to do both
  pbp[,real_datetime:=nafill(real_datetime, 'nocb'),by=game_id]
  pbp[,real_datetime:=nafill(real_datetime, 'locf'),by=game_id]
  # Adjust date forward one day if the real datetime is earlier than the real starttime
  # caused by games that span multiple calendar days in UTC time
  pbp[utc_start > real_datetime, real_datetime:=real_datetime + lubridate::days(1)]
}

combine_weatherdat <- function(pbp, weatherdat){
  # Do a rolling join and take the nearest time data from the weather data
  with_weather <- weatherdat[pbp, on=c(new_game_id='game_id', utc_time='real_datetime'), nomatch=NULL, roll='nearest']
  return(with_weather)
}