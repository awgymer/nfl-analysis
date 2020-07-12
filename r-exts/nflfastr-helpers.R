library(data.table)
library(glue)

get_pbp_from_db <- function(){
  connection <- DBI::dbConnect(RSQLite::SQLite(), "./db/pbp_db")
  pbp <- as.data.table(DBI::dbReadTable(connection, 'nflfastR_pbp'))
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

nflfastr_source <- ggplot2::labs(caption = "Chart by @awgymer | Data from @nflfastR")
