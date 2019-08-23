library(data.table)
library(stringr)
library(na.tools)

URL_PART <- list(
  "reg" = "regular_season/reg",
  "pre" = "pre_season/pre",
  "post" = "post_season/post"
)

save_pbp_data <- function(first_szn, last_szn, szn_type=c("reg", "pre", "post"), path=NULL){
  match.arg(szn_type)
  datalist = list()
  i <- 1
  for (yr in first_szn:last_szn) {
    pbp <- fread(paste0(
      "https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/",
      URL_PART[[szn_type]], "_pbp_", yr, ".csv")
    )
    games <- fread(paste0(
      "https://github.com/ryurko/nflscrapR-data/raw/master/games_data/",
      URL_PART[[szn_type]], "_games_", yr, ".csv")
    )
    games[home_score != away_score, winner:=ifelse(home_score > away_score, home_team, away_team),]
    pbp <- unique(games[, .(game_id, week, season, winner)])[pbp, on='game_id']
    pbp[,season_type := szn_type]
    datalist[[i]] <- pbp # add it to your list
    i <- i+1
  }
  pbp_all <- rbindlist(datalist, fill=TRUE)
  clean_team_abbr(pbp_all, JAX = 'JAC', STL = 'LA', SD = 'LAC')
  add_penalty_play_info(pbp_all)
  outpath <- paste0(
    path,
    first_szn, "_",
    last_szn, "_",
    szn_type,
    "_pbp_all.csv"
  )
  fwrite(pbp_all, file=outpath)
  return(outpath)
}

save_roster_data <- function(first_szn, last_szn, szn_type=c("reg", "pre", "post"), path=NULL){
  match.arg(szn_type)
  datalist = list()
  i <- 1
  for (yr in first_szn:last_szn) {
    roster <- fread(paste0(
      "https://github.com/ryurko/nflscrapR-data/raw/master/roster_data/",
      URL_PART[[szn_type]], "_roster_", yr, ".csv")
    )
    datalist[[i]] <- roster # add it to your list
    i <- i+1
  }
  roster_all <- rbindlist(datalist, fill=TRUE)
  outpath <- paste0(
    path,
    first_szn, "_",
    last_szn, "_",
    szn_type,
    "_roster_all.rds"
  )
  fwrite(roster_all, file=outpath)
  return(outpath)
}

load_dt_data <- function(filename){
  dt <- readRDS(filename)
  alloc.col(dt)
  return(dt)
}

clean_team_abbr <- function(df, ...){
  tm_cols <-c("home_team", "away_team", "posteam", "defteam")
  df[, (tm_cols) := lapply(.SD, dplyr::recode, ...), .SDcols = tm_cols]
}

add_penalty_play_info <- function(df){
  df[,`:=` (
    pass = ifelse(grepl("( pass)|(sacked)|(scramble)", desc), 1, 0),
    rush = ifelse(
      grepl("(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)", desc)
      & !grepl("(pass)|(sacked)|(scramble)", desc), 1, 0),
    success = ifelse(epa>0, 1 , 0)
  )
  ][,
    `:=` (
      passer_player_name = ifelse(play_type == "no_play" & pass == 1,
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                  passer_player_name),
      receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"),
                                    str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                    receiver_player_name),
      rusher_player_name = ifelse(play_type == "no_play" & rush == 1,
                                  str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                  rusher_player_name)
    )][passer_player_name == '' & qb_scramble == 1,
        passer_player_name := str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s(scrambles))")
    ]
}

filt_run_pass <- function(df){
  df <- df[!is_na(epa) & play_type %in% c("no_play", "pass", "run"),]
  return(df[pass==1 | rush==1,])
}

merge_with_pfr <- function(scrapr_df, pfr_df, szn_type, path=NULL){
  df <- scrapr_df[
    pfr_df,
    on=c(
      away_team='away_scrapr',
      home_team='home_scrapr',
      week='week',
      season='season'),
    nomatch=NULL
  ]
  first_yr <- min(df[,season])
  last_yr <- max(df[,season])
  outpath <- paste0(
    path,
    first_yr, "_",
    last_yr, "_",
    szn_type,
    "_pbp_with_meta.csv"
  )
  fwrite(df, file=outpath)
  return(outpath)
}

