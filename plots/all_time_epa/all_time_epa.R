library(data.table)
library(plotly)
library(glue)
source('r-exts/nflfastr-helpers.R')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

twoids <- fread('data/two_name_ids.csv')
uniq_names <- fread('data/unique_names.csv')

pbp <- get_pbp_from_db()

pbp[twoids, joinid:=i.grpid, on=c(id="id")]
pbp[is.na(joinid), joinid:=id]
# Clean up players who have differing versions of their name
pbp[!is.na(joinid), commname:=getmode(name), by='joinid']
pbp[uniq_names, commname:=i.uniq_name, on='joinid']

weekly_epa <- pbp[
  !is.na(joinid), 
  .(
    game_epa=sum(qb_epa, na.rm=T), 
    nplays=.N,
    ndropbacks=sum(pass),
    team=getmode(posteam)
  ), 
  by='joinid,commname,season,week'
]

all_weeks <- CJ(pbp[['season']], pbp[['week']], unique=T)
setnames(all_weeks, c('V1', 'V2'), c('season', 'week'))
setorder(all_weeks, season, week)
all_weeks[, gkey := .I]

setkey(weekly_epa, season, week)
setkey(all_weeks, season, week)
weekly_epa[all_weeks, gkey:=i.gkey]

weekly_epa <- weekly_epa[,.SD[all_weeks, on='gkey', roll=T, rollends=F], by='commname,joinid']
weekly_epa[(i.week != week | i.season != season),
           c('season', 'week', 'game_epa', 'nplays', 'ndropbacks') := list(i.season, i.week, 0L, 0L, 0L)]
weekly_epa[, c('i.week', 'i.season') := NULL]
weekly_epa <- weekly_epa[!is.na(nplays)]

fwrite(weekly_epa, 'data/weekly_epa_to_2019.csv')

