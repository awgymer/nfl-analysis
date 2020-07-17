library(data.table)
library(glue)
source('r-exts/nflfastr-helpers.R')
source('r-exts/nfl_themes.R')

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

pbp <- get_pbp_from_db(2011)

add_playtimes(pbp)
add_realtime_dt(pbp)
weatherdat <- clean_weatherdata()
pbp <- combine_weatherdat(pbp, weatherdat)

pbp <- pbp[pbp[,.(missing=any(is.na(Precipitation))), by=new_game_id][missing==F], on='new_game_id']

beaufort_bins <- c(0, 1.5, 3.5, 7.5, 12.5, 18.5, 24.5, 31.5, 38.5, 46.5)
beaufort_labs <- c(
  'Calm', 'Light Air', 'Light Breeze', 'Gentle Breeze', 'Moderate Breeze', 
  'Fresh Breeze', 'Strong Breeze', 'Near Gale', 'Gale'
)
precip_bins <- c(-Inf, 0, 0.098, 0.3, 2)
precip_labs <- c('None', 'Light', 'Moderate', 'Heavy')
temp_bins <- c(-Inf, 32, 50, 60, 70, 85, 95, Inf)
temp_labs <- c('Freezing', 'Cold', 'Cool', 'Mild', 'Warm', 'Very Warm', 'Hot')
# Bin the windspeeds and temps and also simplify the precipitation bins
pbp[, beaufort_scale_labs:=cut(WindSpeed, beaufort_bins, labels=beaufort_labs, include.lowest=T, ordered_result=T)]
pbp[, beaufort:=cut(WindSpeed, beaufort_bins, include.lowest=T, ordered_result=T)]
pbp[, precip_scale_labs:=cut(Precipitation, precip_bins, labels=precip_labs, include.lowest = T, ordered_result = T)]
pbp[, precip_scale:=cut(Precipitation, precip_bins, include.lowest = T, ordered_result = T)]
pbp[, temp_scale_labs:=cut(Temperature, temp_bins, labels=temp_labs, include.lowest=T, ordered_result=T)]
pbp[, temp_scale:=cut(Temperature, temp_bins, include.lowest=T, ordered_result=T)]

check_weather <- pbp[,
  .(
    wind_mean = mean(WindSpeed),
    wind_med = median(WindSpeed),
    wind_mode = getmode(beaufort_scale_labs),
    precip_mean = mean(Precipitation),
    precip_med = median(Precipitation),
    precip_mode = getmode(precip_scale_labs),
    temp_mean = mean(Temperature),
    temp_med = median(Temperature),
    temp_mode = getmode(temp_scale_labs)
  )
  ,by=new_game_id]

check_weather[, 
              c('wind_mean', 'wind_med', 'precip_mean', 'precip_med', 'temp_mean', 'temp_med') := 
                .(cut(wind_mean, beaufort_bins, labels=beaufort_labs, include.lowest=T, ordered_result = T), 
                  cut(wind_med, beaufort_bins, labels=beaufort_labs, include.lowest=T, ordered_result = T),
                  cut(precip_mean, precip_bins, labels=precip_labs, include.lowest=T, ordered_result = T),
                  cut(precip_med, precip_bins, labels=precip_labs, include.lowest=T, ordered_result = T),
                  cut(temp_mean, temp_bins, labels=temp_labs, include.lowest=T, ordered_result = T),
                  cut(temp_med, temp_bins, labels=temp_labs, include.lowest=T, ordered_result = T)
                  )
              ]
multiple_weathers <-  check_weather[
  ,.(
    wind=length(unique(c(wind_mean, wind_med, wind_mode))), 
    precip=length(unique(c(precip_mean, precip_med, precip_mode))),
    temp=length(unique(c(temp_mean, temp_med, temp_mode)))
    ),
  by=new_game_id
]

weather_gms <- pbp[,
                 .(wind_labs=getmode(beaufort_scale_labs),
                   wind=getmode(beaufort), 
                   temp_labs=getmode(temp_scale_labs),
                   temp=getmode(temp_scale),
                   rain_labs=getmode(precip_scale_labs),
                   rain=getmode(precip_scale),
                   outdoors=roof[1] %in% c('outdoors', 'open')
                  ),
                by=new_game_id]

weather_gms[,outdoors_labs := ifelse(outdoors==T, 'Outdoor/Open Roof', 'Dome/Closed Roof')]
gms <- fread('http://www.habitatring.com/games.csv')
gms[,c('wind', 'temp'):=NULL]
weather_gms <- gms[weather_gms, on=.(game_id=new_game_id)]

weather_gms[, total_diff := total-total_line]
weather_gms[, total_result := ifelse(total_diff<0, 'under', ifelse(total_diff>0, 'over', 'push'))]
weather_gms[, total_result := factor(total_result, levels=c('over', 'under', 'push'))]
weather_gms[, under_profits := mapply(bet_returns, under_odds, 1, (total_result=='under'))]
weather_gms[total_result == 'push', under_profits := 0]

bet_returns <- function(odds, stake, win){
  if(!win){
    return(-stake)
  }
  if(odds > 0){
    returns <- (odds * (stake/100))
  }
  else if(odds < 0){
    returns <- ((100/odds) * stake * -1)
  }
  else {returns <- NA}
  return(returns)
}

betting_result_fill <- scale_fill_manual(
  values = c('over'=THEME_COLS[['Blue']], 'under'=THEME_COLS[['Red']], 'push'=THEME_COLS[['MidGrey']]),
  labels = c('over'='Over', 'under'='Under', 'push'='Push')
)

betting_x_scale <- scale_x_discrete(
  labels = c('over'='Over', 'under'='Under', 'push'='Push')
)

io_grp_sizes <- weather_gms[,.(label=glue_data(.SD, 'N = {.N}')), by=outdoors_labs]
indoor_outdoor <- ggplot(
  weather_gms, aes(x=total_result)
  ) + 
  geom_bar(aes(fill=total_result)) +
  geom_text(data=io_grp_sizes, aes(x='push', y=weather_gms[,.N,by='outdoors,total_result'][,max(N)], label=label)) +
  labs(
    title = 'Over/Under Results by Stadium Type',
    subtitle = '2108 Games since 2011 with full PBP weather available',
    x = '', y = 'Games', caption = glue('{nflfastr_source}\n Weather Data: @DataWithBliss | Betting Data: @LeeSharpeNFL')
  ) +
  theme_nfl() + 
  theme(panel.grid.major.x = element_blank()) + 
  facet_grid(.~outdoors_labs) + 
  betting_x_scale + 
  betting_result_fill +
  guides(fill=F)

precip_labller <- function(x){ glue('Precipitation/mm\n{x}') }
wind_labller <- function(x){ glue('Wind/mph\n{x}') }
temp_labller <- function(x){ glue('Temp/ºF\n{x}') }

ggplot(
  weather_gms[outdoors==T], aes(x=total_result, fill=total_result)
  ) + 
  geom_bar() + 
  labs(
    title = 'Over/Under Results by Precipitation and Field Type',
    subtitle = '1556 Outdoor Games since 2011 with full PBP weather available',
    x = '', y = 'Games', caption = glue('{nflfastr_source}\n Weather Data: @DataWithBliss | Betting Data: @LeeSharpeNFL')
  ) +
  theme_bw() + 
  facet_grid(rain~surface, labeller = labeller(rain = precip_labller)) + 
  betting_result_fill + 
  guides(fill=F)



### Not used ###
#opening_tots <- fread('data/opening_totals_from_07.csv')
#points_df[opening_tots, opening_total_line:=i.opening_total_line,on=.(new_game_id=game_id)]
# Manually correct TB v MIA 2017 (Game got moved, ID is bugged)
#points_df[new_game_id=='2017_11_TB_MIA', opening_total_line:=40]
#points_df[, opening_total_diff:=total-opening_total_line]