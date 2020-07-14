library(data.table)
library(glue)
source('r-exts/nflfastr-helpers.R')
source('r-exts/nfl_themes.R')

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

pbp <- pbp[season >= 2011]

add_playtimes(pbp)
add_realtime_dt(pbp)
weatherdat <- clean_weatherdata()
pbp <- combine_weatherdat(pbp, weatherdat)

# Bin the windspeeds
pbp[, windspeed_bin:=cut(WindSpeed, seq(min(WindSpeed), max(WindSpeed)+2, 2), include.lowest=T, ordered_result=T)]
pbp[,.N,by=.(windspeed_bin,pass)][order(windspeed_bin)]


windspeed_pass_ratio_plt <- (
  ggplot(pbp[roof %in% c('open', 'outdoors') & (pass==1|rush==1), .(pass_ratio=sum(pass)/.N), by=windspeed_bin], 
         aes(x=windspeed_bin, y=pass_ratio)) + 
    geom_bar(stat='identity', fill=THEME_COLS['Blue']) +
    geom_hline(
      yintercept = pbp[roof %in% c('dome', 'closed') & (pass==1|rush==1), sum(pass)/.N], 
      colour=THEME_COLS['Red'], linetype=2
    ) +
    labs(
      title = 'Pass-Run Ratio by Windspeed',
      subtitle = "All games since 2011 | Roof is 'outdoors' or 'open' | Red line is mean pass ratio in domes",
      x = 'Windspeed / mph', y = 'Passing Ratio',
      caption = glue('{nflfastr_source} | Weather Data: @DataWithBliss')
    ) +
    scale_x_discrete(drop = F) +
    theme_nfl() + x_axis_angled()
)
