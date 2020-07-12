library(data.table)
library(glue)
library(nflfastR)
library(ggplot2)
library(plotly)
source('r-exts/nflfastr-helpers.R')

weekly_epa <- fread('data/weekly_epa_to_2019.csv')

weekly_epa[, cum_epa := cumsum(game_epa), by = 'joinid']
# Add which stop in their career this is for each QB
# Needed for making legend display nicer in Plotly
weekly_epa[, career_stop := rleid(team), by = joinid]
weekly_epa[, display_team := revert_to_old_abbr(team, season)]
weekly_epa[, label := glue_data(.SD, '{commname}\n{display_team}')]
weekly_epa[, player_game:=1:.N, by=commname]
weekly_epa[nplays > 0, player_game_played:=1:.N, by=commname]

team_cols <- as.data.table(teams_colors_logos)
weekly_epa[team_cols, c('col1', 'col2') := list(i.team_color, i.team_color2), on =
             .(team == team_abbr)]

player_plays <-
  weekly_epa[, .(
    totplays = sum(nplays),
    totpass = sum(ndropbacks),
    first_gkey = min(gkey),
    last_gkey = max(gkey)
  ), by = 'joinid,commname']

topplayers <- player_plays[(totplays > 1000 & totpass > 600) |
                             (totpass / totplays > 0.6 & totplays > 400 & first_gkey > 399),]

weekly_tops <- weekly_epa[topplayers, on = 'joinid']

p <- plot_ly() %>%
  add_trace(
    type = "scattergl",
    mode = "lines",
    line = list(shape = 'vh'),
    data = weekly_tops[career_stop == 1, ],
    x =  ~ gkey,
    y =  ~ cum_epa,
    color = ~ I(col1),
    text =  ~ label,
    hoverinfo = 'text',
    split =  ~ commname,
    legendgroup =  ~ commname
  )

for (i in seq(2, max(weekly_tops[['career_stop']]))) {
  p <- p %>%
    add_trace(
      type = "scattergl",
      mode = "lines",
      line = list(shape = 'vh'),
      data = weekly_tops[career_stop == i, ],
      x =  ~ gkey,
      y =  ~ cum_epa,
      color = ~ I(col1),
      text =  ~ label,
      hoverinfo = 'text',
      split =  ~ commname,
      legendgroup =  ~ commname,
      showlegend = F
    )
}

seasons <- unique(weekly_epa[['season']])
tickvals <- seq(1, max(weekly_epa[['gkey']]), 21)

p %>%
  layout(
    title = list(text = "Career Cumulative QB EPA (incl. playoffs)"),
    xaxis = list(title = list(text = "'NFL Seasons -->'"), tickvals = tickvals, ticktext = seasons),
    yaxis = list(title = list(text = "Cumulative QB EPA")),
    modebar = list(orientation = 'v')
  ) %>% 
  config(
    modeBarButtonsToRemove = c(
      'hoverCompareCartesian', 
      'hoverClosestCartesian', 
      'lasso2d', 
      'select2d', 
      'toggleSpikelines'
    )
  )


qbs_04_dat <-
  weekly_epa[commname %in% c('E.Manning', 'P.Rivers', 'M.Schaub', 'B.Roethlisberger')]
qbs_04 <- ggplot(qbs_04_dat,
                 aes(x = gkey, y = cum_epa, colour = commname)) +
  geom_step() +
  geom_label(data = qbs_04_dat[, .SD[.N], by = commname], aes(x = gkey, y =
                                                                cum_epa + 10, label = commname)) +
  ggthemes::scale_color_economist(guide = F) +
  labs(title = "Cumulative QB EPA (incl. Playoffs)",
       x = 'NFL Game Weeks -->', y = 'Cumulative QB EPA') +
  nflfastr_source +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

