library(ggplot2)
library(ggthemes)
library(nflscrapR)
library(scales)

nflscrapr_source <- labs(caption = "Graph by @awgymer\nData from @nflscrapR")

team_cols <- as.data.table(nflteams)
nfl_logos_url <- "https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv"
team_cols <- team_cols[fread(nfl_logos_url), on=c(abbr="team_code")]

THEME_COLS <- c(
  "DarkGrey" = "#3C3C3C",
  "MidGrey" = "#D2D2D2",
  "LightGrey" = "#FCFCFC",
  "MidnightGreen" = "#004953",
  "LightGreen" = "#195B64",
  "LighterGreen" = "#2f6b73",
  "GreyGreen" = "#669791",
  "Blue" = "#013369",
  "Red" = "#D50A0A", 
  "GreyBlue" = "#4D7096",
  "White" = "#FFFFFF"
)

### Eagles Theme and extra addons
theme_nfl_basic <- function(base_size = 12, base_family = "ArialMT") {
  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(
     line = element_line(colour = "black"),
     rect = element_rect(linetype = 0, colour = NA),
     text = element_text(colour = THEME_COLS["DarkGrey"]),
     axis.title = element_text(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.grid = element_line(colour = NULL),
     panel.grid.major =
       element_line(colour = THEME_COLS["MidGrey"]),
     panel.grid.minor = element_blank(),
     # unfortunately, can't mimic subtitles TODO!
     plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background = element_rect()))
}

theme_nfl <- function(base_size = 12, base_family = "ArialMT"){
  (theme_nfl_basic(base_size = base_size, base_family = base_family)
   + theme(
     line = element_line(colour = "black"),
     rect = element_rect(linetype = 0, colour = NA),
     text = element_text(colour = THEME_COLS["DarkGrey"]),
     axis.title = element_text(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.border = element_rect(linetype = 1 ,colour = THEME_COLS["DarkGrey"], fill = NA),
     panel.background = element_rect(fill = THEME_COLS["LightGrey"]),
     panel.grid = element_line(colour = NULL),
     panel.grid.major = element_line(colour = THEME_COLS["MidGrey"]),
     panel.grid.minor = element_blank(),
     # unfortunately, can't mimic subtitles TODO!
     plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", colour = THEME_COLS["Blue"]),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background = element_rect(linetype = 1, colour = THEME_COLS["DarkGrey"], fill = THEME_COLS["GreyBlue"]),
     strip.text = element_text(colour = THEME_COLS["White"], face = "bold")
   ))
}

theme_eagles <- function(base_size = 12, base_family = "ArialMT") {
  (theme_nfl_basic(base_size = base_size, base_family = base_family)
   + theme(
     line = element_line(colour = "black"),
     rect = element_rect(linetype = 0, colour = NA),
     text = element_text(colour = THEME_COLS["DarkGrey"]),
     axis.title = element_text(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.border = element_rect(linetype = 1 ,colour = THEME_COLS["DarkGrey"], fill = NA),
     panel.background = element_rect(fill = THEME_COLS["LightGrey"]),
     panel.grid = element_line(colour = NULL),
     panel.grid.major = element_line(colour = THEME_COLS["MidGrey"]),
     panel.grid.minor = element_blank(),
     # unfortunately, can't mimic subtitles TODO!
     plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold", colour = THEME_COLS["MidnightGreen"]),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background = element_rect(linetype = 1, colour = THEME_COLS["DarkGrey"], fill = THEME_COLS["GreyGreen"]),
     strip.text = element_text(colour = THEME_COLS["White"], face = "bold")
   ))
}

## Angle x-axis text
color_box_legend <- guide_legend(override.aes = list(shape = 15, size = 7))

side_legend <- theme(legend.position = "right", legend.direction = "vertical")

powerpoint_plot <- theme(
  plot.background = element_rect(fill='transparent'),
  legend.text = element_text(size=16), 
  legend.title = element_text(size=18), 
  axis.title = element_text(size=18),
  strip.text = element_text(size=14)
)

x_axis_angled <- function(x=305) {
  h = 0
  if(0 <= x & x < 180) {
    h = 1
  }
  theme(axis.text.x = element_text(angle = x, hjust = h))
}

qtrs_scale_x <- scale_x_reverse(
  breaks = seq(3600, 0, -300), 
  labels = c(
    "Q1", "10:00", "5:00",
    "Q2", "10:00", "5:00",
    "Q3", "10:00", "5:00",
    "Q4", "10:00", "5:00",
    "FINAL")
) 
