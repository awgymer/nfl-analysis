library(nflWAR)

league_replacement_functions <- list(
  "find_replacement_QB" = create_percentage_replacement_fn("Perc_Total_Plays", .1),
  "find_replacement_RB_rec" = create_league_replacement_fn(3, "RB", "Targets"), 
  "find_replacement_WR_rec" = create_league_replacement_fn(4, "WR", "Targets"),
  "find_replacement_TE_rec" = create_league_replacement_fn(2, "TE", "Targets"),
  "find_replacement_RB_rush" = create_league_replacement_fn(3, "RB", "Rush_Attempts"),
  "find_replacement_WR_TE_rush" = create_league_replacement_fn(1, "WR", "Rush_Attempts", combine_wrte = 1))


# Create the expected points based modula formulas:
ep_model_formula_list <- list(
  "air_formula" = as.formula(
    airEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
                    QBHit + Receiver_Position + PassLocation + Rush_EPA_Att + 
                    (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)
    ),
  "yac_formula" = as.formula(
    yacEPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
                    QBHit + AirYards*Receiver_Position + PassLocation + Rush_EPA_Att +
                    (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)
    ),
  "qb_rush_formula" = as.formula(
    EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
          (1|Rusher_ID_Name) + (1|DefensiveTeam)
    ),
  "main_rush_formula" = as.formula(
    EPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
          Rusher_Position + Pass_EPA_Att +
          (1|Team_Side_Gap) + (1|Rusher_ID_Name) + (1|DefensiveTeam)
    )
)

# Create the win probability based modula formulas:
wp_model_formula_list <- list(
  "air_formula" = as.formula(
    airWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                    Receiver_Position + PassLocation + Rush_EPA_Att +
                    (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)
    ),
  "yac_formula" = as.formula(
    yacWPA_Result ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + QBHit + 
                    AirYards*Receiver_Position + PassLocation + Rush_EPA_Att +
                    (1|Passer_ID_Name) + (1|Receiver_ID_Name) + (1|DefensiveTeam)
    ),
  "qb_rush_formula" = as.formula(
    WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + Pass_EPA_Att +
          (1|Rusher_ID_Name) + (1|DefensiveTeam)
    ),
  "main_rush_formula" = as.formula(
    WPA ~ Home_Ind + Shotgun_Ind + No_Huddle_Ind + 
          Rusher_Position + Pass_EPA_Att +
          (1|Team_Side_Gap) + (1|Rusher_ID_Name) + (1|DefensiveTeam)
    )
)


warlist <- list()

walk(c(2009:2017), function(x) {
  season_results <- x %>% 
    get_pbp_data() %>%
    add_positions(x) %>%
    add_model_variables() %>%
    prepare_model_data() %>%
    add_position_tables() %>%
    find_positional_replacement_level(league_replacement_functions) %>%
    estimate_player_value_added(ep_model_formula_list) %>%
    calculate_above_replacement() %>%
    convert_points_to_wins(calculate_points_per_win(x))
  
  warlist[[x]] <- season_results
  #saveRDS(season_results, file = paste("epa_model_results_", as.character(x), ".rds", sep = ""))
})

generate_war_results <- . %>%
  resample_season(drive_level = 1) %>%
  prepare_model_data() %>%
  add_position_tables() %>%
  add_replacement_level_sim(epa_war_2017) %>%
  join_position_statistics() %>%
  estimate_player_value_added(wp_model_formula_list, return_models = 0) %>%
  calculate_above_replacement() %>%
  convert_prob_to_wins()
