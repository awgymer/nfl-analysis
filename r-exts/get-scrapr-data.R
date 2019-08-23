source('./nflscrapr_extra.R')

## Save the play-by-play data for regular season
reg_pbp_f <- save_pbp_data(2009, 2018, 'reg', '../data/')

## Save the play-by-play data for post-season
post_pbp_f <- save_pbp_data(2009, 2018, 'post', '../data/')

## Merge reg season with PFR metadata
reg_pbp <- fread(reg_pbp_f)
pfr_meta <- fread('../data/pfr_game_meta.csv')
merge_with_pfr(reg_pbp, pfr_meta, 'reg', '../data/')