remotes::install_github("ffverse/ffscrapr")

library(ffscrapr)
library(tidyverse)
library(janitor)

temp_fffl_league_id = "1266802528331251712"
fffl_league_id = "1249799557684940800"
MY_LEAGUE <- temp_fffl_league_id
CURRENT_SEASON <- 2025
MY_CONN <- sleeper_connect(MY_LEAGUE, season = CURRENT_SEASON)
MY_DRAFT <- ff_draft(MY_CONN)
MY_DRAFTPICKS <- ff_draftpicks(MY_CONN)
MY_INPROGRESS_DRAFT <-sleeper_draft("1266803000068816896")
MY_FRANCHISES <- ff_franchises(MY_CONN)
MY_AUCTION_BUDGET <- 200
MY_INPROGRESS_DRAFT %>% glimpse()


auction_franchises_df_init = MY_FRANCHISES %>%
    select(
        franchise_id, user_name
    ) %>%
    mutate(
        initial_budget = MY_AUCTION_BUDGET
    )


auction_franchise_spend_df = MY_INPROGRESS_DRAFT %>%
    group_by(franchise_id) %>%
    summarize(
        spend = sum(auction_amount),
    )


auction_franchise_budgets = auction_franchises_df_init %>%
    left_join(auction_franchise_spend_df, by = "franchise_id") %>%
    mutate(
        player_count = n(),
        spend = replace_na(spend, 0),
        remaining_budget = initial_budget - spend,
        remaining_rank = rank(-remaining_budget, ties.method = "min"),
        hammer = if_else(remaining_rank == 1, TRUE, FALSE),
        .groups = "drop"
    )


auction_position_spend =  MY_INPROGRESS_DRAFT %>%
    group_by(franchise_id, pos) %>%
    summarize(
        spend = sum(auction_amount),
        .groups = "drop"
    ) %>%
    pivot_wider(
        names_from = pos,
        values_from = spend,
        id_cols = franchise_id,
        values_fill = 0
    ) %>%
    left_join(auction_franchises_df_init %>% select(franchise_id, user_name), by = "franchise_id")



draft_log_df = MY_INPROGRESS_DRAFT %>% 
    select(franchise = franchise_name, player = player_name, pos, team, av = auction_amount)
