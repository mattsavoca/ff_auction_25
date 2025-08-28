# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(easypackages)
library(ffsimulator)
library(ffscrapr)
library(shinyjs)

easypackages::packages(
    'tidyverse',
    'nflreadr',
    'skimr',
    'ffpros',
    'ffopportunity',
    'ffsimulator',
    'plotly',
    'glue',
    'ggtext',
    'janitor'
)

# Source required files
source("adding_fp_ids_to_sleeper_avs_2025.R")

# CONSTANTS
CURRENT_SEASON = 2025
HISTORICAL_SEASONS = (CURRENT_SEASON-10):(CURRENT_SEASON-1)

# All custom functions from the original script
# (Including all the helper functions)

# Custom Functions | Methodology 1 -----
generate_ffsim_projection_summary = function(sims,
                                      ranks, # a converted underdog default ranks file (using `convert_[]_proj_to_ffsimulator()`)
                                      qb_baseline = 12, # Number of Starter QBs
                                      flex_baseline = 84, # Number of Starter FLEX
                                      te_boost = .33, #Positional Value boost to TE (As % of their FLEX baseline)
                                      spike_weight = .33, #Value to weigh players spike weeks vs. usable weeks
                                      sim_weight = .667, # Weight of Sim vs. ADP,
                                      TE_elite_rank = round(qb_baseline/2), #Rank of an elite TE week
                                      sig_digits = 1, # for rounding
                                      team_boosts = c("")
){
  sims %>%
    mutate(flex = if_else(pos == "QB", "QB", "FLEX"),
           flex_baseline = if_else(flex == "QB", qb_baseline, flex_baseline)) %>%
    group_by(pos, season, week) %>%
    mutate(weekly_pos_rank = min_rank(-projected_score),
           te_baseline_score = if_else(pos == "TE" & weekly_pos_rank <= TE_elite_rank, 1, 0)) %>%
    group_by(flex, season, week) %>%
    mutate(weekly_flex_rank = min_rank(-projected_score),
           weekly_max_flex_score = max(projected_score),
           good_score_baseline =  if_else(flex == "FLEX", nth(projected_score[flex=="FLEX"], 8*12, order_by = desc(projected_score[flex=="FLEX"])), quantile(projected_score,.95)), # Is player within 20% of top FLEX/QB score?
           great_score_baseline =  if_else(flex == "FLEX", nth(projected_score[flex=="FLEX"], 12, order_by = desc(projected_score[flex=="FLEX"])), quantile(projected_score,.975)) # Is player within 10% of top FLEX/QB score?
    ) %>%
    ungroup() %>%
    mutate(
      baseline_score = if_else(weekly_flex_rank <= flex_baseline, 1, 0),
      good_score = if_else(projected_score >= good_score_baseline, 1,0),
      great_score = if_else(projected_score >= great_score_baseline, 1,0)
    ) %>%
    group_by(fantasypros_id) %>%
    summarize(
      player = player[1],
      pos = pos[1],
      flex = flex[1],
      ecr = ecr[1],
      sd = sd[1],
      rank = rank[1],
      good_score = sum(good_score)/n(),
      great_score= sum(great_score)/n(),
      lo_range = quantile(projected_score, 0.25),
      mid_range = quantile(projected_score, 0.5),
      hi_range = quantile(projected_score, 0.75),
      weekly_flex_rank = quantile(weekly_flex_rank, .5),
      weekly_pos_rank = quantile(weekly_pos_rank, .5),
      baseline_score = mean(baseline_score),
      injury_rate = mean(gp_model),
      x_games = injury_rate * 16,
      te_baseline_score = mean(te_baseline_score, na.rm =T),
      adj_baseline_score = if_else(pos == "TE", baseline_score * (1+te_baseline_score * te_boost), baseline_score),
      usable_weeks = adj_baseline_score * x_games,
      spike_weeks = great_score * x_games,
      .groups = "drop"
    ) %>%
    left_join(ranks %>% select(fantasypros_id, team)) %>%
    mutate(
      usable_weeks = if_else(team %in% team_boosts & usable_weeks > 0, usable_weeks + 0.5, usable_weeks),
      spike_weeks = if_else(team %in% team_boosts & spike_weeks > 0, spike_weeks + 0.5, spike_weeks),
      usable_rank = min_rank(-usable_weeks),
      spike_rank = min_rank(-spike_weeks),
      sim_score = spike_rank * spike_weight + usable_rank * (1-spike_weight),
      sim_rank = min_rank(sim_score)
    ) %>%
    rename(player_id = fantasypros_id) %>%
    select(player, pos, team, player_id, everything(), -usable_rank, -spike_rank) %>%
    mutate_if(
      is.numeric, round, sig_digits
    ) %>%
    arrange(-usable_weeks) %>%
    group_by(pos) %>%
    mutate(sim_pos_rank = row_number()) %>%
    ungroup()
}

add_te_premium = function(ff_scoringhistory_df, te_bonus = 0.5){
    ff_scoringhistory_df %>%
    # ADDING TE PREMIUM
    mutate(points = case_when(pos == "TE" ~ points + (te_bonus * receptions), T ~ points))
}

sims_to_projection_values = function(league_projections_df = league_projections){
    league_projections_df %>%
    filter(projected_score > 0) %>% mutate(
        projected_score = replace_na(projected_score, 0),
        projected_score = if_else(projected_score < 0, 0, projected_score)) %>%
    arrange(-projected_score) %>%
    group_by(season, week) %>%
    mutate(
        sflex_baseline = nth(projected_score, superflex_starters, order_by=-projected_score),
        qb_baseline_score = nth(projected_score[pos == "QB"], QB_VBD_BASELINE, order_by=-projected_score[pos == "QB"]),
        flex_baseline_score = nth(projected_score[pos != "QB"], N_FLEX_STARTERS*N_LEAGUE_OWNERS, order_by=-projected_score[pos != "QB"]),
    ) %>% ungroup() %>%
    mutate(
        qb_baseline_score = replace_na(qb_baseline_score, mean(qb_baseline_score, na.rm = T)),
        sflex_value = projected_score-sflex_baseline,
        sflex_value = if_else(pos == "QB" & projected_score < qb_baseline_score, 0, sflex_value),
        sflex_value = if_else(pos != "QB" & projected_score < flex_baseline_score, 0, sflex_value),
        #sflex_value = if_else(sflex_value <= 0, projected_score/flex_baseline_score, sflex_value)
    )  %>%
    group_by(week, season) %>%
    mutate(
        sum_sflex_value = sum(sflex_value)
    ) %>%
    ungroup() %>%
    mutate(
        draftable = case_when(
    pos == "QB" & ecr < QB_BASELINE ~ 1,
    pos == "RB" & ecr < 66 ~ 1,
    pos == "WR" & ecr < 68 ~ 1,
    pos == "TE" & ecr < 18 ~ 1,
    T ~ 0),
        sflex_av = sflex_value/sum_sflex_value*2400) %>%
    group_by(fantasypros_id, player, pos) %>%
    summarize(
        draftable = max(draftable),
        usable_pct = sum(sflex_av >0)/n(),
        spike_pct = sum(sflex_av >=20)/n(),
        usable_wks = (usable_pct * 16/.5)*.5,
        spike_wks = (spike_pct * 16/.5)*.5,
        sflex_av_lo = quantile(sflex_av[sflex_av>0], .033, na.rm = T),
        sflex_av_mid = quantile(sflex_av[sflex_av>0], .5, na.rm = T),
        sflex_av_hi = quantile(sflex_av[sflex_av>0], .975, na.rm = T),
        .groups = "drop"
    )
}

projection_values_to_auction_values = function(league_projection_values_df = league_projection_values){
    league_projection_values_df %>%
        filter(draftable == 1) %>%
        #filter(draftable == 0 & pos != "QB") %>%
        mutate(
            usable_val = if_else(draftable == 1, usable_wks - min(usable_wks),0),
            spike_val = if_else(draftable == 1, spike_wks - min(spike_wks), 0),
            usable_av = usable_val/sum(usable_val)*2400,
            spike_av = spike_val/sum(spike_val)*2400,
        ) %>%
        arrange(-sflex_av_mid)
}

# Custom Functions | Methodology 2 (NEWER) -----
sims_to_sim_summary = function(league_projections_df = league_projections, late_rounder_cutoff = .33,
                              QB_VBD_BASELINE, N_FLEX_STARTERS, N_LEAGUE_OWNERS, N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB, TE_PREM_BONUS){
    ranks_df = league_projections_df %>%
        mutate(
            flex = if_else(pos %in% c("RB", "WR","TE"), 1, 0),
            projected_score = if_else(projected_score < 0, 0, projected_score)
        ) %>% 
        group_by(season, week, flex) %>%
        arrange(-projected_score) %>%
        mutate(weekly_flex_rank = row_number()) %>%
        ungroup()

    # Add usable_weeks and spike_weeks calculations similar to Methodology 1
    enhanced_ranks_df = ranks_df %>%
        group_by(pos, season, week) %>%
        mutate(weekly_pos_rank = min_rank(-projected_score),
               te_baseline_score = if_else(pos == "TE" & weekly_pos_rank <= round(QB_VBD_BASELINE/2), 1, 0)) %>%
        group_by(flex, season, week) %>%
        mutate(weekly_max_flex_score = max(projected_score),
               good_score_baseline = if_else(flex == 1, nth(projected_score[flex==1], 8*12, order_by = desc(projected_score[flex==1])), quantile(projected_score,.95)),
               great_score_baseline = if_else(flex == 1, nth(projected_score[flex==1], 12, order_by = desc(projected_score[flex==1])), quantile(projected_score,.975))) %>%
        ungroup() %>%
        mutate(
            baseline_score = if_else(flex == 1 & weekly_flex_rank <= N_FLEX_STARTERS*N_LEAGUE_OWNERS, 1, 
                            if_else(flex == 0 & weekly_flex_rank <= QB_VBD_BASELINE, 1, 0)),
            good_score = if_else(projected_score >= good_score_baseline, 1, 0),
            great_score = if_else(projected_score >= great_score_baseline, 1, 0)
        )

    rank_summary = enhanced_ranks_df %>% 
    group_by(fantasypros_id, player, pos, team, bye, flex, ecr) %>%
    summarize(
        mean_weekly_flex_rank = mean(weekly_flex_rank, na.rm = T), .groups = "drop") %>%
    arrange(mean_weekly_flex_rank) %>%
    mutate(
        flex_mean_projection_rank = row_number()
    ) %>%
    ungroup() %>%
    select(fantasypros_id, mean_weekly_flex_rank, flex_mean_projection_rank)


    proj_df =  enhanced_ranks_df %>%
    left_join(rank_summary) %>%
    group_by(fantasypros_id, player, pos, team, bye, flex, ecr) %>%
    mutate(
        g = n(),
        player_zeroes = sum(projected_score == 0)
    ) %>%
    ungroup()

    proj_df %>% filter(season == 1, week == 10) %>% arrange(flex_mean_projection_rank) %>% glimpse
    
    late_rounders = proj_df %>%
        filter(
            (flex == 1 & flex_mean_projection_rank > N_MULTIDOLLAR_FLEX) |
            (flex == 0 & flex_mean_projection_rank > N_MULTIDOLLAR_QB)
            ) %>%
        #distinct() %>%
        group_by(fantasypros_id, player, pos, team, bye, flex, ecr, g, player_zeroes) %>%
        summarize(
            lo_fpts = quantile(projected_score, .25),
            mid_fpts = quantile(projected_score, .5),
            hi_fpts = quantile(projected_score, .75),
            baseline_score = mean(baseline_score),
            great_score = mean(great_score),
            injury_rate = mean(gp_model),
            te_baseline_score = mean(te_baseline_score, na.rm = T),
            .groups = "drop"
        ) %>%
        mutate(
            x_games = injury_rate * 16,
            adj_baseline_score = if_else(pos == "TE", baseline_score * (1 + te_baseline_score * TE_PREM_BONUS), baseline_score),
            usable_weeks = adj_baseline_score * x_games,
            spike_weeks = great_score * x_games,
            late_rounder = 1
        )

    early_rounders = proj_df %>%
        filter(
            (flex == 1 & flex_mean_projection_rank <= N_MULTIDOLLAR_FLEX) |
            (flex == 0 & flex_mean_projection_rank <= N_MULTIDOLLAR_QB)
        ) %>%
        group_by(fantasypros_id, player, pos, team, bye, flex, ecr, g, player_zeroes) %>%
        filter(projected_score > 0) %>%
        summarize(
            lo_fpts = quantile(projected_score, .25),
            mid_fpts = quantile(projected_score, .5),
            hi_fpts = quantile(projected_score, .75),
            baseline_score = mean(baseline_score),
            great_score = mean(great_score),
            injury_rate = mean(gp_model),
            te_baseline_score = mean(te_baseline_score, na.rm = T),
            .groups = "drop"
        ) %>%
        mutate(
            x_games = injury_rate * 16,
            adj_baseline_score = if_else(pos == "TE", baseline_score * (1 + te_baseline_score * TE_PREM_BONUS), baseline_score),
            usable_weeks = adj_baseline_score * x_games,
            spike_weeks = great_score * x_games,
            late_rounder = 0
        )

    all_players = early_rounders %>%
        rbind(late_rounders)

    return(all_players)
}

sim_summary_to_sflex_auction_values = function(sim_summary_df, value_metric = mid_fpts, n_elite_qb = 4, .suffix = "", N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB, N_ROSTER_SIZE){

    superflex_values = sim_summary_df %>%
        mutate(
            sflex_baseline_score = nth({{value_metric}}, N_MULTIDOLLAR_FLEX + N_MULTIDOLLAR_QB, -{{value_metric}}),
            sflex_value = {{value_metric}} - sflex_baseline_score,
            sflex_value = if_else(sflex_value < 0, 0, sflex_value), 
            sflex_pct_of_max_value = sflex_value/max(sflex_value)
        )

    sflex_auction_metrics = superflex_values %>%
        mutate(
            sum_sflex_value = sum(sflex_value),
            sflex_max_av = sum_sflex_value/N_ROSTER_SIZE,
            sflex_av = sflex_pct_of_max_value*sflex_max_av,
            sflex_av = case_when(
                pos == "QB" & ecr > n_elite_qb ~ sflex_av * .5,
                T ~ sflex_av
            )
        ) %>%
        select(fantasypros_id, sflex_av) %>%
        rename_with(~ paste0(., .suffix), starts_with("sflex_"))
    
    return(sflex_auction_metrics)
}

sim_summary_to_flex_auction_values = function(sim_summary_df, value_metric = mid_fpts, .suffix = "", 
    N_EXP_FLEX, N_EXP_QB, N_ROSTER_SIZE, N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB, QB_VBD_BASELINE, N_LEAGUE_OWNERS, N_TEAM_BUDGET){
    flex_values = sim_summary_df %>%
        mutate(
            flex_baseline_score = case_when(
                flex == 1 ~ nth({{value_metric}}[flex == 1], N_MULTIDOLLAR_FLEX, -{{value_metric}}[flex == 1]),
                T ~ nth({{value_metric}}[flex == 0], if_else(N_MULTIDOLLAR_QB > 32, 32, N_MULTIDOLLAR_QB), -{{value_metric}}[flex ==0]) 
            ),
            flex_value = {{value_metric}} - flex_baseline_score,
            flex_value = if_else(
                flex == 0 & flex_value < nth(flex_value[flex == 0], if_else(N_MULTIDOLLAR_QB > 32, QB_VBD_BASELINE, N_MULTIDOLLAR_QB), order_by = -flex_value[flex==0]), 0,
                flex_value
                ),
            flex_value = if_else(flex_value > 0, flex_value, 0) # STEP 3
        ) %>%
        group_by(flex) %>%
        mutate(flex_pct_of_max_value = flex_value/max(flex_value)) %>%
        ungroup() # STEP 2) 
    
    flex_summary_metrics = flex_values %>%
        group_by(flex) %>%
        summarize(
            sum_flex_value = sum(flex_value) # STEP 4
        ) %>%
        mutate(
            share_of_total_value = sum_flex_value/sum(sum_flex_value), # STEP 5
            share_of_budget = share_of_total_value * N_LEAGUE_OWNERS * N_TEAM_BUDGET, # STEP 6 
            budget_sum = case_when( # STEP 7
                flex == 1 ~ N_EXP_FLEX * share_of_budget,
                T ~ N_EXP_QB * share_of_budget 
            ),
            total_budget_sum = sum(budget_sum), # STEP 8
            value_multiple = total_budget_sum / (N_LEAGUE_OWNERS * N_TEAM_BUDGET), # Step 9
            flex_max_av = sum_flex_value/value_multiple # Step 10
        )

    flex_auction_metrics = flex_values %>%
        left_join(flex_summary_metrics %>% select(flex, flex_max_av), by = "flex") %>%
        mutate(flex_av= flex_pct_of_max_value * flex_max_av) %>%
        select(fantasypros_id, flex_av) %>%
        rename_with(~ paste0(., .suffix), starts_with("flex_"))

    return(flex_auction_metrics)
}

sim_summary_to_qb_auction_values = function(sim_summary_df, value_metric = mid_fpts, .suffix = "", input_n_draftable_qbs, QB_VBD_BASELINE){
    
    # Filter to QBs only and calculate QB-specific VBD values
    qb_values = sim_summary_df %>%
        filter(pos == "QB") %>%
        mutate(
            qb_baseline_score = nth({{value_metric}}, QB_VBD_BASELINE, -{{value_metric}}),
            qb_value = {{value_metric}} - qb_baseline_score,
            qb_value = if_else(qb_value < 0, 0, qb_value),
            qb_pct_of_max_value = qb_value/max(qb_value, na.rm = T)
        )
    
    # Calculate QB auction values
    qb_auction_metrics = qb_values %>%
        mutate(
            sum_qb_value = sum(qb_value),
            qb_max_av = sum_qb_value/input_n_draftable_qbs,
            qb_av = qb_pct_of_max_value * qb_max_av
        ) %>%
        select(fantasypros_id, qb_av) %>%
        rename_with(~ paste0(., .suffix), starts_with("qb_"))
    
    # Create full dataset with NAs for non-QBs
    all_players_qb_values = sim_summary_df %>%
        select(fantasypros_id) %>%
        left_join(qb_auction_metrics, by = "fantasypros_id")
    
    return(all_players_qb_values)
}

map_auction_values = function(join_metric = "fantasypros_id", values_list){
  
  values_list = map(values_list, ~ select(.x, {{join_metric}}, everything()))
  # Join all data frames by the join_metric
  result_df = reduce(values_list, left_join, by = join_metric)
  
  return(result_df)
}

# Helper functions for creating multiple instances of variables
create_sleeper_avs_vars <- function(sleeper_avs_df, n = 1) {
  var_list <- list()
  
  for (i in 1:n) {
    if (i == 1) {
      list_name <- "sleeper_avs"
    } else {
      list_name <- paste0("sleeper_avs_", i)
    }
    
    var_list[[list_name]] <- sleeper_avs_df %>% select(fantasypros_id, av_sleeper = sleeper_av)
  }
  
  return(var_list)
}

create_ecr_av_vars <- function(league_sim_summary, n = 1) {
  var_list <- list()
  
  for (i in 1:n) {
    if (i == 1) {
      list_name <- "ecr_av"
      col_name <- "av_ecr"
    } else {
      list_name <- paste0("ecr_av_", i)
      col_name <- paste0("av_ecr_", i)
    }
    
    var_list[[list_name]] <- league_sim_summary %>% 
      ecr_ranks_to_previous_season_avs() %>% 
      select(fantasypros_id, !!sym(col_name) := ecr_to_prev_season_av)
  }
  
  return(var_list)
}

sim_summary_to_all_auction_values = function(sim_summary_df, auction_values_df, join_metric = "fantasypros_id"){
    joined = sim_summary_df %>%
        select(-g) %>%
        left_join(auction_values_df, by = {{join_metric}}) #%>%
    #arrange(-flex_av_mid_fpts) 
    
    mutated = joined %>%
    rowwise() %>%
    mutate(
        av_lo = quantile(c_across(contains("av_")), 0.25, na.rm = T),
        av_mid = quantile(c_across(contains("av_")), 0.67, na.rm = T),
        av_hi = case_when(
            pos == "QB" ~ quantile(c_across(contains("av_")), 0.75, na.rm = T),
            T ~ quantile(c_across(contains("av_")), 0.9, na.rm = T)
        ),
        av_sflex = median(c_across(contains("sflex_av_")), na.rm = T),
        av_flex = median(c_across(contains("flex_av_")), na.rm = T),
        av_qb = median(c_across(contains("qb_av_")), na.rm = T),
        av_max = max(c_across(contains("av_")), na.rm = T),
        across(contains("av_"), ~round(.x, 2)) 
    ) %>%
    ungroup() %>%
    mutate(
        av_hi = if_else(av_mid == 0 & av_max > 0, av_max, av_hi)
    ) %>%
    mutate(
        safety_score = case_when(
            av_mid == 0 & av_lo == 0 ~ 0,
            T ~ av_mid/(av_mid - av_lo)
        )
    )

    selected = mutated %>% 
    select(fantasypros_id, player, pos, team, bye, flex, ecr, player_zeroes, av_lo, av_mid, av_hi, safety_score, everything()) %>%
    arrange(-av_mid, safety_score, -av_hi, -hi_fpts)

    return(selected)
}

generate_draft_rounds <- function(df, ovr_rank_metric,team_num = 12) {
  df <- df %>%
    mutate(draft_round = ceiling({{ovr_rank_metric}} / team_num))
  return(df)
}

# Function to map ECR rankings to previous season auction values
ecr_ranks_to_previous_season_avs <- function(player_data, pick_db_path = "fffl_pick_db.csv") {
  
  # Load the historical pick database
  pick_db <- read_csv(pick_db_path, show_col_types = FALSE) %>%
    clean_names()
  
  # Filter for 2024 season data and create position rankings
  season_2024_avs <- pick_db %>%
    filter(season == 2024) %>%
    group_by(pos) %>%
    arrange(pos_rk) %>%
    mutate(
      # Extract numeric rank from pos_rk (e.g., "RB1" -> 1, "WR12" -> 12)
      ecr_rank = as.numeric(str_extract(pos_rk, "\\d+"))
    ) %>%
    ungroup() %>%
    select(pos, ecr_rank, historical_av = amt)
  
  # Create ECR position rankings for the input data
  player_data_with_ecr_ranks <- player_data %>%
    group_by(pos) %>%
    arrange(ecr) %>%
    mutate(ecr_pos_rank = row_number()) %>%
    ungroup()
  
  # Join with historical auction values
  result <- player_data_with_ecr_ranks %>%
    left_join(
      season_2024_avs, 
      by = c("pos" = "pos", "ecr_pos_rank" = "ecr_rank")
    ) %>%
    mutate(
      # Set historical_av to 0 if no match found (i.e., ECR rank not present in 2024 data)
      ecr_to_prev_season_av = if_else(is.na(historical_av), 0, historical_av)
    ) %>%
    select(-ecr_pos_rank, -historical_av)
  
  return(result)
}

create_draft_board_with_tooltip <- function(auction_values_data, teams = 12, rounds = 20, av_column = "av_mid") {
    
    position_colors <- c(
        "QB" = "#DC143C", 
        "RB" = "#1ABD1F", 
        "WR" = "#E9D525", 
        "TE" = "#45a6d3", 
        "DEF" = "#E0E0E0",
        "K" = "#E0E0E0"
    )
    
    total_players = teams * rounds
    
    # Prepare data for draft board
    draft_pool_data <- auction_values_data %>%
        ungroup() %>%
        rename(draft_av = all_of(av_column)) %>%
        filter(!is.na(draft_av)) %>%
        arrange(-draft_av)

    draft_pool <- draft_pool_data %>%
        mutate(
            overall_pick = row_number(),
            round = floor((overall_pick - 1) / teams) + 1,
            pick_in_round = (overall_pick - 1) %% teams + 1,
            draft_slot = if_else(round %% 2 == 1, pick_in_round, teams + 1 - pick_in_round)
        )

    draft_pool <- draft_pool %>%
        top_n(total_players, -overall_pick) %>%
        mutate(
            draft_av = pmax(0, draft_av),
            # Create comprehensive tooltip text
            tooltip_text = paste0(
                "<b>", player, "</b><br>",
                "<b>Position:</b> ", pos, " | <b>Team:</b> ", team, "<br>",
                if_else(is.na(age), "", paste0("<b>Age:</b> ", round(age, 1), " | ")),
                "<b>Bye:</b> ", bye, "<br><br>",
                "<b>Auction Values:</b><br>",
                "Sleeper AV: $", if_else(is.na(av_sleeper), "N/A", as.character(round(av_sleeper, 0))), "<br>",
                "Lo: $", round(av_lo, 0), " | ",
                "Mid: $", round(av_mid, 0), " | ",
                "Hi: $", round(av_hi, 0), "<br><br>",
                "<b>Usage & Performance:</b><br>",
                "Usable Weeks: ", round(usable_weeks, 1), "<br>",
                "Spike Weeks: ", round(spike_weeks, 1), "<br>",
                "Baseline Rate: ", round(baseline_score * 100, 1), "%<br>",
                "Great Score Rate: ", round(great_score * 100, 1), "%<br><br>",
                "<b>Fantasy Points Range:</b><br>",
                "Lo: ", round(lo_fpts, 1), " | ",
                "Mid: ", round(mid_fpts, 1), " | ",
                "Hi: ", round(hi_fpts, 1), "<br>",
                "<b>Injury Rate:</b> ", round((1-injury_rate) * 100, 1), "% games missed"
            ),
            player_label = paste0(player, "\n", pos, "\n", 
                                if_else(is.na(av_sleeper), "Sleeper: N/A", paste0("Sleeper: $", round(av_sleeper, 0))), "\n",
                                if_else(draft_av >= 1, paste0("$", round(draft_av, 0), "-$", round(av_hi, 0)), 
                                       paste0("$1 (", round(great_score * 100, 1), "%)")))
        )
    
    # Identify $1 players for thicker outline
    dollar_pool <- draft_pool %>%
        filter(draft_av < 1)
    
    draft_board_plot <- ggplot(draft_pool, aes(x = draft_slot, y = round, fill = pos, text = tooltip_text)) +
        geom_tile(color = "black", linewidth = 0.5, alpha = 0.7) +
        # Thicker outline for $1 players
        geom_tile(data = dollar_pool, color = "black", linewidth = 1.2, fill = NA) +
        geom_text(aes(label = player_label), 
                  size = 1.3,
                  family = "source sans pro", 
                  color = "black",
                  lineheight = 0.2) +
        scale_fill_manual(values = position_colors, na.value = "#E0E0E0") +
        scale_y_reverse(name = "Round", breaks = 1:rounds) +
        scale_x_continuous(name = "Pick", breaks = 1:teams, position = "top") +
        labs(
          title = paste(teams, "Team Draft Board"),
          caption = paste("Based on", av_column, "Auction Values from Simulations")
          ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text = element_text(face = "bold")
        ) 
    
    return(draft_board_plot)
}

create_custom_rank_draft_board <- function(
    league_auction_values,
    teams = 12,
    rounds = 20,
    av_column = "av_mid",
    custom_ranks_data
) {
    stopifnot(exists("sleeper_avs_df"))

    clean_name <- function(x) {
        x %>%
            stringr::str_replace_all("[\u2019\u2018]", "'") %>%
            stringr::str_replace_all("\\.", "") %>%
            stringr::str_replace_all("-", " ") %>%
            stringr::str_replace_all("\\b(Jr|Sr)\\.?\\b", "") %>%
            stringr::str_replace_all("\\b(II|III|IV)\\b", "") %>%
            stringr::str_squish() %>%
            stringr::str_to_lower()
    }

    # 1) Load custom ranks and parse posrk -> (pos, pos_rank)
    cr <- custom_ranks_data %>%
        dplyr::mutate(
            pos = toupper(pos),
            pos_from_posrk = stringr::str_extract(posrk, "^[A-Z]+"),
            pos_rank = suppressWarnings(as.integer(stringr::str_remove(posrk, "^[A-Z]+"))),
            slot_pos = dplyr::coalesce(pos_from_posrk, pos),
            name_key = clean_name(player)
        )

    # 2) Build SIM slot table: kth by position using the requested av metric
    av_col_sym <- rlang::sym(av_column)
    sim_slots <- league_auction_values %>%
        dplyr::ungroup() %>%
        dplyr::mutate(slot_av = !!av_col_sym) %>%
        dplyr::group_by(pos) %>%
        dplyr::arrange(dplyr::desc(slot_av), .by_group = TRUE) %>%
        dplyr::mutate(pos_rank = dplyr::row_number()) %>%
        dplyr::transmute(
            pos,
            pos_rank,
            draft_av = slot_av,
            av_lo = av_lo,
            av_mid = av_mid,
            av_hi = av_hi,
            usable_weeks = usable_weeks,
            spike_weeks = spike_weeks,
            injury_rate = injury_rate,
            bye = bye,
            lo_fpts = lo_fpts,
            mid_fpts = mid_fpts,
            hi_fpts = hi_fpts,
            baseline_score = baseline_score,
            slot_great_score = dplyr::coalesce(great_score, 0)
        ) %>%
        dplyr::ungroup()

    # 3) Build Sleeper mapping -> (fantasypros_id, av_sleeper, age) for actual players in custom ranks
    sleeper_keys <- sleeper_avs_df %>%
        dplyr::transmute(
            name_key = clean_name(player_name),
            pos,
            fantasypros_id = as.character(fantasypros_id),
            av_sleeper = sleeper_av,
            age = age
        ) %>%
        dplyr::distinct(name_key, pos, .keep_all = TRUE)

    # Prefer av_sleeper as stored in league_auction_values (same numbers, consistent source)
    sleeper_from_league <- league_auction_values %>%
        dplyr::select(fantasypros_id = fantasypros_id, av_sleeper_from_league = av_sleeper) %>%
        dplyr::mutate(fantasypros_id = as.character(fantasypros_id))

    # 4) Combine:
    # - Join custom ranks to SIM slots by (slot_pos, pos_rank)
    # - Attach fantasypros_id and sleeper AV for the actual custom player
    cr_enriched <- cr %>%
        dplyr::left_join(sim_slots, by = c("slot_pos" = "pos", "pos_rank" = "pos_rank")) %>%
        dplyr::left_join(sleeper_keys, by = c("name_key", "pos")) %>%
        dplyr::left_join(sleeper_from_league, by = "fantasypros_id") %>%
        dplyr::mutate(
            av_sleeper = dplyr::coalesce(av_sleeper_from_league, av_sleeper),
            great_score = dplyr::coalesce(slot_great_score, 0)
        ) %>%
        dplyr::select(player, pos, team, age, bye, draft_av, av_lo, av_mid, av_hi, av_sleeper, 
                     usable_weeks, spike_weeks, injury_rate, lo_fpts, mid_fpts, hi_fpts, 
                     baseline_score, great_score) %>%
        dplyr::filter(!is.na(draft_av)) %>%
        dplyr::arrange(dplyr::desc(draft_av))

    # 5) Re-use the existing plotting function with enhanced data
    create_draft_board_with_tooltip(
        auction_values_data = cr_enriched,
        teams = teams,
        rounds = rounds,
        av_column = "draft_av"
    )
}

# Main function
main_function = function(
    MY_LEAGUE, input_season_sims, input_te_prem, input_n_owners, input_league_budget, 
    input_n_players, input_n_flex_starters, input_n_draftable_qbs, input_n_starting_qbs,
    input_n_starting_rbs, input_n_starting_wrs, input_n_starting_tes
) {
    # Set up variables based on inputs
    HISTORICAL_SEASONS = (CURRENT_SEASON-10):(CURRENT_SEASON-1)
    FLEX_RK_MULTIDOLLAR_SHARE_MEAN = .805
    FLEX_RK_MULTIDOLLAR_SHARE_SD = .016
    QB_RK_MULTIDOLLAR_SHARE_MEAN = .967
    QB_RK_MULTIDOLLAR_SHARE_SD = .005
    MULTIDOLLAR_FLEX_SHARE = pmax(0, pmin(1, rnorm(1, FLEX_RK_MULTIDOLLAR_SHARE_MEAN, FLEX_RK_MULTIDOLLAR_SHARE_SD)))
    MULTIDOLLAR_QB_SHARE = pmax(0, pmin(1, rnorm(1, QB_RK_MULTIDOLLAR_SHARE_MEAN, QB_RK_MULTIDOLLAR_SHARE_SD)))
    SEASON_SIMS = input_season_sims
    SIM_WEEKS = 1:14
    MY_LEAGUE_POSITIONS = c("QB", "RB", "WR", "TE")
    N_LEAGUE_OWNERS = input_n_owners
    N_TEAM_BUDGET = input_league_budget
    N_ROSTER_SIZE = input_n_players
    N_FLEX_STARTERS = input_n_flex_starters
    N_QB_STARTERS = input_n_starting_qbs
    QB_VBD_BASELINE = N_QB_STARTERS * N_LEAGUE_OWNERS
    QB_BASELINE = if_else(input_n_draftable_qbs * N_LEAGUE_OWNERS > 36, 36, input_n_draftable_qbs * N_LEAGUE_OWNERS - 3)
    FLEX_BASELINE = (N_LEAGUE_OWNERS*N_ROSTER_SIZE)-QB_BASELINE
    N_MULTIDOLLAR_FLEX = floor(FLEX_BASELINE * MULTIDOLLAR_FLEX_SHARE)
    N_MULTIDOLLAR_QB = floor(QB_BASELINE * MULTIDOLLAR_QB_SHARE)
    N_DOLLAR_FLEX = FLEX_BASELINE - N_MULTIDOLLAR_FLEX
    N_DOLLAR_QB = N_LEAGUE_OWNERS * N_ROSTER_SIZE - N_MULTIDOLLAR_QB - N_MULTIDOLLAR_FLEX - N_DOLLAR_FLEX
    N_DOLLAR_QB = if_else(N_DOLLAR_QB < 0, 0, N_DOLLAR_QB)
    TE_PREM_BONUS = input_te_prem
    SIM_WEIGHT = 1
    TE_VBD_BONUS = 0
    N_ELITE_TE = 3
    N_ELITE_QB = 4
    N_RB_STARTERS = input_n_starting_rbs
    N_WR_STARTERS = input_n_starting_wrs
    N_TE_STARTERS = input_n_starting_tes
    
    # Get Current ADP
    current_adp = ffsimulator::ffs_latest_rankings() %>% mutate(bye = replace_na(bye, 0))

    # Connect to FFFL League
    league_connection = ffsimulator::sleeper_connect(season = CURRENT_SEASON, league_id = MY_LEAGUE)

    # Scoring History
    league_scoringhistory = ff_scoringhistory(conn = league_connection, season = HISTORICAL_SEASONS) %>%
        add_te_premium(TE_PREM_BONUS)

    # ADP Outcomes
    league_adp_outcomes = ffs_adp_outcomes(
        scoring_history = league_scoringhistory,
        gp_model = "simple",
        pos_filter = MY_LEAGUE_POSITIONS)
    
    # Projections
    league_projections = ffs_generate_projections(
        adp_outcomes = league_adp_outcomes,
        latest_rankings = current_adp,
        n_seasons = SEASON_SIMS,
        weeks = SIM_WEEKS)

    # Secondary Projection Summary
    league_sim_summary = league_projections %>% 
        sims_to_sim_summary(
            late_rounder_cutoff = .15,
            QB_VBD_BASELINE = QB_VBD_BASELINE,
            N_FLEX_STARTERS = N_FLEX_STARTERS,
            N_LEAGUE_OWNERS = N_LEAGUE_OWNERS,
            N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX,
            N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB,
            TE_PREM_BONUS = TE_PREM_BONUS
        )

    N_EXP_FLEX = N_ROSTER_SIZE - input_n_draftable_qbs + .2
    N_EXP_QB = input_n_draftable_qbs - 0.2

    league_values_list = c(
        list(
            sflex_lo_fpts = league_sim_summary %>% sim_summary_to_sflex_auction_values(lo_fpts, n_elite_qb = N_ELITE_QB, .suffix = "_lo_fpts", N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, N_ROSTER_SIZE = N_ROSTER_SIZE),
            sflex_mid_fpts = league_sim_summary %>% sim_summary_to_sflex_auction_values(mid_fpts, n_elite_qb = N_ELITE_QB, .suffix = "_mid_fpts", N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, N_ROSTER_SIZE = N_ROSTER_SIZE),
            sflex_hi_fpts = league_sim_summary %>% sim_summary_to_sflex_auction_values(hi_fpts, n_elite_qb = N_ELITE_QB, .suffix = "_hi_fpts", N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, N_ROSTER_SIZE = N_ROSTER_SIZE),
            flex_lo_fpts = league_sim_summary %>% sim_summary_to_flex_auction_values(lo_fpts, "_lo_fpts", 
                N_EXP_FLEX = N_EXP_FLEX, N_EXP_QB = N_EXP_QB, N_ROSTER_SIZE = N_ROSTER_SIZE, 
                N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, 
                QB_VBD_BASELINE = QB_VBD_BASELINE, N_LEAGUE_OWNERS = N_LEAGUE_OWNERS, N_TEAM_BUDGET = N_TEAM_BUDGET),
            flex_mid_fpts = league_sim_summary %>% sim_summary_to_flex_auction_values(mid_fpts, "_mid_fpts",
                N_EXP_FLEX = N_EXP_FLEX, N_EXP_QB = N_EXP_QB, N_ROSTER_SIZE = N_ROSTER_SIZE, 
                N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, 
                QB_VBD_BASELINE = QB_VBD_BASELINE, N_LEAGUE_OWNERS = N_LEAGUE_OWNERS, N_TEAM_BUDGET = N_TEAM_BUDGET),
            flex_hi_fpts = league_sim_summary %>% sim_summary_to_flex_auction_values(hi_fpts, "_hi_fpts",
                N_EXP_FLEX = N_EXP_FLEX, N_EXP_QB = N_EXP_QB, N_ROSTER_SIZE = N_ROSTER_SIZE, 
                N_MULTIDOLLAR_FLEX = N_MULTIDOLLAR_FLEX, N_MULTIDOLLAR_QB = N_MULTIDOLLAR_QB, 
                QB_VBD_BASELINE = QB_VBD_BASELINE, N_LEAGUE_OWNERS = N_LEAGUE_OWNERS, N_TEAM_BUDGET = N_TEAM_BUDGET),
            qb_lo_fpts = league_sim_summary %>% sim_summary_to_qb_auction_values(lo_fpts, "_lo_fpts", input_n_draftable_qbs = input_n_draftable_qbs, QB_VBD_BASELINE = QB_VBD_BASELINE),
            qb_mid_fpts = league_sim_summary %>% sim_summary_to_qb_auction_values(mid_fpts, "_mid_fpts", input_n_draftable_qbs = input_n_draftable_qbs, QB_VBD_BASELINE = QB_VBD_BASELINE),
            qb_hi_fpts = league_sim_summary %>% sim_summary_to_qb_auction_values(hi_fpts, "_hi_fpts", input_n_draftable_qbs = input_n_draftable_qbs, QB_VBD_BASELINE = QB_VBD_BASELINE)
        ),
        create_sleeper_avs_vars(sleeper_avs_df, n = 1),
        create_ecr_av_vars(league_sim_summary, n = 1)
    )

    league_auction_values_df = map_auction_values(values_list = league_values_list)

    league_auction_values = league_sim_summary %>% 
        sim_summary_to_all_auction_values(auction_values_df = league_auction_values_df) %>%
        rowwise() %>%
        mutate(
            player_avs = list(c_across(matches("^(flex_av|sflex_av)"))),
            av_vars = list(names(select(cur_data(), matches("^(flex_av|sflex_av)"))))
        ) %>%   
        ungroup() %>%
        select(
            -contains(c("sflex_av", "flex_av", "adj_", "zeroes"))
        ) %>%
        arrange(-av_mid, -spike_weeks, -usable_weeks) %>%
        mutate(
            my_rank = row_number()
        )

    return(league_auction_values)
}

# Define UI
ui <- fluidPage(
    useShinyjs(),
    
    # Custom CSS for collapsible sidebar
    tags$head(
        tags$style(HTML("
            .sidebar-collapsed {
                transform: translateX(-100%);
                transition: transform 0.3s ease;
            }
            
            .sidebar-expanded {
                transform: translateX(0%);
                transition: transform 0.3s ease;
            }
            
            .main-panel-expanded {
                margin-left: 0px !important;
                width: 100% !important;
                transition: all 0.3s ease;
            }
            
            .main-panel-collapsed {
                margin-left: 25% !important;
                width: 75% !important;
                transition: all 0.3s ease;
            }
            
            .toggle-btn {
                position: fixed;
                top: 20px;
                left: 20px;
                z-index: 1000;
                background-color: #007bff;
                color: white;
                border: none;
                border-radius: 4px;
                padding: 8px 12px;
                cursor: pointer;
                font-size: 14px;
                transition: all 0.3s ease;
            }
            
            .toggle-btn:hover {
                background-color: #0056b3;
            }
            
            .toggle-btn-collapsed {
                left: 20px;
            }
            
            .toggle-btn-expanded {
                left: calc(25% - 60px);
            }
            
            .sidebar-panel {
                position: fixed;
                left: 0;
                top: 0;
                height: 100vh;
                overflow-y: auto;
                background-color: #f8f9fa;
                border-right: 1px solid #dee2e6;
                z-index: 999;
            }
        "))
    ),
    
    # Custom JavaScript for sidebar toggle
    tags$script(HTML("
        $(document).ready(function() {
            // Initialize sidebar as expanded
            $('#sidebar-panel').addClass('sidebar-expanded');
            $('#main-panel').addClass('main-panel-collapsed');
            $('#toggle-btn').addClass('toggle-btn-expanded');
            
            $('#toggle-btn').click(function() {
                var sidebar = $('#sidebar-panel');
                var mainPanel = $('#main-panel');
                var toggleBtn = $('#toggle-btn');
                
                if (sidebar.hasClass('sidebar-expanded')) {
                    // Collapse sidebar
                    sidebar.removeClass('sidebar-expanded').addClass('sidebar-collapsed');
                    mainPanel.removeClass('main-panel-collapsed').addClass('main-panel-expanded');
                    toggleBtn.removeClass('toggle-btn-expanded').addClass('toggle-btn-collapsed');
                    toggleBtn.html('&#9658;'); // Right arrow
                } else {
                    // Expand sidebar
                    sidebar.removeClass('sidebar-collapsed').addClass('sidebar-expanded');
                    mainPanel.removeClass('main-panel-expanded').addClass('main-panel-collapsed');
                    toggleBtn.removeClass('toggle-btn-collapsed').addClass('toggle-btn-expanded');
                    toggleBtn.html('&#9664;'); // Left arrow
                }
            });
        });
    ")),
    
    titlePanel("FFFL Auction Sim"),
    
    # Toggle button
    tags$button(
        id = "toggle-btn",
        class = "toggle-btn",
        HTML("&#9664;") # Left arrow
    ),
    
    sidebarLayout(
        sidebarPanel(
            id = "sidebar-panel",
            class = "sidebar-panel",
            width = 3,
            
            # MY_LEAGUE input
            textInput("my_league", "MY_LEAGUE (Sleeper League ID):", value = "1249799557684940800"),
            
            # SLEEPER DRAFT ID input
            textInput("sleeper_draft_id", "SLEEPER DRAFT ID (for Live Auction):", value = "1266803000068816896"),
            
            # Separator
            hr(),
            
            # USER INPUTS section
            h4("League Settings"),
            
            numericInput("season_sims", "Number of Sims:", value = 25, min = 1, max = 100),
            numericInput("te_prem", "TE Premium Bonus:", value = 0.5, min = 0, max = 5, step = 0.1),
            numericInput("n_owners", "Number of Teams in League:", value = 12, min = 4, max = 20),
            numericInput("league_budget", "Auction Budget Per Team:", value = 200, min = 100, max = 500),
            numericInput("n_players", "Number of Players per Roster:", value = 14, min = 10, max = 25),
            numericInput("n_flex_starters", "Number of FLEX starters per team:", value = 7, min = 3, max = 12),
            numericInput("n_draftable_qbs", "Number of MAX QBs per team:", value = 3, min = 1, max = 6),
            numericInput("n_starting_qbs", "(1)QB or (2)QB League:", value = 2, min = 1, max = 3),
            numericInput("n_starting_rbs", "Number of RB starters per team:", value = 2, min = 1, max = 5),
            numericInput("n_starting_wrs", "Number of WR starters per team:", value = 2, min = 1, max = 5),
            numericInput("n_starting_tes", "Number of TE starters per team:", value = 1, min = 1, max = 3),
            
            # Separator
            hr(),
            
            # File upload
            h4("Custom Rankings"),
            fileInput("custom_ranks_file", "Upload custom_ranks.csv",
                     accept = c(".csv")),
            
            # Separator
            hr(),
            
            # Action buttons
            actionButton("reset_btn", "RESET", class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("start_btn", "START", class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("refresh_draft_btn", "REFRESH DRAFT", class = "btn-info", style = "width: 100%;")
        ),
        
        mainPanel(
            id = "main-panel",
            width = 9,
            
            # Loading indicator
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                div(
                    style = "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); z-index: 1000;",
                    div(
                        style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);",
                        h3("Processing...", style = "text-align: center; margin: 0;"),
                        br(),
                        div(class = "progress progress-striped active",
                            div(class = "progress-bar", style = "width: 100%")
                        )
                    )
                )
            ),
            
            # Error message area
            conditionalPanel(
                condition = "output.error_message",
                div(
                    style = "background-color: #f8d7da; color: #721c24; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                    h4("Error:"),
                    textOutput("error_message")
                )
            ),
            
            # Tabbed interface
            tabsetPanel(
                id = "main_tabs",
                
                # Draft Board Tab
                tabPanel(
                    "Draft Board",
                    value = "draft_board_tab",
                    
                    # Live draft hammer info
                    conditionalPanel(
                        condition = "output.draft_data_ready && output.franchise_data_available",
                        div(
                            style = "background-color: #d4edda; color: #155724; padding: 10px; border-radius: 5px; margin-bottom: 15px; border: 1px solid #c3e6cb;",
                            htmlOutput("hammer_info")
                        )
                    ),
                    
                    # Main plot output
                    conditionalPanel(
                        condition = "output.plot_ready",
                        plotlyOutput("draft_board_plot", height = "800px")
                    ),
                    
                    # Instructions when no plot is available
                    conditionalPanel(
                        condition = "!output.plot_ready",
                        div(
                            style = "text-align: center; margin-top: 100px;",
                            h3("Welcome to the Fantasy Football Draft Board Generator"),
                            p("1. Adjust your league settings in the sidebar"),
                            p("2. Optionally upload a custom_ranks.csv file"),
                            p("3. Click START to generate your draft board"),
                            br(),
                            p("The process may take several minutes depending on the number of simulations.")
                        )
                    )
                ),
                
                # Live Auction Tab
                tabPanel(
                    "Live Auction",
                    value = "live_auction_tab",
                    
                    br(),
                    
                    # Draft status info
                    conditionalPanel(
                        condition = "output.draft_data_ready",
                        
                        # Show franchise and position data only if available
                        conditionalPanel(
                            condition = "output.franchise_data_available",
                            fluidRow(
                                column(6,
                                    h4("Franchise Budgets"),
                                    DT::dataTableOutput("auction_franchise_budgets_table")
                                ),
                                column(6,
                                    h4("Position Spending"),
                                    DT::dataTableOutput("auction_position_spend_table")
                                )
                            ),
                            br()
                        ),
                        
                        # Always show draft log if we have draft data
                        fluidRow(
                            column(12,
                                h4("Draft Log"),
                                DT::dataTableOutput("draft_log_table")
                            )
                        )
                    ),
                    
                    # Instructions when no draft data is available
                    conditionalPanel(
                        condition = "!output.draft_data_ready",
                        div(
                            style = "text-align: center; margin-top: 100px;",
                            h3("Live Auction Tracker"),
                            p("1. Enter your Sleeper Draft ID in the sidebar"),
                            p("2. Click REFRESH DRAFT to load live auction data"),
                            br(),
                            p("Make sure your draft is currently active on Sleeper.")
                        )
                    )
                )
            )
        )
    )
)

# Define server logic
server <- function(input, output, session) {
    
    # Reactive values to store state
    values <- reactiveValues(
        plot_ready = FALSE,
        error_message = NULL,
        custom_ranks_data = NULL,
        league_auction_values = NULL,
        draft_data_ready = FALSE,
        my_draft = NULL,
        my_draftpicks = NULL,
        my_inprogress_draft = NULL,
        my_franchises = NULL,
        auction_franchise_budgets = NULL,
        auction_position_spend = NULL,
        draft_log_df = NULL,
        # Backup copies
        backup_my_draft = NULL,
        backup_my_draftpicks = NULL,
        backup_my_inprogress_draft = NULL,
        backup_my_franchises = NULL
    )
    
    # Handle file upload
    observeEvent(input$custom_ranks_file, {
        req(input$custom_ranks_file)
        
        tryCatch({
            values$custom_ranks_data <- read_csv(input$custom_ranks_file$datapath, show_col_types = FALSE)
            showNotification("Custom ranks file uploaded successfully!", type = "message")
        }, error = function(e) {
            values$error_message <- paste("Error reading custom ranks file:", e$message)
            showNotification("Error uploading file. Please check the format.", type = "error")
        })
    })
    
    # Reset button
    observeEvent(input$reset_btn, {
        values$plot_ready <- FALSE
        values$error_message <- NULL
        values$custom_ranks_data <- NULL
        values$league_auction_values <- NULL
        values$draft_data_ready <- FALSE
        values$my_draft <- NULL
        values$my_draftpicks <- NULL
        values$my_inprogress_draft <- NULL
        values$my_franchises <- NULL
        values$auction_franchise_budgets <- NULL
        values$auction_position_spend <- NULL
        values$draft_log_df <- NULL
        
        # Reset file input
        shinyjs::reset("custom_ranks_file")
        
        showNotification("All inputs have been reset.", type = "default")
    })
    
    # Function to create auction franchise budgets (from in_draft_testing.r logic)
    create_auction_franchise_budgets <- function(my_franchises, my_inprogress_draft, my_auction_budget = 200) {
        auction_franchises_df_init <- my_franchises %>%
            select(franchise_id, user_name) %>%
            mutate(initial_budget = my_auction_budget)
        
        auction_franchise_spend_df <- my_inprogress_draft %>%
            group_by(franchise_id) %>%
            summarize(spend = sum(auction_amount, na.rm = TRUE), .groups = "drop")
        
        auction_franchise_budgets <- auction_franchises_df_init %>%
            left_join(auction_franchise_spend_df, by = "franchise_id") %>%
            mutate(
                spend = replace_na(spend, 0),
                remaining_budget = initial_budget - spend,
                remaining_rank = rank(-remaining_budget, ties.method = "min"),
                hammer = if_else(remaining_rank == 1, TRUE, FALSE)
            ) %>%
            select(franchise_name = user_name, remaining_budget, hammer)
        
        return(auction_franchise_budgets)
    }
    
    # Function to create auction position spend (from in_draft_testing.r logic)
    create_auction_position_spend <- function(my_inprogress_draft, my_franchises) {
        auction_position_spend <- my_inprogress_draft %>%
            group_by(franchise_id, pos) %>%
            summarize(spend = sum(auction_amount, na.rm = TRUE), .groups = "drop") %>%
            pivot_wider(
                names_from = pos,
                values_from = spend,
                id_cols = franchise_id,
                values_fill = 0
            ) %>%
            left_join(my_franchises %>% select(franchise_id, user_name), by = "franchise_id") %>%
            select(franchise_name = user_name, everything(), -franchise_id)
        
        return(auction_position_spend)
    }
    
    # Function to create draft log (from in_draft_testing.r logic)
    create_draft_log <- function(my_inprogress_draft) {
        draft_log_df <- my_inprogress_draft %>%
            select(franchise = franchise_name, player = player_name, pos, team, av = auction_amount) %>%
            arrange(desc(av))
        
        return(draft_log_df)
    }
    
    # Refresh Draft button - fetch live auction data
    observeEvent(input$refresh_draft_btn, {
        
        # Reset previous state
        values$draft_data_ready <- FALSE
        values$error_message <- NULL
        
        # Validate inputs
        if (is.na(input$my_league) || input$my_league == "") {
            values$error_message <- "Please provide a valid MY_LEAGUE ID"
            return()
        }
        
        if (is.na(input$sleeper_draft_id) || input$sleeper_draft_id == "") {
            values$error_message <- "Please provide a valid SLEEPER DRAFT ID"
            return()
        }
        
        tryCatch({
            withProgress(message = 'Fetching draft data...', value = 0, {
                
                incProgress(0.2, detail = "Connecting to league...")
                
                # Create league connection using user's MY_LEAGUE input
                MY_CONN <- sleeper_connect(input$my_league, season = 2025)
                
                incProgress(0.4, detail = "Fetching draft info...")
                
                # Fetch draft data - with individual error handling
                my_draft <- tryCatch({
                    ff_draft(MY_CONN)
                }, error = function(e) {
                    message("Error in ff_draft: ", e$message)
                    NULL
                })
                
                my_draftpicks <- tryCatch({
                    ff_draftpicks(MY_CONN)
                }, error = function(e) {
                    message("Error in ff_draftpicks: ", e$message)
                    NULL
                })
                
                my_franchises <- tryCatch({
                    ff_franchises(MY_CONN)
                }, error = function(e) {
                    message("Error in ff_franchises: ", e$message)
                    NULL
                })
                
                incProgress(0.6, detail = "Fetching live auction data...")
                
                # Fetch live auction data using sleeper_draft_id
                my_inprogress_draft <- tryCatch({
                    sleeper_draft(input$sleeper_draft_id)
                }, error = function(e) {
                    message("Error in sleeper_draft: ", e$message)
                    stop("Failed to fetch live auction data: ", e$message)
                })
                
                incProgress(0.8, detail = "Processing auction data...")
                
                # Store data and create backups
                values$my_draft <- my_draft
                values$my_draftpicks <- my_draftpicks
                values$my_inprogress_draft <- my_inprogress_draft
                values$my_franchises <- my_franchises
                
                # Create backup copies
                values$backup_my_draft <- my_draft
                values$backup_my_draftpicks <- my_draftpicks
                values$backup_my_inprogress_draft <- my_inprogress_draft
                values$backup_my_franchises <- my_franchises
                
                # Create derived tables - only if we have the required data
                if (!is.null(my_franchises) && !is.null(my_inprogress_draft)) {
                    values$auction_franchise_budgets <- create_auction_franchise_budgets(
                        my_franchises, my_inprogress_draft, input$league_budget
                    )
                    values$auction_position_spend <- create_auction_position_spend(
                        my_inprogress_draft, my_franchises
                    )
                    values$draft_log_df <- create_draft_log(my_inprogress_draft)
                } else {
                    # If we don't have franchise data, we can still create a basic draft log
                    if (!is.null(my_inprogress_draft)) {
                        values$draft_log_df <- create_draft_log(my_inprogress_draft)
                    }
                }
                
                incProgress(1.0, detail = "Complete!")
            })
            
            values$draft_data_ready <- TRUE
            showNotification("Draft data refreshed successfully!", type = "message")
            
        }, error = function(e) {
            # Try to use backup data if available
            if (!is.null(values$backup_my_inprogress_draft)) {
                showNotification("Using backup data due to connection error.", type = "warning")
                
                values$my_draft <- values$backup_my_draft
                values$my_draftpicks <- values$backup_my_draftpicks
                values$my_inprogress_draft <- values$backup_my_inprogress_draft
                values$my_franchises <- values$backup_my_franchises
                
                # Recreate derived tables with backup data
                if (!is.null(values$backup_my_franchises)) {
                    values$auction_franchise_budgets <- create_auction_franchise_budgets(
                        values$backup_my_franchises, values$backup_my_inprogress_draft, input$league_budget
                    )
                    values$auction_position_spend <- create_auction_position_spend(
                        values$backup_my_inprogress_draft, values$backup_my_franchises
                    )
                }
                values$draft_log_df <- create_draft_log(values$backup_my_inprogress_draft)
                
                values$draft_data_ready <- TRUE
            } else {
                values$error_message <- paste("Error fetching draft data:", e$message)
                showNotification("Error occurred. Please check your inputs and try again.", type = "error")
            }
        })
    })
    
    # Start button - main processing
    observeEvent(input$start_btn, {
        
        # Reset previous state
        values$plot_ready <- FALSE
        values$error_message <- NULL
        
        tryCatch({
            
            # Validate inputs
            if (is.na(input$my_league) || input$my_league == "") {
                stop("Please provide a valid MY_LEAGUE ID")
            }
            
            # Run main function
            withProgress(message = 'Generating projections...', value = 0, {
                
                incProgress(0.1, detail = "Setting up league connection...")
                
                league_auction_values <- main_function(
                    MY_LEAGUE = input$my_league,
                    input_season_sims = input$season_sims,
                    input_te_prem = input$te_prem,
                    input_n_owners = input$n_owners,
                    input_league_budget = input$league_budget,
                    input_n_players = input$n_players,
                    input_n_flex_starters = input$n_flex_starters,
                    input_n_draftable_qbs = input$n_draftable_qbs,
                    input_n_starting_qbs = input$n_starting_qbs,
                    input_n_starting_rbs = input$n_starting_rbs,
                    input_n_starting_wrs = input$n_starting_wrs,
                    input_n_starting_tes = input$n_starting_tes
                )
                
                incProgress(0.8, detail = "Creating draft board...")
                
                values$league_auction_values <- league_auction_values
                
                incProgress(1.0, detail = "Complete!")
            })
            
            values$plot_ready <- TRUE
            showNotification("Draft board generated successfully!", type = "message")
            
        }, error = function(e) {
            values$error_message <- paste("Error generating draft board:", e$message)
            showNotification("Error occurred. Please check your inputs and try again.", type = "error")
        })
    })
    
    # Generate the plot
    output$draft_board_plot <- renderPlotly({
        req(values$plot_ready, values$league_auction_values)
        
        tryCatch({
            if (!is.null(values$custom_ranks_data)) {
                # Use custom ranks
                custom_board <- create_custom_rank_draft_board(
                    league_auction_values = values$league_auction_values,
                    teams = input$n_owners,
                    rounds = ceiling(input$n_players * 1.5),
                    av_column = "av_mid",
                    custom_ranks_data = values$custom_ranks_data
                )
                ggplotly(custom_board, tooltip = "text")
            } else {
                # Use regular draft board
                draft_board <- create_draft_board_with_tooltip(
                    auction_values_data = values$league_auction_values,
                    teams = input$n_owners,
                    rounds = ceiling(input$n_players * 1.5),
                    av_column = "av_mid"
                )
                ggplotly(draft_board, tooltip = "text")
            }
        }, error = function(e) {
            values$error_message <- paste("Error creating plot:", e$message)
            NULL
        })
    })
    
    # Render auction tables
    output$auction_franchise_budgets_table <- DT::renderDataTable({
        req(values$auction_franchise_budgets)
        
        DT::datatable(
            values$auction_franchise_budgets,
            options = list(
                pageLength = 15,
                scrollX = TRUE,
                autoWidth = TRUE
            ),
            rownames = FALSE
        ) %>%
        DT::formatCurrency(c("remaining_budget"), "$") %>%
        DT::formatStyle(
            "hammer",
            backgroundColor = DT::styleEqual(TRUE, "lightgreen")
        )
    })
    
    output$auction_position_spend_table <- DT::renderDataTable({
        req(values$auction_position_spend)
        
        # Get numeric columns (positions) for currency formatting
        numeric_cols <- values$auction_position_spend %>%
            select(where(is.numeric)) %>%
            names()
        
        DT::datatable(
            values$auction_position_spend,
            options = list(
                pageLength = 15,
                scrollX = TRUE,
                autoWidth = TRUE
            ),
            rownames = FALSE
        ) %>%
        DT::formatCurrency(numeric_cols, "$")
    })
    
    output$draft_log_table <- DT::renderDataTable({
        req(values$draft_log_df)
        
        DT::datatable(
            values$draft_log_df,
            options = list(
                pageLength = 25,
                scrollX = TRUE,
                autoWidth = TRUE,
                order = list(list(4, 'desc'))  # Sort by av (auction value) descending
            ),
            rownames = FALSE
        ) %>%
        DT::formatCurrency("av", "$")
    })
    
    # Generate hammer info text
    output$hammer_info <- renderText({
        req(values$auction_franchise_budgets)
        
        # Find teams with hammer = TRUE
        hammer_teams <- values$auction_franchise_budgets %>%
            filter(hammer == TRUE)
        
        if (nrow(hammer_teams) == 0) {
            return("<strong>🔨 HAMMER:</strong> No team currently has the hammer.")
        } else if (nrow(hammer_teams) == 1) {
            team_name <- hammer_teams$franchise_name[1]
            budget <- hammer_teams$remaining_budget[1]
            return(paste0("<strong>🔨 HAMMER:</strong> <strong>", team_name, "</strong> has the most remaining budget with <strong>$", 
                         format(budget, big.mark = ","), "</strong> left."))
        } else {
            # Multiple teams tied for hammer
            team_names <- paste(hammer_teams$franchise_name, collapse = ", ")
            budget <- hammer_teams$remaining_budget[1]  # Should be the same for all
            return(paste0("<strong>🔨 HAMMER:</strong> <strong>", team_names, "</strong> are tied with the most remaining budget at <strong>$", 
                         format(budget, big.mark = ","), "</strong> each."))
        }
    })
    
    # Output conditionals
    output$plot_ready <- reactive({
        values$plot_ready
    })
    
    output$draft_data_ready <- reactive({
        values$draft_data_ready
    })
    
    output$franchise_data_available <- reactive({
        !is.null(values$auction_franchise_budgets) && !is.null(values$auction_position_spend)
    })
    
    output$error_message <- renderText({
        values$error_message
    })
    
    outputOptions(output, "plot_ready", suspendWhenHidden = FALSE)
    outputOptions(output, "draft_data_ready", suspendWhenHidden = FALSE)
    outputOptions(output, "franchise_data_available", suspendWhenHidden = FALSE)
    outputOptions(output, "error_message", suspendWhenHidden = FALSE)
}

# Run the application
shinyApp(ui = ui, server = server)
