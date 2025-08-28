library(tidyverse)
library(ffscrapr)
library(jsonlite)
library(ggtext)
library(glue)
library(httr)
library(googlesheets4)
library(rlang)

CURRENT_SEASON = 2025


create_sleeper_draft_board <- function(sleeper_avs_data, teams = 12, rounds = 20) {
    
    total_players = teams * rounds
    
    draft_pool <- sleeper_avs_data %>%
        dplyr::slice_head(n = total_players) %>%
        dplyr::mutate(overall_pick = dplyr::row_number()) %>%
        dplyr::mutate(
            round = floor((.data$overall_pick - 1) / teams) + 1,
            pick_in_round = (.data$overall_pick - 1) %% teams + 1
        ) %>%
        dplyr::mutate(
            draft_slot = dplyr::if_else(.data$round %% 2 == 1, .data$pick_in_round, teams + 1 - .data$pick_in_round)
        )
    
    position_colors <- c(
        "QB" = "#DC143C", 
        "RB" = "#1ABD1F", 
        "WR" = "#E9D525", 
        "TE" = "#45a6d3", 
        "DEF" = "#E0E0E0",
        "K" = "#E0E0E0"
    )
    
    draft_pool <- draft_pool %>%
        dplyr::mutate(
            sleeper_av = pmax(0, .data$sleeper_av),
            player_label = paste0(.data$player_name, "\n", .data$pos, " (", .data$team, ")\n$", round(.data$sleeper_av, 0))
        )
    
    # Identify $1 players for thicker outline
    dollar_pool <- draft_pool %>%
        dplyr::filter(.data$sleeper_av < 1)
    
    draft_board_plot <- ggplot2::ggplot(draft_pool, ggplot2::aes(x = .data$draft_slot, y = .data$round, fill = .data$pos)) +
        ggplot2::geom_tile(color = "black", linewidth = 0.5, alpha = 0.7) +
        # Thicker outline for $1 players
        ggplot2::geom_tile(data = dollar_pool, color = "black", linewidth = 1.2, fill = NA) +
        ggplot2::geom_text(ggplot2::aes(label = .data$player_label), 
                  size = 3.5,
                  family = "source sans pro", 
                  color = "black",
                  lineheight = 0.8) + 
        ggplot2::scale_fill_manual(values = position_colors, na.value = "#E0E0E0") +
        ggplot2::scale_y_reverse(name = "Round", breaks = 1:rounds) +
        ggplot2::scale_x_continuous(name = "Pick", breaks = 1:teams, position = "top") +
        ggplot2::labs(
          title = paste(teams, "Team Draft Board"),
          caption = "Based on Sleeper Average Values"
          ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            legend.position = "none",
            panel.grid = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
            axis.text = ggplot2::element_text(face = "bold")
        )
    
    return(draft_board_plot)
}


main = function(save = FALSE) {
# Load Player DB from Sleeper
    sleeper_players_df = sleeper_players()

    sleeper_players_df %>% dplyr::glimpse()

# Load latest AVs from Sleeper
    sleeper_avs_raw = httr::GET("https://api.sleeper.com/players/nfl/values/regular/2025/2qb?idp=false&is_dynasty=false") %>%
        parse_json(as = "text")

    # Alternative - local load
    # sleeper_avs_raw = read_json("./2025 prep/av data/sleeper_avs_raw.json", simplifyVector = TRUE)


    sleeper_avs <- tibble::tibble(
        player_id = names(sleeper_avs_raw),
        sleeper_av = purrr::map_dbl(sleeper_avs_raw, `[[`, 1)) %>% 
        dplyr::left_join(
            sleeper_players_df %>%
                dplyr::select(dplyr::all_of(c("player_id","player_name","pos","age","team")))
            ) %>% 
            dplyr::arrange(
                dplyr::desc(.data$sleeper_av)
            ) %>%
            dplyr::mutate(
                sleeper_av = round(.data$sleeper_av)
            ) %>%
            dplyr::filter(
                !.data$pos %in% c("K","DEF"),
                !is.na(.data$sleeper_av)
            )

    sleeper_avs %>% readr::write_csv("current_sleeper_avs.csv")
    # View Sleeper ADP Draft Board ----

    sleeper_draft_board <- create_sleeper_draft_board(sleeper_avs, teams = 12, rounds = 20)

    sleeper_draft_board

    ## Save Sleeper ADP Draft Board ----
    if(save) {
        ggplot2::ggsave(
            "sleeper_draft_board.png",
            sleeper_draft_board,
            width = 14,
            height = 20,
            units = "in",
            dpi = 300)}

    return(sleeper_avs)
}

sleeper_avs = main(save = T)
