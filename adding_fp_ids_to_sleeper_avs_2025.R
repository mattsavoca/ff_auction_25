library(nflreadr)
source("sleeper_avs.r")

load_sleeper_fp_id_key = function(get_ids = FALSE) {
    if(get_ids) {
    source("./4for4/own_model/R/player_id_functions.R")
    gs4_auth("fantasydraftaholic@gmail.com")

    player_db_4for4 = read_sheet(
        "https://docs.google.com/spreadsheets/d/105OLXorO-p-4z1VuerL8wF6ywK554-CrpP1Wy0g3brc/edit?gid=1815226455#gid=1815226455",
        sheet = "Players")

    joined_sleeper_avs = sleeper_avs %>%
        left_join(player_db_4for4 %>% 
            mutate(sleeper_id = as.character(sleeper_id), fantasypros_id = as.character(fantasypros_id)) %>% 
            select(player_id = sleeper_id, fantasypros_id))


    # Function to clean player names for FantasyPros ID matching
    clean_player_names = function(names) {
        # Named vector of player name mappings (original -> corrected)
        name_mappings = c(
            "Cameron Ward" = "Cam Ward",
            "Luther Burden" = "Luther Burden III",
            "Harold Fannin" = "Harold Fannin Jr",
            "Jacory Croskey-Merritt" = "Jacory Croskeymerritt",
            "Montrell Johnson" = "Montrell Johnson Jr",
            "Dont'e Thornton" = "Donte Thornton Jr",
            "Lequint Allen" = "Lequint Allen Jr",
            "Drew Ogletree" = "Andrew Ogletree",
            "Ja'Quinden Jackson" = "Jaquinden Jackson",
            "Jha'Quan Jackson" = "Jhaquan Jackson",
            "Efton Chism" = "Efton Chismi",
            "Jimmy Horn" = "Jimmy Horn Jr",
            "Keandre Lambert-Smith" = "Keandre Lambert Smith",
            "Ja'Corey Brooks" = "Jacorey Brooks",
            "Ricky White" = "Ricky White III",
            "Antwane Wells" = "Antwane Wells Jr"
        )
        
        # Apply all name mappings
        cleaned_names = names
        for (i in seq_along(name_mappings)) {
            cleaned_names = str_replace(cleaned_names, names(name_mappings)[i], name_mappings[i])
        }
        
        return(cleaned_names)
    }

    rookie_fp_ids =  joined_sleeper_avs %>%
        filter(is.na(fantasypros_id)) %>%
        mutate(
            adj_player_name = clean_player_names(player_name),
            fantasypros_id = map_chr(adj_player_name, get_fp_id)
        ) %>%
        mutate(
            fantasypros_id = case_when(
                !is.na(fantasypros_id) ~ fantasypros_id,
                T ~ get_fp_id_direct(adj_player_name)
            )
        ) %>%
        select(-adj_player_name)

    sleepers_avs_with_fp_ids = joined_sleeper_avs %>%
        filter(!is.na(fantasypros_id)) %>%
        bind_rows(rookie_fp_ids) %>%
        arrange(-sleeper_av) 


    sleeper_fantasypros_id_key = sleepers_avs_with_fp_ids %>% select(sleeper_id = player_id, fantasypros_id)
    
    sleeper_fantasypros_id_key %>%
        write_csv("sleeper_fantasypros_id_key.csv")
    
    return(sleeper_fantasypros_id_key)

    } else {
        sleeper_fantasypros_id_key = read_csv("sleeper_fantasypros_id_key.csv")
        return(sleeper_fantasypros_id_key)
    }
}

# Get FP IDs for Sleeper AVs ----
main = function(get_ids = FALSE) {
    sleeper_fantasypros_id_key = load_sleeper_fp_id_key(get_ids = get_ids)

    sleeper_avs_df = sleeper_avs %>%
        mutate(sleeper_id = player_id) %>%
        left_join(
            sleeper_fantasypros_id_key %>% mutate(sleeper_id = as.character(sleeper_id))
        )

    return(sleeper_avs_df)
    }


sleeper_avs_df =  main(get_ids = F)
