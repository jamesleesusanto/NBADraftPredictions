# ## Loading in Data

library(tidyverse)
library(hoopR)
library(readr)
library(stringr)
library(purrr)



# ncaa_seasons <- 2021:2025

# player_games <- map_df(
#   ncaa_seasons,
#   ~ load_mbb_player_box(seasons = .x)
# )

# player_clean <- player_games %>%
#   filter(!is.na(points), minutes > 0) %>%
#   select(
#     season,
#     athlete_id,
#     athlete_display_name,
#     team_display_name,
#     minutes,
#     points,
#     rebounds,
#     assists,
#     steals,
#     blocks,
#     field_goals_made, field_goals_attempted,
#     three_point_field_goals_made, three_point_field_goals_attempted,
#     free_throws_made, free_throws_attempted
#   )


# season_stats <- player_clean %>%
#   group_by(season, athlete_id, athlete_display_name, team_display_name) %>%
#   summarize(
#     games = n(),
#     mpg   = mean(minutes),
#     ppg   = mean(points),
#     rpg   = mean(rebounds),
#     apg   = mean(assists),
#     spg   = mean(steals),
#     bpg   = mean(blocks),

#     fg_pct = sum(field_goals_made, na.rm = TRUE) /
#              sum(field_goals_attempted, na.rm = TRUE),

#     three_pct = sum(three_point_field_goals_made, na.rm = TRUE) /
#                 sum(three_point_field_goals_attempted, na.rm = TRUE),

#     ft_pct = sum(free_throws_made, na.rm = TRUE) /
#              sum(free_throws_attempted, na.rm = TRUE),

#     .groups = "drop"
#   )



# clean_name <- function(x) {
#   x %>%
#     str_to_lower() %>%
#     str_replace_all("[[:punct:]]", "") %>%
#     str_replace_all("\\b(jr|sr|ii|iii|iv)\\b", "") %>%
#     str_squish()
# }

# season_stats2 <- season_stats %>%
#   mutate(clean_name = clean_name(athlete_display_name))


# draft_years <- 2021:2025

# draft_list <- map(
#   draft_years,
#   ~ nba_drafthistory(season = .x)$DraftHistory
# )

# draft_all <- bind_rows(draft_list)


# draft_clean <- draft_all %>%
#   filter(ORGANIZATION_TYPE == "College/University") %>%
#   mutate(
#     draft_season = as.integer(SEASON),
#     overall_pick = as.integer(OVERALL_PICK),
#     draft_round  = as.integer(ROUND_NUMBER),
#     draft_team   = TEAM_NAME,
#     college      = ORGANIZATION,
#     draft_clean_name = clean_name(PLAYER_NAME)
#   ) %>%
#   select(
#     draft_season,
#     overall_pick,
#     draft_round,
#     draft_team,
#     college,
#     draft_clean_name
#   )


# draft_clean <- draft_clean %>%
#   distinct(draft_clean_name, draft_season, .keep_all = TRUE)

# data_joined <- season_stats2 %>%
#   left_join(draft_clean, by = c("clean_name" = "draft_clean_name")) %>%
#   mutate(drafted = if_else(!is.na(overall_pick), 1L, 0L)) %>%
#   distinct(athlete_id, season, .keep_all = TRUE)


# final_players <- data_joined %>%
#   group_by(athlete_id) %>%
#   filter(season == max(season)) %>%
#   ungroup()

# final_players <- final_players %>%
#   select(
#     athlete_id,
#     athlete_display_name,
#     team_display_name,
#     season,
#     games,
#     mpg, ppg, rpg, apg, spg, bpg,
#     fg_pct, three_pct, ft_pct,
#     drafted,
#     overall_pick,
#     draft_season,
#     draft_team,
#     college
#   )


nca_seasons <- 2021:2025
draft_years <- 2021:2025

message("Loading NCAA player data...")
player_games <- map_df(
  nca_seasons,
  ~ load_mbb_player_box(seasons = .x)
)

if (nrow(player_games) == 0) stop("NCAA API returned no data.")

player_clean <- player_games |>
  filter(!is.na(points), minutes > 0) |>
  select(
    season, athlete_id, athlete_display_name, team_display_name,
    minutes, points, rebounds, assists, steals, blocks,
    field_goals_made, field_goals_attempted,
    three_point_field_goals_made, three_point_field_goals_attempted,
    free_throws_made, free_throws_attempted
  )

season_stats <- player_clean |>
  group_by(season, athlete_id, athlete_display_name, team_display_name) |>
  summarize(
    games = n(),
    mpg = mean(minutes, na.rm = TRUE),
    ppg = mean(points, na.rm = TRUE),
    rpg = mean(rebounds, na.rm = TRUE),
    apg = mean(assists, na.rm = TRUE),
    spg = mean(steals, na.rm = TRUE),
    bpg = mean(blocks, na.rm = TRUE),
    fg_pct = sum(field_goals_made, na.rm = TRUE) /
        sum(field_goals_attempted, na.rm = TRUE),
    three_pct = sum(three_point_field_goals_made, na.rm = TRUE) /
      sum(three_point_field_goals_attempted, na.rm = TRUE),
    ft_pct = sum(free_throws_made, na.rm = TRUE) /
        sum(free_throws_attempted, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    fg_pct = replace_na(fg_pct, 0),
    three_pct = replace_na(three_pct, 0),
    ft_pct = replace_na(ft_pct, 0)
  )

clean_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[[:punct:]]", "") |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_replace_all("\\b(jr|sr|i|ii|iii|iv|v)$", "") |>
    stringr::str_replace_all("\\bjunior\\b", "") |>
    stringr::str_replace_all("\\bsenior\\b", "") |>
    stringr::str_trim() |>
    stringr::str_squish()
}

season_stats <- season_stats |>
  mutate(clean_name = clean_name(athlete_display_name))

message("Loading NBA draft data...")
draft_list <- purrr::map(
  draft_years,
  ~ nba_drafthistory(season = .x)$DraftHistory
)

draft_all <- bind_rows(draft_list)

draft_clean <- draft_all |>
  filter(ORGANIZATION_TYPE == "College/University") |>
  mutate(
    draft_season = as.integer(SEASON),
    overall_pick = as.integer(OVERALL_PICK),
    draft_round = as.integer(ROUND_NUMBER),
    draft_team = TEAM_NAME,
    college = ORGANIZATION,
    draft_clean_name = clean_name(PLAYER_NAME)
  ) |>
  select(
    draft_season,
    overall_pick,
    draft_round,
    draft_team,
    college,
    draft_clean_name
  )

data_joined <- season_stats |>
  left_join(draft_clean, by = c("clean_name" = "draft_clean_name")) |>
  mutate(drafted = if_else(!is.na(overall_pick), 1L, 0L))

matched_count <- sum(!is.na(data_joined$overall_pick))
total_draft <- nrow(draft_clean)
message(paste0("Draft matching: ", 
    matched_count, " of ", 
    total_draft, 
    " drafted players matched to NCAA stats"))

final_players <- data_joined |>
  group_by(athlete_id) |>
  filter(season == max(season)) |>
  ungroup()

message("Training prediction model...")
model <- glm(
  drafted ~ ppg + rpg + apg + spg + bpg + mpg,
  data = final_players,
  family = binomial()
)

coef_summary <- summary(model)$coefficients
feature_importance <- tibble(
  feature = rownames(coef_summary)[-1],
  coefficient = coef_summary[-1, "Estimate"],
  std_error = coef_summary[-1, "Std. Error"],
  z_value = coef_summary[-1, "z value"],
  p_value = coef_summary[-1, "Pr(>|z|)"]
) |>
  mutate(
    feature_label = case_when(
      feature == "ppg" ~ "Points Per Game",
      feature == "rpg" ~ "Rebounds Per Game",
      feature == "apg" ~ "Assists Per Game",
      feature == "spg" ~ "Steals Per Game",
      feature == "bpg" ~ "Blocks Per Game",
      feature == "mpg" ~ "Minutes Per Game"
    ),
    abs_coefficient = abs(coefficient),
    odds_ratio = exp(coefficient),
    significance = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) |>
  arrange(desc(abs_coefficient))

final_players <- final_players |>
  mutate(pred_prob = predict(model, newdata = final_players, type = "response"))

model_predictions <- predict(model, type = "response")
model_actual <- final_players$drafted
model_auc <- pROC::auc(pROC::roc(model_actual, model_predictions, quiet = TRUE))
model_accuracy <- mean(ifelse(model_predictions > 0.5, 1, 0) == model_actual)

player_names_sorted <- sort(unique(final_players$athlete_display_name))

message("Data preparation complete!")

saveRDS(
  list(
    final_players = final_players,
    model = model,
    feature_importance = feature_importance,
    model_accuracy = model_accuracy,
    model_auc = model_auc,
    player_names_sorted = player_names_sorted
  ),
  "data.rds"
)
