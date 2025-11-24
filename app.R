library(shiny)
library(tidyverse)
library(hoopR)
library(stringr)
library(purrr)

# Load and clean ncaa player data (From Aadi Notebook)

nca_seasons <- 2021:2024
draft_years <- 2021:2025

# ncaa box scores
player_games <- map_df(
  nca_seasons,
  ~ load_mbb_player_box(seasons = .x)
)

# Clean raw games
player_clean <- player_games |>
  filter(!is.na(points), minutes > 0) |>
  select(
    season, athlete_id, athlete_display_name, team_display_name,
    minutes, points, rebounds, assists, steals, blocks,
    field_goals_made, field_goals_attempted,
    three_point_field_goals_made, three_point_field_goals_attempted,
    free_throws_made, free_throws_attempted
  )

# Summarize per-player season averages
season_stats <- player_clean |>
  group_by(season, athlete_id, athlete_display_name, team_display_name) |>
  summarize(
    games = n(),
    mpg = mean(minutes),
    ppg = mean(points),
    rpg = mean(rebounds),
    apg = mean(assists),
    spg = mean(steals),
    bpg = mean(blocks),
    fg_pct = sum(field_goals_made) / sum(field_goals_attempted),
    three_pct = sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted),
    ft_pct = sum(free_throws_made) / sum(free_throws_attempted),
    .groups = "drop"
  )

# Name cleaner
clean_name <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[[:punct:]]", "") |>
    stringr::str_replace_all("\\b(jr|sr|i|ii|iii|iv)\\b", "") |>
    stringr::str_squish()
}

season_stats <- season_stats |>
  mutate(clean_name = clean_name(athlete_display_name))



# Load Draft Results

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
  select(draft_season, overall_pick, draft_round, draft_team, college, draft_clean_name)



# Merge draft outcomes with stats

data_joined <- season_stats |>
  left_join(draft_clean, by = c("clean_name" = "draft_clean_name")) |>
  mutate(drafted = if_else(!is.na(overall_pick), 1L, 0L))

# Keep only final season per player
final_players <- data_joined |>
  group_by(athlete_id) |>
  filter(season == max(season)) |>
  ungroup()


# Train simple draft model

model <- glm(
  drafted ~ ppg + rpg + apg + spg + bpg + mpg,
  data = final_players,
  family = binomial()
)



# SHINY APP

ui <- fluidPage(
  titlePanel("Draft Predictor based on NCAA Stats"),

  sidebarLayout(
    sidebarPanel(
      selectInput(
        "player",
        "Select NCAA Player:",
        choices = c("Custom Stats", final_players$athlete_display_name)
      ),

      conditionalPanel(
        condition = "input.player == 'Custom Stats'",
        numericInput("ppg", "Points Per Game", 10),
        numericInput("rpg", "Rebounds Per Game", 5),
        numericInput("apg", "Assists Per Game", 3),
        numericInput("spg", "Steals Per Game", 1),
        numericInput("bpg", "Blocks Per Game", 1),
        numericInput("mpg", "Minutes Per Game", 24)
      )
    ),

    mainPanel(
      h3("Draft Probability"),
      textOutput("prob_output"),
      hr(),
      h3("Details"),
      verbatimTextOutput("details_output")
    )
  )
)

server <- function(input, output, session) {

  # Select player OR custom stats
  player_stats <- reactive({
    if (input$player == "Custom Stats") {
      tibble(
        ppg = input$ppg,
        rpg = input$rpg,
        apg = input$apg,
        spg = input$spg,
        bpg = input$bpg,
        mpg = input$mpg
      )
    } else {
      final_players |>
        filter(athlete_display_name == input$player) |>
        select(ppg, rpg, apg, spg, bpg, mpg)
    }
  })

  # predict
  output$prob_output <- renderText({
    prob <- predict(model, newdata = player_stats(), type = "response")
    paste0("Draft Probability: ", round(prob * 100, 1), "%")
  })

  output$details_output <- renderPrint({
    player_stats()
  })
}


# run app
shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
