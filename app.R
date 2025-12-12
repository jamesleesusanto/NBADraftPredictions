library(shiny)
library(tidyverse)
library(hoopR)
library(stringr)
library(purrr)
library(dplyr)
library(plotly)
library(DT)
library(scales)
library(bslib)

# Data Prep

nca_seasons <- 2021:2025
draft_years <- 2021:2025


data <- readRDS("data.rds")

final_players <- data$final_players
model <- data$model
feature_importance <- data$feature_importance
model_accuracy <- data$model_accuracy
model_auc <- data$model_auc
player_names_sorted <- data$player_names_sorted


# star system function

get_star_rating <- function(percentile) {
  if (percentile >= 99.5) {
    return(list(stars = 5, label = "Elite / Lottery Lock"))
  } else if (percentile >= 99.3) {
    return(list(stars = 4.5, label = "Lottery Range"))
  } else if (percentile >= 99.0) {
    return(list(stars = 4, label = "First Round"))
  } else if (percentile >= 98.5) {
    return(list(stars = 3.5, label = "Late First / Early Second"))
  } else if (percentile >= 97.5) {
    return(list(stars = 3, label = "Second Round"))
  } else if (percentile >= 96) {
    return(list(stars = 2.5, label = "Fringe Draftable"))
  } else if (percentile >= 93) {
    return(list(stars = 2, label = "On the Radar"))
  } else if (percentile >= 85) {
    return(list(stars = 1.5, label = "Long Shot"))
  } else {
    return(list(stars = 1, label = "Unlikely"))
  }
}

# html for stars
render_stars <- function(star_count) {
  full_stars <- floor(star_count)
  half_star <- (star_count %% 1) >= 0.5
  empty_stars <- 5 - ceiling(star_count)
  
  stars_html <- ""
  if (full_stars > 0) {
    for (i in seq_len(full_stars)) {
      stars_html <- paste0(stars_html, "<span style='color: #ffc107; font-size: 24px;'>★</span>")
    }
  }
  if (half_star) {
    stars_html <- paste0(stars_html, "<span style='position: relative; display: inline-block; font-size: 24px;'>",
                         "<span style='color: #e0e0e0;'>★</span>",
                         "<span style='position: absolute; left: 0; top: 0; width: 50%; overflow: hidden; color: #ffc107;'>★</span>",
                         "</span>")
  }
  if (empty_stars > 0) {
    for (i in seq_len(empty_stars)) {
      stars_html <- paste0(stars_html, "<span style='color: #e0e0e0; font-size: 24px;'>★</span>")
    }
  }
  return(stars_html)
}

# ui (shiny)

ui <- fluidPage(

  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Source Sans Pro"),
    heading_font = font_google("Montserrat")
  ),

  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        min-height: 100vh;
      }
      .main-header {
        background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 20px -15px;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 20px rgba(0,0,0,0.15);
      }
      .main-header h1 { margin: 0; font-weight: 700; font-size: 2.2em; }
      .main-header p { margin: 10px 0 0 0; opacity: 0.9; font-size: 1.1em; }
      .stat-card {
        background: white;
        padding: 25px;
        border-radius: 16px;
        margin: 10px 0;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        transition: transform 0.2s, box-shadow 0.2s;
        border: 1px solid rgba(0,0,0,0.05);
      }
      .stat-card:hover {
        transform: translateY(-3px);
        box-shadow: 0 8px 25px rgba(0,0,0,0.12);
      }
      .stat-card h4 {
        color: #2a5298;
        margin-top: 0;
        font-weight: 600;
        border-bottom: 2px solid #e8e8e8;
        padding-bottom: 10px;
      }
      .metric-big { 
        font-size: 56px; 
        font-weight: 700; 
        text-align: center; 
        margin: 20px 0; 
     }
      .metric-label { 
        text-align: center; 
        color: #666; 
        font-size: 14px; 
        text-transform: uppercase; 
        letter-spacing: 1px; 
      }
      .insight-box {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 25px;
        border-radius: 16px;
        margin: 15px 0;
      }
      .insight-box h4 { margin-top: 0; color: white; }
      .key-stat {
        display: inline-block;
        background: rgba(255,255,255,0.2);
        padding: 5px 12px;
        border-radius: 20px;
        margin: 3px;
        font-weight: 600;
      }
      .player-card {
        background: white;
        border-radius: 16px;
        padding: 20px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 15px;
      }
      .player-name { font-size: 24px; font-weight: 700; color: #1e3c72; }
      .player-team { color: #666; font-size: 14px; }
      .draft-badge { 
        display: inline-block; 
        padding: 8px 16px; 
        border-radius: 20px; 
        font-weight: 600; 
        margin-top: 10px; 
      }
      .drafted { background: #28a745; color: white; }
      .undrafted { background: #6c757d; color: white; }
      .prob-meter { 
        height: 12px; 
        background: #e9ecef; 
        border-radius: 6px; 
        overflow: hidden; 
        margin: 10px 0; 
      }
      .prob-fill { 
        height: 100%; 
        border-radius: 6px; 
        transition: width 0.5s ease; 
      }
      .nav-tabs .nav-link { 
        font-weight: 600; 
        color: #555; 
        border: none; 
        padding: 12px 20px; 
      }
      .nav-tabs .nav-link.active { 
        color: #2a5298; border-bottom: 3px solid #2a5298; background: transparent;
      }
      .explanation-text {
        background: #f8f9fa;
        border-left: 4px solid #2a5298;
        padding: 15px 20px;
        margin: 15px 0;
        border-radius: 0 8px 8px 0;
        font-size: 14px;
        line-height: 1.6;
      }
      .filter-panel {
        background: white;
        padding: 20px;
        border-radius: 16px;
        box-shadow: 0 4px 15px rgba(0,0,0,0.08);
        margin-bottom: 20px;
      }
      .comparison-vs { 
        font-size: 32px; 
        font-weight: 700; 
        color: #2a5298; 
        text-align: center; 
        padding: 20px; 
      }
    "))
  ),

  div(
    class = "main-header",
    h1("🏀 NCAA to NBA Draft Probability Analyzer"),
    p("Discover which performance metrics truly matter for making it to the NBA")
  ),

  tabsetPanel(
    id = "main_tabs",
    type = "tabs",

    # tab 1: home page, draft metrics
    tabPanel(
      "📊 Draft Success Metrics",
      value = "metrics_tab",
      br(),
      fluidRow(
        column(
          width = 12,
          div(
            class = "insight-box",
            h4("🔑 Key Findings at a Glance"),
            uiOutput("key_insights_summary")
          ),
          div(
            class = "explanation-text",
            uiOutput("metrics_explanation")
          ),
          fluidRow(
            column(4,
                   div(class = "stat-card",
                       h4("Model Accuracy"),
                       div(class = "metric-big", style = "color: #28a745;",
                           textOutput("accuracy_display", inline = TRUE)),
                       div(class = "metric-label", "Prediction Accuracy")
                   )
            ),
            column(4,
                   div(class = "stat-card",
                       h4("AUC-ROC Score"),
                       div(class = "metric-big", style = "color: #2a5298;",
                           textOutput("auc_display", inline = TRUE)),
                       div(class = "metric-label", "Model Quality")
                   )
            ),
            column(4,
                   div(class = "stat-card",
                       h4("Draft Rate"),
                       div(class = "metric-big", style = "color: #764ba2;",
                           textOutput("draft_rate_display", inline = TRUE)),
                       div(class = "metric-label", "Of NCAA Players")
                   )
            )
          ),
          br(),
          div(
            class = "stat-card",
            h4("🎯 Which Stats Matter Most for Getting Drafted?"),
            plotlyOutput("feature_importance_main", height = "400px")
          ),
          br(),
          fluidRow(
            column(6,
                   div(class = "stat-card",
                       h4("📊 Drafted vs Undrafted: Stat Comparison"),
                       fluidRow(
                         column(6,
                                selectInput("metric_view", "Select Metric:",
                                            choices = c("Points Per Game" = "ppg", "Rebounds Per Game" = "rpg",
                                                        "Assists Per Game" = "apg", "Steals Per Game" = "spg",
                                                        "Blocks Per Game" = "bpg", "Minutes Per Game" = "mpg"),
                                            selected = "ppg")
                         ),
                         column(6,
                                sliderInput("min_games_filter", "Min Games:", min = 1, max = 35, value = 10, step = 1)
                         )
                       ),
                       plotlyOutput("drafted_comparison_box", height = "300px")
                   )
            ),
            column(6,
                   div(class = "stat-card",
                       h4("🔬 Statistical Significance of Each Metric"),
                       plotlyOutput("significance_plot", height = "380px")
                   )
            )
          ),
          br(),
          div(
            class = "stat-card",
            h4("📈 Odds Ratio: How Much Does Each Point Increase Draft Chances?"),
            p("An odds ratio > 1 means the stat increases draft probability; < 1 means it decreases."),
            plotlyOutput("odds_ratio_plot", height = "350px")
          )
        )
      )
    ),
    
    # tb data & csv
    tabPanel(
      "📋 Data",
      value = "data_tab",
      br(),
      fluidRow(
        column(
          width = 3,
          div(
            class = "filter-panel",
            h4("🔍 Filter Data"),
            selectInput("data_view_type", "View:",
                        choices = c("Drafted Players" = "drafted", "All Players" = "all"),
                        selected = "drafted"),
            selectInput("data_draft_year", "Draft Year:",
                        choices = c("All Years" = "all", as.character(draft_years)),
                        selected = "all"),
            conditionalPanel(
              condition = "input.data_view_type == 'drafted'",
              sliderInput("data_pick_range", "Pick Range:", min = 1, max = 60, value = c(1, 60), step = 1),
              selectInput("data_round_filter", "Round:",
                          choices = c("All Rounds" = "all", "Round 1" = "1", "Round 2" = "2"),
                          selected = "all")
            ),
            sliderInput("data_ppg_range", "PPG Range:", min = 0, max = 35, value = c(0, 35), step = 1),
            sliderInput("data_games_min", "Minimum Games:", min = 1, max = 35, value = 1, step = 1),
            hr(),
            downloadButton("download_data", "📥 Download CSV", class = "btn-primary btn-block")
          )
        ),
        column(
          width = 9,
          fluidRow(
            column(4,
                   div(class = "stat-card",
                       h4("Total Players"),
                       div(class = "metric-big", style = "color: #2a5298; font-size: 42px;",
                           textOutput("data_total_players", inline = TRUE))
                   )
            ),
            column(4,
                   div(class = "stat-card",
                       h4("Average PPG"),
                       div(class = "metric-big", style = "color: #28a745; font-size: 42px;",
                           textOutput("data_avg_ppg", inline = TRUE))
                   )
            ),
            column(4,
                   div(class = "stat-card",
                       h4("Average Pred. Prob"),
                       div(class = "metric-big", style = "color: #764ba2; font-size: 42px;",
                           textOutput("data_avg_prob", inline = TRUE))
                   )
            )
          ),
          br(),
          div(
            class = "stat-card",
            h4("🏆 Player Database"),
            p("Browse and filter all NCAA players. Use the filters on the left to narrow down the data."),
            DTOutput("main_data_table")
          )
        )
      )
    ),
    
    # tb 3, player explorer
    tabPanel(
      "🔍 Player Explorer",
      value = "player_tab",
      br(),
      fluidRow(
        column(
          width = 3,
          div(
            class = "filter-panel",
            h4("🎯 Select Player"),
            radioButtons("input_mode", NULL,
                         choices = c("Search Player" = "player", "Custom Stats" = "custom"),
                         selected = "player"),
            conditionalPanel(
              condition = "input.input_mode == 'player'",
              selectizeInput("player", "Type to Search:", choices = NULL, selected = NULL,
                             options = list(placeholder = "Start typing a name...", maxOptions = 100))
            ),
            conditionalPanel(
              condition = "input.input_mode == 'custom'",
              h5("📊 Enter Custom Stats"),
              sliderInput("ppg_custom", "Points Per Game", min = 0, max = 35, value = 15, step = 0.5),
              sliderInput("rpg_custom", "Rebounds Per Game", min = 0, max = 15, value = 5, step = 0.5),
              sliderInput("apg_custom", "Assists Per Game", min = 0, max = 12, value = 3, step = 0.5),
              sliderInput("spg_custom", "Steals Per Game", min = 0, max = 4, value = 1, step = 0.1),
              sliderInput("bpg_custom", "Blocks Per Game", min = 0, max = 5, value = 0.7, step = 0.1),
              sliderInput("mpg_custom", "Minutes Per Game", min = 0, max = 40, value = 28, step = 1)
            ),
            hr(),
            h4("⚖️ Comparison"),
            checkboxInput("show_comparison", "Enable Player Comparison", FALSE),
            conditionalPanel(
              condition = "input.show_comparison == true",
              selectizeInput("compare_player", "Compare With:", choices = NULL, selected = NULL,
                             options = list(placeholder = "Select player to compare...", maxOptions = 100))
            )
          )
        ),
        column(
          width = 9,
          uiOutput("player_header_card"),
          div(class = "explanation-text", uiOutput("player_probability_explanation")),
          tabsetPanel(
            id = "player_subtabs",
            tabPanel(
              "📊 Stats Overview",
              br(),
              fluidRow(
                column(6, div(class = "stat-card", h4("🎯 Performance Radar"),
                              p("Visual representation of player's stats normalized to league averages."),
                              plotlyOutput("radar_chart", height = "400px"))),
                column(6, div(class = "stat-card", h4("📈 Percentile Rankings"),
                              p("How this player ranks compared to all NCAA players."),
                              plotlyOutput("percentile_plot", height = "400px")))
              ),
              br(),
              div(class = "stat-card", h4("📋 Complete Statistics"), DTOutput("stats_table"))
            ),
            tabPanel(
              "📈 Where They Stand",
              br(),
              div(class = "explanation-text",
                  HTML("<strong>Understanding these charts:</strong> The histograms show the distribution of stats for drafted (green) vs undrafted (gray) players. The red line shows where the selected player falls.")),
              br(),
              div(class = "stat-card", h4("Points Distribution: Drafted vs Undrafted"),
                  plotlyOutput("distribution_plot", height = "350px")),
              br(),
              div(class = "stat-card", h4("Draft Probability Distribution"),
                  p("Where does this player's predicted probability fall among all players?"),
                  plotlyOutput("probability_distribution", height = "350px"))
            ),
            tabPanel(
              "🎚️ What-If Analysis",
              br(),
              div(class = "explanation-text",
                  HTML("<strong>How to use this:</strong> This analysis shows how changing each stat would affect the player's draft probability, holding all other stats constant.")),
              br(),
              fluidRow(
                column(4, div(class = "filter-panel",
                              selectInput("sensitivity_stat", "Select Stat to Analyze:",
                                          choices = c("Points Per Game" = "ppg", "Rebounds Per Game" = "rpg",
                                                      "Assists Per Game" = "apg", "Steals Per Game" = "spg",
                                                      "Blocks Per Game" = "bpg", "Minutes Per Game" = "mpg")))),
                column(8, div(class = "stat-card", plotlyOutput("sensitivity_plot", height = "400px")))
              )
            ),
            tabPanel(
              "⚖️ Player Comparison",
              br(),
              conditionalPanel(
                condition = "input.show_comparison == true",
                uiOutput("comparison_header"),
                br(),
                fluidRow(
                  column(6, div(class = "stat-card", h4("📊 Head-to-Head Stats"),
                                plotlyOutput("comparison_plot", height = "400px"))),
                  column(6, div(class = "stat-card", h4("🎯 Radar Comparison"),
                                plotlyOutput("comparison_radar", height = "400px")))
                ),
                br(),
                div(class = "stat-card", h4("📋 Detailed Comparison"), DTOutput("comparison_table"))
              ),
              conditionalPanel(
                condition = "input.show_comparison == false",
                div(class = "stat-card", style = "text-align: center; padding: 60px;",
                    h3("Enable comparison in the sidebar"),
                    p("Check the 'Enable Player Comparison' box to compare two players side by side."))
              )
            )
          )
        )
      )
    )
  )
)

# server - shiny

server <- function(input, output, session) {
  
  updateSelectizeInput(session, "player", choices = player_names_sorted, selected = player_names_sorted[1], server = TRUE)
  updateSelectizeInput(session, "compare_player", choices = player_names_sorted, selected = player_names_sorted[2], server = TRUE)
  


  current_stats <- reactive({
    if (input$input_mode == "custom") {
      tibble(
        athlete_display_name = "Custom Player", team_display_name = "N/A", season = NA_integer_,
        ppg = input$ppg_custom, rpg = input$rpg_custom, apg = input$apg_custom,
        spg = input$spg_custom, bpg = input$bpg_custom, mpg = input$mpg_custom,
        games = NA_integer_, fg_pct = NA_real_, three_pct = NA_real_, ft_pct = NA_real_,
        drafted = NA_integer_, overall_pick = NA_integer_, draft_round = NA_integer_, draft_team = NA_character_
      )
    } else {
      req(input$player)
      final_players |> filter(athlete_display_name == input$player) |> slice(1)
    }
  })
  
  compare_stats <- reactive({
    req(input$compare_player)
    final_players |> filter(athlete_display_name == input$compare_player) |> slice(1)
  })
  
  draft_prob <- reactive({
    stats <- current_stats()
    if (nrow(stats) == 0) return(0)
    predict(model, newdata = stats |> select(ppg, rpg, apg, spg, bpg, mpg), type = "response")[1]
  })
  
  compare_prob <- reactive({
    stats <- compare_stats()
    if (nrow(stats) == 0) return(0)
    predict(model, newdata = stats |> select(ppg, rpg, apg, spg, bpg, mpg), type = "response")[1]
  })
  
  # outputs - tb1
  output$key_insights_summary <- renderUI({
    top_feature <- feature_importance$feature_label[1]
    top_or <- round(feature_importance$odds_ratio[1], 2)
    sig_features <- feature_importance |> filter(p_value < 0.05)
    HTML(paste0(
      "<div style='display: flex; flex-wrap: wrap; gap: 10px;'>",
      "<span class='key-stat'>🥇 Most Important: ", top_feature, "</span>",
      "<span class='key-stat'>📊 ", nrow(sig_features), " of 6 metrics statistically significant</span>",
      "<span class='key-stat'>📈 Each +1 PPG = ", top_or, "x draft odds</span>",
      "<span class='key-stat'>🎯 Model AUC: ", round(model_auc, 3), "</span>",
      "</div>"
    ))
  })
  
  output$metrics_explanation <- renderUI({
    top_3 <- head(feature_importance, 3)
    HTML(paste0(
      "<strong>What does this analysis tell us?</strong><br><br>",
      "Using logistic regression on ", format(nrow(final_players), big.mark = ","), " NCAA players from 2021-2025, ",
      "we identified which performance metrics best predict NBA draft selection. ",
      "<strong>", top_3$feature_label[1], "</strong> emerged as the strongest predictor (coefficient = ",
      round(top_3$coefficient[1], 3), ", p ", ifelse(top_3$p_value[1] < 0.001, "< 0.001", paste0("= ", round(top_3$p_value[1], 4))), "), ",
      "followed by <strong>", top_3$feature_label[2], "</strong> and <strong>", top_3$feature_label[3], "</strong>. ",
      "The model achieves ", round(model_accuracy * 100, 1), "% accuracy with an AUC of ", round(model_auc, 3), "."
    ))
  })
  
  output$accuracy_display <- renderText({ paste0(round(model_accuracy * 100, 1), "%") })
  output$auc_display <- renderText({ round(model_auc, 3) })
  output$draft_rate_display <- renderText({ paste0(round(mean(final_players$drafted) * 100, 2), "%") })
  
  output$feature_importance_main <- renderPlotly({
    df <- feature_importance |>
      mutate(
        bar_color = case_when(p_value < 0.001 ~ "#28a745", p_value < 0.01 ~ "#17a2b8", p_value < 0.05 ~ "#ffc107", TRUE ~ "#dc3545"),
        sig_label = case_when(p_value < 0.001 ~ "Highly Significant (p < 0.001)", p_value < 0.01 ~ "Very Significant (p < 0.01)",
                              p_value < 0.05 ~ "Significant (p < 0.05)", TRUE ~ "Not Significant")
      )
    plot_ly(data = df, x = ~abs_coefficient, y = ~reorder(feature_label, abs_coefficient), type = 'bar', orientation = 'h',
            marker = list(color = ~bar_color),
            text = ~paste0("<b>", feature_label, "</b><br>Coefficient: ", round(coefficient, 4), "<br>Odds Ratio: ", round(odds_ratio, 3),
                           "<br>P-value: ", format(p_value, scientific = TRUE, digits = 3), "<br>Status: ", sig_label),
            hoverinfo = 'text') |>
      layout(xaxis = list(title = "Impact on Draft Probability (Absolute Coefficient)"), yaxis = list(title = ""),
             margin = list(l = 150), showlegend = FALSE)
  })
  
  output$drafted_comparison_box <- renderPlotly({
    selected_stat <- input$metric_view
    stat_label <- c(ppg = "Points Per Game", rpg = "Rebounds Per Game", apg = "Assists Per Game",
                    spg = "Steals Per Game", bpg = "Blocks Per Game", mpg = "Minutes Per Game")[selected_stat]
    df <- final_players |> filter(games >= input$min_games_filter) |>
      mutate(draft_status = ifelse(drafted == 1, "Drafted", "Undrafted"))
    plot_ly(data = df, x = ~draft_status, y = ~get(selected_stat), type = 'box', color = ~draft_status,
            colors = c("Drafted" = "#28a745", "Undrafted" = "#6c757d")) |>
      layout(yaxis = list(title = stat_label), xaxis = list(title = ""), showlegend = FALSE)
  })
  
  output$significance_plot <- renderPlotly({
    df <- feature_importance |>
      mutate(neg_log_p = -log10(p_value),
             sig_color = case_when(p_value < 0.001 ~ "#28a745", p_value < 0.01 ~ "#17a2b8", p_value < 0.05 ~ "#ffc107", TRUE ~ "#dc3545"))
    plot_ly(data = df, x = ~neg_log_p, y = ~reorder(feature_label, neg_log_p), type = 'bar', orientation = 'h',
            marker = list(color = ~sig_color), text = ~paste0("P-value: ", format(p_value, scientific = TRUE, digits = 3)), hoverinfo = 'text') |>
      layout(xaxis = list(title = "-log10(P-value)", tickvals = c(0, 1.3, 2, 3), ticktext = c("1", "0.05", "0.01", "0.001")), yaxis = list(title = ""),
             shapes = list(list(type = "line", x0 = 1.3, x1 = 1.3, y0 = -0.5, y1 = 5.5, line = list(color = "red", dash = "dash"))),
             annotations = list(list(x = 1.5, y = 5.5, text = "p = 0.05", showarrow = FALSE, font = list(size = 10, color = "red"))))
  })
  
  output$odds_ratio_plot <- renderPlotly({
    df <- feature_importance |> mutate(or_color = ifelse(odds_ratio > 1, "#28a745", "#dc3545"))
    plot_ly(data = df, x = ~odds_ratio, y = ~reorder(feature_label, odds_ratio), type = 'bar', orientation = 'h',
            marker = list(color = ~or_color),
            text = ~paste0(feature_label, "<br>Odds Ratio: ", round(odds_ratio, 3), "<br>Each +1 unit = ", round(odds_ratio, 2), "x draft odds"),
            hoverinfo = 'text') |>
      layout(xaxis = list(title = "Odds Ratio"), yaxis = list(title = ""),
             shapes = list(list(type = "line", x0 = 1, x1 = 1, y0 = -0.5, y1 = 5.5, line = list(color = "black", dash = "dash", width = 2))))
  })
  
  # outputs - tb2
  filtered_data <- reactive({
    if (input$data_view_type == "drafted") {
      df <- final_players |> filter(drafted == 1) |>
        filter(overall_pick >= input$data_pick_range[1], overall_pick <= input$data_pick_range[2])
      if (input$data_round_filter != "all") df <- df |> filter(draft_round == as.integer(input$data_round_filter))
    } else {
      df <- final_players
    }
    if (input$data_draft_year != "all") {
      if (input$data_view_type == "drafted") {
        df <- df |> filter(draft_season == as.integer(input$data_draft_year))
      } else {
        df <- df |> filter(season == as.integer(input$data_draft_year))
      }
    }
    df |> filter(ppg >= input$data_ppg_range[1], ppg <= input$data_ppg_range[2], games >= input$data_games_min)
  })
  
  output$data_total_players <- renderText({ format(nrow(filtered_data()), big.mark = ",") })
  output$data_avg_ppg <- renderText({ round(mean(filtered_data()$ppg, na.rm = TRUE), 1) })
  output$data_avg_prob <- renderText({ paste0(round(mean(filtered_data()$pred_prob, na.rm = TRUE) * 100, 1), "%") })
  
  output$main_data_table <- renderDT({
    df <- filtered_data()
    if (input$data_view_type == "drafted") {
      df <- df |> arrange(overall_pick) |>
        select(Player = athlete_display_name, College = team_display_name, Season = season, `Pick #` = overall_pick,
               Round = draft_round, `NBA Team` = draft_team, PPG = ppg, RPG = rpg, APG = apg, SPG = spg, BPG = bpg, MPG = mpg,
               Games = games) |>
        mutate(PPG = round(PPG, 1), RPG = round(RPG, 1), APG = round(APG, 1), SPG = round(SPG, 1), BPG = round(BPG, 1),
               MPG = round(MPG, 1))
    } else {
      df <- df |> arrange(desc(pred_prob)) |>
        select(Player = athlete_display_name, College = team_display_name, Season = season, Drafted = drafted,
               PPG = ppg, RPG = rpg, APG = apg, SPG = spg, BPG = bpg, MPG = mpg, Games = games, ) |>
        mutate(Drafted = ifelse(Drafted == 1, "Yes", "No"), PPG = round(PPG, 1), RPG = round(RPG, 1), APG = round(APG, 1),
               SPG = round(SPG, 1), BPG = round(BPG, 1), MPG = round(MPG, 1))
    }
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE, filter = 'top')
  })
  
  output$download_data <- downloadHandler(
    filename = function() { paste0("ncaa_draft_data_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- filtered_data()
      if (input$data_view_type == "drafted") {
        df <- df |> select(Player = athlete_display_name, College = team_display_name, Season = season, Pick = overall_pick,
                           Round = draft_round, NBA_Team = draft_team, PPG = ppg, RPG = rpg, APG = apg, SPG = spg, BPG = bpg,
                           MPG = mpg, Games = games, Pred_Prob = pred_prob)
      } else {
        df <- df |> select(Player = athlete_display_name, College = team_display_name, Season = season, Drafted = drafted,
                           PPG = ppg, RPG = rpg, APG = apg, SPG = spg, BPG = bpg, MPG = mpg, Games = games, Pred_Prob = pred_prob)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # outputs tb 3
  output$player_header_card <- renderUI({
    stats <- current_stats(); prob <- draft_prob(); percentile <- mean(final_players$pred_prob <= prob) * 100
    star_info <- get_star_rating(percentile); stars_html <- render_stars(star_info$stars)
    prob_color <- if(star_info$stars >= 4) "#28a745" else if(star_info$stars >= 3) "#ffc107" else "#dc3545"
    draft_info <- if (!is.na(stats$drafted) && stats$drafted == 1) { paste0("<span class='draft-badge drafted'>✓ DRAFTED - Pick #", stats$overall_pick, " (Round ", stats$draft_round, ") by ", stats$draft_team, "</span>")
    } else if (!is.na(stats$drafted)) { "<span class='draft-badge undrafted'>Not Drafted</span>" } else { "" }
    HTML(paste0(
      "<div class='player-card'><div style='display: flex; justify-content: space-between; align-items: center;'>",
      "<div><div class='player-name'>", stats$athlete_display_name, "</div>",
      "<div class='player-team'>", stats$team_display_name, if(!is.na(stats$season)) paste0(" | Season: ", stats$season) else "", "</div>", draft_info, "</div>",
      "<div style='text-align: center; min-width: 280px;'>",
      "<div style='margin-bottom: 8px;'>", stars_html, "</div>",
      "<div style='font-size: 16px; font-weight: 600; color: ", prob_color, "; margin-bottom: 5px;'>", star_info$label, "</div>",
      "<div style='font-size: 42px; font-weight: 700; color: #1e3c72;'>", round(percentile, 1), "<span style='font-size: 20px;'>th</span></div>",
      "<div class='metric-label'>Percentile</div>",
      "<div style='margin-top: 10px; padding-top: 10px; border-top: 1px solid #eee;'>",

      
      "</div></div></div></div>"))
  })
  
  output$player_probability_explanation <- renderUI({
    prob <- draft_prob(); percentile <- mean(final_players$pred_prob <= prob) * 100; star_info <- get_star_rating(percentile)
    HTML(paste0("<strong>What does ", star_info$stars, " stars mean?</strong><br>This player is in the <strong>", round(percentile, 1), "th percentile</strong> among all NCAA players — rated as \"<strong>", star_info$label, "</strong>\". "))
  })
  
  output$radar_chart <- renderPlotly({
    stats <- current_stats()
    max_vals <- c(ppg = 30, rpg = 12, apg = 10, spg = 3, bpg = 4, mpg = 38)
    values <- c(min(stats$ppg / max_vals["ppg"] * 100, 100), min(stats$rpg / max_vals["rpg"] * 100, 100),
                min(stats$apg / max_vals["apg"] * 100, 100), min(stats$spg / max_vals["spg"] * 100, 100),
                min(stats$bpg / max_vals["bpg"] * 100, 100), min(stats$mpg / max_vals["mpg"] * 100, 100))
    plot_ly(type = 'scatterpolar', r = c(values, values[1]), theta = c('PPG', 'RPG', 'APG', 'SPG', 'BPG', 'MPG', 'PPG'),
            fill = 'toself', fillcolor = 'rgba(42, 82, 152, 0.3)', line = list(color = '#2a5298', width = 2)) |>
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), showlegend = FALSE)
  })
  
  output$percentile_plot <- renderPlotly({
    stats <- current_stats()
    percentiles <- tibble(
      stat = c("PPG", "RPG", "APG", "SPG", "BPG", "MPG"),
      value = c(stats$ppg, stats$rpg, stats$apg, stats$spg, stats$bpg, stats$mpg),
      percentile = c(mean(final_players$ppg <= stats$ppg, na.rm = TRUE) * 100, mean(final_players$rpg <= stats$rpg, na.rm = TRUE) * 100,
                     mean(final_players$apg <= stats$apg, na.rm = TRUE) * 100, mean(final_players$spg <= stats$spg, na.rm = TRUE) * 100,
                     mean(final_players$bpg <= stats$bpg, na.rm = TRUE) * 100, mean(final_players$mpg <= stats$mpg, na.rm = TRUE) * 100)
    ) |> mutate(bar_color = case_when(percentile >= 90 ~ "#28a745", percentile >= 70 ~ "#17a2b8", percentile >= 50 ~ "#ffc107", TRUE ~ "#dc3545"))
    plot_ly(data = percentiles, x = ~percentile, y = ~reorder(stat, percentile), type = 'bar', orientation = 'h',
            marker = list(color = ~bar_color), text = ~paste0(round(percentile, 1), "th percentile (", round(value, 1), ")"), hoverinfo = 'text') |>
      layout(xaxis = list(title = "Percentile", range = c(0, 100)), yaxis = list(title = ""),
             shapes = list(list(type = "line", x0 = 50, x1 = 50, y0 = -0.5, y1 = 5.5, line = list(color = "gray", dash = "dash"))))
  })
  
  output$stats_table <- renderDT({
    stats <- current_stats()
    display_stats <- tibble(
      Metric = c("Points Per Game", "Rebounds Per Game", "Assists Per Game", "Steals Per Game", "Blocks Per Game",
                 "Minutes Per Game", "Field Goal %", "3-Point %", "Free Throw %", "Games Played"),
      Value = c(round(stats$ppg, 2), round(stats$rpg, 2), round(stats$apg, 2), round(stats$spg, 2), round(stats$bpg, 2), round(stats$mpg, 2),
                if(!is.na(stats$fg_pct)) paste0(round(stats$fg_pct * 100, 1), "%") else "N/A",
                if(!is.na(stats$three_pct)) paste0(round(stats$three_pct * 100, 1), "%") else "N/A",
                if(!is.na(stats$ft_pct)) paste0(round(stats$ft_pct * 100, 1), "%") else "N/A",
                if(!is.na(stats$games)) stats$games else "N/A")
    )
    datatable(display_stats, options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
  
  output$distribution_plot <- renderPlotly({
    stats <- current_stats()
    drafted_dist <- final_players |> filter(drafted == 1) |> pull(ppg)
    undrafted_dist <- final_players |> filter(drafted == 0) |> pull(ppg)
    plot_ly(alpha = 0.6) |>
      add_histogram(x = undrafted_dist, name = "Not Drafted", marker = list(color = "#6c757d"), nbinsx = 30) |>
      add_histogram(x = drafted_dist, name = "Drafted", marker = list(color = "#28a745"), nbinsx = 30) |>
      add_trace(x = c(stats$ppg, stats$ppg), y = c(0, 300), type = 'scatter', mode = 'lines',
                line = list(color = '#dc3545', width = 3, dash = 'dash'), name = "Selected Player") |>
      layout(barmode = "overlay", xaxis = list(title = "Points Per Game"), yaxis = list(title = "Count"), legend = list(x = 0.7, y = 0.95))
  })
  
  output$probability_distribution <- renderPlotly({
    current_prob <- draft_prob()
    plot_ly(alpha = 0.7) |>
      add_histogram(x = final_players$pred_prob, name = "All Players", marker = list(color = "#2a5298"), nbinsx = 50) |>
      add_trace(x = c(current_prob, current_prob), y = c(0, 500), type = 'scatter', mode = 'lines',
                line = list(color = '#dc3545', width = 3, dash = 'dash'), name = "Selected Player") |>
      layout(xaxis = list(title = "Predicted Draft Probability"), yaxis = list(title = "Count"))
  })
  
  output$sensitivity_plot <- renderPlotly({
    base_stats <- current_stats() |> select(ppg, rpg, apg, spg, bpg, mpg)
    selected_stat <- input$sensitivity_stat
    stat_labels <- c(ppg = "Points Per Game", rpg = "Rebounds Per Game", apg = "Assists Per Game",
                     spg = "Steals Per Game", bpg = "Blocks Per Game", mpg = "Minutes Per Game")
    max_val <- max(final_players[[selected_stat]], na.rm = TRUE)
    stat_range <- seq(0, max_val * 1.2, length.out = 50)
    probs <- map_dbl(stat_range, function(val) { test_stats <- base_stats; test_stats[[selected_stat]] <- val; predict(model, newdata = test_stats, type = "response") })
    current_val <- base_stats[[selected_stat]]
    current_prob <- draft_prob()
    plot_ly() |>
      add_trace(x = stat_range, y = probs * 100, type = 'scatter', mode = 'lines', line = list(width = 3, color = '#2a5298'), name = "Draft Probability") |>
      add_trace(x = c(current_val, current_val), y = c(0, 100), type = 'scatter', mode = 'lines',
                line = list(color = '#dc3545', width = 2, dash = 'dash'), name = "Current Value") |>
      add_trace(x = current_val, y = current_prob * 100, type = 'scatter', mode = 'markers', marker = list(size = 14, color = '#dc3545'), name = "Current Point") |>
      layout(title = paste("Impact of", stat_labels[selected_stat], "on Draft Probability"),
             xaxis = list(title = stat_labels[selected_stat]), yaxis = list(title = "Draft Probability (%)", range = c(0, 100)))
  })
  
  # comparisons

  
  output$comparison_header <- renderUI({
    req(input$show_comparison)
    stats1 <- current_stats(); stats2 <- compare_stats()
    prob1 <- draft_prob(); prob2 <- compare_prob()


    
    percentile1 <- mean(final_players$pred_prob <= prob1) * 100
    percentile2 <- mean(final_players$pred_prob <= prob2) * 100

    star_info1 <- get_star_rating(percentile1)
    star_info2 <- get_star_rating(percentile2)

    prob_color1 <- if(star_info1$stars >= 4) "#28a745" else if(star_info1$stars >= 3) "#ffc107" else "#dc3545"
    prob_color2 <- if(star_info2$stars >= 4) "#28a745" else if(star_info2$stars >= 3) "#ffc107" else "#dc3545"
  
    stars_html1 <- render_stars(star_info1$stars)
    stars_html2 <- render_stars(star_info2$stars)

    HTML(paste0(
      # left card

      "<div style='display: flex; justify-content: space-around; align-items: center; padding: 20px;'>",
      "<div class='player-card' style='flex: 1; margin: 10px; text-align: center;'>",
      "<div class='player-name'>", stats1$athlete_display_name, "</div>",
      "<div class='player-team'>", stats1$team_display_name, "</div>",
      # "<div style='font-size: 36px; font-weight: 700; color: #2a5298; margin-top: 10px;'>", round(prob1 * 100, 1), "%</div></div>",
      "<div style='margin-bottom: 8px;'>", stars_html1, "</div>",
      "<div style='font-size: 16px; font-weight: 600; color: ", prob_color1, "; margin-bottom: 5px;'>", star_info1$label, "</div>",
      "</div>",

      "<div class='comparison-vs'>VS</div>",
      
      #Right card
      "<div class='player-card' style='flex: 1; margin: 10px; text-align: center;'>",
      "<div class='player-name'>", stats2$athlete_display_name, "</div>",
      "<div class='player-team'>", stats2$team_display_name, "</div>",
      # "<div style='font-size: 36px; font-weight: 700; color: #764ba2; margin-top: 10px;'>", round(prob2 * 100, 1), "%</div></div></div>"
      "<div style='margin-bottom: 8px;'>", stars_html2, "</div>",
      "<div style='font-size: 16px; font-weight: 600; color: ", prob_color2, "; margin-bottom: 5px;'>", star_info2$label, "</div>",
      "</div>"
    
    ))
  })
  
  output$comparison_plot <- renderPlotly({
    req(input$show_comparison)
    stats1 <- current_stats(); stats2 <- compare_stats()
    if (nrow(stats1) == 0 || nrow(stats2) == 0) return(NULL)
    comparison_data <- tibble(
      stat = c("PPG", "RPG", "APG", "SPG", "BPG", "MPG"),
      player1 = as.numeric(c(stats1$ppg, stats1$rpg, stats1$apg, stats1$spg, stats1$bpg, stats1$mpg)),
      player2 = as.numeric(c(stats2$ppg, stats2$rpg, stats2$apg, stats2$spg, stats2$bpg, stats2$mpg))
    )
    plot_ly(data = comparison_data) |>
      add_trace(x = ~stat, y = ~player1, type = 'bar', name = as.character(stats1$athlete_display_name), marker = list(color = '#2a5298')) |>
      add_trace(x = ~stat, y = ~player2, type = 'bar', name = as.character(stats2$athlete_display_name), marker = list(color = '#764ba2')) |>
      layout(barmode = 'group', xaxis = list(title = ""), yaxis = list(title = "Value"), legend = list(x = 0.7, y = 1))
  })
  
  output$comparison_radar <- renderPlotly({
    req(input$show_comparison)
    stats1 <- current_stats(); stats2 <- compare_stats()
    if (nrow(stats1) == 0 || nrow(stats2) == 0) return(NULL)
    max_vals <- c(ppg = 30, rpg = 12, apg = 10, spg = 3, bpg = 4, mpg = 38)
    values1 <- c(min(as.numeric(stats1$ppg) / max_vals["ppg"] * 100, 100), min(as.numeric(stats1$rpg) / max_vals["rpg"] * 100, 100),
                 min(as.numeric(stats1$apg) / max_vals["apg"] * 100, 100), min(as.numeric(stats1$spg) / max_vals["spg"] * 100, 100),
                 min(as.numeric(stats1$bpg) / max_vals["bpg"] * 100, 100), min(as.numeric(stats1$mpg) / max_vals["mpg"] * 100, 100))
    values2 <- c(min(as.numeric(stats2$ppg) / max_vals["ppg"] * 100, 100), min(as.numeric(stats2$rpg) / max_vals["rpg"] * 100, 100),
                 min(as.numeric(stats2$apg) / max_vals["apg"] * 100, 100), min(as.numeric(stats2$spg) / max_vals["spg"] * 100, 100),
                 min(as.numeric(stats2$bpg) / max_vals["bpg"] * 100, 100), min(as.numeric(stats2$mpg) / max_vals["mpg"] * 100, 100))
    categories <- c('PPG', 'RPG', 'APG', 'SPG', 'BPG', 'MPG', 'PPG')
    plot_ly(type = 'scatterpolar') |>
      add_trace(r = c(values1, values1[1]), theta = categories, fill = 'toself', fillcolor = 'rgba(42, 82, 152, 0.3)',
                line = list(color = '#2a5298', width = 2), name = as.character(stats1$athlete_display_name)) |>
      add_trace(r = c(values2, values2[1]), theta = categories, fill = 'toself', fillcolor = 'rgba(118, 75, 162, 0.3)',
                line = list(color = '#764ba2', width = 2), name = as.character(stats2$athlete_display_name)) |>
      layout(polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))), legend = list(x = 0.8, y = 1))
  })
  
  output$comparison_table <- renderDT({
    req(input$show_comparison)
    stats1 <- current_stats(); stats2 <- compare_stats()
    if (nrow(stats1) == 0 || nrow(stats2) == 0) return(NULL)
    prob1 <- draft_prob(); prob2 <- compare_prob()
    comparison <- tibble(
      Metric = c("Draft Probability", "Points Per Game", "Rebounds Per Game", "Assists Per Game", "Steals Per Game",
                 "Blocks Per Game", "Minutes Per Game", "Season", "Team"),
      !!as.character(stats1$athlete_display_name) := c(paste0(round(prob1 * 100, 1), "%"), round(as.numeric(stats1$ppg), 1),
        round(as.numeric(stats1$rpg), 1), round(as.numeric(stats1$apg), 1), round(as.numeric(stats1$spg), 1),
        round(as.numeric(stats1$bpg), 1), round(as.numeric(stats1$mpg), 1), as.character(stats1$season), as.character(stats1$team_display_name)),
      !!as.character(stats2$athlete_display_name) := c(paste0(round(prob2 * 100, 1), "%"), round(as.numeric(stats2$ppg), 1),
        round(as.numeric(stats2$rpg), 1), round(as.numeric(stats2$apg), 1), round(as.numeric(stats2$spg), 1),
        round(as.numeric(stats2$bpg), 1), round(as.numeric(stats2$mpg), 1), as.character(stats2$season), as.character(stats2$team_display_name)),
      Difference = c(paste0(round((prob1 - prob2) * 100, 1), " pp"), round(as.numeric(stats1$ppg) - as.numeric(stats2$ppg), 1),
        round(as.numeric(stats1$rpg) - as.numeric(stats2$rpg), 1), round(as.numeric(stats1$apg) - as.numeric(stats2$apg), 1),
        round(as.numeric(stats1$spg) - as.numeric(stats2$spg), 1), round(as.numeric(stats1$bpg) - as.numeric(stats2$bpg), 1),
        round(as.numeric(stats1$mpg) - as.numeric(stats2$mpg), 1), "-", "-")
    )
    datatable(comparison, options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui = ui, server = server)