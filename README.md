# NBADraftPredictions




# NCAA to NBA Draft Probability Analyzer

An interactive R Shiny application that uses logistic regression to predict NBA draft probability for NCAA basketball players based on their performance statistics.

## Overview

This app analyzes over **27,000 NCAA players** from 2021-2025 seasons and matches them against actual NBA draft results to identify which performance metrics best predict draft selection. The model achieves:

- **99.1% Accuracy** (note: baseline is ~99% since only ~1% of players get drafted)
- **0.94 AUC-ROC Score** (strong discriminative ability)
- **4 statistically significant predictors** identified (p < 0.001)

## Key Findings

| Metric | Odds Ratio | Significance |
|--------|------------|--------------|
| Blocks Per Game | 2.73x | *** (p < 0.001) |
| Assists Per Game | 1.44x | *** (p < 0.001) |
| Rebounds Per Game | 1.23x | *** (p < 0.001) |
| Points Per Game | 1.18x | *** (p < 0.001) |
| Minutes Per Game | 1.02x | ns |
| Steals Per Game | 0.98x | ns |

## Star Rating System

The app uses an intuitive 5-star rating system based on percentile rankings:

| Stars | Percentile | Label |
|-------|------------|-------|
| ★★★★★ | 99.5th+ | Elite / Lottery Lock |
| ★★★★½ | 99.3 - 99.5 | Lottery Range |
| ★★★★ | 99.0 - 99.3 | First Round |
| ★★★½ | 98.5 - 99.0 | Late First / Early Second |
| ★★★ | 97.5 - 98.5 | Second Round |
| ★★½ | 96 - 97.5 | Fringe Draftable |
| ★★ | 93 - 96 | On the Radar |
| ★½ | 85 - 93 | Long Shot |
| ★ | Below 85 | Unlikely |

## Features

### Tab 1: Draft Success Metrics
- Model performance overview (accuracy, AUC, draft rate)
- Feature importance visualization
- Statistical significance analysis
- Odds ratio interpretation
- Drafted vs. undrafted stat comparison

### Tab 2: Data Explorer
- Filter by draft year, pick range, round
- Filter by PPG range and minimum games
- Browse all players or drafted only
- Download filtered data as CSV

### Tab 3: Player Explorer
- **Search any NCAA player** from 2021-2025
- **Custom stat input** for hypothetical scenarios
- **Star rating display** with percentile ranking
- **Performance radar chart**
- **Percentile rankings** for each stat
- **What-If analysis** - see how changing stats affects probability
- **Player comparison** - side-by-side analysis

## Installation

### Prerequisites

- R (≥ 4.0.0)
- RStudio (recommended)

### Required Packages

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "hoopR",
  "plotly",
  "DT",
  "scales",
  "bslib",
  "pROC"
))
```

## Running the App Locally

##### **Option A: Run directly from RStudio**

1. Open `app.R` in RStudio  
2. Click the **"Run App"** button in the top-right corner  

---

##### **Option B: Run from R console**

1. Navigate to the project folder
2. Open R console and run:

```bash
shiny::runApp("app.R") 
```

## Project Structure

```
NBADraftPredictions/
├── app.R              # Main Shiny application
├── data.R             # Data loading and model training script
├── data.rds           # Pre-computed data and model (faster loading)
├── requirements.txt   
├── README.md          
└── .gitignore
```

## Methodology

### Data Sources
- **NCAA Player Stats:** [hoopR package](https://hoopr.sportsdataverse.org/) - ESPN men's college basketball data
- **NBA Draft History:** [hoopR package](https://hoopr.sportsdataverse.org/) - Official NBA draft records

### Model
- **Algorithm:** Logistic Regression
- **Features:** PPG, RPG, APG, SPG, BPG, MPG
- **Target:** Binary (Drafted = 1, Not Drafted = 0)
- **Training Data:** ~27,000 NCAA players (2021-2025)
- **Validation:** AUC-ROC curve analysis




## License

[MIT](https://opensource.org/licenses/MIT)

## Deployment

[Shiny App](https://jamesleesusanto.shinyapps.io/nba-draft-app/)

[URL](https://jamesleesusanto.shinyapps.io/nba-draft-app/)

## Authors

**James Susanto** GitHub: [@jamesleesusanto](https://github.com/jamesleesusanto)

**Miguel Angel Truschel Orta** GitHub: [@atormiguel](https://github.com/atormiguel)

**Aadi Huria** GitHub: [@aadihuria](https://github.com/aadihuria)

**Elliot Ryu** GitHub: [@elliotsr1](https://github.com/elliotsr1)