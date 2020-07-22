library(shiny)

library(conflicted)
library(dplyr)

# Handle conflicts
conflict_prefer('filter', 'dplyr')
conflict_prefer('lag', 'dplyr')
conflict_prefer('box', 'shinydashboard')
conflict_prefer('last_plot', 'plotly')
conflict_prefer('layout', 'plotly')

# Enable bookmarking
enableBookmarking(store = "server")

# Load dataset
raw_league_table <- readRDS("league_tables.rds") %>%
  mutate(course = as.character(course)) %>%
  # Remove pairs with data for one year only
  group_by(institution, course) %>%
  filter(n() > 1) %>%
  ungroup()

# Create Russell Group subset
russell_league_table <- raw_league_table %>%
  filter(institution %in% c(
    "Birmingham",
    "Bristol",
    "Cambridge",
    "Cardiff",
    "Durham",
    "Edinburgh",
    "Exeter",
    "Glasgow",
    "Imperial College",
    "King's College London",
    "Leeds",
    "Liverpool",
    "London School of Economics",
    "Manchester",
    "Newcastle",
    "Nottingham",
    "Oxford",
    "Queen Mary, University of London",
    "Queen's, Belfast",
    "Sheffield",
    "Southampton",
    "University College London",
    "Warwick",
    "York"
  ))
