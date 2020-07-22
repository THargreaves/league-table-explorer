library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)

library(plotly)

ui <- dashboardPage(
  dashboardHeader(
    title = 'WDSS Presents: League Table Explorer',
    titleWidth = 400,
    tags$li(
      actionLink(
        "open_modal",
        label = "",
        title = "Info",
        icon = icon("question")
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = paste0('https://research.warwickdatascience.com/',
                      'league-table-explorer'),
        target = '_blank',
        icon("file-alt"),
        title = "Write-up",
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        href = paste0('https://github.com/warwickdatascience/',
                      'league-table-explorer'),
        target = '_blank',
        icon("github"),
        title = "Source",
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(
    width = 400,
    tags$p(tags$i(
      "Please note that this web app is optimised for laptop/deskop PCs"
    ), id = 'compat-info'),
    selectInput(
      'comparison_type',
      'Comparison Type',
      choices = c('Universities', 'Courses'),
      width = '100%'
    ),
    bsTooltip(
      'comparison_type',
      paste("Choose whether to compare multiple universities for a single",
            "course (Universities) or multiple courses for a single university",
            "(Courses)"),
      placement = "right"
    ),
    selectInput(
      'comparison_metric',
      'Comparison Metric',
      choices = c('Score', 'Rank'),
      width = '100%'
    ),
    bsTooltip(
      'comparison_metric',
      paste("Choose whether to use the Complete University Guide scores",
            "directly (Score) or to use the corresponding ranking (Rank)"),
      placement = "right"
    ),
    uiOutput('fixed_feature_selector'),
    uiOutput('variable_feature_selector'),
    uiOutput('date_range'),
    checkboxInput(
      'russell_group',
      'Filter for Russell Group only',
      value = FALSE,
      width = '100%'
    ),
    bsTooltip(
      'russell_group',
      paste("If checked, remove any non-Russell Group universities from the",
            "dropdown selectors"),
      placement = "right"
    ),
    checkboxInput(
      'smooth_lines',
      'Smooth Lines',
      value = FALSE,
      width = '100%'
    ),
    bsTooltip(
      'smooth_lines',
      paste("If checked, replace exact line chart with a smoothed moving",
            "average to better visualise overall trends"),
      placement = "right"
    ),
    checkboxInput(
      'show_all_features',
      'Show all features',
      value = FALSE,
      width = '100%'
    ),
    bsTooltip(
      'show_all_features',
      paste("If checked, show all available features used by the Complete",
            "University Guide in determining the overall score of a course",
            "at a given university when a data point is hovered over"),
      placement = "right"
    ),
    bookmarkButton(
      'Save Settings',
      icon = icon('save'),
      width = 'calc(100% - 30px)'
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
      tags$script(src = 'main.js')
    ),
    fluidRow(
      box(
        id = 'plot_container',
        width = 12,
        uiOutput('choice_prompt'),
        plotlyOutput('plot')
      )
    )
  )
)
