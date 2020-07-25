library(shiny)
library(shinyBS)
library(shinyWidgets)

library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)
library(stringr)
library(tidyr)

server <- function(input, output, session) {

  league_table <- reactive({
    if (input$russell_group) {
      russell_league_table
    } else {
      raw_league_table
    }
  })

  output$fixed_feature_selector <- renderUI({
    if (input$comparison_type == "Universities") {
      selectInput(
        'course',
        'Course',
        choices = sort(unique(league_table()$course)),
        width = '100%'
      )
    } else if (input$comparison_type == "Courses") {
      selectInput(
        'university',
        'University',
        choices = sort(unique(league_table()$institution)),
        width = '100%'
      )
    }
  })

  fixed_filter <- reactive({
    if (input$comparison_type == "Universities") {
      req(input$course)
      league_table() %>%
        filter(course == input$course)
    } else if (input$comparison_type == "Courses") {
      req(input$university)
      eague_table %>%
        filter(institution == input$university)
    }
  })

  output$variable_feature_selector <- renderUI({
    if (input$comparison_type == "Universities") {
      selectInput(
        'universities',
        'Universities',
        choices = sort(unique(fixed_filter()$institution)),
        multiple = TRUE,
        width = '100%'
      )
    } else if (input$comparison_type == "Courses") {
      selectInput(
        'courses',
        'Courses',
        choices = sort(unique(fixed_filter()$course)),
        multiple = TRUE,
        width = '100%'
      )
    }
  })

  comp_filter <- reactive({
    if (input$comparison_type == "Universities") {
      req(input$universities)
      fixed_filter() %>%
        filter(institution %in% input$universities)
    } else if (input$comparison_type == "Courses") {
      req(input$courses)
      fixed_filter() %>%
        filter(course %in% input$courses)
    }
  })

  output$date_range <- renderUI({
    min_year <- min(comp_filter()$year)
    max_year <- max(comp_filter()$year)

    sliderInput(
      'date_range',
      'Date Range',
      min = min_year,
      max = max_year,
      value = c(min_year, max_year),
      step = 1,
      width = '100%',
      sep = '',
      ticks = FALSE
    )
  })

  date_filter <- reactive({
    req(input$date_range)
    comp_filter() %>%
      filter(between(year, input$date_range[1], input$date_range[2]))
  })

  output$plot <- renderPlotly({
    metric_var = ifelse(input$comparison_metric == 'Score',
                        'overall_score',
                        'rank')
    comp_var = ifelse(input$comparison_type == 'Universities',
                      'institution',
                      'course')
    comp_label = ifelse(input$comparison_type == 'Universities',
                        'University',
                        'Course')

    p <- date_filter() %>%
      # Add tooltip text
      mutate(
        text = str_c(
          comp_label, ": ", !!as.name(comp_var), '\n',
          "Year: ", year, '\n',
          input$comparison_metric, ": ", !!as.name(metric_var),
          case_when(
            input$show_all_features ~ str_c(
              '\n\n',
              "Entry Standards: ",
              replace_na(entry_standards, 'N/A'), '\n',
              "Student Satisfaction: ",
              replace_na(student_satisfaction, 'N/A'), '\n',
              "Research Intensity: ",
              replace_na(research_intensity, 'N/A'), '\n',
              "Graduate Prospects: ",
              replace_na(graduate_prospects, 'N/A')
            ),
            TRUE ~ ""
          )
        )
      ) %>%
      ggplot(aes_string(x = "year", y = metric_var,
                        col = comp_var, text = "text", group = comp_var)) +
        geom_point(size = 2, alpha = ifelse(input$smooth_lines, 0.5, 1))

    lw <- 1.2
    if (input$smooth_lines) {
      p <- p +
        geom_smooth(size = lw, method = 'loess', formula = y ~ x,
                    se = FALSE, span = 1)
    } else {
      p <- p +
        geom_line(size = lw)
    }

    p <- p +
      labs(
        x = "Year",
        y = input$comparison_metric,
        col = comp_label
      ) +
      theme_minimal(base_size = 16) +
      theme(legend.title = element_blank())

    if (input$comparison_metric == 'Rank') {
      p <- p +
        scale_y_reverse()
    }

    ggplotly(p, tooltip = 'text') %>%
      add_annotations(text = comp_label, xref = "paper", yref="paper",
                      x = 1.02, xanchor = "left",
                      y = 0.8, yanchor = "bottom",
                      legendtitle=TRUE, showarrow=FALSE) %>%
      layout(legend = list(y=0.8, yanchor="top"))
  })

  output$choice_prompt <- renderUI({
    text <- NULL
    if (input$comparison_type == "Universities" &&
        is.null(input$universities)) {
      text <- "university"
    } else if (input$comparison_type == "Courses" &&
               is.null(input$courses)) {
      text <- "course"
    }
    req(text)
    tags$div(
      tags$p(tags$i(
        paste("Please select at least one",
              text,
              "using the sidebar")
      )), id = "prompt-container"
    )
  })

  observeEvent(input$open_modal, {
    showModal(
      modalDialog(title = "Information", easyClose = TRUE,
                  p(paste(
                    "This web app takes data from the Complete University",
                    "Guide and visualises it in a way that lets you track",
                    "trends across the years. You can either pick one",
                    "university and track the scores/rankings of its courses",
                    "over time, or fix the course and watch universities",
                    "tussle it out over the years."
                  )))
    )
  })
}
