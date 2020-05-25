library(shiny)
library(ggplot2)
library(dplyr)
library(precisely)
library(markdown)
library(shinycssloaders)

ui <- fluidPage(
   theme = shinythemes::shinytheme("united"),

   # Application title
   titlePanel("Estimate Sample Size Based on Precision"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         selectInput("outcome",
                     "Calculate:",
                     choices = list(
                      "Sample Size based on Precision" = "n_",
                      "Precision based on Sample Size" = "precision_",
                      "Sample Size based on Upper Limit" = "upper_"
                     ), selected = 1),

         selectInput("measure",
                     "Measure:",
                     choices = list(
                      "Risk Difference" = "risk_difference",
                      "Risk Ratio" = "risk_ratio",
                      "Rate Difference" = "rate_difference",
                      "Rate Ratio" = "rate_ratio",
                      "Odds Ratio" = "odds_ratio"
                     ), selected = 1),

        conditionalPanel(
          condition = "input.outcome == 'n_'",
            uiOutput("precision_desc"),
            splitLayout(
              numericInput("precision_from",
                          label = "From",
                          value = .1),
              numericInput("precision_to",
                          "To",
                          value = .2)
          )),
        conditionalPanel(
          condition = "input.outcome == 'precision_' & input.measure != 'odds_ratio'",
           tags$label("Number of Exposed Participants", class = "control-label"),
            splitLayout(
              numericInput("n_from_cohort",
                          label = "From",
                          value = 500),
              numericInput("n_to_cohort",
                          "To",
                          value = 1000)
          )),
        conditionalPanel(
          condition = "input.outcome == 'precision_' & input.measure == 'odds_ratio'",
           tags$label("Number of Cases", class = "control-label"),
            splitLayout(
              numericInput("n_from_cc",
                          label = "From",
                          value = 500),
              numericInput("n_to_cc",
                          "To",
                          value = 1000)
          )),
        conditionalPanel(
          condition = "input.outcome == 'upper_'",
           tags$label("The upper limit of the confidence interval (level of concern).", class = "control-label"),
            splitLayout(
              numericInput("upper_from",
                          label = "From",
                          value = .1),
              numericInput("upper_to",
                          "To",
                          value = .2)
          )),

        conditionalPanel(
          condition = "input.outcome == 'upper_'",
              uiOutput("prob_slider")
          ),

        conditionalPanel(
          condition = "input.measure == 'odds_ratio'",
           tags$label("The proportion of exposed cases and controls.", class = "control-label"),
            splitLayout(
              uiOutput("exposed_cases_slider"),
              uiOutput("exposed_controls_slider")
          )),

        conditionalPanel(
          condition = "input.measure != 'odds_ratio'",
           tags$label("The risk or rate among participants", class = "control-label"),
            splitLayout(
              uiOutput("exposed_slider"),
              uiOutput("unexposed_slider")
          )),

        conditionalPanel(
          condition = "input.measure == 'odds_ratio' & input.group_var != 'group_ratio'",
              numericInput("group_ratio_cc",
                          "The ratio of number of controls to cases",
                          value = 1)
          ),

        conditionalPanel(
          condition = "input.measure != 'odds_ratio' & input.group_var != 'group_ratio'",
              numericInput("group_ratio_cohort",
                          "The ratio of number of unexposed to exposed participants",
                          value = 1)
          ),

        conditionalPanel(
          condition = "input.measure == 'odds_ratio' & input.group_var == 'group_ratio'",
           tags$label("The ratio of number of controls to cases", class = "control-label"),
            splitLayout(
              numericInput("group_ratio_cc_from",
                          "From",
                          value = 1),
              numericInput("group_ratio_cc_to",
                          "To",
                          value = 4)
          )),

        conditionalPanel(
          condition = "input.measure != 'odds_ratio' & input.group_var == 'group_ratio'",
           tags$label("The ratio of number of unexposed to exposed participants", class = "control-label"),
            splitLayout(
              numericInput("group_ratio_cohort_from",
                          "From",
                          value = 1),
              numericInput("group_ratio_cohort_to",
                          "To",
                          value = 4)
          )),

        sliderInput("ci",
                    "Confidence Interval Coverage",
                    value = .95, min = 0, max = 1),

        selectInput("group_var",
                    "Group By",
                    choices = list(
                      "None" = "none",
                      "Exposed" = "exposed",
                      "Unexposed" = "unexposed",
                      "Group Ratio" = "group_ratio")),

        conditionalPanel(
          condition = "input.group_var != 'none'",
              numericInput("group_var_groups",
                          "Number of groups",
                          value = 4)
          ),

        width = 3),
      # Main panel
      mainPanel(
        includeMarkdown("intro.md"),
        tabsetPanel(
          type = "tabs",
          tabPanel("Plot", withSpinner(plotOutput("precisely_plot", height = "600px"))),
          tabPanel(
            "Table",
            dataTableOutput("precisely_table"),
            downloadButton('downloadData', 'Download as CSV')
           ),
          tabPanel("About", includeMarkdown("about.md"))
          ),
        width = 7
      )
   )
)

# Define server logic
server <- function(input, output, session) {

  observe({
    precisely_function <- paste0(input$outcome, input$measure)

    function_args <- names(formals(precisely_function))

    function_args[1] <- "none"
    function_args <- function_args[function_args != "ci"]

    arg_labels <- case_when(
      function_args == "none" ~ "None",
      function_args == "exposed" ~ "Exposed Participants",
      function_args == "unexposed" ~ "Unexposed Participants",
      function_args == "exposed_cases" ~ "Exposed Cases",
      function_args == "exposed_controls" ~ "Exposed Controls",
      function_args == "group_ratio" ~ "Group Ratio",
      function_args == "prob" ~ "Probability"
    )

    names(function_args) <- arg_labels

    # Can also set the label and select items
    updateSelectInput(session, "group_var",
      label = "Group By",
      choices = function_args
    )
  })

  update_slider <- function(id, label, grouped, ungrouped, var) {
    renderUI({
      value <- if (input$group_var == var) grouped else ungrouped
      sliderInput(
        id,
        label = label,
        value = value,
        min = 0,
        max = 1
      )
    })
  }

  output$exposed_slider <- update_slider(
    id = "exposed",
    label = "Exposed Participants",
    grouped = c(.1, .2),
    ungrouped = .1,
    var = "exposed"
  )

  output$unexposed_slider <- update_slider(
    id = "unexposed",
    label = "Unexposed Participants",
    grouped = c(.1, .2),
    ungrouped = .1,
    var = "unexposed"
  )

  output$exposed_cases_slider <- update_slider(
    id = "exposed_cases",
    label = "Exposed Cases",
    grouped = c(.1, .2),
    ungrouped = .1,
    var = "exposed_cases"
  )

  output$exposed_controls_slider <- update_slider(
    id = "exposed_controls",
    label = "Exposed Controls",
    grouped = c(.1, .2),
    ungrouped = .1,
    var = "exposed_controls"
  )

  output$prob_slider <- update_slider(
    id = "prob",
    label = "The probability of the upper confidence interval being at or below the level of concern.",
    grouped = c(.7, .9),
    ungrouped = .9,
    var = "prob"
  )

  output$precision_desc <- renderUI({
    measure <- input$measure
    descr <- ifelse(measure %in% c("risk_difference", "rate_difference"),
                    "the absolute width of the CI",
                    "the Upper to Lower CI Ratio")
    tags$label(paste0("Level of Precision (", descr ,")"), class = "control-label")
  })

  process_slider <- function(x, groups = input$group_var_groups) {
    if (length(x) == 1) x <- c(x, x)
    seq(from = x[1], to = x[2], by = (x[2] - x[1]) / (groups - 1))
  }

  process_box <- function(group_from, group_to, groups = input$group_var_groups) {
    process_slider(c(group_from, group_to), groups = groups)
  }

  process_group_ratio <- function(measure = input$measure, group_var = input$group_var) {
    if (group_var == "group_ratio") {
      if (measure == "odds_ratio") {
        x <- process_box(input$group_ratio_cc_to, input$group_ratio_cc_from)
      } else {
        x <- process_box(input$group_ratio_cohort_to, input$group_ratio_cohort_from)
      }
    } else {
      if (measure == "odds_ratio") {
        x <- input$group_ratio_cc
      } else {
        x <- input$group_ratio_cohort
      }
    }
    x
  }

  precisely_data <- reactive({
    inputted_args <- list(
      "precision" = process_box(input$precision_from, input$precision_to, groups = 100),
      "n_cases" = process_box(input$n_from_cc, input$n_to_cc, groups = 100),
      "n_exposed" = process_box(input$n_from_cohort, input$n_to_cohort, groups = 100),
      "upper_limit" = process_box(input$upper_from, input$upper_to, groups = 100),
      "prob" = process_slider(input$prob),
      "exposed_cases" = process_slider(input$exposed_cases),
      "exposed_controls" = process_slider(input$exposed_controls),
      "exposed" = process_slider(input$exposed),
      "unexposed" = process_slider(input$unexposed),
      "group_ratio" = process_group_ratio(),
      "ci" = input$ci
    )

    precisely_function <- paste0(input$outcome, input$measure)

    function_args <- names(formals(precisely_function))

    expand.grid(inputted_args[function_args])
  })

  precisely_output <- reactive({
    precisely_function <- paste0(input$outcome, input$measure)
    map_precisely(getFunction(precisely_function), precisely_data())
  })

  output$precisely_table <- renderDataTable({
    precisely_output() %>%
      mutate_if(is.numeric, round, 2)
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("precisely_output", Sys.Date(), ".csv", sep = "")
    },
    content = function(con) {
      write.csv(precisely_output(), con)
    }
  )

  output$precisely_plot <- renderPlot({
    .data <- precisely_output()
    outcome <- input$outcome
    group_var <- input$group_var

    group_var_label <- case_when(
      group_var == "none" ~ "None",
      group_var == "exposed" ~ "Exposed Participants",
      group_var == "unexposed" ~ "Unexposed Participants",
      group_var == "exposed_cases" ~ "Exposed Cases",
      group_var == "exposed_controls" ~ "Exposed Controls",
      group_var == "group_ratio" ~ "Group Ratio",
      group_var == "prob" ~ "Probability"
    )

    if (group_var != "none") {
      .data <- .data %>%
        mutate(!!group_var_label := factor(round(!!ensym(group_var), 2))) %>%
        group_by(!!ensym(group_var_label))
    }

    if (outcome == "n_") {
      p <- plot_sample_size(.data, line_size = 2)
    } else if (outcome == "precision_") {
      p <- plot_precision(.data, line_size = 2)
    } else {
      p <- plot_upper_limit(.data, line_size = 2)
    }

    if (group_var != "none") p <- p + scale_color_viridis_d(name = group_var_label)

    p + theme_precisely(base_size = 28)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

