source("tsa.R", local = T)

ui <- (
  shinyUI(
    dashboardPage(
      skin = "green",
      title = "Time Series Energy Forecast for USA",
      # 3 major components: Header, Sidebar, Body
      # Component 1: Header
      dashboardHeader(title = "Energy Forecast"),
      # Component 2: Sidebar
      dashboardSidebar(
        collapsed = T,
        sidebarMenu(
          menuItem("Home", icon = icon("house", verify_fa = F), tabName = "homeTab"),
          menuItem("Data Visualization", icon = icon("table", verify_fa = F), tabName = "datasets"),
          menuItem("Forecast", icon = icon("fa-duotone fa-chart-line", verify_fa = F),
                   tabName = "forecast"),
          menuItem("Compare Models", icon = icon("fa-solid fa-chart-line", verify_fa = F),
                   tabName = "compareModelPlots"),
          menuItem("Predict Future", icon = icon("fa-dutone fa-magnifying-glass", verify_fa = F),
                   tabName = "predict"),
          menuItem("Years to Goal", icon = icon("fa-duotone fa-bullseye", verify_Fa = F),
                   tabName = "ytg"),
          menuItem("Visualize Years to Goal", icon = icon("fa-solid fa-magnifying-glass-chart", 
                                                          verify_fa = F), tabName = "vytg")
        )
      ),
      # Component 3: Body
      dashboardBody(
        tabItems(
          tabItem(
            # code for home tab
            tabName = "homeTab", 
            div(class = "container", tags$img(src = "sts.png"))
          ),
          # code for data visualization tab
          tabItem(
            tabName = "datasets",
            fluidRow(
              column(2, selectInput("selectXaxis", label = "Choose X-axis",
                                    choices = "Time", multiple = F)),
              column(3, selectInput("selectYaxis", choices = viz_cols, 
                                    label = "Choose Data Series", multiple = F)),
              column(2, selectInput("add_ma", label = "Add Moving Average", 
                                    choices = c("Yes", "No"), multiple = F)),
              column(1, HTML("<br><br>"), actionButton("display", label = "Display", class = "btn"))
            ),
            fluidRow(
              column(1),
              column(10, withSpinner(plotOutput("data_Viz"), type = 8)),
              column(1)
            )
          ),
          # Create Forecast tab
          tabItem(tabName = "forecast",
                  fluidRow(
                    column(3, selectInput("series", label = "Select Data Series", 
                                          choices = viz_cols, multiple = F)),
                    column(3, selectInput("nValid", label = "Validation Period", choices = c(50:100),
                                          selected = 84, multiple = F)),
                    column(1, HTML("<br><br>"), actionButton("compute", label = "Compute", class = "btn"))
                  ),
                  fluidRow(column(1),
                           column(10, DTOutput("accrTable") |> withSpinner(type = 7)),
                           column(1)
                           )
                  ),

          # Compare Models Page
          tabItem(tabName = "compareModelPlots",
                  fluidRow(
                    column(3, selectInput("parameter", label = "Select Data Series", 
                                          choices = viz_cols, multiple = F)),
                    column(3, selectInput("variable", label = "Choose Models",
                          choices = c("Trend Accuracy" = "Trend", "Season Accuracy" = "Season", 
                          "Polynomial Trend Accuracy" = "Polynomial", 
                          "Exponential Season Accuracy" = "Exponential Season", 
                          "Seasonal Naive Accuracy" = "Seasonal Naive", 
                          "Simple Exponential Smoothing Accuracy" = "Exponential Smoothing", 
                          "Holt-Winter Model Accuracy" = "Holt-Winter", "Arima Model Accuracy" = "ARIMA"),
                                                 selected = "Trend", multiple = T)),
                    column(2, selectInput("time_period", label = "Choose Validation Duration", 
                                          choices = c(50:100), selected = 84, multiple = F)),
                    column(1, HTML("<br><br>"), actionButton("showModelPlots", label = "Visualize", 
                                                             class = "btn"))
                  ),
                  fluidRow(column(1),
                           column(10, plotOutput("outputModel") |> withSpinner(type = 6)),
                           column(1)
                           )
                  ),
          
          # Predict Future Tab
          tabItem(tabName = "predict",
                  fluidRow(
                    column(3, selectInput("dataseries", label = "Select Data Series",
                                          choices = viz_cols, multiple = F)),
                    column(3, selectInput("futurePeriod", label = "Select Time Period", 
                                          choices = c(1:120), selected = 84, multiple = F)),
                    column(1, HTML("<br><br>"), actionButton("showForecast", label = "Compute", 
                                                             class = "btn"))
                  ),
                  fluidRow(
                    column(1),
                    column(10, withSpinner(plotOutput("plotPredictions"), type = 3, 
                                           color.background = "lavender")),
                    column(1)
                  )
                  ),
          # Years to Goal Tab
          tabItem(tabName = "ytg",
                  fluidRow(
                    column(3, selectInput("selectSeries1", label = "Choose first Series", 
                                          choices = viz_cols, multiple = F)),
                    column(3, selectInput("selectSeries2", label = "Choose second Series", 
                                          choices = viz_cols, multiple = F)),
                    column(2, selectInput("selectRatio", label = "Choose Ratio", multiple = F,
                                          choices = seq(0.1, 1, 0.05), selected = 0.25)),
                    column(2, selectInput("selectPred", label = "Choose Prediction Duration", 
                                          choices = c(1:45), selected = 10, multiple = F)),
                    column(1, HTML("<br><br>"), actionButton("computeTime", label = "Compute", 
                                                             class = "btn"))
                  ),
                  fluidRow(
                    column(3),
                    column(6, DTOutput("resultTable") |> withSpinner(type = 4)), 
                    column(3)
                  )
                  ),
          #Visualize Years to Goal Tab
          tabItem(tabName = "vytg",
                  fluidRow(
                    column(3, selectInput("ser1", label = "Choose first Series", 
                                          choices = viz_cols, multiple = F)),
                    column(3, selectInput("ser2", label = "Choose second Series", 
                                          choices = viz_cols, multiple = F)),
                    column(2, selectInput("chooseRatio", label = "Choose Ratio", multiple = F,
                                          choices = seq(0.1, 1, 0.05), selected = 0.25)),
                    column(2, selectInput("selectPredict", label = "Choose Prediction Duration", 
                                          choices = c(1:45), selected = 10, multiple = F)),
                    column(1, HTML("<br><br>"), actionButton("displayPlot", label = "Display", 
                                                             class = "btn"))
                  ),
                  fluidRow(
                    column(1),
                    column(10, plotOutput("resultPlot") |> withSpinner(type = 6)), 
                    column(1) 
                  )
                  )
        ),
        # Place it within the body tag, not within page tag
        tags$head(
          tags$script(src = "sidebar.js"),
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
          tags$link(rel="stylesheet", type = "text/css",
            href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.3.0/css/fontawesome.min.css"),
          tags$link(rel = "shortcut icon", href = "logo.ico")
        ),
        tags$style("#resultTable{font-size: 20px}") # increase font-size of textOutput
      )
    )
  )
)

server <- (
  shinyServer(function(input, output, session){
    # Data Visualization Tab
    # run the event for plot on button click
    plot_data <- eventReactive(input$display, {
      plot_func(x_axis = input$selectXaxis, y_axis = input$selectYaxis, add_mov_avg = input$add_ma)
    })
    
    # render the plot output using renderPlot
    output$data_Viz <- renderPlot(plot_data(), height = 600)
    
    # render the table for forecasting
    model_accr <- eventReactive(input$compute, {
      ts_func(data = input$series, validation = input$nValid)
    })
    output$accrTable <- renderDT(model_accr())
    
    
    # Compare Models Page
    func_input <- eventReactive(input$showModelPlots, {
      variables <- input$variable
      ts_multi_plot(data = input$parameter, validation = input$time_period, variables = variables)
    })
    
    output$outputModel <- renderPlot(func_input(), height = 600)
    
    # Predict Future Tab
    forecast_input <- eventReactive(input$showForecast, {
      forecast_func(data = input$dataseries, time = input$futurePeriod)
    })
    
    output$plotPredictions <- renderPlot(forecast_input(), height = 600)

    # Years to Goal Tab
    ytg_input <- eventReactive(input$computeTime, {
      years_to_reach(ratio = input$selectRatio, series1 = input$selectSeries1, 
                     pred_start = input$selectPred, series2 = input$selectSeries2)
    })
    
    output$resultTable <- renderDT(ytg_input())
    
    vytg_input <- eventReactive(input$displayPlot, {
      plot_years_to_reach(ratio = input$chooseRatio, pred_start = input$selectPredict, 
                          series1 = input$ser1, series2 = input$ser2)
    })
    output$resultPlot <- renderPlot(vytg_input(), height = 600)
  })
)

shinyApp(ui = ui, server = server)