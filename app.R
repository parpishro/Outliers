library(shiny)
library(ggplot2)
library(broom)
library(gridExtra)

# Define UI
ui <- fluidPage(
  titlePanel("Simple Linear Regression: LS Fit Demo with Diagnostics"),
  
  sidebarLayout(
    sidebarPanel(
      width = 2,
      lapply(1:11, function(i) {
        numericInput(inputId = paste0("obs", i), 
                     label = paste("Observation", i), 
                     value = round(-45 + (2/3) * (160 + ((i-1) * 1)) + rnorm(1, mean = 0, sd = sqrt(1)), 2))
      })
    ),
    
    mainPanel(
      width = 10,
      actionButton("showAll", "All Plots"),
      actionButton("showReg", "Regression Plot"),
      actionButton("showLev", "Leverage Plot"),
      actionButton("showRes", "Residual Plot"),
      actionButton("showCooks", "Cook's Distance Plot"),
      uiOutput("plotOutput")
    )
  )
)

# Define server logic
server <- function(input, output) {
  data <- reactive({
    heights <- seq(160, 170, length.out = 11)
    weights <- sapply(1:11, function(i) input[[paste0("obs", i)]])
    data.frame(Height = heights, Weight = weights)
  })
  
  model <- reactive({
    lm(Weight ~ Height, data = data())
  })
  
  plotData <- reactive({
    list(
      regPlot = ggplot(data(), aes(x = Height, y = Weight)) +
        geom_point() +
        geom_smooth(method = "lm", col = "blue") +
        geom_vline(xintercept = mean(data()$Height), linetype = "dashed", color = "red") +
        geom_hline(yintercept = mean(data()$Weight), linetype = "dashed", color = "red") +
        labs(x = "Height (cm)", y = "Weight (kg)") +
        theme_minimal(),
      
      leveragePlot = ggplot(augment(model()), aes(x = Height, y = .hat)) +
        geom_point() +
        geom_hline(yintercept = 4/length(data()$Height), linetype = "dashed", color = "black") +
        labs(title = "Leverage Plot", x = "Height (cm)", y = "Leverage") +
        theme_minimal(),
      
      residualPlot = ggplot(augment(model()), aes(x = Height, y = .resid)) +
        geom_point() +
        geom_hline(yintercept = 0, color = "black") +
        labs(title = "Residual Plot", x = "Height (cm)", y = "Residuals") +
        theme_minimal(),
      
      cooksPlot = ggplot(augment(model()), aes(x = Height, y = .cooksd)) +
        geom_point() +
        geom_hline(yintercept = 4/length(data()$Height), linetype = "dashed", color = "black") +
        labs(title = "Cook's Distance Plot", x = "Height (cm)", y = "Cook's Distance") +
        theme_minimal()
    )
  })
  
  observeEvent(input$showAll, {
    output$plotOutput <- renderUI({
      plotOutput("allPlots", height = "800px")
    })
    output$allPlots <- renderPlot({
      grid.arrange(plotData()$regPlot, plotData()$leveragePlot, plotData()$residualPlot, plotData()$cooksPlot, ncol = 2)
    })
  })
  
  observeEvent(input$showReg, {
    output$plotOutput <- renderUI({
      plotOutput("regPlot", height = "400px")
    })
  })
  
  output$regPlot <- renderPlot({ plotData()$regPlot })
  
  observeEvent(input$showLev, {
    output$plotOutput <- renderUI({
      plotOutput("leveragePlot", height = "400px")
    })
  })
  
  output$leveragePlot <- renderPlot({ plotData()$leveragePlot })
  
  observeEvent(input$showRes, {
    output$plotOutput <- renderUI({
      plotOutput("residualPlot", height = "400px")
    })
  })
  
  output$residualPlot <- renderPlot({ plotData()$residualPlot })
  
  observeEvent(input$showCooks, {
    output$plotOutput <- renderUI({
      plotOutput("cooksPlot", height = "400px")
    })
  })
  
  output$cooksPlot <- renderPlot({ plotData()$cooksPlot })
}

# Run the application 
shinyApp(ui = ui, server = server)
