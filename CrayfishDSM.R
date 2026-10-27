library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Crayfish Decision Support Tool 0.0.1"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fluidRow(
      # Box for Criterion Weights
      box(
        title = "1. Set Criteria Weights (Must sum to 100%)", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 4,
        sliderInput("w_path", "Pathogens:", min = 0, max = 100, value = 20),
        sliderInput("w_metal", "Metals:", min = 0, max = 100, value = 20),
        sliderInput("w_dens", "Abundance/Density:", min = 0, max = 100, value = 20),
        sliderInput("w_therm", "Thermal:", min = 0, max = 100, value = 20),
        sliderInput("w_sub", "Substrate:", min = 0, max = 100, value = 20),
        uiOutput("weight_warning")
      ),
      
      box(
        title = "2. Modifier values (applied to raw score, multiplicative)", 
        status = "primary", 
        solidHeader = TRUE, 
        width = 3,
        sliderInput("m_bold", "Boldness", min = 0, max = 5, value = 1, step=0.1),
        sliderInput("m_right", "righting", min = 0, max = 5, value = 1, step=0.1)
      ),
      
      # Box for Option Scoring
      box(
        title = "3. Score Options (Scale 1-5)", 
        status = "warning", 
        solidHeader = TRUE, 
        width = 8,
        fluidRow(
          column(4,
                 h4("Site Criteria"),
                 numericInput("a_path", "Pathogens:", value = 1, min = 0, max = 5),
                 numericInput("a_metals","Metals", value = 1, min = 0, max = 5),
                 numericInput("a_dens", "Abundance/Density", value = 0, min = 1, max = 10),
                 numericInput("a_therm", "Temperature", value = 1, min = 0, max = 5),
                 numericInput("a_sub", "Substrate", value = 1, min = 0, max = 5)
           )#,
          # column(4,
          #        h4("Location Option B"),
          #        numericInput("b_env", "Environmental Score:", value = 6, min = 1, max = 10),
          #        numericInput("b_econ", "Economic Score:", value = 9, min = 1, max = 10),
          #        numericInput("b_acc", "Accessibility Score:", value = 4, min = 1, max = 10)
          # ),
          # column(4,
          #        h4("Location Option C"),
          #        numericInput("c_env", "Environmental Score:", value = 4, min = 1, max = 10),
          #        numericInput("c_econ", "Economic Score:", value = 6, min = 1, max = 10),
          #        numericInput("c_acc", "Accessibility Score:", value = 9, min = 1, max = 10)
          # )
        )
      )
    ),
    
    fluidRow(
      # Results Chart
      box(
        title = "Decision Results", 
        status = "success", 
        solidHeader = TRUE, 
        width = 12,
        plotOutput("results_plot", height = "350px")
      )
    )
  )
)

# Define Server Logic
server <- function(input, output) {
  
  # Reactive calculation of total weights to ensure user stays at 100%
  total_weight <- reactive({
    input$w_path + input$w_metal + input$w_dens + input$w_therm + input$w_sub
  })
  
  # Render a warning if weights do not equal 100%
  output$weight_warning <- renderUI({
    tw <- total_weight()
    if (tw != 100) {
      p(paste0("⚠️ Current total weight is ", tw, "%. Please adjust to equal exactly 100%."), 
        style = "color: red; font-weight: bold;")
    } else {
      p("✅ Weights equal 100%.", style = "color: green; font-weight: bold;")
    }
  })
  
  # Calculate final weighted scores
  matrix_data <- reactive({
    # Convert weights to proportions
    wpath <- input$w_path / 100
    wmetal <- input$w_metal / 100
    wdens <- input$w_dens / 100
    wtherm <- input$w_therm / 100
    wsub <- input$w_sub / 100
    
    # Calculate totals
    score_a <- ((input$a_path*input$m_bold*input$m_right) * wpath) + ((input$a_metals*input$m_bold*input$m_right) * wmetal) + (input$a_therm * wtherm)+
      (input$a_sub * wsub)
    # score_b <- (input$b_env * we) + (input$b_econ * w_ec) + (input$b_acc * wa)
    # score_c <- (input$c_env * we) + (input$c_econ * w_ec) + (input$c_acc * wa)
    
    # Structure data frame for plotting
    data.frame(
      Option = c("Site Score"#, "Location B", "Location C"
                 ),
      WeightedScore = c(score_a#, score_b, score_c
                        )
    )
  })
  
  # Render the results bar chart
  output$results_plot <- renderPlot({
    df <- matrix_data()
    
    ggplot(df, aes(x = Option, y = WeightedScore, fill = Option)) +
      geom_bar(stat = "identity", width = 0.5, color = "black") +
      geom_text(aes(label = round(WeightedScore, 2)), vjust = -0.5, fontface = "bold", size = 5) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(
        x = "Spatial Options / Candidates",
        y = "Overall Weighted Score",
        title = "Multi-Criteria Decision Evaluation"
      ) +
      ylim(0, 11) + # Scale up to 10 max
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none"
      )
  })
}

# Run the Application 
shinyApp(ui = ui, server = server)