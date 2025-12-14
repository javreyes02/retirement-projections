rm(list = ls())
header <- source('code/header.R') 
library(shiny)
library(ggplot2)
library(scales)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Retirement Projections"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput(inputId = "current_age", 
                   label = "Current Age", 
                   value = 25, 
                   min = 0, 
                   max = 100),
      
      numericInput(inputId = "retire_age", 
                   label = "Retirement Age", 
                   value = 65, 
                   min = 0, 
                   max = 100),
      
      sliderInput(inputId = "inflation_rate",
                  label = "Inflation Rate:",
                  min = 0,
                  max = 10,
                  value = 3, 
                  step = 0.25,
                  post = "%"),
      
      sliderInput(inputId = "retire_invest_annual",
                  label = "Annual Retirement Contributions:",
                  min = 0,          
                  max = 200000,      
                  value = 6000),    
      
      sliderInput(inputId = "curr_retire_invest",
                  label = "Current Retirement Investments:",
                  min = 0,          
                  max = 1000000,     
                  value = 7000),    
      
      sliderInput(inputId = "spend_retire",
                  label = "Annual Spending in Retirement (Today's $):",
                  min = 10000,
                  max = 500000,
                  value = 60000),  
      
      numericInput(inputId = "retire_years", 
                   label = "Retirement Length in Years", 
                   value = 25,      
                   min = 0, 
                   max = 100)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line graph of projections ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    current_age = input$current_age
    retire_age = input$retire_age
    retire_invest_annual = input$retire_invest_annual
    curr_retire_invest = input$curr_retire_invest
    spend_retire = input$spend_retire
    retire_years = input$retire_years
    
    rr_pre_retire = .08
    rr_post_retire = .052
    inflation_per = input$inflation_rate / 100
    
    # Setup -------------------------------------------------------------------
    
    ## Define key time variables
    total_time <- retire_years + retire_age - current_age
    pre_retire_time <- retire_age - current_age
    years <- c(0:total_time)
    
    # Calculate the future dollar value of the desired spending 
    # based on inflation between now and retirement age.
    future_spend_start <- spend_retire * ((1 + inflation_per) ^ pre_retire_time)

    
    ## Set up model structure
    start_model <- tibble(years = years) %>%
      ## inputs
      mutate(
        age = current_age + years,
        flag_pre_retire = ifelse(years < pre_retire_time, 1, 0)) 
    
    
    # Projections -------------------------------------------------------------
    
    projections <- start_model %>%
      mutate(invest_grow = 0, 
             spend_inflate = 0,
             first_year = ifelse(years == min(years), 1, 0),
             invest_grow = ifelse(first_year == 1, curr_retire_invest, 0),
             switch_retire = ifelse(flag_pre_retire == 0 & lag(flag_pre_retire) == 1, 1, 0)) %>%
      vctrs::vec_chop() %>%
      accumulate(function(out, new) {
        if (out$first_year == 1) {
          new$invest_grow <- (curr_retire_invest*(1 + rr_pre_retire)) + retire_invest_annual
        } else if (new$switch_retire == 1) {
          new$invest_grow <- (out$invest_grow - future_spend_start)*(1 + rr_post_retire)
          new$spend_inflate <- future_spend_start
        } else if (out$flag_pre_retire == 1) {
          new$invest_grow <- (out$invest_grow*(1 + rr_pre_retire)) + retire_invest_annual
        }  else {
          new$invest_grow <- (out$invest_grow - out$spend_inflate)*(1 + rr_post_retire)
          new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
        }
        new
      }) %>%
      bind_rows() 
    
    ## graphing
    projections %>%
      pivot_longer(cols = c(invest_grow, spend_inflate), names_to = "type", values_to = "amount") %>%
      ggplot(aes(x = age, y = amount, color = type)) +
      geom_line(linewidth = 1.2) + # Added linewidth for better visibility
      labs(title = "Retirement Projections", x = "Age") +
      scale_y_continuous(name = "", labels = scales::dollar_format()) +
      scale_color_manual(values = c("invest_grow" = "#2E86C1", "spend_inflate" = "#E74C3C"),
                         labels = c('Retirement Investments', "Annual Spending (Future $)")) + 
      theme_light() +
      theme(
        legend.position = 'bottom',
        plot.title = element_text(size = 18, hjust = 0.5)
      ) 
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
