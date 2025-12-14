rm(list = ls())
# header <- source('code/header.R') 
library(shiny)
library(ggplot2)
library(scales)
library(shinyWidgets)
library(plotly)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Retirement Projections"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --- SECTION 1: TIMELINE ---
      h4("Timeline"),
      numericInput(inputId = "current_age", 
                   label = "Current Age", 
                   value = 25, 
                   min = 0, max = 100),
      
      numericInput(inputId = "retire_age", 
                   label = "Retirement Age", 
                   value = 65, 
                   min = 0, max = 100),
      
      numericInput(inputId = "retire_years", 
                   label = "Retirement Length (Years)", 
                   value = 25, 
                   min = 0, max = 100),
      
      hr(), # Horizontal Line for visual separation
      
      # --- SECTION 2: FINANCIALS ---
      h4("Financials"),
      
      autonumericInput(inputId = "curr_retire_invest",
                       label = "Current Investments",
                       value = 7000,
                       currencySymbol = "$",
                       digitGroupSeparator = ",",
                       decimalPlaces = 0),
      
      autonumericInput(inputId = "retire_invest_annual",
                       label = "Annual Contributions",
                       value = 6000,
                       currencySymbol = "$",
                       digitGroupSeparator = ",",
                       decimalPlaces = 0),
      
      # --- SECTION 3: CoastFI Section ---
      checkboxInput("coast_mode", "Enable 'CoastFI' Mode?", value = FALSE),
      
      conditionalPanel(
        condition = "input.coast_mode == true",
        wellPanel( # wellPanel gives it a gray background to stand out
          helpText("Stop/reduce contributions at age:"),
          numericInput("coast_age", "Coast Age", 40, 0, 100),
          autonumericInput("coast_contrib", "New Contribution Amount", 0, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0)
        )
      ),
      
      autonumericInput(inputId = "spend_retire",
                       label = "Annual Spending in Retirement (Today's $)",
                       value = 60000,
                       currencySymbol = "$",
                       digitGroupSeparator = ",",
                       decimalPlaces = 0),
      
      autonumericInput(inputId = "retire_income",
                       label = "Other Retirement Income (SS/Pension)",
                       value = 0,
                       currencySymbol = "$",
                       digitGroupSeparator = ",",
                       decimalPlaces = 0),
      
      hr(),
      
      # --- SECTION 4: RATES ---
      h4("Market Assumptions"),
      
      numericInput(inputId = "pre_retire_roi",
                   label = "Pre-Retirement Return (%)",
                   value = 7,
                   step = 0.1),
      
      numericInput(inputId = "post_retire_roi",
                   label = "Post-Retirement Return (%)",
                   value = 4,
                   step = 0.1),
      
      sliderInput(inputId = "inflation_rate",
                  label = "Inflation Rate:",
                  min = 0, max = 10,
                  value = 3,
                  step = 0.25,
                  post = "%")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Line graph of projections ----
      plotlyOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    current_age = input$current_age
    retire_age = input$retire_age
    retire_invest_annual = input$retire_invest_annual
    curr_retire_invest = input$curr_retire_invest
    spend_retire = input$spend_retire
    retire_years = input$retire_years
    
    coast_mode = input$coast_mode
    coast_age = input$coast_age
    coast_contrib = input$coast_contrib
    
    rr_pre_retire = input$pre_retire_roi / 100
    rr_post_retire = input$post_retire_roi / 100
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
        
        # Default to standard contribution
        this_year_contrib <- retire_invest_annual
        
        # If Coast Mode is ON and we have passed the Coast Age, switch to the new amount
        if (coast_mode == TRUE && new$age > coast_age) {
          this_year_contrib <- coast_contrib
        }
        
        if (out$first_year == 1) {
          new$invest_grow <- (curr_retire_invest*(1 + rr_pre_retire)) + this_year_contrib
        } else if (new$switch_retire == 1) {
          new$invest_grow <- (out$invest_grow - future_spend_start)*(1 + rr_post_retire)
          new$spend_inflate <- future_spend_start
        } else if (out$flag_pre_retire == 1) {
          # UPDATED: Now uses 'this_year_contrib' instead of the static input
          new$invest_grow <- (out$invest_grow*(1 + rr_pre_retire)) + this_year_contrib
        }  else {
          new$invest_grow <- (out$invest_grow - out$spend_inflate)*(1 + rr_post_retire)
          new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
        }
        new
      }) %>%
      bind_rows() 
    
    ## graphing
    p <- projections %>%
      pivot_longer(cols = c(invest_grow, spend_inflate), names_to = "type", values_to = "amount") %>%
      ggplot(aes(x = age, y = amount, color = type, group = type)) +
      geom_line(aes(text = paste0("Age: ", age, "<br>", 
                                  "Amount: ", scales::dollar(amount, accuracy = 1))), 
                linewidth = 1.2) + 
      labs(title = "Retirement Projections", x = "Age") +
      scale_y_continuous(name = "", 
                         labels = scales::dollar_format(),
                         breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 15)) +
      scale_color_manual(values = c("invest_grow" = "#2E86C1", "spend_inflate" = "#E74C3C"),
                         labels = c('Retirement Investments', "Annual Spending (Future $)")) + 
      theme_light() +
      theme(
        legend.position = 'bottom',
        plot.title = element_text(size = 18, hjust = 0.5)
      ) 
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2), # Fix legend position for plotly
             hovermode = "x")
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)