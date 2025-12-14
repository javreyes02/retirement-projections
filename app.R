rm(list = ls())
# header <- source('code/header.R') 
library(shiny)
library(ggplot2)
library(scales)
library(shinyWidgets)
library(plotly)

ui <- fluidPage(
  
  titlePanel("Retirement Projections"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # --- SECTION 1: TIMELINE ---
      h4("Timeline"),
      numericInput("current_age", "Current Age", 25, 0, 100),
      numericInput("retire_age", "Retirement Age", 65, 0, 100),
      numericInput("retire_years", "Retirement Length (Years)", 25, 0, 100),
      hr(), 
      
      # --- SECTION 2: FINANCIALS ---
      h4("Financials"),
      autonumericInput("curr_retire_invest", "Current Investments", 7000, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0),
      autonumericInput("retire_invest_annual", "Annual Contributions", 6000, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0),
      
      # --- CoastFI Section ---
      checkboxInput("coast_mode", "Enable 'CoastFI' Mode?", value = FALSE),
      conditionalPanel(
        condition = "input.coast_mode == true",
        wellPanel(
          helpText("Stop/reduce contributions at age:"),
          numericInput("coast_age", "Coast Age", 40, 0, 100),
          autonumericInput("coast_contrib", "New Contribution Amount", 0, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0)
        )
      ),
      # ---------------------
      
      autonumericInput("spend_retire", "Annual Spending in Retirement (Today's $)", 60000, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0),
      autonumericInput("retire_income", "Other Retirement Income (SS/Pension)", 0, currencySymbol = "$", digitGroupSeparator = ",", decimalPlaces = 0),
      hr(),
      
      # --- SECTION 3: RATES ---
      h4("Market Assumptions"),
      numericInput("pre_retire_roi", "Pre-Retirement Return (%)", 7, step = 0.1),
      numericInput("post_retire_roi", "Post-Retirement Return (%)", 4, step = 0.1),
      sliderInput("inflation_rate", "Inflation Rate:", min = 0, max = 10, value = 3, step = 0.25, post = "%")
    ),
    
    mainPanel(
      plotlyOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    # Inputs
    current_age = input$current_age
    retire_age = input$retire_age
    retire_invest_annual = input$retire_invest_annual
    curr_retire_invest = input$curr_retire_invest
    spend_retire = input$spend_retire
    retire_income = input$retire_income # <--- Ensure this is grabbed
    retire_years = input$retire_years
    
    # CoastFI Inputs
    coast_mode = input$coast_mode
    coast_age = input$coast_age
    coast_contrib = input$coast_contrib
    
    rr_pre_retire = input$pre_retire_roi / 100
    rr_post_retire = input$post_retire_roi / 100
    inflation_per = input$inflation_rate / 100
    
    # Setup -------------------------------------------------------------------
    
    total_time <- retire_years + retire_age - current_age
    pre_retire_time <- retire_age - current_age
    years <- c(0:total_time)
    
    # --- MATH FIX 1: Inflate Income too ---
    # Just like spending, your Pension/SS usually grows (COLA) or needs to be valued in future dollars
    future_spend_start <- spend_retire * ((1 + inflation_per) ^ pre_retire_time)
    future_income_start <- retire_income * ((1 + inflation_per) ^ pre_retire_time)
    # --------------------------------------
    
    start_model <- tibble(years = years) %>%
      mutate(age = current_age + years, 
             flag_pre_retire = ifelse(years < pre_retire_time, 1, 0)) 
    
    
    # Projections -------------------------------------------------------------
    
    projections <- start_model %>%
      mutate(invest_grow = 0, 
             spend_inflate = 0,
             income_inflate = 0, # <--- Track Income
             first_year = ifelse(years == min(years), 1, 0),
             invest_grow = ifelse(first_year == 1, curr_retire_invest, 0),
             switch_retire = ifelse(flag_pre_retire == 0 & lag(flag_pre_retire) == 1, 1, 0)) %>%
      vctrs::vec_chop() %>%
      accumulate(function(out, new) {
        
        # CoastFI Logic
        this_year_contrib <- retire_invest_annual
        if (coast_mode == TRUE && new$age > coast_age) {
          this_year_contrib <- coast_contrib
        }
        
        if (out$first_year == 1) {
          # Year 1
          new$invest_grow <- (curr_retire_invest*(1 + rr_pre_retire)) + this_year_contrib
          
        } else if (new$switch_retire == 1) {
          # --- MATH FIX 2: Net Withdrawal (Transition Year) ---
          net_withdraw <- future_spend_start - future_income_start
          new$invest_grow <- (out$invest_grow - net_withdraw)*(1 + rr_post_retire)
          
          new$spend_inflate <- future_spend_start
          new$income_inflate <- future_income_start
          # ----------------------------------------------------
          
        } else if (out$flag_pre_retire == 1) {
          # Accumulation
          new$invest_grow <- (out$invest_grow*(1 + rr_pre_retire)) + this_year_contrib
          
        }  else {
          # --- MATH FIX 3: Net Withdrawal (Post-Retire) ---
          new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
          new$income_inflate <- (out$income_inflate*(1 + inflation_per))
          
          # Withdrawal is Spending MINUS Income
          net_withdraw <- new$spend_inflate - new$income_inflate
          
          new$invest_grow <- (out$invest_grow - net_withdraw)*(1 + rr_post_retire)
          # ------------------------------------------------
        }
        new
      }) %>%
      bind_rows() 
    
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
      layout(legend = list(orientation = "h", x = 0.1, y = -0.2), 
             hovermode = "x")
  })
}

shinyApp(ui = ui, server = server)