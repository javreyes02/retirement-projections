rm(list = ls())
header <- source('code/header.R')

## Set parameters
# current_age = 26
# retire_age = 50
# hh_income = 150000
# retire_invest_annual = 50000
# curr_retire_invest = 150000
# exp_income_increase_per = 0.03
# spend_retire = 120000
# retire_years = 35
# 
# rr_pre_retire = .08
# rr_post_retire = .052
# inflation_per = 0.04

error_num_inputs <- function(input, friendly_name) {
  if(input < 0) {
    stop(glue("{friendly_name} is an invalid number. Please input value greater than 0."))
  }
}

get_projections <- function(
    
  current_age,
  retire_age,
  retire_invest_annual,
  curr_retire_invest,
  spend_retire,
  retire_years,
  rr_pre_retire = .08,
  rr_post_retire = .052,
  inflation_per = 0.04
  
) {
 
# error handling ----------------------------------------------------------


  if (current_age > retire_age) {
    stop("Current age is greater than retirement age. Please input valid ages.")
  } 
  
  input_xwalk <- tribble(
    ~input, ~friendly_name,
    retire_invest_annual, "Annual Retirement Contribution",
    curr_retire_invest, "Current Retirement Investments",
    spend_retire, "Yearly Retirement Spending"
  )
  
  pmap(input_xwalk, error_num_inputs)
  

# Setup -------------------------------------------------------------------
  
  ## Define key time variables
  total_time <- retire_years + retire_age - current_age
  pre_retire_time <- retire_age - current_age
  years <- c(0:total_time)
  
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
        new$invest_grow <- (out$invest_grow*(1 + rr_post_retire)) - spend_retire
        new$spend_inflate <- spend_retire
      } else if (out$flag_pre_retire == 1) {
        new$invest_grow <- (out$invest_grow*(1 + rr_pre_retire)) + retire_invest_annual
      }  else {
        new$invest_grow <- (out$invest_grow*(1 + rr_post_retire)) - out$spend_inflate
        new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
      }
      new
    }) %>%
    bind_rows() 
  
}

inputs <- tribble(~current_age, ~retire_age, ~retire_invest_annual, ~curr_retire_invest, ~spend_retire, ~retire_years,
        26, 50, 50000, 150000, 120000, 50)

projections <- pmap(inputs, get_projections)

## Visualize
library(ggplot2)
library(scales)

make_graph <- function(df) {
  df %>%
    pivot_longer(cols = c(invest_grow, spend_inflate), names_to = "type", values_to = "amount") %>%
    ggplot(aes(x = age, y = amount, color = type)) +
    geom_line() + 
    labs(title = "Retirement Projections", x = "Age") +
    scale_y_continuous(name = "", labels = scales::dollar_format()) +
    scale_color_discrete(name = "", labels = c('Retirement Investments', "Retirement Spending")) +
    theme_light() +
    theme(
      legend.position = 'bottom',
      plot.title = element_text(size = 18, hjust = 0.5)
    ) 
  
}

map(projections, make_graph)

