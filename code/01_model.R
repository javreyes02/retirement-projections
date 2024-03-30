rm(list = ls())
header <- source('code/header.R')

## Set parameters
current_age = 26
retire_age = 47
hh_income = 150000
retire_invest_annual = 50000
curr_retire_invest = 150000
exp_income_increase_per = 0.03
spend_retire = 120000
retire_years = 45

rr_pre_retire = .08
rr_post_retire = .052
inflation_per = 0.04

## Define key time variables
total_time <- retire_years + retire_age - current_age
pre_retire_time <- retire_age - current_age
years <- c(1:total_time)

## Set up model structure
start_model <- tibble(years = years) %>%
  ## inputs
  mutate(flag_pre_retire = ifelse(years <= pre_retire_time, 1, 0)) 

## Project investment growth, net spend
projections <- start_model %>%
  mutate(invest_grow = 0, 
         spend_inflate = 0,
         first_year = ifelse(years == min(years), 1, 0),
         switch_retire = ifelse(flag_pre_retire == 0 & lag(flag_pre_retire == 1), 1, 0)) %>%
  vctrs::vec_chop() %>%
  accumulate(function(out, new) {
    if (out$first_year == 1) {
      new$invest_grow <- (curr_retire_invest*(1 + rr_pre_retire)) + retire_invest_annual
    } else if (out$flag_pre_retire == 1) {
      new$invest_grow <- (out$invest_grow*(1 + rr_pre_retire)) + retire_invest_annual
    } else if (out$switch_retire == 1) {
      new$spend_inflate <- (spend_retire*(1 + inflation_per))
      new$invest_grow <- (out$invest_grow*(1 + rr_post_retire)) - out$spend_inflate
    } else {
      new$invest_grow <- (out$invest_grow*(1 + rr_post_retire)) - out$spend_inflate
      new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
    }
    new
  }) %>%
  bind_rows()

## Visualize
library(ggplot2)
projections %>%
  pivot_longer(cols = c(invest_grow, spend_inflate), names_to = "type", values_to = "amount") %>%
  ggplot(aes(x = years, y = amount, color = type)) +
  geom_line()
