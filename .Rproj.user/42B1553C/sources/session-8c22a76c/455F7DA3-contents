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
  mutate(curr_retire_invest = curr_retire_invest,
         spend_retire = spend_retire,
         flag_pre_retire = ifelse(years <= pre_retire_time, 1, 0)) 

## Project pre-retirement investment growth
pre_retire_projections <- start_model %>%
  filter(flag_pre_retire == 1) %>%
  ## calculations
  mutate(
  ## investment growth
  invest_grow = accumulate(curr_retire_invest, ~(.x*(1 + rr_pre_retire)) + retire_invest_annual)
  )

end_pre_retire_invest = pre_retire_projections %>% tail(1) %>% pull(invest_grow)

## Project post-retirement investment growth, net spend
post_retire_projections <- start_model %>%
  filter(flag_pre_retire == 0) %>%
  ## left_join(post_retire_spend_projections) %>%
  mutate(invest_grow = 0, 
         spend_inflate = 0,
         first_year = ifelse(years == min(years), 1, 0)) %>%
  vctrs::vec_chop() %>%
  accumulate(function(out, new) {
    if (out$first_year == 1) {
      new$spend_inflate <- (spend_retire*(1 + inflation_per))
      new$invest_grow <- (end_pre_retire_invest*(1 + rr_post_retire)) - out$spend_inflate
    } else {
      new$invest_grow <- (out$invest_grow*(1 + rr_post_retire)) - out$spend_inflate
      new$spend_inflate <- (out$spend_inflate*(1 + inflation_per))
    }
    new
  }) %>%
  bind_rows()

# mutate(spend_inflate = accumulate(spend_retire, ~(.x*(1 + inflation_per)))) %>%

         
         # ~(.x*(1 + rr_post_retire)) - .y)

## Combine projections
stack <- pre_retire_projections %>% bind_rows(post_retire_projections)

## Visualize
library(ggplot2)
ggplot(stack, aes(x = years, y = invest_grow)) +
  geom_line()
