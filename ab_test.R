#' ab_test.R
#'
#' Contributor: lachlandeer
#'
#' What this file does:
#' 
#' - Summary statistics for landing pages data
#' - t test for landing pages data
#' - simple linear regression for landing pages data
#' - simple linear regression by group (internet / email)
#' - regression tables via modelsummary
#' - coefficient plots with modelplot
#' - Heteroskedasticity Robust Standard Errors

# --- Load Libraries --- # 
library(readr)
library(dplyr)
library(tidyr)
library(skimr)
library(infer)
library(modelsummary)
library(broom)
library(purrr)
library(ggplot2)
library(viridis)
library(estimatr)

# --- Load data --- # 
df <- 
    read_csv("data/landing_pages.csv")  %>%
    # adds a binary sales indicator
    mutate(sale_indicator = if_else(sales > 0, 1, 0))

#--- Descriptive Statistics --- #
YOUR CODE

# --- Plot Distribution of Sales ---# 
df %>%
    filter(sales > 0) %>%
    ggplot() + 
    geom_histogram(aes(x=sales, fill = version), alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

# --- T-test, difference in means --- #
YOUR CODE

# --- Simple Linear Regression ---# 
YOUR CODE

# --- Regression by Group --- #
# Are there differences between email and internet
YOUR CODE

# --- Displaying Regression Output ---# 
# Regression Table
YOUR CODE

# Coefficient Plot
YOUR CODE

# --- Heteroskedasticity and Robust Standard Errors --- #
# Evidence of heteroskdasticty?
df <- 
  df %>%
  mutate(resid = resid(model_simple),
         predict = predict(model_simple))

df %>%
  ggplot() +
  geom_point(aes(y = resid, x = predict))

# A "test" for heteroskedasticity
YOUR CODE

# Heteroskedasticity Robust Standard Errors
YOUR CODE


