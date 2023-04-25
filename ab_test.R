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
install.packages("skimr")
install.packages("infer")
install.packages("modelsummary")
install.packages("broom")
install.packages("purrr")
install.packages("viridis")
install.packages("estimatr")


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
skim(df)

datasummary_balance(~version, data = df)

# --- Plot Distribution of Sales ---# 
df %>%
    filter(sales > 0) %>%
    ggplot() + 
    geom_histogram(aes(x=sales, fill = version), alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

df %>%
    filter(sales > 0) %>%
    ggplot() + 
    geom_histogram(aes(x=sales, fill = version), alpha = 0.5) +
    facet_wrap(~source) # split data by email and internet
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

# --- T-test, difference in means --- #
t_test(df, 
       sales ~ version)


# --- Simple Linear Regression ---# 
model_simple <- lm(sales ~ version,
                   data = df
                   )

summary(model_simple)
# intercept average sales of version A, versionB = difference betweeen version A and B, on average B is higher 0.4124 cent then A, but we can not say that because it is not statistically significant. 


# --- Regression by Group --- #
# Are there differences between email and internet
model_email <- lm(sales ~ version,
                  data = df %>% filter(source=="email")
                  )


model_internet <- lm(sales ~ version,
                     data = df %>% filter(source=="internet"))

summary(model_email)
summary(model_internet)
# version B and version a do not generate different sales(not significant)

# the above is for all consumers (no matter whether sales +ve sales?)
# what if i want to focus only on consumers that have +ve sales? 
model_conditional <- lm(sales ~ version,
                        data = df %>% filter(sales > 0))
summary(model_conditional)

# split this into email and internet
model_conditional_email <- lm(sales ~ version,
                              data = df %>% filter(sales > 0) %>% 
                                     filter(source == "email"))

model_conditional_internet <- lm(sales ~ version,
                              data = df %>% filter(sales > 0) %>% 
                                  filter(source == "internet"))
summary(model_conditional_email)
# email spend positive amount of money version b increses expenditire relative to average on A for email and who spend money.

summary(model_conditional_internet)
# for internet this is not significant 
# for guys spend money for email guys they spend more on version b. 

# sales > 0 who spend money
#                internet    email 
# version a           
# version b.                 better


# --- Displaying Regression Output ---# 
# Regression Table
model_list <- 
    list(model_conditional_email,
         model_conditional_internet)

modelsummary(model_list)

modelsummary(model_list,
             fmt = 2,
             gof_omit = "AIC|BIC|Log|F|RMSE")


# Coefficient Plot

model_conditional_internet <- lm(sales ~ version,
                                 data = df %>% filter(sales > 0) %>% 
                                        filter(source == "internet"))



# --- Heteroskedasticity and Robust Standard Errors --- #
# Homoscedasticity: variance of error is constant(strong assumption)
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
model_het <- lm_robust(sales ~ version,
                       data = df)

summary(model_het) # more realistic assumption that variance differ across individuals
summary(model_simple)


# Assumption: version b guys would have acted the same way as those that saw version a if they see version a
# what if not the case ? 

#                                   could have seen?
#                               a               b
#                   a           b0              
# actual saw        b           b0+delta        b0+delta+b1
# 
# do our std difference 
# b0+delta+ b1 - b0 = delta +b1 
# what if I had 2 time periods
# before and after 
# two groups version A and version B
#                 BEFORE                AFTER                   diff rowwise
# SAW A         b0                      b0 + t 
# SAW B         b0 + delta              b0 + delta + t + b1
# diff          delta                   delta + b1                b1

# b1 = treatment effect












