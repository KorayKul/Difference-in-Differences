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
skim(df)

# summarize data by version
datasummary_balance(~version, data = df)

# --- Plot Distribution of Sales ---# 
df %>%
    filter(sales > 0) %>%
    ggplot() + 
    geom_histogram(aes(x=sales, fill = version), alpha = 0.5) +
    scale_fill_viridis(discrete = TRUE) +
    theme_bw()

# --- T-test, difference in means --- #
df %>% 
    group_by(version) %>%
    summarize(mean_sales = mean(sales, na.rm = TRUE))

t_test(df, 
       sales ~ version, 
       order = c("A", "B"), 
       var.equal = FALSE
       )

# --- Simple Linear Regression ---# 
model_simple <- lm(sales ~ version, data = df)
summary(model_simple)
tidy(model_simple)
glance(model_simple)

# With a binary y?
model_simple_binary <- lm(sale_indicator ~ version, data = df)
tidy(model_simple_binary)
glance(model_simple_binary)

# What about conditional on any sales being +ve?
model_conditional <-
    df %>%
    filter(sales > 0) %>%
    lm(sales ~ version, data = .)
    
tidy(model_conditional)

# log y, with conditional sales
model_log_conditional <-
    df %>%
    filter(sales > 0) %>%
    lm(log(sales) ~ version, data = .)

tidy(model_log_conditional)

# --- Regression by Group --- #
# Are there differences between email and internet
model_by_source <- 
    df %>%
    group_by(source) %>%
    nest() %>%
    mutate(
        model = map(data, ~ lm(sales  ~ version, data = .))
    )

model_by_source %>%
    mutate(tidy_mod = map(model, tidy)) %>%
    select(tidy_mod) %>%
    unnest(cols = c(tidy_mod))

# conditional on +ve sale?
model_by_source <- 
    df %>%
    filter(sales > 0) %>%
    group_by(source) %>%
    nest() %>%
    mutate(
        model = map(data, ~ lm(log(sales)  ~ version, data = .))
    )

model_by_source %>%
    mutate(tidy_mod = map(model, tidy)) %>%
    select(tidy_mod) %>%
    unnest(cols = c(tidy_mod))

# probability of a sale?
model_by_source <- 
    df %>%
    group_by(source) %>%
    nest() %>%
    mutate(
        model = map(data, ~ lm(sale_indicator  ~ version, data = .))
    )

model_by_source %>%
    mutate(tidy_mod = map(model, tidy)) %>%
    select(tidy_mod) %>%
    unnest(cols = c(tidy_mod))

# --- Displaying Regression Output ---# 
# Regression Table
models <- list(model_simple, 
               model_conditional,
               model_log_conditional)

modelsummary(models = dvnames(models),
             coef_rename  = c("versionB" = "Version B"),
             fmt = 2,
             coef_omit = "Intercept",
             gof_omit = 'AIC|BIC|F|RMSE|Log')

# Coefficient Plot
modelplot(model_by_source$model, 
          coef_omit = 'Interc'
            )
    
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
lmtest::bptest(model_simple)

# Heteroskedasticity Robust Standard Errors
lm_robust(sales~version, data = df)
lm_robust(sales~version, data = df, se_type = "HC3")

