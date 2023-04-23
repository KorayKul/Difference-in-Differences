#' did.R
#'
#' Contributor: lachlandeer
#'
#' What this file does:
#'    - Computes the Diff in Diff estimate by "hand" 
#'       using the pre-post x treat-untreated means
#'    - Diff in Diff via regression
#'    - Introduced the fixest package for fixed effects in regression
#'    - Clusters standard errors using fixest
#'    - Event Study regression via fixest    

# --- Libraries --- #
library(readr)
library(dplyr)
library(modelsummary)
library(broom)
library(ggplot2)
library(fixest)

# --- Load the Data --- #
df <- 
  read_csv("data/app_data.csv") %>%
  mutate(post = if_else(month >= 19, 1, 0))  

# --- The Diff-in-Diff Table --- #
treat_table <-
  df %>%
  group_by(post, treat) %>%
  summarise(qty = mean(qty_purchases)) %>%
  ungroup()

# --- "Eyeconometrics" of Diff in Diff --- #
treat_table %>%
    ggplot() +
    geom_point(
        aes(x = post,
            y = qty,
            color = as.factor(treat)
            )
        ) +
    geom_line(aes(x = post,
                  y = qty,
                  color = as.factor(treat)
                  )
    )

# --- Diff in Diff via the Treatment Table --- #
treat_table %>%
  # makes treat and control group each on column
  tidyr::pivot_wider(names_from = treat,
              values_from = qty
  ) %>%
  mutate_all(list(~ . - lag(.))) %>%
  mutate(did_simple = `1` - `0`) %>%
  na.omit() %>%
  select(did_simple)

# --- Diff in Diff as a Regression --- #
did_simple <-
    lm(qty_purchases ~  treat + post + treat:post, data = df)

tidy(did_simple)    

# --- Econometrics Redux --- #
df %>%
    group_by(treat, month) %>%
    summarize(qty = mean(qty_purchases)) %>%
    ggplot() +
    geom_line(aes(x = month, 
                  y = qty,
                  color = as.factor(treat)
                  )
              ) + 
    geom_vline(xintercept = 19, color = "red", linestyle = '--')

# --- Add a Common monthly 'trend' to the data --- #
model_monthly <- 
    lm(qty_purchases ~  treat + post + treat:post + 
           factor(month), 
            data = df
       )

tidy(model_monthly)

# Equivalent to
model_month_fe <- 
    feols(qty_purchases ~  treat + post + treat:post | month, 
            vcov = "iid",
            data = df
          )

tidy(model_month_fe)

# is this a large effect?
df %>%
    filter(post == 0) %>%
    summarise(sd_qty = sd(qty_purchases))

# --- Adding User Fixed effects --- #
model_user_fe <- 
    feols(qty_purchases ~  treat + post + treat:post | month + id, 
          vcov = "iid",
          data = df
    )

tidy(model_user_fe)

# --- Heteroskedasticity Redux --- #

model_hetero <-
    feols(qty_purchases ~  treat + post + treat:post | month + id, 
          vcov = "hetero",
          data = df
          )

tidy(model_hetero)

# --- Clustered Standard Errors --- #
model_cluster_id  <- 
    feols(qty_purchases ~  treat + post + treat:post | month + id, 
          cluster = ~id,
          data = df
          )

tidy(model_cluster_id)

model_cluster_zip  <- 
    feols(qty_purchases ~  treat + post + treat:post | month + id, 
          cluster = ~zip,
          data = df
    )

tidy(model_cluster_id)

# --- Is the Treatment Effect Constant Over Time --- #
model_event <- 
    feols(qty_purchases ~  i(month, treat, 19)  | month + id, 
            cluster = ~zip,
            data = df)

tidy(model_event)

iplot(model_event)
