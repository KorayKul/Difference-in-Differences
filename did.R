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
YOUR CODE

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
YOUR CODE

# --- Diff in Diff as a Regression --- #
YOUR CODE

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

# --- Add a common monthly fixed effects to the data --- #
YOUR CODE


# --- Adding User Fixed effects --- #
YOUR CODE


# --- Heteroskedasticity Redux --- #
YOUR CODE

# --- Clustered Standard Errors --- #
YOUR CODE


# --- Is the Treatment Effect Constant Over Time --- #
YOUR CODE

