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

# --- The Diff-in-Diff Table --- 

treat_table <- df %>% 
    # keep months immidately before and after treatment
    # becomes available
    filter(month %in% c(18,19)) %>% 
    group_by(treat,post) %>% 
    summarise(val_purchases = mean(val_purchases))


# --- "Eyeconometrics" of Diff in Diff --- #
treat_table %>%
    ggplot() +
    geom_point(
        aes(x = post,
            y = val_purchases,
            color = as.factor(treat)
            )
        ) +
    geom_line(aes(x = post,
                  y = val_purchases,
                  color = as.factor(treat)
                  )
    )



# --- Diff in Diff via the Treatment Table --- #
treat_table %>% 
    ungroup() %>% 
    tidyr::pivot_wider(names_from = post,
                       values_from = val_purchases) %>% # defining colums before and after
    mutate_all(list(~ . -lag(.))) %>% 
    mutate(did_simple = `1`-`0`) %>% 
    na.omit() %>% 
    select(did_simple)



# --- Diff in Diff as a Regression --- #
did_simple_reg <- 
    lm(val_purchases ~ treat + post + treat:post,
       data =df %>% filter(month %in% c(18,19)))
tidy(did_simple_reg)





# --- Econometrics Redux --- #
df %>%
    group_by(treat, month) %>%
    summarize(val_purchases = mean(val_purchases)) %>%
    ggplot() +
    geom_line(aes(x = month, 
                  y = val_purchases,
                  color = as.factor(treat)
                  )
              ) + 
    geom_vline(xintercept = 19, color = "red", linestyle = 'dashed')


# excluding month 18 and 19 instead doing it to all months
treat_table <- df %>% 
    group_by(treat,post) %>% 
    summarise(val_purchases =mean(val_purchases))


treat_table %>%  
    ungroup %>% 
    tidyr::pivot_wider(names_from = post,
                       values_from = val_purchases) %>% 
    mutate_all(list(~ . - lag(.))) %>% 
    mutate(did_simple = `1`-`0`) %>% 
    na.omit() %>% 
    select(did_simple)


did_simple <- 
    lm(val_purchases ~ treat + post + treat:post,
       data = df)

tidy(did_simple) # mobile have effect because this time look at treat:post it is significant

# --- Add a common monthly fixed effects to the data --- #
model_month_fe <- 
    lm(val_purchases ~ treat + post +treat:post + as.factor(month),
       data = df)
summary(model_month_fe)

model_month_fe2 <- 
    feols(val_purchases ~ treat + post +treat:post | month,
          data =df,                # fixed effects ordinary square roots
          vcov = "iid")            # impose homosecedastic standart errors to match lm() command
tidy(model_month_fe2)


# --- Adding User Fixed effects --- #
model_user_fe <- 
    feols(val_purchases ~ treat + post +treat:post | month + id ,
          data = df,
          vcov ="iid")
tidy(model_user_fe)


# --- Heteroskedasticity Redux --- #
model_user_fe <- 
    feols(val_purchases ~ treat + post +treat:post | month + id ,
          data = df,
          vcov ="hetero")
tidy(model_user_fe)



# --- Clustered Standard Errors --- #
model_user_fe <- 
    feols(val_purchases ~ treat + post +treat:post | month + id ,
          data = df,
          cluster = ~ zip)
tidy(model_user_fe) # increases pvalue and estimated std. 




# --- Is the Treatment Effect Constant Over Time --- #

model_event_study <- 
    feols(val_purchases ~ i(month,treat,18) | month + id ,
          data = df,
          cluster = ~ zip)
tidy(model_event_study) # increases pvalue and estimated std. 
iplot(model_event_study) # important for parallel trends










