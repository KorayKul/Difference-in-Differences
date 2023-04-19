library(readr)
library(dplyr)
library(modelsummary)
library(broom)
library(ggplot2)
library(fixest)


df <- 
  read_csv("data/app_data.csv") %>%
  mutate(post = if_else(month >= 19, 1, 0))  

treat_table <-
  df %>%
  group_by(post, treat) %>%
  summarise(qty = mean(qty_purchases)) %>%
  ungroup()

treat_table %>%
  # makes treat and control group each on column
  tidyr::pivot_wider(names_from = treat,
              values_from = qty
  ) %>%
  mutate_all(list(~ . - lag(.))) %>%
  mutate(did_simple = `1` - `0`) %>%
  na.omit() %>%
  select(did_simple)

m1 <- lm(qty_purchases ~  treat + post + treat:post + factor(month), data = df)

summary(m1)

m2 <- feols(qty_purchases ~  treat + post + treat:post | month, 
            vcov = "iid",
            data = df)

summary(m2)


df %>%
  filter(treatxpost == 1) %>%
  summarise(min = max(month))


# parallel trends?

df %>%
  group_by(treat, month) %>%
  summarise(qty = mean(qty_purchases)) %>%
  ggplot() + 
  geom_line(aes(x = month, y = qty, color = as.factor(treat))) +
  geom_vline(xintercept = 19, color = "red", linestyle = '--')

# clustering, heterosk etc
m2 <- feols(qty_purchases ~  treat + post + treat:post | month, 
                 vcov = "HC1",
                 data = df)
summary(m2)


m2  <- feols(qty_purchases ~  treat + post + treat:post | month, 
                 cluster = ~id,
                 data = df)

summary(m2)

m2  <- feols(log(1+qty_purchases) ~  treat + post + treat:post + imr | month, 
             cluster = ~id,
             data = df)

summary(m2)


# eventstudy?
m3 <- feols(frq_purchases ~  i(month, treat, 19) + imr | month, 
            #vcov = "iid",
            cluster = ~zip,
            data = df)

summary(m3)

iplot(m3)
