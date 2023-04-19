library(readr)
library(dplyr)
library(tidyr)
library(modelsummary)
library(broom)
library(purrr)
library(ggplot2)
library(estimatr)


df <- read_csv("data/landing_pages.csv")

df <- 
  df %>%
  mutate(sale_indicator = if_else(sales > 0, 1, 0))

# summarize data by version
datasummary_balance(~version, data = df)

# straight regression
summary(lm(sales ~ version, data = df))

summary(lm(sale_indicator ~ version, data = df))

# sales filtered on +ve (all in one)
summary(lm(log(sales) ~ version, data = df %>% filter(sales > 0)))

# grouped 
sales_models <- 
  df %>%
  nest(-source) %>%
  mutate(
    model = map(data, ~ lm(sales  ~ version, data = .))
  )

sales_models %>%
  mutate(tidy_mod = map(model, tidy)) %>%
  select(tidy_mod) %>%
  unnest()

# sales with an interaction term?
summary(lm(sales ~ version*source, data = df))


# model summary
modelsummary(sales_models$model)

modelplot(sales_models$model, coef_omit = 'Interc')

# robust standard errors?
m1 <- lm(sales~version, data = df)

df <- 
  df %>%
  mutate(resid = resid(m1),
         predict = predict(m1))

df %>%
  ggplot() +
  geom_point(aes(y = resid, x = predict))


lmtest::bptest(m1)

lm_robust(sales~version, data = df)

lm_robust(sales~version, data = df, se_type = "HC3")

