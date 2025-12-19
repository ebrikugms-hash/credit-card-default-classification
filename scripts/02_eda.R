# ============================================
# 02_eda.R
# Credit Card Default Classification
# MAST6100 Final Project
#
# Purpose:
# - Perform exploratory data analysis (EDA)
# - Visualise class balance and key predictors
# ============================================

library(tidyverse)

credit <- read.csv("data/credit_default_clean.csv")

credit %>%
  count(default) %>%
  ggplot(aes(x = default, y = n, fill = default)) +
  geom_col() +
  labs(
    title = "Class Distribution of Credit Card Default",
    x = "Default Indicator",
    y = "Number of Customers"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/class.balance.png",
  width = 6,
  height = 4,
)

credit %>%
  ggplot(aes(x = default,  y = credit_limit, fill = default)) +
  geom_boxplot() +
  labs(
    title = "Credit Limit by Default Status",
    x = "Default Indicator",
    y = "Credit Limit"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/credit_limit_by_default.png",
  width = 6,
  height = 4
)

credit %>%
  ggplot(aes(x = PAY_0, fill = default)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Repayment Status (PAY_0) by Default",
    x = "Repayment Status",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/pay_status_by_default.png",
  width = 6,
  height = 4
)

credit %>%
  ggplot(aes(x = age, fill = default)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(
    title = "Age Distribution by Default Status",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()

ggsave(
  filename = "figures/age_by_default.png",
  width = 6,
  height = 4
)

