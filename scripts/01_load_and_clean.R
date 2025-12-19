# ============================================
# 01_load_and_clean.R
# Credit Card Default Classification
# MAST6100 Final Project
#
# Purpose:
# - Load raw credit default data
# - Clean and rename variables
# - Prepare data set for analysis
# ============================================

library(tidyverse)

credit_raw <- read.csv("data/credit_default.csv")

glimpse(credit_raw)

credit_raw <- read.csv(
  "data/credit_default.csv",
  skip = 1
)

credit <- credit_raw %>%
  rename(
    credit_limit = LIMIT_BAL,
    sex = `SEX`,
    education = EDUCATION,
    marriage = MARRIAGE,
    age = AGE,
    default = `default.payment.next.month`
  )

credit <- credit %>%
  mutate(
    default = factor(default, levels = c(0, 1)),
    sex = factor(sex),
    education = factor(education),
    marriage = factor(marriage)
  )
credit %>%
  count(default)

credit %>%
  summarise(across(everything(), ~sum(is.na(.))))
head(credit)

write.csv(credit, "data/credit_default_clean.csv")
