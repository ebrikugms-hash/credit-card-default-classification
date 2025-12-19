# ============================================
# 03_models.R
# Credit Card Default Classification
# MAST6100 Final Project
#
# Purpose:
# - Split data into training and test sets
# - Fit multiple classification models
# - Save fitted models for evaluation
# ============================================

library(tidyverse)
library(class)

credit <- read.csv("data/credit_default_clean.csv")

set.seed(123)

credit_split <- credit %>%
  mutate(row_id = row_number()) %>%
  sample_frac(1)

train <- credit_split %>%
  slice_head(prop = 0.8) %>%
  select(-row_id)

test <- credit_split %>%
  slice_tail(prop = 0.2) %>%
  select(-row_id)

glm_model <- glm(
  default ~ credit_limit + age + sex + education + marriage +
    PAY_0 + PAY_2 + PAY_3 +  PAY_4 + PAY_5 + PAY_6,
  data = train,
  family = binomial
)

summary(glm_model)

library(MASS)

lda_model <- lda(
  default ~ credit_limit + age + sex + education + marriage +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6,
  data = train
)

numeric_vars <- train %>%
  dplyr::select(
    credit_limit, age,
    PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6
  )

numeric_test <- test %>%
  dplyr::select(
    credit_limit, age,
    PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6
  )

train_scaled <- scale(numeric_vars)

test_scaled <- scale(numeric_test,
                     center = attr(train_scaled, "scaled:center"),
                     scale = attr(train_scaled, "scaled:scale"))

knn_pred <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = train$default,
  k = 5
)

library(nnet)

nn_model <- nnet(
  default ~ credit_limit + age + sex + education + marriage +
    PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6,
  data = train,
  size = 5,
  maxit = 200,
  trace = FALSE
)

dir.create("models", showWarnings = FALSE)

saveRDS(glm_model, "models/glm_model.rds")
saveRDS(lda_model, "models/lda_model.rds")
saveRDS(nn_model, "models/nn_model.rds")
