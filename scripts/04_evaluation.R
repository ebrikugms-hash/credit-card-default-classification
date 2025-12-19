# ============================================
# 04_evaluation.R
# Credit Card Default Classification
# MAST6100 Final Project
#
# Purpose:
# - Evaluate fitted classification models
# - Compare predictive performance on test data
# ============================================

library(tidyverse)
library(class)
library(MASS)
library(nnet)

credit <- read_csv("data/credit_default_clean.csv")

set.seed(123)

credit_shuffled <- credit %>%
  sample_frac(1)

train <- credit_shuffled %>%
  slice_head(prop = 0.8)

test <- credit_shuffled %>%
  slice_tail(prop = 0.2)

glm_model <- readRDS("models/glm_model.rds")
lda_model <- readRDS("models/lda_model.rds")
nn_model  <- readRDS("models/nn_model.rds")

glm_prob <- predict(glm_model, newdata = test, type = "response")

glm_pred <- ifelse(glm_prob > 0.5, 1, 0) %>%
  factor(levels = c(0, 1))

glm_cm <- table(Predicted = glm_pred, Actual = test$default)
glm_cm

glm_accuracy <- mean(glm_pred == test$default)
glm_accuracy

lda_pred <- predict(lda_model, newdata = test)$class

lda_cm <- table(Predicted = lda_pred, Actual = test$default)
lda_cm

lda_accuracy <- mean(lda_pred == test$default)
lda_accuracy

numeric_train <- train %>%
  dplyr::select(credit_limit, age, PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6)

numeric_test <- test %>%
  dplyr::select(credit_limit, age, PAY_0, PAY_2, PAY_3, PAY_4, PAY_5, PAY_6)

train_scaled <- scale(numeric_train)
test_scaled <- scale(
  numeric_test,
  center = attr(train_scaled, "scaled:center"),
  scale  = attr(train_scaled, "scaled:scale")
)

knn_pred <- knn(
  train = train_scaled,
  test = test_scaled,
  cl = train$default,
  k = 5
)

knn_cm <- table(Predicted = knn_pred, Actual = test$default)
knn_cm

knn_accuracy <- mean(knn_pred == test$default)
knn_accuracy

nn_prob <- predict(nn_model, newdata = test, type = "raw")

nn_pred <- ifelse(nn_prob > 0.5, 1, 0) %>%
  factor(levels = c(0, 1))

nn_cm <- table(Predicted = nn_pred, Actual = test$default)
nn_cm

nn_accuracy <- mean(nn_pred == test$default)
nn_accuracy

results <- tibble(
  Model = c("Logistic Regression", "LDA", "kNN (k=5)", "Neural Network"),
  Accuracy = c(
    glm_accuracy,
    lda_accuracy,
    knn_accuracy,
    nn_accuracy
  )
)

results

write_csv(results, "figures/model_accuracy.csv")




