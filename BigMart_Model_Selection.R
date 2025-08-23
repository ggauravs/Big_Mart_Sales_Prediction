# Load libraries
library(caret)
library(xgboost)
library(glmnet)
library(randomForest)
library(rpart)
library(dplyr)
library(ggplot2)

# Set seed
set.seed(42)

# Load the processed data
train_processed <- readRDS("train_processed.rds")
test_processed <- readRDS("test_processed.rds")

# Training control
ctrl <- trainControl(method = "cv", number = 5, verboseIter = FALSE)

# create model dataset
dataset_original_var <- train_processed %>%
  select(-c(Item_Identifier, Outlet_Establishment_Year, Item_Type_grouped, Item_Fat_Content_Clean))
dataset_grouped_var <- train_processed %>%
  select(-c(Item_Identifier, Outlet_Establishment_Year, Item_Type, Item_Fat_Content))

# Define models to compare
models_to_test <- list(
  # Linear Regression
  LM = list(
    method = "lm",
    tuneGrid = NULL,
    preProcess = NULL
  ),

  # Ridge Regression
  Ridge = list(
    method = "glmnet",
    tuneGrid = expand.grid(
      alpha = 0,
      lambda = 10^seq(-3, 0, length = 5)
    ),
    preProcess = c("center", "scale")
  ),

  # Lasso Regression
  Lasso = list(
    method = "glmnet",
    tuneGrid = expand.grid(
      alpha = 1,
      lambda = 10^seq(-3, 0, length = 5)
    ),
    preProcess = c("center", "scale")
  ),

  # Decision Tree
  DT = list(
    method = "rpart",
    tuneGrid = expand.grid(
      cp = seq(0.001, 0.01, length = 5)
    ),
    preProcess = NULL
  ),

  # Random Forest
  RF = list(
    method = "rf",
    tuneGrid = expand.grid(
      mtry = c(3, 5, 7)
    ),
    preProcess = NULL
  ),

  # XGBoost
  XGB = list(
    method = "xgbTree",
    tuneGrid = expand.grid(
      nrounds = c(50, 100),
      max_depth = c(4, 6),
      eta = c(0.05, 0.1),
      gamma = 0,
      colsample_bytree = 0.8,
      min_child_weight = c(1, 3),
      subsample = 0.8
    ),
    preProcess = NULL
  )
)

# Train all models
results_original_var <- list()
for (model_name in names(models_to_test)) {
  cat("Training", model_name, "...\n")

  config <- models_to_test[[model_name]]

  # ridge and lasso required preprocessing
  if (!is.null(config$preProcess)) {
    results_original_var[[model_name]] <- train(
      Item_Outlet_Sales ~ .,
      data = dataset_original_var,
      method = config$method,
      trControl = ctrl,
      tuneGrid = config$tuneGrid,
      preProcess = config$preProcess
    )
  } else {
    results_original_var[[model_name]] <- train(
      Item_Outlet_Sales ~ .,
      data = dataset_original_var,
      method = config$method,
      trControl = ctrl,
      tuneGrid = config$tuneGrid
    )
  }
}

# Compare results
resamples_comparison_original_var <- resamples(results_original_var)
summary(resamples_comparison_original_var)

# Extract performance metrics
performance_metrics <- summary(resamples_comparison_original_var)
cat("Model Performance Comparison:\n")
print(performance_metrics$statistics$RMSE)

# best performing model
mean_rmse <- as.data.frame(performance_metrics$statistics$RMSE)["Mean"]

# Print all models sorted by RMSE
performance_df <- data.frame(
  Model = rownames(mean_rmse),
  RMSE = mean_rmse
) %>% arrange(Mean)

print(performance_df)

best_model_name <- rownames(mean_rmse)[which.min(mean_rmse$Mean)]
cat("\nBest performing model:", best_model_name, "with RMSE:", min(mean_rmse), "\n")

# Save results
saveRDS(results_original_var, "model/model_comparison_results_original_var.rds")
saveRDS(resamples_comparison_original_var, "model/model_resamples_comparison_original_var.rds")

# Train all models for grouped variable/features
results_grouped_var <- list()
for (model_name in names(models_to_test)) {
  cat("Training", model_name, "...\n")

  config <- models_to_test[[model_name]]

  # ridge and lasso required preprocessing
  if (!is.null(config$preProcess)) {
    results_grouped_var[[model_name]] <- train(
      Item_Outlet_Sales ~ .,
      data = dataset_grouped_var,
      method = config$method,
      trControl = ctrl,
      tuneGrid = config$tuneGrid,
      preProcess = config$preProcess
    )
  } else {
    results_grouped_var[[model_name]] <- train(
      Item_Outlet_Sales ~ .,
      data = dataset_grouped_var,
      method = config$method,
      trControl = ctrl,
      tuneGrid = config$tuneGrid
    )
  }
}

# Compare results
resamples_comparison_grouped_var <- resamples(results_grouped_var)
summary(resamples_comparison_grouped_var)

# Extract performance metrics
performance_metrics <- summary(resamples_comparison_grouped_var)
cat("Model Performance Comparison:\n")
print(performance_metrics$statistics$RMSE)

# best performing model
mean_rmse <- as.data.frame(performance_metrics$statistics$RMSE)["Mean"]

# Print all models sorted by RMSE
performance_df <- data.frame(
  Model = rownames(mean_rmse),
  RMSE = mean_rmse
) %>% arrange(Mean)

print(performance_df)

best_model_name <- rownames(mean_rmse)[which.min(mean_rmse$Mean)]
cat("\nBest performing model:", best_model_name, "with RMSE:", min(mean_rmse), "\n")

# Save results
saveRDS(results_grouped_var, "model/model_comparison_results_grouped_var.rds")
saveRDS(resamples_comparison_grouped_var, "model/model_resamples_comparison_grouped_var.rds")

