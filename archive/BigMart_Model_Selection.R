library(caret)
library(randomForest)
library(xgboost)
library(dplyr)

# Set seed
set.seed(42)

# Training control
ctrl <- trainControl(method = "cv", number = 5)

# Prepare datasets
datasets <- list(
  # set1 = train_processed %>% select(-c(Item_Identifier, Item_Fat_Content_Clean, Item_Type_grouped, Outlet_Establishment_age)),
  # set2 = train_processed %>% select(-c(Item_Identifier, Item_Fat_Content, Item_Type, Outlet_Establishment_Year)),
  set3 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content_Clean, Item_Type_grouped, Outlet_Establishment_age)),
  set4 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Item_Type, Outlet_Establishment_Year)),
  set5 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content_Clean, Item_Type, Outlet_Establishment_Year)),
  set6 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content_Clean, Item_Type_grouped, Outlet_Establishment_Year)),
  set7 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Item_Type_grouped, Outlet_Establishment_Year)),
  set8 = train_processed %>% select(-c(Item_Identifier, Outlet_Identifier, Item_Fat_Content, Item_Type, Outlet_Establishment_Year))
)

# Define models
model_methods <- list(
  LM  = "lm",
  DT  = "rpart",
  RF  = "rf",
  XGB = "xgbTree"
)

# Run models on all datasets
all_results <- list()

for (data in names(datasets)) {
  cat("==== Running models on", data, "====\n")

  data <- datasets[[data]]

  models <- list()
  for (m in names(model_methods)) {
    cat("Training", m, "...\n")
    models[[m]] <- train(
      Item_Outlet_Sales ~ .,
      data = data,
      method = model_methods[[m]],
      trControl = ctrl
    )
  }

  # store resamples for comparison
  all_results[[data]] <- resamples(models)
}

# Compare results per dataset
for (data in names(all_results)) {
  cat("\n\n### Performance for", data, "###\n")
  print(summary(all_results[[data]]))
}

# Based on the performance metrics for the datasets set3 to set8,
# the XGBoost (XGB) model consistently outperforms the other models
# (Linear Model - LM, Decision Tree - DT, and Random Forest - RF)
# across all evaluation metrics:
# Mean Absolute Error (MAE), Root Mean Square Error (RMSE), and R-squared.
# dataset #set8 is showing the best choice for your final model
# by dropping Item_Identifier, Outlet_Identifier,
# instead of Item_Fat_Content using revised one ,
# instead of Item_Type using grouped version,
# instead Outlet_Establishment_Year by subtracting from year 2013 data was collected



