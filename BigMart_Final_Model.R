library(xgboost)
library(dplyr)
library(Matrix)

# Final XGBoost model on full training data
final_xgb_model_full <- train(
  Item_Outlet_Sales ~ .,
  data = dataset_original_var,
  method = "xgbTree",
  trControl = trainControl(method = "none"),  # No CV for final model
  tuneGrid = results_original_var$XGB$bestTune, # picking best tune parameter
  verboseIter = FALSE
)

# Predict on test set
predictions <- predict(final_xgb_model_full, newdata = test_processed)

# set any negative prediction to 0
predictions <- pmax(predictions, 0)

summary(predictions)

# Save the final model
saveRDS(final_xgb_model_full, "model/final_xgb_model.rds")

# submission file
submission <- data.frame(
  Item_Identifier = test_processed$Item_Identifier,
  Outlet_Identifier = test_processed$Outlet_Identifier,
  Item_Outlet_Sales = predictions
)

write.csv(submission, "final_submission.csv", row.names = FALSE)

# Final XGBoost model full training data using grouped varaibles
final_xgb_model_grouped <- train(
  Item_Outlet_Sales ~ .,
  data = dataset_grouped_var,
  method = "xgbTree",
  trControl = trainControl(method = "none"),  # No CV for final model
  tuneGrid = results_grouped_var$XGB$bestTune, # picking best tune parameter
  verboseIter = FALSE
)

# Predict on test set
predictions_grouped <- predict(final_xgb_model_grouped, newdata = test_processed)

# set any negative prediction to 0
predictions_grouped <- pmax(predictions_grouped, 0)

summary(predictions_grouped)

# Save the final model
saveRDS(final_xgb_model_grouped, "model/final_xgb_model_grouped.rds")

# Create submission
submission_grouped <- data.frame(
  Item_Identifier = test_processed$Item_Identifier,
  Outlet_Identifier = test_processed$Outlet_Identifier,
  Item_Outlet_Sales = predictions_grouped
)

write.csv(submission_grouped, "final_submission_grouped.csv", row.names = FALSE)

