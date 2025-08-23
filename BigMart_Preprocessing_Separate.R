library(dplyr)
library(tidyr)

# Load data
train_data <- read.csv("data\\train_v9rqX0R.csv")
test_data <- read.csv("data\\test_AbJTz2l.csv")

# Display structure of the data
str(train_data)
str(test_data)

# Check column names
names(train_data)
names(test_data)
setdiff(names(train_data), names(test_data))

# Check for missing values in target variable
sum(is.na(train_data$Item_Outlet_Sales))

# Explore data without combining
# Check character variables
char_vars <- sapply(train_data, is.character)
train_data[char_vars] %>% sapply(function(x) table(x, useNA = "always"))

test_data[char_vars] %>% sapply(function(x) table(x, useNA = "always"))

# Check for missing values and empty strings
colSums(is.na(train_data))
sapply(train_data, function(x) sum(x == ""))

# Number of unique identifiers
n_unique_items <- length(unique(train_data$Item_Identifier))
print(paste("Number of unique items:", n_unique_items))

n_unique_outlets <- length(unique(train_data$Outlet_Identifier))
print(paste("Number of unique outlets:", n_unique_outlets))

# Clean Item_Fat_Content in both datasets
clean_fat_content <- function(df) {
  df %>%
    mutate(Item_Fat_Content = case_when(
      Item_Fat_Content %in% c("low fat", "LF") ~ "Low Fat",
      Item_Fat_Content == "reg" ~ "Regular",
      TRUE ~ Item_Fat_Content
    ))
}

train_data <- clean_fat_content(train_data)
test_data <- clean_fat_content(test_data)

# Calculate imputation values from training data
item_weight_impute_id <- train_data %>%
  group_by(Item_Identifier) %>%
  summarise(Item_Weight_Impute = mean(Item_Weight, na.rm = TRUE)) %>%
  ungroup()

# there are still missing reocrds in weight colummn so impute using item type
item_weight_impute_type <- train_data %>%
  group_by(Item_Type) %>%
  summarise(Item_Weight_Impute = mean(Item_Weight, na.rm = TRUE)) %>%
  ungroup()

item_visibility_impute <- train_data %>%
  filter(Item_Visibility > 0) %>%
  group_by(Item_Identifier) %>%
  summarise(Item_Visibility_Impute = mean(Item_Visibility, na.rm = TRUE)) %>%
  ungroup()

# Impute Item_Weight using training data
impute_item_weight <- function(df, impute_values, by) {
  df %>%
    left_join(impute_values, by = by) %>%
    mutate(Item_Weight = ifelse(is.na(Item_Weight), Item_Weight_Impute, Item_Weight)) %>%
    select(-Item_Weight_Impute)
}

# keep the order first Item_Identifier then Item_Type if needed
train_data <- impute_item_weight(train_data, item_weight_impute_id, "Item_Identifier")
train_data <- impute_item_weight(train_data, item_weight_impute_type, "Item_Type")
test_data <- impute_item_weight(test_data, item_weight_impute_id, "Item_Identifier")
test_data <- impute_item_weight(test_data, item_weight_impute_type, "Item_Type")

# Impute zero visibility using training data
impute_visibility <- function(df, impute_values) {
  df %>%
    left_join(impute_values, by = "Item_Identifier") %>%
    mutate(Item_Visibility = ifelse(Item_Visibility == 0 | is.na(Item_Visibility),
                                    Item_Visibility_Impute, Item_Visibility)) %>%
    select(-Item_Visibility_Impute)
}

train_data <- impute_visibility(train_data, item_visibility_impute)
test_data <- impute_visibility(test_data, item_visibility_impute)

# Outlet_Size imputation using training data
outlet_size_mode <- train_data %>%
  filter(Outlet_Size != "") %>%
  group_by(Outlet_Type) %>%
  summarise(Outlet_Size_Mode = names(sort(table(Outlet_Size), decreasing = TRUE))[1]) %>%
  ungroup()

impute_outlet_size <- function(df, mode_values) {
  df %>%
    left_join(mode_values, by = "Outlet_Type") %>%
    mutate(Outlet_Size = ifelse(Outlet_Size == "", Outlet_Size_Mode, Outlet_Size)) %>%
    select(-Outlet_Size_Mode)
}

train_data <- impute_outlet_size(train_data, outlet_size_mode)
test_data <- impute_outlet_size(test_data, outlet_size_mode)

# derived new features/variables by grouping
create_new_features <- function(df) {
  df %>%
    mutate(
      Item_Type_grouped = case_when(
        substr(Item_Identifier, 1, 2) == "FD" ~ "Food",
        substr(Item_Identifier, 1, 2) == "DR" ~ "Drinks",
        substr(Item_Identifier, 1, 2) == "NC" ~ "Non-Consumable",
        TRUE ~ "Other"
      ),
      Item_Fat_Content_Clean = if_else(
        Item_Type_grouped == "Non-Consumable",
        "Non-Edible",
        Item_Fat_Content
      ),
      Outlet_Establishment_age = 2013 - Outlet_Establishment_Year #data was collected in 2013
    )
}

train_data <- create_new_features(train_data)
test_data <- create_new_features(test_data)

# Convert categorical variables to factors
convert_to_factors <- function(df) {
  df %>%
    mutate(
      Item_Fat_Content = as.factor(Item_Fat_Content),
      Item_Type = as.factor(Item_Type),
      Outlet_Identifier = as.factor(Outlet_Identifier),
      Outlet_Size = as.factor(Outlet_Size),
      Outlet_Location_Type = as.factor(Outlet_Location_Type),
      Outlet_Type = as.factor(Outlet_Type),
      Item_Type_grouped = as.factor(Item_Type_grouped),
      Item_Fat_Content_Clean = as.factor(Item_Fat_Content_Clean)
    )
}

train_data <- convert_to_factors(train_data)
test_data <- convert_to_factors(test_data)

# Item_Outlet_Sales and Item_Visibility data are right skewed
# Outlier handling functions
get_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR

  outliers <- x[x < lower | x > upper]
  return(list(
    lower_bound = lower,
    upper_bound = upper,
    outlier_values = outliers
  ))
}

get_outliers(train_data$Item_Outlet_Sales)
# get_outliers(test_data$Item_Outlet_Sales)
get_outliers(train_data$Item_Visibility)
get_outliers(test_data$Item_Visibility)

# cap outlier based on training data in both the dataset
cap_outliers <- function(x,train_x) {
  Q1 <- quantile(train_x, 0.25, na.rm = TRUE)
  Q3 <- quantile(train_x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR

  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

train_data$Item_Outlet_Sales <- cap_outliers(train_data$Item_Outlet_Sales,
                                             train_data$Item_Outlet_Sales)
train_data$Item_Visibility <- cap_outliers(train_data$Item_Visibility,
                                           train_data$Item_Visibility)

test_data$Item_Visibility <- cap_outliers(test_data$Item_Visibility,
                                          train_data$Item_Visibility)

# Save processed data
saveRDS(train_data, "train_processed.rds")
saveRDS(test_data, "test_processed.rds")

item_sales <- train_data %>%
  group_by(Item_Identifier) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales),
            Avg_Sales = mean(Item_Outlet_Sales))
print(head(item_sales))

outlet_sales <- train_data %>%
  group_by(Outlet_Identifier) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales),
            Avg_Sales = mean(Item_Outlet_Sales))
print(outlet_sales)

