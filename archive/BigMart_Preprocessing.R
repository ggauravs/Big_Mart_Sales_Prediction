library(dplyr)

# Load data
train_data <- read.csv("data\\train_v9rqX0R.csv")
test_data <- read.csv("data\\test_AbJTz2l.csv")

# Display structure of the data
str(train_data)
str(test_data)

# Check column names
names(train_data)
names(test_data)
setdiff(names(train_data),names(test_data))

# Check for missing values in target variable
sum(is.na(train_data$Item_Outlet_Sales))

# Combine test and train for processing
test_data$Item_Outlet_Sales <- NA
combined_data <- rbind(train_data, test_data)


data_var <- names(combined_data)
numeric_var <- combined_data[sapply(combined_data, function(x) is.numeric(x))]
char_var <- combined_data[sapply(combined_data, function(x) is.character(x))]

# summary details
sapply(combined_data, function(x) summary(x))

# Check for missing values
sapply(combined_data, function(x) sum(is.na(x)))
# Check for data type
sapply(combined_data, function(x) typeof(x))

sapply(combined_data, function (x) unique(x))

# explore data by item group and outlet group
# check char variable using table
combined_data %>%
  select_if(is.character) %>%
  dplyr::select(-Item_Identifier) %>%
  sapply(function(x) table(x,useNA = "a"))

table(duplicated(char_var$Item_Identifier))

# Check for missing values (NA) and empty strings
colSums(is.na(combined_data))
sapply(combined_data, function(x) sum(x == ""))

# Number of unique Item_Identifier
n_unique_items <- length(unique(combined_data$Item_Identifier))
print(paste("Number of unique items:", n_unique_items))

# Number of unique Outlet_Identifier
n_unique_outlets <- length(unique(combined_data$Outlet_Identifier))
print(paste("Number of unique outlets:", n_unique_outlets))

# Group by Item_Identifier and check unique values for Item_Weight, Item_Fat_Content, Item_Visibility
item_consistency <- combined_data %>%
  group_by(Item_Identifier) %>%
  summarise(
    Unique_Weights = length(unique(Item_Weight[!is.na(Item_Weight)])),
    Unique_Fat_Contents = length(unique(Item_Fat_Content)),
    Unique_Visibilities = length(unique(Item_Visibility)),
    Missing_Weights = sum(is.na(Item_Weight)),
    Missing_Fat_Contents = sum(Item_Fat_Content == ""),
    Missing_Visibilities = sum(is.na(Item_Visibility))
  )

print(item_consistency)

# Clean Item_Fat_Content
table(combined_data$Item_Fat_Content)
combined_data$Item_Fat_Content <- gsub("low fat", "Low Fat", combined_data$Item_Fat_Content)
combined_data$Item_Fat_Content <- gsub("LF", "Low Fat", combined_data$Item_Fat_Content)
combined_data$Item_Fat_Content <- gsub("reg", "Regular", combined_data$Item_Fat_Content)
table(combined_data$Item_Fat_Content,useNA = "a")

# replacing na in item weight based on item_identifier weight since it's unique
# Impute Item_Weight --- Using the mean weight per Item_Identifier
combined_data <- combined_data %>%
  group_by(Item_Identifier) %>%
  mutate(
    Item_Weight = ifelse(is.na(Item_Weight), mean(Item_Weight, na.rm = TRUE), Item_Weight)
  ) %>%
  ungroup()

# check Item_Visibility
# Visibility can not be missing or 0
num_zero_vis <- sum(combined_data$Item_Visibility == 0)
print(paste("Number of zero visibility records:", num_zero_vis))

# impute 0 with mean per Item_Identifier
combined_data <- combined_data %>%
  group_by(Item_Identifier) %>%
  mutate(
    Item_Visibility = ifelse(Item_Visibility == 0, mean(Item_Visibility[Item_Visibility != 0], na.rm = TRUE), Item_Visibility)
  ) %>%
  ungroup()

# explore data by outlet group

# check and impute by Outlet_Identifier
# Group by Outlet_Identifier and check unique Outlet_Size
outlet_consistency <- combined_data %>%
  group_by(Outlet_Identifier) %>%
  summarise(
    Unique_Sizes = length(unique(Outlet_Size[Outlet_Size != ""])),
    Missing_Sizes = sum(Outlet_Size == "")
  )

print(outlet_consistency)

# Impute Outlet_Size
# Using mode per Outlet_Type and thne Outlet_Location_Type
table(combined_data$Outlet_Size,useNA = "a")
# First, try to fill per Outlet_Identifier if any non-missing
combined_data$Outlet_Size[combined_data$Outlet_Size == ""] <- NA
combined_data <- combined_data %>%
  group_by(Outlet_Type) %>%
  mutate(Outlet_Size = ifelse(is.na(Outlet_Size), names(sort(table(Outlet_Size), decreasing = TRUE)[1]), Outlet_Size)) %>%
  ungroup()

# combined_data<- combined_data %>% ungroup()

# grouping items based on product type
combined_data <- combined_data %>%
  mutate(Item_Type_grouped = case_when(
    substr(Item_Identifier, 1, 2) == "FD" ~ "Food",
    substr(Item_Identifier, 1, 2) == "DR" ~ "Drinks",
    substr(Item_Identifier, 1, 2) == "NC" ~ "Non-Consumable",
    TRUE ~ NA_character_
  ))

table(combined_data$Item_Type, combined_data$Item_Type_grouped)

# updating Item_Fat_Content using grouped item type
# changing fat content to non-edible for non consumable

combined_data <- combined_data %>%
  mutate(Item_Fat_Content_Clean = if_else(
    Item_Type_grouped == "Non-Consumable",
                   "Non-Edible",
                   Item_Fat_Content))

table(combined_data$Item_Fat_Content, combined_data$Item_Fat_Content_Clean)

# adding new variable Outlet_Establishment_age by substracting from 2013
# data was collected in 2013

combined_data$Outlet_Establishment_age <- 2013 - combined_data$Outlet_Establishment_Year

# Convert categorical variables to factors
combined_data$Item_Fat_Content <- as.factor(combined_data$Item_Fat_Content)
combined_data$Item_Type <- as.factor(combined_data$Item_Type)
combined_data$Outlet_Identifier <- as.factor(combined_data$Outlet_Identifier)
combined_data$Outlet_Size <- as.factor(combined_data$Outlet_Size)
combined_data$Outlet_Location_Type <- as.factor(combined_data$Outlet_Location_Type)
combined_data$Outlet_Type <- as.factor(combined_data$Outlet_Type)
combined_data$Item_Type_grouped <- as.factor(combined_data$Item_Type_grouped)
combined_data$Item_Fat_Content_Clean <- as.factor(combined_data$Item_Fat_Content_Clean)


# split data back into train and test
test_processed <- combined_data %>%
  filter(is.na(Item_Outlet_Sales)) %>%
  select(-Item_Outlet_Sales)

train_processed <- combined_data %>%
  filter(!is.na(Item_Outlet_Sales))

# Saving processed data in RDS for further use
saveRDS(train_processed, "train_processed.rds")
saveRDS(test_processed, "test_processed.rds")

# Sales by Item_Identifier
item_sales <- train_processed %>%
  group_by(Item_Identifier) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales), Avg_Sales = mean(Item_Outlet_Sales))
print(head(item_sales))

# Sales by Outlet_Identifier
outlet_sales <- train_processed %>%
  group_by(Outlet_Identifier) %>%
  summarise(Total_Sales = sum(Item_Outlet_Sales), Avg_Sales = mean(Item_Outlet_Sales))
print(outlet_sales)



