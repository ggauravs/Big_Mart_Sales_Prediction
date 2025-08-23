library(tidyverse)
library(ggplot2)
library(gridExtra)

# Set up a multi-panel plot layout
par(mfrow = c(2, 2))

# Boxplot to identify outliers
boxplot(train_processed$Item_Outlet_Sales,
        main = "Boxplot of Outlet Sales",
        ylab = "Item_Outlet_Sales",
        col = "lightgreen")

boxplot(train_processed$Item_Weight,
        main = "Boxplot of Items Weight",
        ylab = "Items Weight",
        col = "lightgreen")

boxplot(train_processed$Item_Visibility,
        main = "Boxplot of Items Visibility",
        ylab = "Items Visibility",
        col = "lightgreen")

boxplot(train_processed$Item_MRP,
        main = "Boxplot of Items MRP",
        ylab = "Items MRP",
        col = "lightgreen")


# Density plot
par(mfrow = c(2, 2))
plot(density(train_processed$Item_Outlet_Sales, na.rm = TRUE),
     main = "Density Plot of Outlet Sales",
     xlab = "Item_Outlet_Sales",
     col = "darkblue",
     lwd = 2)
plot(density(train_processed$Item_Weight, na.rm = TRUE),
     main = "Density Plot of Items Weight",
     xlab = "Items Weight",
     col = "darkblue",
     lwd = 2)
plot(density(train_processed$Item_Visibility, na.rm = TRUE),
     main = "Density Plot of Items Visibility",
     xlab = "Items Visibility",
     col = "darkblue",
     lwd = 2)
plot(density(train_processed$Item_MRP, na.rm = TRUE),
     main = "Density Plot of Items MRP",
     xlab = "Items MRP",
     col = "darkblue",
     lwd = 2)

