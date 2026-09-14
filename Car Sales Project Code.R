install.packages(c("tidyverse", "ggplot2", "plotly", "lubridate" ,"dplyr"))

library(tidyverse) #packages 
library(dplyr) #data manipulation 
library(ggplot2) #visualizations
library(plotly) #interactive graphs
library(lubridate) #date and time

car_data <- read.csv("C:/Users/gupta/Downloads/Car_sales.csv") #load data

head(car_data) #head

str(car_data) #check type

summary(car_data) #summary

#duplicate and null
colSums(is.na(car_data))

duplicate_rows <- car_data[duplicated(car_data), ]

print(duplicate_rows)

library(ggplot2)

custom_scatterplot <- ggplot(car_data, aes(x = Horsepower, y = Price_in_thousands,
                                           color = Fuel_efficiency)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "red", high = "green") + # 70% opacity and point size 3
  labs(
    title = "Horsepower vs Price",
    x = "Horsepower",
    y = "Price in Thousands (USD)",
    color = "Fuel Efficiency"
  ) +
  theme_minimal() #ggplot theme

print(custom_scatterplot)

ggplot(car_data, aes(x = Price_in_thousands)) +
  geom_histogram(aes(y = ..density..), fill = "lightpink", color = "black", bins = 30) +
  geom_density(alpha = 0.7, fill = "skyblue") +
  labs(title = "Car Price Distribution Curve",
       x = "Price in Thousands (USD)", #hist and density
       y = "Density")

par(mar = c(5, 5, 2, 2))

boxplot(car_data$Price_in_thousands,
        main = "Distribution of Car Prices",
        col = "orange",
        border = "black",
        horizontal = TRUE,  
        notch = TRUE,       
        notchwidth = 0.5,   
        outline = TRUE,     
        cex.main = 1.2,     
        cex.axis = 1.1,     
        cex.lab = 1.1,     
        ylim = c(0, max(car_data$Price_in_thousands, na.rm = TRUE) * 1.1)
)


title(xlab = "Price in Thousands (USD)", cex.lab = 1.2)
title(ylab = "")
grid(lty = 2, col = "gray", lwd = 0.5) #boxplot

#time series analysis

ts_data <- ts(car_data$Sales_in_thousands , frequency = 1)

plot(ts_data, main = "Time Series Analysis (Sales)", xlab = "Date", ylab = "Sales",
     col = "blue", type = "l") #line 
# simple moving average
sma_window <- 12  #12 data points
sma <- stats::filter(ts_data, rep(1/sma_window, sma_window), sides = 2)

lines(sma, col = "red", lwd = 2)
#legend
legend("topright", legend = c("Original Sales", paste("SMA (", sma_window, ")",
                                                      sep = "")), col = c("blue", "red"), lty = 1)

#train test split
#random forest
install.packages("randomForest")
library(randomForest)

set.seed(123) #initialize random number generator
car <- car_data[c("Engine_size","Horsepower","Price_in_thousands","Fuel_efficiency","Sales_in_thousands")]
#load data
car <- tidyr::drop_na(car) #drop null

nrow(car)
summary(car)

train_indices <- sample(1:nrow(car), 0.8 * nrow(car)) #train 80% 
train_data <- car[train_indices, ]
test_data <- car[-train_indices, ]

mtry_opt <- floor(sqrt(ncol(train_data) - 1))

rf_model <- randomForest(
  Sales_in_thousands ~ .,
  data      = train_data,
  ntree     = 500,          #number of decision trees
  mtry      = mtry_opt, #hyperparameter
  nodesize  = 5,
  importance = TRUE
)

pred <- predict(rf_model, newdata = test_data) #model prediction

rmse <- sqrt(mean((pred - test_data$Sales_in_thousands)^2))
cat("Root‑Mean‑Squared‑Error (RMSE):", round(rmse, 4), "\n")
#accuracy of model
mapping_accuracy <- 1 - (rmse / sd(test_data$Sales_in_thousands))
cat("Mapping Accuracy:", round(mapping_accuracy, 4), "\n")
#predict sales
new_observation <- data.frame(
  Price_in_thousands = c(25),
  Engine_size = c(2),
  Horsepower = c(150),
  Fuel_efficiency = c(25)
)

future_prediction <- predict(rf_model, newdata = new_observation)

print(future_prediction)


# total number of cars by brand
brand_count <- car_data %>%
  count(Manufacturer, name = "Number_of_Cars") %>%
  arrange(desc(Number_of_Cars)) # pipe operator
brand_plot <- plot_ly(
  brand_count,
  y = ~Manufacturer,
  x = ~Number_of_Cars,
  type = "bar"
) %>%
  layout(
    title = "Total Cars by Brand",
    yaxis = list(title = "Brand", categoryorder = "total ascending"),
    xaxis = list(title = "Total Cars")
  )

brand_plot

# number of cars by vehicle type
type_count <- car_data %>%
  count(Vehicle_type, name = "Number_of_Cars")

type_plot <- plot_ly(
  type_count,
  x = ~Vehicle_type,
  y = ~Number_of_Cars,
  type = "bar"
) %>%
  layout(
    title = "Total Cars by Vehicle Type",
    xaxis = list(title = "Vehicle Type"),
    yaxis = list(title = "Total Cars")
  )

type_plot

# sales by brand
brand_sales <- car_data %>%
  group_by(Manufacturer) %>%
  summarise(Total_Sales = sum(Sales_in_thousands, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales))

brand_sales_plot <- plot_ly(
  brand_sales,
  y = ~Manufacturer,
  x = ~Total_Sales,
  type = "bar"
) %>%
  layout(
    title = "Total Sales by Brand",
    yaxis = list(title = "Brand", categoryorder = "total ascending"),
    xaxis = list(title = "Sales in Thousands (USD)")
  )

brand_sales_plot

# sales by vehicle type
type_sales <- car_data %>%
  group_by(Vehicle_type) %>%
  summarise(Total_Sales = sum(Sales_in_thousands, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) # remove missing values

type_sales_plot <- plot_ly(
  type_sales,
  x = ~Vehicle_type,
   = ~Total_Sales,
  type = "bar"
) %>%
  layout(
    title = "Total Sales by Vehicle Type",
    xaxis = list(title = "Vehicle Type"),
    yaxis = list(title = "Sales in Thousands (USD)")
  )

type_sales_plot

# date analysis using lubridate 

car_data$Latest_Launch <- mdy(car_data$Latest_Launch)

car_data$Launch_Year <- year(car_data$Latest_Launch)
car_data$Launch_Month <- month(car_data$Latest_Launch, label = TRUE)

# number of cars by launch year
launch_year <- car_data %>%
  count(Launch_Year)

print(launch_year)
# number of cars by launch month
launch_month <- car_data %>%
  count(Launch_Month)

print(launch_month)
# number of cars by launch month and year
launch_month_year <- car_data %>%
  count(Launch_Year, Launch_Month)

print(launch_month_year)

# testing hypothesis for bestselling car

# define using median scale
sales_median <- median(car_data$Sales_in_thousands, na.rm = TRUE)

car_data$Bestselling <- ifelse(
  car_data$Sales_in_thousands > sales_median,
  "Yes",
  "No"
) #find bestseller cars

# counting bestseller and non bestseller
bestselling_count <- car_data %>%
  count(Bestselling)

print(bestselling_count)

# Price vs Bestselling Hypothesis Test, determine whether there is difference between price of bestseller and non-bestseller

price_test <- t.test(
  Price_in_thousands ~ Bestselling,
  data = car_data,
  alternative = "two.sided" #two tailed #t-test parametric 
)

print(price_test)

# Horsepower vs Bestselling Hypothesis to determine difference between horsepowers 

horsepower_test <- t.test(
  Horsepower ~ Bestselling,
  data = car_data,
  alternative = "two.sided"  #two tailed t-test parametric
)

print(horsepower_test)

# Vehicle Type vs Bestselling to determine the difference between vehicle types

vehicle_test <- chisq.test(
  table(car_data$Vehicle_type, car_data$Bestselling)
)
# chi squared test, non parametric, categorical data
print(vehicle_test)