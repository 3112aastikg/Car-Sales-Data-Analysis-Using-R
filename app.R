#install packages
library(tidyverse)  #tidyverse packages
library(dplyr) #data manipulation
library(ggplot2) #data visualisation
library(plotly) #interactive visuals
library(lubridate) # date and time 
library(randomForest) #prediction  
library(shiny)  #shiny dashboard app


# load data
car_data <- read.csv("Car_sales.csv")

# data inspection
head(car_data)
str(car_data)
summary(car_data)


# check missing values
colSums(is.na(car_data))


# check duplicate rows
duplicate_rows <- car_data[duplicated(car_data), ]
print(duplicate_rows)


# Scatter plot - Horsepower vs Price
custom_scatterplot <- ggplot(car_data, aes(x = Horsepower, y = Price_in_thousands,
                                           color = Fuel_efficiency)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_color_gradient(low = "red", high = "green") +
  labs(
    title = "Horsepower vs Price",
    x = "Horsepower",
    y = "Price in Thousands (USD)",
    color = "Fuel Efficiency"
  ) +
  theme_minimal()

print(custom_scatterplot)


# Histogram + Density
ggplot(car_data, aes(x = Price_in_thousands)) +
  geom_histogram(aes(y = ..density..), fill = "lightpink", color = "black", bins = 30) +
  geom_density(alpha = 0.7, fill = "skyblue") +
  labs(title = "Car Price Distribution Curve",
       x = "Price in Thousands (USD)",
       y = "Density")


# Boxplot
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
        xlim = c(0, max(car_data$Price_in_thousands, na.rm = TRUE) * 1.1)
)

title(xlab = "Price in Thousands (USD)", cex.lab = 1.2)
title(ylab = "")
grid(lty = 2, col = "gray", lwd = 0.5)


# Time Series Analysis
ts_data <- ts(car_data$Sales_in_thousands, frequency = 1)

plot(ts_data,
     main = "Time Series Analysis (Sales)",
     xlab = "Date",
     ylab = "Sales",
     col = "blue",
     type = "l")

sma_window <- 12

sma <- stats::filter(
  ts_data,
  rep(1 / sma_window, sma_window),
  sides = 2
)

lines(sma, col = "red", lwd = 2)

legend(
  "topright",
  legend = c(
    "Original Sales",
    paste("SMA (", sma_window, ")", sep = "")
  ),
  col = c("blue", "red"),
  lty = 1
)


# Random Forest Model

set.seed(123)

car <- car_data[c(
  "Engine_size",
  "Horsepower",
  "Price_in_thousands",
  "Fuel_efficiency",
  "Sales_in_thousands"
)] #values used for prediction: engine size, horse power, fuel efficiency, price

car <- tidyr::drop_na(car)

nrow(car)
summary(car)


train_indices <- sample(
  1:nrow(car),
  0.8 * nrow(car)
)

train_data <- car[train_indices, ]
test_data <- car[-train_indices, ] #train test split


mtry_opt <- floor(sqrt(ncol(train_data) - 1))


rf_model <- randomForest(
  Sales_in_thousands ~ .,
  data = train_data,
  ntree = 500, #number of decision trees
  mtry = mtry_opt,
  nodesize = 5,
  importance = TRUE
)


pred <- predict(
  rf_model,
  newdata = test_data
)

#make prediction
rmse <- sqrt(
  mean(
    (pred - test_data$Sales_in_thousands)^2
  )
)

cat(
  "Root-Mean-Squared-Error (RMSE):",
  round(rmse, 4),
  "\n"
)
#rmse 

mapping_accuracy <- 1 - (
  rmse / sd(test_data$Sales_in_thousands)
)
# accuracy test
cat(
  "Mapping Accuracy:",
  round(mapping_accuracy, 4),
  "\n"
)


# future prediction

new_observation <- data.frame(
  Price_in_thousands = c(25),
  Engine_size = c(2),
  Horsepower = c(150),
  Fuel_efficiency = c(25)
)

future_prediction <- predict(
  rf_model,
  newdata = new_observation
)

print(future_prediction)


# Cars by Brand

brand_count <- car_data %>%
  count(Manufacturer, name = "Number_of_Cars") %>%
  arrange(desc(Number_of_Cars))
#vertical graph highest to lowest

brand_plot <- plot_ly(
  brand_count,
  y = ~Manufacturer,
  x = ~Number_of_Cars,
  type = "bar"
) %>%
  layout(
    title = "Total Cars by Brand",
    yaxis = list(
      title = "Brand",
      categoryorder = "array",
      categoryarray = rev(brand_count$Manufacturer),
      automargin = TRUE,
      tickfont = list(size = 10)
    ),
    xaxis = list(
      title = "Total Cars"
    ),
    margin = list(
      l = 120,
      r = 20,
      t = 60,
      b = 70
    )
  )


# Cars by Vehicle Type

type_count <- car_data %>%
  count(Vehicle_type, name = "Number_of_Cars")

#horizontal graph 
type_plot <- plot_ly(
  type_count,
  x = ~Vehicle_type,
  y = ~Number_of_Cars,
  type = "bar"
) %>%
  layout(
    title = "Total Cars by Vehicle Type",
    xaxis = list(
      title = "Vehicle Type"
    ),
    yaxis = list(
      title = "Total Cars"
    )
  )


# Sales by Brand

brand_sales <- car_data %>%
  group_by(Manufacturer) %>%
  summarise(
    Total_Sales = sum(
      Sales_in_thousands,
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(Total_Sales))
# similar vertical graph to cars by brand

brand_sales_plot <- plot_ly(
  brand_sales,
  y = ~Manufacturer,
  x = ~Total_Sales,
  type = "bar"
) %>%
  layout(
    title = "Total Sales by Brand",
    yaxis = list(
      title = "Brand",
      categoryorder = "array",
      categoryarray = rev(brand_sales$Manufacturer),
      automargin = TRUE,
      tickfont = list(size = 10)
    ),
    xaxis = list(
      title = "Sales in Thousands (USD)"
    ),
    margin = list(
      l = 120,
      r = 20,
      t = 60,
      b = 70
    )
  )


# Sales by Vehicle Type
#horizontal bars
type_sales <- car_data %>%
  group_by(Vehicle_type) %>%
  summarise(
    Total_Sales = sum(
      Sales_in_thousands,
      na.rm = TRUE
    )
  ) %>%
  arrange(desc(Total_Sales))


type_sales_plot <- plot_ly(
  type_sales,
  x = ~Vehicle_type,
  y = ~Total_Sales,
  type = "bar"
) %>%
  layout(
    title = "Total Sales by Vehicle Type",
    xaxis = list(
      title = "Vehicle Type"
    ),
    yaxis = list(
      title = "Sales in Thousands (USD)"
    )
  )


# Date Analysis

car_data$Latest_Launch <- mdy(
  car_data$Latest_Launch
)

car_data$Launch_Year <- year(
  car_data$Latest_Launch
)

car_data$Launch_Month <- month(
  car_data$Latest_Launch,
  label = TRUE
) #launch month launch year latest launch


launch_year <- car_data %>%
  count(Launch_Year)

launch_month <- car_data %>%
  count(Launch_Month)

launch_month_year <- car_data %>%
  count(
    Launch_Year,
    Launch_Month
  )
#launch month year

#  Classification of Bestselling

sales_median <- median(
  car_data$Sales_in_thousands,
  na.rm = TRUE
)

car_data$Bestselling <- ifelse(
  car_data$Sales_in_thousands > sales_median,
  "Yes",
  "No"
)


bestselling_count <- car_data %>%
  count(Bestselling)

print(bestselling_count)


# hypothesis for Price vs Bestselling

price_test <- t.test(
  Price_in_thousands ~ Bestselling,
  data = car_data,
  alternative = "two.sided"
) #parametric t-test two tailed

print(price_test)


# Hypothesis for Horsepower vs Bestselling

horsepower_test <- t.test(
  Horsepower ~ Bestselling,
  data = car_data,
  alternative = "two.sided"
) #parametric t-test two tailed

print(horsepower_test)


# hypothesis for Vehicle Type vs Bestselling

vehicle_test <- chisq.test(
  table(
    car_data$Vehicle_type,
    car_data$Bestselling
  )
) #chi square- non parametric, no tail

print(vehicle_test)


# KPI calculations
bestselling_car <- car_data %>%
  filter(!is.na(Sales_in_thousands)) %>%
  slice_max(Sales_in_thousands, n = 1, with_ties = FALSE) %>%
  mutate(Car_Name = paste(Manufacturer, Model)) %>%
  pull(Car_Name) #best selling car overall

lowest_selling_car <- car_data %>%
  filter(!is.na(Sales_in_thousands)) %>%
  slice_min(Sales_in_thousands, n = 1, with_ties = FALSE) %>%
  mutate(Car_Name = paste(Manufacturer, Model)) %>%
  pull(Car_Name) #lowest selling car overall

brand_sales_kpi <- car_data %>%
  group_by(Manufacturer) %>% #calculate total sales by manufacturer 
  summarise(Total_Sales = sum(Sales_in_thousands, na.rm = TRUE)) %>%
  arrange(desc(Total_Sales)) #best brand and lowest brand

bestselling_brand <- brand_sales_kpi %>%
  slice_max(Total_Sales, n = 1, with_ties = FALSE) %>%
  pull(Manufacturer)

lowest_selling_brand <- brand_sales_kpi %>%
  slice_min(Total_Sales, n = 1, with_ties = FALSE) %>%
  pull(Manufacturer)

# UI for Dashboard
#fluid for creating UI
#dark colored theme: dark navy blue background, blue for active tabs, cyan for KPI headings, green for prediction button, white for text, slate for borders/secondary text, purple for hypothesis headings
ui <- fluidPage( #CSS for UI, not part of R code, just for UI styling unlike streamlit in python which has built in UI styling
  tags$style(HTML("
  
  /* overall dashboard */
  
  body {
    font-family: Arial, sans-serif;
    background-color: #0f172a;
    color: white;
  }
  
  .container-fluid {
    padding-left: 30px;
    padding-right: 30px;
  }
  
  
  /* main title */
  
  .title-panel {
    text-align: center;
    color: white;
  }
  
  .navbar-header {
    float: none;
  } /* navigation */
  
  
  /* dashboard description */
  
  .dashboard-info {
    text-align: center;
    font-size: 14px;
    color: #cbd5e1;
    margin-top: -10px;
    margin-bottom: 20px;
  }
  
  
  /* tabs */
  
  .nav-tabs {
    margin-bottom: 20px;
    border-bottom: 1px solid #475569;
  }
  
  .nav-tabs > li > a {
    font-size: 16px;
    font-weight: bold;
    padding: 12px 20px;
    color: #cbd5e1;
    background-color: #1e293b;
    border: 1px solid #334155;
    margin-right: 5px;
    border-radius: 6px 6px 0 0;
  }
  
  .nav-tabs > li > a:hover {
    color: white;
    background-color: #334155;
  }
  
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover,
  .nav-tabs > li.active > a:focus {
    color: white;
    background-color: #2563eb;
    border-color: #2563eb;
  }
  
  
  /* section headings */
  
  h2 {
    font-weight: bold;
    margin-bottom: 25px;
    color: white;
  }
  
  h3 {
    font-weight: bold;
    color: #f8fafc;
  }
  
  h4 {
    color: #f8fafc;
  }
  
  
  /* KPI cards */
  
  .kpi-card {
    border: 1px solid #334155;
    border-radius: 8px;
    padding: 15px;
    margin-bottom: 20px;
    text-align: center;
    background-color: #1e293b;
    color: white;
  }
  
  .kpi-card h4 {
    font-weight: bold;
    margin-top: 5px;
    color: #38bdf8;
  }
  
  .kpi-card h3 {
    margin-bottom: 5px;
    color: white;
  }
  
  
  /* graph containers */
  
  .plot-container {
    margin-bottom: 25px;
  }
  
  
  /* requested text styling */

  .hypothesis-content {
    font-size: 18px;
    font-weight: bold;
    color: white;
  }

  .hypothesis-content h2 {
    font-size: 28px;
    font-weight: bold;
  }

  .hypothesis-content h4 {
    font-size: 20px;
    font-weight: bold;
    color: #a78bfa;
  }

  .prediction-content {
    font-size: 18px;
    font-weight: bold;
    color: white;
  }

  .prediction-content h2 {
    font-size: 28px;
    font-weight: bold;
  }

  .prediction-content label {
    font-size: 18px;
    font-weight: bold;
    color: white;
  }

  .prediction-content .form-control {
    font-size: 17px;
    font-weight: bold;
  }

  .prediction-content #predict_button {
    background-color: #16a34a !important;
    color: white !important;
    border: none !important;
    font-weight: bold;
    font-size: 18px;
  }

  .prediction-content #predict_button:hover {
    background-color: #22c55e !important;
  }

  .prediction-result {
    font-size: 18px;
    font-weight: bold;
  }

  .prediction-result .shiny-text-output {
    font-size: 28px;
    font-weight: bold;
  }


  /* hypothesis testing */

  .hypothesis-box {
    border: 1px solid #334155;
    border-radius: 10px;
    padding: 25px;
    margin-top: 20px;
    background-color: #1e293b;
  }
  
  .hypothesis-box h2 {
    margin-top: 0;
    margin-bottom: 25px;
  }
  
  .hypothesis-section {
    border: 1px solid #334155;
    border-radius: 8px;
    padding: 20px;
    margin-bottom: 20px;
    background-color: #1e293b;
  }
  
  .hypothesis-section h4 {
    margin-top: 0;
    font-weight: bold;
    color: #a78bfa;
  }
  
  .hypothesis-section .shiny-text-output {
    font-size: 15px;
    line-height: 1.6;
    color: white;
  }
  
  
  /* prediction */
  
  .prediction-box {
    border: 1px solid #334155;
    border-radius: 10px;
    padding: 25px;
    margin-top: 20px;
    background-color: #1e293b;
  }
  
  .prediction-box h2 {
    margin-top: 0;
    margin-bottom: 25px;
  }
  
  .prediction-box .form-group {
    margin-bottom: 20px;
  }
  
  .prediction-box .form-control {
    border-radius: 6px;
    background-color: #0f172a;
    color: white;
    border: 1px solid #475569;
  }
  
  .prediction-box .btn {
    margin-top: 10px;
    border-radius: 6px;
    padding: 8px 20px;
    font-weight: bold;
    background-color: #16a34a;
    color: white;
    border: none;
  }
  
  .prediction-box .btn:hover {
    background-color: #22c55e;
  }
  
  .prediction-result {
    border: 1px solid #334155;
    border-radius: 8px;
    padding: 15px;
    margin-top: 25px;
    background-color: #0f172a;
    color: white;
    font-size: 16px;
    line-height: 1.8;
  }
  
  
  /* spacing between rows */
  
  .row {
    margin-bottom: 10px;
  }
  
  ")),
  
  titlePanel("Car Sales Analysis Dashboard"), #title
  
  p(
    "Interactive dashboard for analyzing car sales, pricing, vehicle performance, testing hypotheses against various factors, and predicting sales based on several factors.",
    style = "font-size: 14px; color: white; font-weight: bold; font-style: italic; margin-top: -10px; margin-bottom: 20px;"
  ),
  
  tabsetPanel(
    type = "tabs", #shiny tabs 3 tabs
    
    tabPanel(
      "Analysis", #Analysis tab
      
      h2("Data Analysis Overview"), #subtitle
      #kpi's key performance indicators
      fluidRow(
        column(
          3,
          div(
            style = "border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 20px; text-align: center;",
            h4("Bestselling Car"),
            h3(bestselling_car)
          )
        ),
        column(
          3,
          div(
            style = "border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 20px; text-align: center;",
            h4("Bestselling Brand"),
            h3(bestselling_brand)
          )
        ),
        column(
          3,
          div(
            style = "border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 20px; text-align: center;",
            h4("Lowest Selling Car"),
            h3(lowest_selling_car)
          )
        ),
        column(
          3,
          div(
            style = "border: 1px solid #ddd; border-radius: 8px; padding: 15px; margin-bottom: 20px; text-align: center;",
            h4("Lowest Selling Brand"),
            h3(lowest_selling_brand)
          )
        )
      ),
      
      # 1 row
      fluidRow(
        column(
          6,
          h3("Horsepower vs Price"),
          div(
            style = "margin-bottom: 25px;",
            plotOutput("scatter_plot")
          )
        ),
        
        column(
          6,
          h3("Car Price Distribution - Histogram"),
          div(
            style = "margin-bottom: 25px;",
            plotOutput("price_hist")
          )
        )
      ),
      
      
      # 2 row
      fluidRow(
        column(
          6,
          h3("Distribution of Car Prices - Boxplot"),
          div(
            style = "margin-bottom: 25px;",
            plotOutput("price_boxplot", height = "340px")
          )
        ),
        
        column(
          6,
          h3("Sales Time Series Analysis"),
          plotOutput("ts_plot", height = "300px")
        )
      ),
      
      
      # 3 row
      fluidRow(
        column(
          6,
          h3("Cars by Brand"),
          div(
            style = "margin-bottom: 25px;",
            plotlyOutput("brand_plot", height = "600px")
          )
        ),
        
        column(
          6,
          h3("Sales by Brand"),
          div(
            style = "margin-bottom: 25px;",
            plotlyOutput("brand_sales_plot", height = "600px")
          )
        )
      ),
      
      
      # 4 row
      fluidRow(
        column(
          6,
          h3("Cars by Vehicle Type"),
          div(
            style = "margin-bottom: 25px;",
            plotlyOutput("type_plot")
          )
        ),
        
        column(
          6,
          h3("Sales by Vehicle Type"),
          div(
            style = "margin-bottom: 25px;",
            plotlyOutput("type_sales_plot")
          )
        )
      ) 
    ), # all 8 graphs
    #hypothesis panel
    tabPanel(
      "Hypothesis Testing",
      
      div(
        class = "hypothesis-content",
        
        h2("Hypothesis Testing"),
        
        h4("Price vs Bestselling"),
        textOutput("price_hypothesis"),
        
        h4("Horsepower vs Bestselling"),
        textOutput("horsepower_hypothesis"),
        
        h4("Vehicle Type vs Bestselling"),
        textOutput("vehicle_hypothesis")
      )
    ),
    
    tabPanel(
      "Prediction",
      #prediction panel
      div(
        class = "prediction-content",
        h2("Sales Prediction"),
        
        numericInput(
          "prediction_price",
          "Price in Thousands (USD)",
          value = NULL, #25 for example
          min = 0
        ),
        
        numericInput(
          "prediction_engine",
          "Engine Size",
          value = NULL, #2 for example
          min = 0
        ),
        
        numericInput(
          "prediction_horsepower",
          "Horsepower",
          value = NULL, #150 for example
          min = 0
        ),
        
        numericInput(
          "prediction_fuel",
          "Fuel Efficiency",
          value = NULL,#25 for example
          min = 0
        ),
        
        actionButton(
          "predict_button",
          "Predict Sales"
        ), #submit inputs using action button
        
        br(),
        br(),
        
        div(
          class = "prediction-result",
          textOutput("prediction_result") #predict result output
        ),
        br(),
        textOutput("rmse_result"), 
        br(),
        textOutput("accuracy_result") #print rmse & accuracy results
      )
    )
  )
)
# server logic where the brain is calculating the data

server <- function(input, output) {
  
  
  output$scatter_plot <- renderPlot({
    custom_scatterplot
  })
  #scatter
  
  
  #hist density
  output$price_hist <- renderPlot({
    
    ggplot(
      car_data,
      aes(x = Price_in_thousands)
    ) +
      geom_histogram(
        aes(y = ..density..),
        fill = "lightpink",
        color = "black",
        bins = 30
      ) +
      geom_density(
        alpha = 0.7,
        fill = "skyblue"
      ) +
      labs(
        title = "Car Price Distribution Curve",
        x = "Price in Thousands (USD)",
        y = "Density"
      )
    
  })
  
  
  #boxplot
  output$price_boxplot <- renderPlot({
    
    price_values <- car_data$Price_in_thousands
    price_values <- price_values[is.finite(price_values)]
    
    boxplot_data <- data.frame(
      Price_in_thousands = price_values
    )
    
    ggplot(
      boxplot_data,
      aes(
        x = Price_in_thousands,
        y = ""
      )
    ) +
      geom_boxplot(
        fill = "orange",
        color = "black",
        notch = TRUE,
        notchwidth = 0.5,
        outlier.shape = 1,
        outlier.size = 2.5,
        width = 0.5
      ) +
      labs(
        title = "Distribution of Car Prices",
        x = "Price in Thousands (USD)",
        y = ""
      ) +
      scale_x_continuous(
        limits = c(
          0,
          max(price_values) * 1.1
        )
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 14,
          face = "bold"
        ),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
  }, height = 300)
  
  
  output$brand_plot <- renderPlotly({
    brand_plot
  })
  
  
  output$type_plot <- renderPlotly({
    type_plot
  })
  
  
  output$brand_sales_plot <- renderPlotly({
    brand_sales_plot
  })
  
  
  output$type_sales_plot <- renderPlotly({
    type_sales_plot
  }) #renderplot and renderplotly functions for displaying reactive visualization 
  #horizontal and vertical graphs
  
  #time series analysis using year and month
  output$ts_plot <- renderPlot({
    
    par(mar = c(6, 4, 5, 2))
    
    monthly_sales <- car_data %>%
      mutate(
        Month_Year = format(Latest_Launch, "%b %Y")
      ) %>%
      group_by(Latest_Launch, Month_Year) %>%
      summarise(
        Total_Sales = sum(Sales_in_thousands, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Latest_Launch)
    
    plot(
      monthly_sales$Total_Sales,
      main = "Time Series Analysis (Sales)",
      xlab = NA,
      ylab = "Sales",
      col = "blue",
      type = "l",
      xaxt = "n"
    )
    
    axis(
      1,
      at = 1:nrow(monthly_sales),
      labels = monthly_sales$Month_Year,
      las = 2,
      cex.axis = 0.7
    ) #year and month rather than numbers like 50 100 150
    
    sma_window <- 3
    
    sma <- stats::filter(
      monthly_sales$Total_Sales,
      rep(1 / sma_window, sma_window),
      sides = 2
    )
    
    lines(
      1:nrow(monthly_sales),
      sma,
      col = "red",
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c(
        "Original Sales",
        paste("SMA (", sma_window, ")", sep = "")
      ),
      col = c("blue", "red"),
      lty = 1
    )
    
  })
  # hypothesis testing
  
  output$price_hypothesis <- renderText({
    if (price_test$p.value < 0.05) {
      paste(
        "p-value:",
        paste0(format(signif(price_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "< 5% - Reject the null hypothesis. There is a significant difference in price between bestselling and non-bestselling cars, so accept alternative hypothesis."
      ) #reject null hypothesis, accept alternative hypothesis
    } else if (price_test$p.value > 0.05) {
      paste( #accept null hypothesis, reject alternative hypothesis
        "p-value:",
        paste0(format(signif(price_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "> 5% - There is insufficient evidence to reject the null hypothesis, so reject alternative hypothesis and accept null hypothesis."
      ) #if condition to accept or reject null and alternative hypothesis
    } else {
      paste(
        "p-value:",
        paste0(format(signif(price_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "= 5% - The p-value is equal to the significance level, so reject null hypothesis and accept alternative hypothesis."
      ) #it is equal and reject it, there is sufficient evidence
    } #bestseller hypothesis
  })
  #horsepower hypothesis
  output$horsepower_hypothesis <- renderText({
    if (horsepower_test$p.value < 0.05) {
      paste(
        "p-value:",
        paste0(format(signif(horsepower_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "< 5% - Reject the null hypothesis. There is a significant difference in horsepower between bestselling and non-bestselling cars, so accept alternative hypothesis."
      )
    } else if (horsepower_test$p.value > 0.05) {
      paste(
        "p-value:",
        paste0(format(signif(horsepower_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "> 5% - There is insufficient evidence to reject the null hypothesis, so reject alternative hypothesis and accept null hypothesis."
      )
    } else {
      paste(
        "p-value:",
        paste0(format(signif(horsepower_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "= 5% - The p-value is equal to the significance level, so reject null hypothesis and accept alternative hypothesis."
      ) #sufficient evidence
    }
  })
  #
  output$vehicle_hypothesis <- renderText({
    if (vehicle_test$p.value < 0.05) {
      paste(
        "p-value:",
        paste0(format(signif(vehicle_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "< 5% - Reject the null hypothesis. There is a significant association between vehicle type and bestselling status, so accept alternative hypothesis."
      )
    } else if (vehicle_test$p.value > 0.05) {
      paste(
        "p-value:",
        paste0(format(signif(vehicle_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "> 5% - There is insufficient evidence to reject the null hypothesis, so reject alternative hypothesis and accept null hypothesis."
      )
    } else {
      paste(
        "p-value:",
        paste0(format(signif(vehicle_test$p.value * 100, 3), scientific = FALSE, trim = TRUE), "%"),
        "= 5% - The p-value is equal to the significance level, so reject null hypothesis and accept alternative hypothesis."
      )
    }
  })
  
  # prediction using observeevent
  observeEvent(input$predict_button, {
    
    new_observation <- data.frame(
      Price_in_thousands = input$prediction_price,
      Engine_size = input$prediction_engine,
      Horsepower = input$prediction_horsepower,
      Fuel_efficiency = input$prediction_fuel
    )
    
    future_prediction <- predict(
      rf_model,
      newdata = new_observation
    )
    
    output$prediction_result <- renderText({
      paste(
        "Predicted Sales:",
        format(round(future_prediction * 1000, 0), big.mark = ",", scientific = FALSE)
      ) #format prediction
    })
    
    output$rmse_result <- renderText({
      paste(
        "RMSE:",
        round(rmse, 2)
      )
    })
    
    output$accuracy_result <- renderText({
      paste(
        "Mapping Accuracy:",
        paste0(round(mapping_accuracy * 100, 2), "%")
      )
    })
    
  })
  
  # combine and run
  
}

shinyApp(ui, server) #run app, launch it


