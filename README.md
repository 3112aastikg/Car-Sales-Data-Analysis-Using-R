# 🚗 Car Sales Data Analysis Using R

An interactive **Car Sales Data Analysis Dashboard** built with **R and Shiny** to explore car sales, pricing, vehicle characteristics, hypothesis testing, and sales prediction.

The project combines exploratory data analysis, statistical testing, visualization, and predictive modeling into an interactive web application.

## 🌐 Live Dashboard

👉 **[Open the Car Sales Analysis Dashboard](https://3112aastikg-car-sales-data-analysis.share.connect.posit.cloud/)**

The dashboard is deployed using **Posit Connect Cloud** and provides interactive sections for:

- 📊 Data Analysis
- 🧪 Hypothesis Testing
- 🔮 Sales Prediction

---

## 📌 Project Overview

The objective of this project is to analyze car sales data and identify relationships between **sales, price, horsepower, engine size, fuel efficiency, vehicle type, and brand**.

The analysis helps answer questions such as:

- Which car and brand have the highest sales?
- Which cars and brands have the lowest sales?
- How does price relate to sales?
- Is horsepower associated with bestselling cars?
- Does vehicle type influence sales?
- How are car prices distributed?
- Which brands and vehicle types generate the most sales?
- Can car sales be predicted using vehicle characteristics?

---

## 📊 Key Findings

According to the interactive dashboard:

| Metric | Result |
|---|---|
| 🏆 Bestselling Car | **Ford F-Series** |
| 🏆 Bestselling Brand | **Ford** |
| 📉 Lowest Selling Car | **Mitsubishi 3000GT** |
| 📉 Lowest Selling Brand | **Porsche** |

The dashboard also provides visual analysis of:

- Horsepower vs. Price
- Car Price Distribution
- Price Boxplots
- Sales Time Series
- Cars by Brand
- Sales by Brand
- Cars by Vehicle Type
- Sales by Vehicle Type

---

## 🧪 Hypothesis Testing

The application includes statistical analysis to investigate relationships between different vehicle characteristics and sales performance.

### Hypotheses explored

#### 1. Price vs. Bestselling Cars

Investigates whether vehicle price is associated with whether a car becomes a bestselling vehicle.

#### 2. Horsepower vs. Bestselling Cars

Examines whether horsepower has a significant relationship with bestselling status.

#### 3. Vehicle Type vs. Bestselling Cars

Tests whether vehicle type is associated with bestselling performance.

These analyses provide statistical support for understanding which vehicle characteristics may influence sales.

---

## 🔮 Sales Prediction

The dashboard includes an interactive prediction component.

Users can provide:

- **Price in Thousands (USD)**
- **Engine Size**
- **Horsepower**
- **Fuel Efficiency**

The application then uses the predictive model to estimate expected car sales.

This makes the project useful not only for historical analysis but also for exploring how different vehicle specifications could influence sales.
RMSE (Root Mean Square Error) and Mapping Accuracy was also calculated. 

---

## 🛠️ Technologies Used

### Programming Language
- **R**

### Framework
- **Shiny**

### Data Analysis
- Data cleaning and transformation
- Exploratory Data Analysis
- Statistical analysis
- Hypothesis testing
- Predictive modeling

### Visualization
- R visualization libraries
- Interactive Shiny visualizations

### Deployment
- **Posit Connect Cloud**

### Version Control
- **Git & GitHub**

---

## 📁 Project Structure

```text
Car-Sales-Data-Analysis-Using-R/
│
├── Car Sales Project Code.R
├── Car_sales.csv
├── app.R
├── manifest.json
└── README.md
```

### File Description

| File | Description |
|---|---|
| `Car Sales Project Code.R` | R script containing the main data analysis |
| `Car_sales.csv` | Car sales dataset used for the analysis |
| `app.R` | Shiny application used to create the interactive dashboard |
| `manifest.json` | Deployment configuration for Posit Connect |
| `README.md` | Project documentation |

---

## 📈 Dashboard Sections

### 📊 Analysis

The Analysis section provides an overview of the dataset and interactive visualizations including:

- Bestselling and lowest-selling cars
- Bestselling and lowest-selling brands
- Horsepower vs. price
- Price distribution
- Price boxplot
- Sales over time
- Cars by brand
- Sales by brand
- Cars by vehicle type
- Sales by vehicle type

### 🧪 Hypothesis Testing

This section evaluates relationships between:

```text
Price              → Bestselling
Horsepower         → Bestselling
Vehicle Type       → Bestselling
```

### 🔮 Prediction

The Prediction section allows users to enter vehicle characteristics and obtain a sales prediction.

Input variables include:

```text
Price
Engine Size
Horsepower
Fuel Efficiency
```

---

## 💡 Project Goals

This project demonstrates how **R can be used for end-to-end data analysis**, from raw data to an interactive analytical application.

The main goals are to:

1. Understand car sales patterns.
2. Identify high- and low-performing vehicles.
3. Compare sales across brands and vehicle types.
4. Investigate relationships between vehicle characteristics and sales.
5. Apply statistical hypothesis testing.
6. Build a predictive model for sales.
7. Present analytical results through an interactive Shiny dashboard.

---



## 📚 Dataset

The project uses the `Car_sales.csv` dataset included in this repository.

The dataset contains information about cars and their characteristics, which is used to investigate relationships between vehicle specifications and sales performance.

