# Monthly Retail Sales Prediction
Time Series Forecasting for Retail Sales in Restaurants.


Project Overview:

This project applies various time series forecasting methods to predict monthly retail sales in the restaurant and eating places industry. The dataset used in this analysis is sourced from the "MRTSSM7225USN.xls" file, which contains historical sales data. The goal is to evaluate different forecasting models and identify the most accurate one for future sales prediction.

Key Steps in the Analysis:

1. Data Preprocessing

Load the dataset and convert the date column to an appropriate tsibble format.

Perform exploratory data analysis (EDA) by visualizing trends, seasonality, and autocorrelations.

Apply STL decomposition to analyze the components of the time series.

2. Data Splitting

The dataset is divided into a training set (80% of the data) and a test set (20%) to evaluate forecasting accuracy.

3. Benchmark Forecasting Models

Several simple benchmark models are implemented:

Mean Forecasting (MEAN)

Naïve Forecasting (NAIVE)

Seasonal Naïve (SNAIVE)

Random Walk with Drift (RWWD)

STL Decomposition Model

The performance of these models is evaluated using accuracy metrics such as RMSE, MPE, and MAPE.

4. Advanced Time Series Models

The project explores multiple time series forecasting techniques:

a) Time Series Linear Model (TSLM)

Models trend and seasonality using linear regression.

Accuracy is evaluated, and the best model is selected.

b) Exponential Smoothing State Space Models (ETS)

Multiple ETS models are fitted, and the one with the lowest AICc is selected.

c) Seasonal ARIMA (SARIMA)

Conducts seasonal differencing and fits multiple ARIMA models.

Uses AICc for model selection and evaluates residual diagnostics.

d) Box-Cox Transformation

Applied to stabilize variance across different models.

Transformed data is used in benchmark, TSLM, ETS, and ARIMA models.

5. Model Selection

The best model from each category is chosen based on RMSE, MPE, and MAPE.

The final selection is made by comparing all models to identify the most accurate.

6. Forecasting Future Sales

Using the best-selected model (SARIMA (0,1,0)(2,1,2)), forecasts are generated for the next three years.

Forecasted values are visualized with historical data.

Key Findings:

Seasonal decomposition confirms strong seasonal patterns in retail sales.

The SARIMA model performs the best, demonstrating the lowest RMSE and MAPE.

The forecast for the next three years provides insights into expected industry trends.

Technologies Used

R Programming (using fpp3, ggplot2, tsibble, fable, lubridate, tidyverse, etc.)

Time Series Analysis & Forecasting

Model Evaluation & Residual Diagnostics

Conclusion

This project successfully applies various time series forecasting techniques to predict restaurant and eating place retail sales. By leveraging multiple models and evaluating their accuracy, the best approach for future sales predictions was identified. The methodology and insights from this study can be extended to other industries for robust forecasting.
