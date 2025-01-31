#Final Project: Group 2
#Load necessary packages
library(fpp3)
library(ggplot2)
library(tsibble)
library(lubridate)
library(tidyverse)
library(dplyr)
library(seasonal)
library(fable)
library(urca)
library(readxl)

#Set working directory
#setwd("/link to the folder containing data")

set.seed(42) #set seed to replicate the result

#Load the dataset and covert it to tsibble
dat <- read_excel("MRTSSM7225USN.xls")
dat <- dat |>
  mutate(Month = yearmonth(ymd(Month))) |> #Covert Month to yearmonth format
  as_tsibble(index = Month)

##Plot the graphs to understand the data
#Time plot to see trend, seasonality and cyclic characteristics
autoplot(dat, Sales) +
  labs(y = "Retail Sales ($ Million)",
       title = "Monthly Retail Sales in Restaurant and Eating Places Industry")

#Seasonal plot
dat |>
  gg_season(Sales, labels = "both") +
  labs(y = "Retail Sales ($ Million)",
       title = "Seasonal plot: Monthly Retail Sales in Restaurant and Eating Places Industry")

#ACF Plot for Autocorrelation
dat |>
  ACF(Sales) |>
  autoplot() + labs(title="Monthly Retail Sales in Restaurant and Eating Places Industry")

#Plotting STL decomposition
dcmp <- dat |>
  model(stl = STL(Sales))
components(dcmp) |> autoplot()

# Split into data train and test
train <- head(dat, 310)
test <- tail(dat, 77) #We hold 20% of the dataset as the test set

##Develop models by benchmark methods
# Variances increase over the time series, so we need to use log transformation
benchmark_models <- train %>%
  model(
    MEAN = MEAN(log(Sales)),
    NAIVE = NAIVE(log(Sales)),
    SNAIVE = SNAIVE(log(Sales)),
    RWWD = RW(log(Sales) ~ drift()),
    STL = decomposition_model(
      STL(log(Sales) ~ trend(window = 7), robust = TRUE), 
      NAIVE(season_adjust))
  )

#Forecast on the next 77 months
fc_benchmarks <- benchmark_models %>% forecast(h = 77)

#Compute accuracy on the test set for benchmark methods
accuracy_benchmarks <- accuracy(fc_benchmarks, dat)
print(accuracy_benchmarks)

#Residuals analysis to see if the best benchmark methods do well
best_benchmark <- train %>%
  model(
    RWWD = RW(log(Sales) ~ drift()))

#Residuals plotting
best_benchmark %>%
  gg_tsresiduals() + ggtitle("Residual")

#Ljung_box test
augment(best_benchmark) %>% features(.resid, ljung_box, lag = 10, dof = 0) 

##Develop TSLM models

#Fit the TSLM models
tslm_models <- train %>%
  model(
    trend = TSLM(log(Sales) ~ trend()),
    trend_season = TSLM(log(Sales) ~ trend() + season())
  )

#Forecast on the next 77 months
fc_tslm <- tslm_models %>% forecast(h = 77)

#Compute accuracy on the test set for TSLM models
accuracy_tslm <- accuracy(fc_tslm, dat)
print(accuracy_tslm)

#Residuals analysis to see if the best TSLM model do well
best_tslm <- train %>%
  model(
    TSLM = TSLM(log(Sales) ~ trend() + season()))

#Residuals plotting
best_tslm %>%
  gg_tsresiduals() + ggtitle("Residual")

#Ljung_box test
augment(best_tslm) %>% features(.resid, ljung_box, lag = 10, dof = 0) 

##Develop ETS models

#Select model with lowest AICc
ets_selected <- train |>
  model(ETS(log(Sales)))
report(ets_selected)

#Fit ETS models
ets_models <- train %>%
  model(
    ets_auto = ETS(log(Sales)),
    `MAM` = ETS(log(Sales) ~ error("M") + trend("A") + season("M")),
    `AAdM` = ETS(log(Sales) ~ error("A") + trend("Ad") + season("M")),
    `AAM` = ETS(log(Sales) ~ error("A") + trend("A") + season("M")),
    `MAdA` = ETS(log(Sales) ~ error("M") + trend("Ad") + season("A")),
    `MAA` = ETS(log(Sales) ~ error("M") + trend("A") + season("A")),
    `MAdM` = ETS(log(Sales) ~ error("M") + trend("Ad") + season("M")),
    `AAA` = ETS(log(Sales) ~ error("A") + trend("A") + season("A"))
  )

#Forecast on the next 77 months
fc_ets <- ets_models %>% forecast(h = 77)

#Compute accuracy on the test set for ETS models
accuracy_ets <- accuracy(fc_ets, dat)
print(accuracy_ets)

#Residuals analysis to see if the best benchmark methods do well
best_ets <- train %>%
  model(
    `AAM` = ETS(log(Sales) ~ error("A") + trend("A") + season("M")))

#Residuals plotting
best_ets %>%
  gg_tsresiduals() + ggtitle("Residual")

#Ljung_box test
augment(best_ets) %>% features(.resid, ljung_box, lag = 10, dof = 0)

##Develop ARIMA models

#Taking seasonal differencing 
train |>
  gg_tsdisplay(difference(log(Sales), 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

#Take further one differencing
train |>
  gg_tsdisplay(difference(log(Sales), 12) |> difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

#Fit ARIMA models
sarima_models <- train |>
  model(
    auto_arima = ARIMA(log(Sales)),
    stepwise = ARIMA(log(Sales), stepwise = FALSE, approx = FALSE),
    #Variation models based on auto_arima and stepwise
    arima_1 = ARIMA(log(Sales) ~ pdq(3,0,0) + PDQ(0,1,2)),
    arima_2 = ARIMA(log(Sales) ~ pdq(3,0,0) + PDQ(1,1,1)),
    arima_3 = ARIMA(log(Sales) ~ pdq(1,0,0) + PDQ(2,1,2)),
    arima_4 = ARIMA(log(Sales) ~ pdq(1,0,1) + PDQ(1,1,2)),
    arima_5 = ARIMA(log(Sales) ~ pdq(1,0,1) + PDQ(2,1,1)),
    arima_6 = ARIMA(log(Sales) ~ pdq(0,1,0) + PDQ(2,1,2)),
    arima_7 = ARIMA(log(Sales) ~ pdq(1,1,1) + PDQ(2,1,2))
  )

#Print all ARIMA models and related AICc
sarima_models |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")
glance(sarima_models) |> arrange(AICc) |> select(.model:BIC)

#Forecast on the next 113 months
fc_sarima <- sarima_models %>% forecast(h = 77)

#Compute accuracy on the test set for SARIMA models
accuracy_sarima <- accuracy(fc_sarima, dat)
print(accuracy_sarima)

##Box Cox Transformation, select the optimal lambda
lambda <- train %>%
  features(Sales, features = guerrero) %>%
  pull(lambda_guerrero)

#Fit all models with BoxCox Transformation
box_cox_models <- train |>
  model(
    bc_MEAN = MEAN(box_cox(Sales, lambda)),
    bc_NAIVE = NAIVE(box_cox(Sales, lambda)),
    bc_SNAIVE = SNAIVE(box_cox(Sales, lambda)),
    bc_RWWD = RW(box_cox(Sales, lambda) ~ drift()),
    bc_STL = decomposition_model(
      STL(box_cox(Sales, lambda) ~ trend(window = 7), robust = TRUE),
      NAIVE(season_adjust)
    ),
    bc_trend = TSLM(box_cox(Sales, lambda) ~ trend()),
    bc_trend_season = TSLM(box_cox(Sales, lambda) ~ trend() + season()),
    bc_MAdM = ETS(box_cox(Sales, lambda) ~ error("M") + trend("Ad") + season("M")),
    bc_MAM = ETS(box_cox(Sales, lambda) ~ error("M") + trend("A") + season("M")),
    bc_AAdM = ETS(box_cox(Sales, lambda) ~ error("A") + trend("Ad") + season("M")),
    bc_AAM = ETS(box_cox(Sales, lambda) ~ error("A") + trend("A") + season("M")),
    bc_MAdA = ETS(box_cox(Sales, lambda) ~ error("M") + trend("Ad") + season("A")),
    bc_MAA = ETS(box_cox(Sales, lambda) ~ error("M") + trend("A") + season("A")),
    bc_AAdA = ETS(box_cox(Sales, lambda) ~ error("A") + trend("Ad") + season("A")),
    bc_AAA = ETS(box_cox(Sales, lambda) ~ error("A") + trend("A") + season("A")),
    bc_auto_arima = ARIMA(box_cox(Sales, lambda)),
    bc_stepwise = ARIMA(box_cox(Sales, lambda), stepwise = FALSE, approx = FALSE),
    bc_arima_1 = ARIMA(box_cox(Sales, lambda) ~ pdq(3,0,0) + PDQ(0,1,2)),
    bc_arima_2 = ARIMA(box_cox(Sales, lambda) ~ pdq(3,0,0) + PDQ(1,1,1)),
    bc_arima_3 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,0,0) + PDQ(2,1,2)),
    bc_arima_4 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,0,1) + PDQ(1,1,2)),
    bc_arima_5 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,0,1) + PDQ(2,1,1)),
    bc_arima_6 = ARIMA(box_cox(Sales, lambda) ~ pdq(0,1,0) + PDQ(2,1,2)),
    bc_arima_7 = ARIMA(box_cox(Sales, lambda) ~ pdq(1,1,1) + PDQ(2,1,2))
  )

#Forecast models with BoxCox Transformation
fc_boxcox <- box_cox_models |>
  forecast(h = 77)

#Calculate the accuracy
accuracy_boxcox <- accuracy(fc_boxcox,dat)
print(fc_boxcox)
print(accuracy_boxcox)

#Residual analysis for best ARIMA model
best_arima <- train %>%
  model(
    arima_6 = ARIMA(log(Sales) ~ pdq(0,1,0) + PDQ(2,1,2)))

#Residuals plotting 
best_arima %>%
  gg_tsresiduals() + ggtitle("Residual")

#Ljung_box test
augment(best_arima) %>% features(.resid, ljung_box, lag = 10, dof = 0)

##Chose the best model based on RMSE, MPE, MAPE

#Combine accuracy measures for best model of each model type into a single data frame
accuracy_summary <- bind_rows(
  accuracy(fc_benchmarks, dat) %>% filter(.model == "RWWD") %>% mutate(Model = "Best Benchmark (RWWD)"),
  accuracy(fc_tslm, dat) %>% filter(.model == "trend_season") %>% mutate(Model = "Best TSLM (trend_season)"),
  accuracy(fc_ets, dat) %>% filter(.model == "AAM") %>% mutate(Model = "Best ETS (AAM)"),
  accuracy(fc_sarima, dat) %>% filter(.model == "arima_6") %>% mutate(Model = "Best ARIMA (arima_6)")
) %>%
  select(Model, RMSE, MPE, MAPE)

#Print the summary of accuracy measures
print(accuracy_summary)

##Using the best model, forecasting Retail Sale for the next 3 years
dat |>
  model(ARIMA(log(Sales) ~ pdq(0,1,0) + PDQ(2,1,2))) |>
  forecast(h="3 years") |>
  autoplot(dat) +
  labs(title = "Retail Sales in Restaurant and Eating Places Industry",
       y = "$ million")

