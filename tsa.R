# import all libraries including shiny libraries
library(forecast)
library(dplyr)
library(zoo)
library(glue)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(DT)

# load the datasets
load("prod_data.rda")

# need to uncomment these lines if the data is loaded from a csv file
# prod_wider <- read.csv("Team03_Report_Data.csv", header = T)

# prod_wider$Time <- as.Date(prod_wider$Time, format = "%d-%m-%Y")


# Add production to consumption ratio to the dataframe
prod_wider$pc_ratio <- prod_wider$Renewable_energy_production/prod_wider$Total_primary_energy_consumption

viz_cols <- colnames(prod_wider)[2:14]

# Function for the Data Visualization Tab
# Plot the data an check how they vary with time
plot_func <- function(x_axis, y_axis, add_mov_avg){
  
  viz_dataset <- prod_wider |> select(x_axis, y_axis)
  production_ts <- ts(viz_dataset[, 2], frequency = 12, start = c(1973, 1), end = c(2022, 10))
  
  if (add_mov_avg == "No"){
    plot_1 <- plot(production_ts, lwd = 2, col = 'blue',
                   xlab = colnames(viz_dataset)[1], ylab = colnames(viz_dataset)[2], 
                   main = glue::glue("{colnames(viz_dataset)[2]} vs {colnames(viz_dataset)[1]} Graph")
    )
  } else{
    
    ma.trailing <- rollmean(production_ts, k = 12, align = "right")
    ma.centered <- ma(production_ts, order = 12)
    
    plot_1 <- plot(production_ts, lwd = 2, col = 'blue',
                   xlab = colnames(viz_dataset)[1], ylab = colnames(viz_dataset)[2], 
                   main = glue::glue("{colnames(viz_dataset)[2]} vs {colnames(viz_dataset)[1]} Graph")
    )
    lines(ma.centered, lwd = 2, col = 'red')
    lines(ma.trailing, lwd = 2, lty = 2)
    legend("topleft", col = c("blue", "red", "black"), 
           c(y_axis, "Centered Moving Average", "Trailing Moving Average"), 
           lty = c(1,1,2), lwd = c(1,2,2))
  }
  
  return (plot_1)
}

# Fitting the models and check the best model fit
# Function for the Forecast Tab
ts_func <- function(data, validation) {
  
  ts_data <- prod_wider[prod_wider$Time >= "2000-01-01", ] %>% select(Time, "Renewable_energy_production")
  production_ts <- ts(ts_data[, 2], start = c(2000, 1), end = c(2022, 10), frequency = 12)
  nValid <- as.numeric(validation)
  nTrain <- length(production_ts) - nValid
  train_prod <- window(production_ts, start = c(2000, 1), end = c(2000, nTrain))
  vali_prod <- window(production_ts, start = c(2000, nTrain + 1), end = c(2000, nTrain + nValid))
  # fitting only trend model
  trend_reg <- tslm(train_prod ~ trend)
  trend_pred <- forecast(trend_reg, h = nValid, level = 0)
  
  # adding seasonality to it
  season_reg <- tslm(train_prod ~ trend + season)
  season_pred <- forecast(season_reg, h = nValid, level = 0)
  
  # adding polynomial trend to it
  poly_season_reg <- tslm(train_prod ~ trend + I(trend)^2 + season)
  poly_season_pred <- forecast(poly_season_reg, h = nValid, level = 0)
  
  # checking with exponential trend
  exp_season_reg <- tslm(train_prod ~ trend + season, lambda = 0)
  exp_season_pred <- forecast(exp_season_reg, h = nValid, level = 0)
  
  # Add smoothing methods
  # Simple Exponential Smoothing
  ses_model <- ets(train_prod, model = "ANN")
  ses_model_pred <- forecast(ses_model, h = nValid, level = 0)
  
  # Holt-Winter model
  hw_model <- hw(train_prod, h = nValid)
  
  # ARIMA Model
  arima_model <- auto.arima(train_prod)
  arima_pred <- forecast(arima_model, h = nValid, level = 0)
  
  # add seasonal naive as benchmark
  snaive_pred <- snaive(train_prod, h = nValid, level = 0)
  
  # Computing accuracies
  trend_accr <- accuracy(trend_pred$mean, vali_prod)
  season_accr <- accuracy(season_pred$mean, vali_prod)
  poly_season_accr <- accuracy(poly_season_pred$mean, vali_prod)
  exp_season_accr <- accuracy(exp_season_pred$mean, vali_prod)
  snaive_accr <- accuracy(snaive_pred$mean, vali_prod)
  ses_accr <- accuracy(ses_model_pred$mean, vali_prod)
  hw_accr <- accuracy(hw_model$mean, vali_prod)
  arima_accr <- accuracy(arima_pred$mean, vali_prod)
  
  # Storing them in a dataframe
  accr_df <- rbind.data.frame(trend_accr, season_accr, poly_season_accr, exp_season_accr, 
                              snaive_accr, ses_accr, hw_accr, arima_accr)
  rownames(accr_df) <- c("Trend Accuracy", "Season Accuracy", "Polynomial Trend +Season Accuracy", 
                         "Exponential Season Accuracy", "Seasonal Naive Accuracy", 
                         "Simple Exponential Smoothing Accuracy", "Holt-Winter Model Accuracy", 
                         "Arima Model Accuracy")
  
  return (accr_df)
}

# Plot multiple models to compare best fit
ts_multi_plot <- function(data, validation, variables) {
  ts_data <- prod_wider[prod_wider$Time >= "2000-01-01", ] %>% select(Time, data)
  production_ts <- ts(ts_data[, 2], start = c(2000, 1), end = c(2022, 10), frequency = 12)
  nValid <- as.numeric(validation)
  nTrain <- length(production_ts) - nValid
  train_prod <- window(production_ts, start = c(2000, 1), end = c(2000, nTrain))
  vali_prod <- window(production_ts, start = c(2000, nTrain + 1), end = c(2000, nTrain + nValid))
  
  # fitting only trend model
  trend_reg <- tslm(train_prod ~ trend)
  trend_pred <- forecast(trend_reg, h = nValid, level = 0)
  
  # adding seasonality to it
  season_reg <- tslm(train_prod ~ trend + season)
  season_pred <- forecast(season_reg, h = nValid, level = 0)
  
  # adding polynomial trend to it
  poly_season_reg <- tslm(train_prod ~ trend + I(trend)^2 + season)
  poly_season_pred <- forecast(poly_season_reg, h = nValid, level = 0)
  
  # checking with exponential trend
  exp_season_reg <- tslm(train_prod ~ trend + season, lambda = 0)
  exp_season_pred <- forecast(exp_season_reg, h = nValid, level = 0)
  
  # add seasonal naive as benchmark
  snaive_pred <- snaive(train_prod, h = nValid, level = 0)
  
  # Add smoothing methods
  # Simple Exponential Smoothing
  ses_model <- ets(train_prod, model = "ANN")
  ses_model_pred <- forecast(ses_model, h = nValid, level = 0)
  
  # Holt-Winter model
  hw_model <- hw(train_prod, h = nValid)
  
  # ARIMA Model
  arima_model <- auto.arima(train_prod)
  arima_pred <- forecast(arima_model, h = nValid, level = 0)  
  
  df <- cbind.data.frame(trend_pred$mean, season_pred$mean, poly_season_pred$mean, exp_season_pred$mean, 
                         snaive_pred$mean, ses_model_pred$mean, hw_model$mean, arima_pred$mean)
  colnames(df) <- c("Trend", "Season", "Polynomial", "Exponential Season", "Seasonal Naive", 
                    "Exponential Smoothing", "Holt-Winter", "ARIMA")

  plot_data <- df[, variables]

  plot_2 <- plot(production_ts, main = "Actual versus Models", xlab = "Year")
  legend_text <- c(variables)  # Add each variable name to the legend text
  colors = c(rainbow(length(variables)))
  for (i in 1:length(variables)){
    lines(plot_data[, variables[i]], col = colors[i])
  }
  
  # Create a dynamic legend
  legend("topleft", legend = legend_text, col = colors, lty = 1)
  return (plot_2)
}

# Forecast based on best fit
# Function for the Predict Future Tab 
forecast_func <- function(data, time) {
  ts_data <- prod_wider[prod_wider$Time >= "2000-01-01", ] |> select(Time, data)
  production_ts <- ts(ts_data[, 2], start = c(2000, 1), end = c(2022, 10), frequency = 12)
  forecast_time <- as.numeric(time)
  
  # fit the models
  # Season, ARIMA, trend, exponential
  # Fit trend model
  trend_fit <- tslm(production_ts ~ trend)
  trend_pred <- forecast(trend_fit, h = forecast_time, level = 0)
  
  # Fit Season Model
  season_fit <- tslm(production_ts ~ trend + season)
  season_pred <- forecast(season_fit, h = forecast_time, level = 0)
  
  # Fit Exponential Model
  exponential_fit <- tslm(production_ts ~ trend + season, lambda = 0)
  exponential_pred <- forecast(exponential_fit, h = forecast_time, level = 0)
  
  # ARIMA Model fit
  arima_fit <- auto.arima(production_ts)
  arima_pred <- forecast(arima_fit, h = forecast_time, level = 0)
  
  combined <- ts(c(production_ts, arima_pred$mean), start = c(2000, 1), frequency = 12)
  
  # plot fitted data
  ymin <- min(production_ts[140:274, 1], arima_pred$mean, exponential_pred$mean, 
              season_pred$mean, trend_pred$mean)
  ymax <- max(arima_pred$mean, exponential_pred$mean, season_pred$mean, trend_pred$mean)
  plot_3 <- plot(arima_pred, type = 'l', lwd = 2, include = 130, 
                 ylim = c(ymin, ymax + 0.1 * ymax),
                 main = glue::glue("{forecast_time/12} Years Forecast values"),
                 ylab = glue::glue("{colnames(production_ts)}"), 
                 xlab = "Year")
  lines(exponential_pred$mean, col = "green", lwd = 2)
  lines(season_pred$mean, col = 'purple', lwd = 2)
  lines(trend_pred$mean, col = 'red', lwd = 2)
  legend("bottomright", fill = c('red', 'purple', 'green', 'blue'), 
         legend = c("Trend Model Output", "Seasonal Model Output", "Exponential Model Output", 
                    "ARIMA Model Output"))
  return(plot_3)
  
}

# Years to Goal Tab function
years_to_reach <- function(ratio, pred_start, series1, series2){
  h <- as.numeric(pred_start) * 12
  ratio <- as.numeric(ratio)
  if (prod_wider[1, series1]/prod_wider[1, series2] > 1){
    prod_wider$new_col <- prod_wider[, series2]/prod_wider[, series1]
  } else{
    prod_wider$new_col <- prod_wider[, series1]/prod_wider[, series2]
  }
  ts_data <- prod_wider[prod_wider$Time >= "2000-01-01", ] |> select(Time, new_col) |> as.data.frame()
  production_ts <- ts(ts_data[, 2], start = c(2000, 1), end = c(2022, 10), frequency = 12)
  
  # fit for simple trend model
  trend_fit <- tslm(production_ts ~ trend)
  trend_pred <- forecast(trend_fit, h = h, level = 0)
  while(tail(trend_pred$mean, 1) < ratio){
    h = h + 1
    trend_pred <- forecast(trend_fit, h = h, level = 0)
  }
  trend_pred <- forecast(trend_fit, h = h, level = 0)
  first_crossing_index_trend <- which(round(trend_pred$mean, 2) >= ratio)[1]
  first_crossing_year_trend <- floor((first_crossing_index_trend - 1)/12) + start(trend_pred$mean)[1]
  first_crossing_month_trend <- (first_crossing_index_trend - 1) %% 12 + 1
  
  # fit for season model
  h <- as.numeric(pred_start) * 12
  season_fit <- tslm(production_ts ~ trend + season)
  season_pred <- forecast(season_fit, h = h, level = 0)
  while(tail(season_pred$mean, 1) < ratio) {
    h = h + 1
    season_pred <- forecast(season_fit, h = h, level = 0)
  }
  season_pred <- forecast(season_fit, h = h, level = 0)
  first_crossing_index_season <- which(round(season_pred$mean, 2) >= ratio)[1]
  first_crossing_year_season <- floor((first_crossing_index_season - 1)/12) + start(season_pred$mean)[1]
  first_crossing_month_season <- (first_crossing_index_season - 1) %% 12 + 1
  
  # fit for exponential model
  h <- as.numeric(pred_start) * 12
  exponential_fit <- tslm(production_ts ~ trend + season, lambda = 0)
  exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  while(tail(exponential_pred$mean, 1) < ratio) {
    h = h + 1
    exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  }
  exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  first_crossing_index_exp <- which(round(exponential_pred$mean, 2) >= ratio)[1]
  first_crossing_year_exp <- floor((first_crossing_index_exp - 1)/12) + start(exponential_pred$mean)[1]
  first_crossing_month_exp <- (first_crossing_index_exp - 1) %% 12 + 1
  
  # fit for ARIMA model
  h <- as.numeric(pred_start) * 12
  arima_fit <- auto.arima(production_ts)
  arima_pred <- forecast(arima_fit, h = h, level = 0)
  while(tail(arima_pred$mean, 1) < ratio) {
    h = h + 1
    arima_pred <- forecast(arima_fit, h = h, level = 0)
  }
  arima_pred <- forecast(arima_fit, h = h, level = 0)
  first_crossing_index_arima <- which(round(arima_pred$mean, 2) >= ratio)[1]
  first_crossing_year_arima <- floor((first_crossing_index_arima - 1)/ 12) + start(arima_pred$mean)[1]
  first_crossing_month_arima <- (first_crossing_index_arima - 1) %% 12 + 1
  
  df <- cbind.data.frame(
    Model = c("Simple Trend", "Season + Trend", "Exponential Season", "ARIMA Model"), 
    Reaching_Date = c(glue("{month.abb[first_crossing_month_trend]}, {first_crossing_year_trend}"),
                     glue("{month.abb[first_crossing_month_season]}, {first_crossing_year_season}"),
                     glue("{month.abb[first_crossing_month_exp]}, {first_crossing_year_exp}"),
                     glue("{month.abb[first_crossing_month_arima]}, {first_crossing_year_arima}")
                    ))
  return (df)
}

# Years to Goal Visualize
plot_years_to_reach <- function(ratio, pred_start, series1, series2){
  h <- as.numeric(pred_start) * 12
  ratio <- as.numeric(ratio)
  if (prod_wider[1, series1]/prod_wider[1, series2] > 1) {
    prod_wider$new_col <- prod_wider[, series2]/prod_wider[, series1]
  } else {
    prod_wider$new_col <- prod_wider[, series1]/prod_wider[, series2]
  }
  ts_data <- prod_wider[prod_wider$Time >= "2000-01-01", ] |> select(Time, new_col) |> 
    as.data.frame()
  production_ts <- ts(ts_data[, 2], start = c(2000, 1), end = c(2022, 10), frequency = 12)
  
  # fit for simple trend model
  trend_fit <- tslm(production_ts ~ trend)
  trend_pred <- forecast(trend_fit, h = h, level = 0)
  while(tail(trend_pred$mean, 1) < ratio){
    h = h + 1
    trend_pred <- forecast(trend_fit, h = h, level = 0)
  }
  trend_pred <- forecast(trend_fit, h = h, level = 0)
  
  # fit for season model
  h <- as.numeric(pred_start) * 12
  season_fit <- tslm(production_ts ~ trend + season)
  season_pred <- forecast(season_fit, h = h, level = 0)
  while(tail(season_pred$mean, 1) < ratio) {
    h = h + 1
    season_pred <- forecast(season_fit, h = h, level = 0)
  }
  season_pred <- forecast(season_fit, h = h, level = 0)
  
  # fit for exponential model
  h <- as.numeric(pred_start) * 12
  exponential_fit <- tslm(production_ts ~ trend + season, lambda = 0)
  exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  while(tail(exponential_pred$mean, 1) < ratio) {
    h = h + 1
    exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  }
  exponential_pred <- forecast(exponential_fit, h = h, level = 0)
  
  # fit for ARIMA model
  h <- as.numeric(pred_start) * 12
  arima_fit <- auto.arima(production_ts)
  arima_pred <- forecast(arima_fit, h = h, level = 0)
  while(tail(arima_pred$mean, 1) < ratio) {
    h = h + 1
    arima_pred <- forecast(arima_fit, h = h, level = 0)
  }
  arima_pred <- forecast(arima_fit, h = h, level = 0)

  plot_4 <- plot(arima_pred, type = 'l', lwd = 2, include = 130,
                 main = glue::glue("Years to Reach Target Ratio of {ratio}"),
                 ylab = "Ratio", 
                 xlab = "Year")
  lines(exponential_pred$mean, col = "green", lwd = 2)
  lines(season_pred$mean, col = 'purple', lwd = 2)
  lines(trend_pred$mean, col = 'red', lwd = 2)
  legend("bottomright", fill = c('red', 'purple', 'green', 'blue'), 
         legend = c("Trend Model Output", "Seasonal Model Output", "Exponential Model Output", 
                    "ARIMA Model Output"))
  
  return (plot_4)
}