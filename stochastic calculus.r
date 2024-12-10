# Load necessary libraries
library(tidyverse)
library(tidyr)
library(lubridate)
library(readxl)

# Load data (replace with correct file path)
debt_data_public <- read_excel("C:/Users/Richwanga/Downloads/Public Debt (Ksh Million).xlsx")  # Correct path here

# Check the column names to ensure 'Total' exists
names(debt_data_public)

# Clean the 'Total' column (remove commas and convert to numeric)
debt_data_public$Total <- as.numeric(gsub(",", "", debt_data_public$Total))

# Split data into training and test sets
train <- debt_data_public$Total[1:(length(debt_data_public$Total) - 12)]
test <- debt_data_public$Total[(length(debt_data_public$Total) - 11):length(debt_data_public$Total)]

# Calculate log returns
debt_data_public$Log_Returns <- log(debt_data_public$Total / lag(debt_data_public$Total))
log_returns <- na.omit(debt_data_public$Log_Returns)

# Estimate parameters
mu <- mean(log_returns)  # Drift (average return)
sigma <- sd(log_returns)  # Volatility

# Print estimated parameters
print(paste("Estimated Drift (mu):", mu))
print(paste("Estimated Volatility (sigma):", sigma))

# Simulate future debt using GBM
num_simulations <- 100  # Number of simulations
forecast_horizon <- 12  # Months into the future
dt <- 1  # Time step in months

last_debt <- tail(debt_data_public$Total, 1)
z <- matrix(rnorm(forecast_horizon * num_simulations), ncol = num_simulations)  # Random shocks
drift <- (mu - 0.5 * sigma^2) * dt
diffusion <- sigma * sqrt(dt) * z

# Calculate GBM paths
simulated_paths <- matrix(0, nrow = forecast_horizon, ncol = num_simulations)
simulated_paths[1, ] <- last_debt
for (t in 2:forecast_horizon) {
  simulated_paths[t, ] <- simulated_paths[t - 1, ] * exp(drift + diffusion[t, ])
}

# Plot simulated paths
plot(1:forecast_horizon, simulated_paths[, 1], type = "l", col = "purple", lty = 1,
     xlab = "Months into the Future", ylab = "Debt (Ksh Million)", 
     main = "Simulated Public Debt (GBM)")
for (i in 2:num_simulations) {
  lines(1:forecast_horizon, simulated_paths[, i], col = rgb(0.5, 0, 0.5, alpha = 0.1))
}

# Forecast test set using GBM
forecast_gbm <- c(tail(train, 1))
for (t in 1:length(test)) {
  z <- rnorm(1)
  next_val <- forecast_gbm[length(forecast_gbm)] * exp((mu - 0.5 * sigma^2) * dt + sigma * sqrt(dt) * z)
  forecast_gbm <- c(forecast_gbm, next_val)
}

# Plot predictions vs. actuals
plot(1:length(test), test, type = "l", col = "blue", xlab = "Months", ylab = "Debt (Ksh Million)", 
     main = "Model Validation: Predicted vs Actual")
lines(1:length(test), forecast_gbm[-1], col = "red", lty = 2)  # Predicted values (excluding the first point)

# Calculate validation metrics
mae <- mean(abs(test - forecast_gbm[-1]))
mse <- mean((test - forecast_gbm[-1])^2)
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Mean Squared Error (MSE):", mse))


