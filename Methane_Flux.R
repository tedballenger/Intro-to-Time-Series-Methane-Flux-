# load up all the libraries we need
library(tidyr)
library(dplyr)
library(forecast)
library(MARSS)

# setting seed so we can reproduce the results
set.seed(123)

# simulating our methane flux data for 2 years, adding some seasonality and noise
days <- seq(as.Date('2020-01-01'), by = "day", length.out = 730)
methane_flux <- 100 + 20*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 10)

# simulating environmental factors, also with some noise and seasonality
env_params <- matrix(c(
  7 + 0.2*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 0.2), # pH
  2 + 1*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 0.4),   # chl_a
  5 + 1*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 1.5),   # turbidity
  15 + 15*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 3),   # temperature
  10 + rnorm(length(days), 0, 2),                                      # depth
  8 + 1*sin(2*pi*seq_along(days)/365) + rnorm(length(days), 0, 1)      # DO
), nrow = length(days), byrow = FALSE)

# put all data together in one place
data <- data.frame(methane_flux, env_params)
colnames(data) <- c("methane_flux", "pH", "chl_a", "turbidity", "temperature", "depth", "DO")

# decompose methane flux to see the seasonality
methane_ts <- ts(data$methane_flux, frequency=365)  # make sure this matches your data
flux_stl <- stl(methane_ts, s.window="periodic")
plot(flux_stl)  # let's see what it looks like

# trying out arima on temperature - see if we can model univariately
temp_ts <- ts(data$temperature, frequency=365)  #make sure it fits your data
arima_temp <- auto.arima(temp_ts)
summary(arima_temp) 
checkresiduals(arima_temp)  

#fitting everything into a marss model
model_list <- list(
  B = matrix(1),  
  U = "zero",  # no external input
  Q = matrix("q"),  # estimate process error variance
  A = matrix(1),  # identity for direct observation
  Z = "identity",  # same for the design matrix, direct observation
  R = matrix("r"),  # estimating measurement error variance
  x0 = "zero",  # starting state
  tinitx = 0,  # starting at time 0
  C = matrix(c(rep("c", ncol(data)-1)), nrow = 1),  # effects of our covariates
  c = t(data[,-1])  # excluding methane flux from the covariates
)

# fitting the model to our data
fit <- MARSS(data$methane_flux, model = model_list)

# output
print(fit)
tidy(fit)
