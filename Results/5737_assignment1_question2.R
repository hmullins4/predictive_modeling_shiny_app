# QUESTION 2


# install.packages("stargazer")
# install.packages("dplyr")
# install.packages("glmnet")
# install.packages("e1071")
# install.packages("caret")
# install.packages("infotheo")
# install.packages("rhdf5")
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("Metrics")
# install.packages("glmnet")

library(stargazer)
library(dplyr)
library(glmnet)
library(e1071)
library(caret)
library(infotheo)
library(rhdf5)
library(httr)
library(jsonlite)
library(Metrics)
library(glmnet)

# Create an empty HDF5 dataset that will be used to
# store crypto prices

h5createFile("crypto_prices.h5")
h5createDataset("crypto_prices.h5", "prices", dims = c(400), storage.mode = "numeric")

# Using a method shown in class or another method you
# can produce, draw a crypto price sequentially in a
# for loop with a stopping condition of at 400 price
# values that are at least 3 seconds apart. Storing
# each value obtained into the HDF5 store.

get_price_bitstamp <- function() {
  url <- "https://www.bitstamp.net/api/v2/ticker/btcusd/"
  response <- GET(url, timeout(30))
  
  if (status_code(response) == 200) {
    content_text <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content_text)
    return(as.numeric(json_data$last))
  } else {
    cat("Error fetching data. Status code:", status_code(response), "\n")
    return(NA)
  }
}


prices <- numeric(400)

for (i in 1:400) {
  prices[i] <- get_price_bitstamp()
  
  if (is.na(prices[i])) {
    print(paste("Price", i, ": Failed to fetch. Skipping..."))
  } else {
    print(paste("Price", i, ":", prices[i]))
  }
  
  Sys.sleep(runif(1, 3, 15))
  
}


h5write(prices, "crypto_prices.h5", "prices")


# Then load the data from HDF5 and produce a
# dataframe with 6 columns where the first
# column has the values from time point 6 till
# the end, the second column time point 5 till
# the end time point minus 1, and continuing
# till the 6th column has the first time point
# has the first time point till 6th last. 

prices <- h5read("crypto_prices.h5", "prices")
prices <- as.numeric(prices)


lagged_df <- data.frame(
  c1 = prices[6:length(prices)],
  c2 = prices[5:(length(prices) - 1)],
  c3 = prices[4:(length(prices) - 2)],
  c4 = prices[3:(length(prices) - 3)],
  c5 = prices[2:(length(prices) - 4)],
  c6 = prices[1:(length(prices) - 5)]
)


# Using that Data Frame use 75 percent of
# the rows for training and the rest for
# testing (randomly selected rows)

set.seed(123456)
index <- sample(nrow(lagged_df), nrow(lagged_df)*.75)
train = lagged_df[index,] 
test = lagged_df[-index,]


# Train a linear model on the training
# data and get the RMSE on the predictions
# for the testing data


lm_train <- lm(c1 ~ c2 + c3 + c4 + c5 + c6, data = train)
summary(lm_train)

lm_predict <- predict(lm_train, newdata = test)

residuals <- test$c1 - lm_predict
lm_rmse <- sqrt(mean(residuals^2))
lm_rmse

# The RMSE under this model is 19.67728.


# Perform LASSO on the training dataset to
# remove variables where LASSO recommends
# and use that potentially reduced model
# on the testing dataset stating the RMSE

x <- model.matrix(c1 ~ c2 + c3 + c4 + c5 + c6, data = train)[, -1]
y <- train$c1

cv_lasso <- cv.glmnet(x, y, alpha = 1)

penalty_opt <- cv_lasso$lambda.min
penalty_opt

lasso_coefs <- coef(cv_lasso, s = penalty_opt)
lasso_coefs


# The only variable with a coefficient that
# was not shrunk to 0 is our second column.
# Applying this:

lasso_train <- lm(c1 ~ c2, data = train)
summary(lasso_train)

x_test <- model.matrix(c1 ~ c2, data = test)[, -1]
y_test <- test$c1

lasso_predict <- predict(lasso_train, newdata = test[, c("c2"), drop = FALSE])

lasso_rmse <- sqrt(mean((y_test - lasso_predict)^2))
lasso_rmse

# Therefore, the RMSE decreased to 
# 19.39466 when applying LASSO. This is
# not a drastic decrease, so we might only
# say this model is slightly better than
# the model without LASSO. This makes sense
# because each of the other columns/variables
# contain almost the exact same information
# as that contained in column 2 (excluding
# one to five rows depending on the column,
# and the fact that the rows are lagged).


# Train a SVM model on the training data and
# get the RMSE on the predictions for the 
# testing data

# Let's first run an svm model with all the
# variables, regardless of relevance.

svm_train <- svm(c1 ~ c2 + c3 + c4 + c5 + c6, data = train, kernel = "radial")
svm_predict <- predict(svm_train, newdata = test)
svm_rmse <- sqrt(mean((test$c1 - svm_predict)^2))
svm_rmse

# The root mean squared error went up from
# what it was under the linear model and the
# lasso model, up to 20.44663. This is still
# not a drastic increase, but it does demonstrate
# that the linear model is the best option to
# utilize for prediction.

# Now let's try with the LASSO-relevant variables,
# just training c1 on c2:


svm_lasso <- svm(c1 ~ c2, data = train, kernel = "radial")
test_reduced <- test[, c("c1", "c2"), drop = FALSE]
svm_predict_lasso <- predict(svm_lasso, newdata = test_reduced)
svm_rmse2 <- sqrt(mean((test$c1 - svm_predict_lasso)^2))
svm_rmse2


# This RMSE is better than the RMSE obtained in
# the previous support vector machine model, indi-
# cating that LASSO does offer at least a slight
# improvement for this price data. Again, this
# is not a significant change in the root mean
# squared error. 

# LASSO is supposed to shrink irrelevant
# variables to a coefficient of 0. This 
# left us with just the column 2 variable.
# Therefore, we eliminate all the other
# variables, giving us a slightly lower
# root mean squared error than when our
# model contained all the column variables.

# The support vector machine, on the other
# hand, did not improve our root mean
# squared error. Whether we apply our
# improved LASSO model or the original, the
# RMSE goes up slightly regardless. It is
# a lower value when just using c2 as our
# independent variable compared to using
# all the columns, telling us that those
# other columns really do not add any
# information to our model.

# The fact that our data is structured so
# particularly may contribute to the fact
# that our RMSE does not change much even
# when utilizing different models. Moreover,
# it makes sense that column 2 is the only
# variable that our model kept. A data point
# in time series data is always dependent on
# the data that came previously. Column 2
# contains only the data points that come
# directly before the data points in column
# 1, our dependent variable. These two variables
# would naturally have the closest relationship.











