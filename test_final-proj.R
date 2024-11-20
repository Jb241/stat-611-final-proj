library(testthat)
context("Check forward stepwise model selection")
source("final-proj-fn.R")

test_that("data frame simulated with no associations", {
    set.seed(55)
  
    #Simple case, X1 should not be related to Y
    data <- simulate_data(300, 1)
    test_model <- lm(Y ~ 1 + X1, data=data)
    p <- summary(test_model)$coefficients[-1, 4]
    expect_true(p > 0.05)
    
    #Check format of output
    expect_equal(colnames(data)[1], "Y")
    expect_equal(colnames(data)[2], "X1")
    expect_equal(ncol(data), 2)
    expect_equal(nrow(data), 300)
    expect_is(data, "data.frame")
    expect_true(is.numeric(data$Y))
    expect_true(is.numeric(data$X1))
    
    #Edge cases
    expect_error(simulate_data(0, 10))
    expect_error(simulate_data(10, -1))
})

test_that("forward stepwise selects correct model", {
    set.seed(55)
    
    #Standard case
    Y <- rnorm(5000)
    X1 <- rnorm(5000)
    X2 <- rnorm(5000)
    X3 <- rnorm(5000)
    data <- data.frame(Y, X1, X2, X3)
    selected <- forward_stepwise(data)
    
    #Output type is correct
    expect_is(selected, "summary.lm")
    
    #Coefficient values make sense
    expect_true(all(selected$coefficients[, "Estimate"] != 0))
    
    #Error if wrong input type
    expect_error(forward_stepwise(c(1, 5, 7, 3, 5)))
    
    #Edge case of null model
    Y <- rnorm(5000)
    data_null <- data.frame(Y)
    selected_null <- forward_stepwise(data_null)
    expect_equal(nrow(selected_null$coefficients), 1)
    
    #Simple case with one very good predictor
    Y <- rnorm(5000)
    X1 <- rnorm(5000)
    X2 <- rnorm(5000)
    X3 <- rnorm(5000)
    X4 <- Y + + rnorm(5000, sd = 0.01)
    data_simple <- data.frame(Y, X1, X2, X3, X4)
    selected_simple <- forward_stepwise(data_simple)
    expect_equal(rownames(selected_simple$coefficients)[2], "X4")
    expect_true(selected_simple$coefficients["X4", "Pr(>|t|)"] < 0.001)
})

test_that("hypothesis test works", {
  set.seed(55)
  
  #Handling wrong input type
  expect_error(hypothesis_test(c(3, 5, 7, 3, 6, 5)))
  
  #Edge case of null model
  Y <- rnorm(5000)
  data_null <- data.frame(Y)
  selected <- forward_stepwise(data_null)
  expect_error(hypothesis_test(selected))
  
  #Simple case with one random, non-associated predictor
  Y <- rnorm(5000)
  X1 <- rnorm(5000)
  data_simple1 <- data.frame(Y, X1)
  model <- lm(Y ~ 1 + X1, data=data_simple1)
  model_sum <- summary(model)
  output <- capture.output(hypothesis_test(model_sum))
  expect_equal(output, 
               "Fail to reject the null hypothesis that all \u03B2s=0.")
  result <- hypothesis_test(model_sum)
  expect_equal(result, NULL)
  
  #Simple case with one very good predictor
  Y <- rnorm(5000)
  X1 <- Y + rnorm(5000, sd = 0.01)
  data_simple2 <- data.frame(Y, X1)
  model <- lm(Y ~ 1 + X1, data=data_simple2)
  model_sum <- summary(model)
  output <- capture.output(hypothesis_test(model_sum))
  expect_equal(output, 
               "Reject the null that all \u03B2s=0.")
  result <- hypothesis_test(model_sum)
  expect_equal(result, "X1")
})
