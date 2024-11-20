library(testthat)
context("Check forward stepwise model selection")
source("final-proj-fn.R")

test_that("data frame simulated with no associations", {
    set.seed(45)
  
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
    set.seed(45)
    
    #Standard case
    data <- simulate_data(5000, 3)
    selected <- forward_stepwise(data)
    
    #Output type is correct
    expect_is(selected, "summary.lm")
    
    #Coefficient values make sense
    expect_true(all(selected$coefficients[, "Estimate"] != 0))
    
    #Error if wrong input type
    expect_error(forward_stepwise(c(1, 5, 7, 3, 5)))
    
    #Edge case of 0 predictors
    data_p0 <- simulate_data(5000, 0)
    selected_p0 <- forward_stepwise(data_p0)
    expect_equal(nrow(selected_p0$coefficients), 1)
    
    #Simple case with one very good predictor
    data_simple <- simulate_data(5000, 3)
    X4 <- data_simple[[1]] + rnorm(5000, sd = 0.01)
    data_simple <- cbind(data_simple, X4)
    selected_simple <- forward_stepwise(data_simple)
    expect_equal(rownames(selected_simple$coefficients)[2], "X4")
    expect_true(selected_simple$coefficients["X4", "Pr(>|t|)"] < 0.001)
})

