library(testthat)
context("Check forward stepwise model selection")
source("final-proj-fn.R")

test_that("data frame simulated with no associations", {
    set.seed(50)
    data <- simulate_data(300, 1)
    test_model <- lm(Y ~ 1 + X1, data=data)
    p <- summary(test_model)$coefficients[-1, 4]
    expect_true(p > 0.05)
    expect_equal(colnames(data)[1], "Y")
    expect_equal(colnames(data)[2], "X1")
    expect_equal(ncol(data), 2)
    expect_equal(nrow(data), 300)
    expect_is(data, "data.frame")
    expect_true(is.numeric(data$Y))
    expect_true(is.numeric(data$X1))
    expect_error(simulate_data(0, 10))
})

