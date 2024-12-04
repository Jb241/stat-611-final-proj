
#Perform tests
library(testthat)
test_dir(".")

#Implement for project
source("final-proj-fn.R")
set.seed(55)
data <- simulate_data(5000, 30)
best_model <- forward_stepwise(data)
hypothesis_test(best_model)
which_xs_sig <- hypothesis_test(best_model)
which_xs_sig
