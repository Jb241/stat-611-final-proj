
#Perform tests
library(testthat)
test_dir(".")

#Implement for project
set.seed(50)
full_forward_stepwise(5000, 30)