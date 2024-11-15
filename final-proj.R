# Final Project 611

# 1. Simulate data
set.seed(5)
n <- 5000
#Create predictor columns
simdata <- as.data.frame(matrix(rnorm(n * 15), nrow = n, ncol = 15))
colnames(simdata) <- c("X1", "X2", "X3", "X4", "X5", 
                       "X6", "X7", "X8", "X9", "X10", 
                       "X11", "X12", "X13", "X14", "X15")
#Create outcome column
Y <- rnorm(n)
simdata <- cbind(simdata, Y)
#Check
head(simdata)
cor_matrix <- cor(simdata)
cor_matrix
