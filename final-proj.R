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

# 2. Forward Step-wise
null <- lm(Y ~ 1, data = simdata)
vars <- colnames(simdata)[-1]
n_x <- ncol(simdata) - 1
remain_vars <- vars
oldvars <- "1"
modlist <- list()
for(k in 1:n_x){
  rvec <- numeric(length(remain_vars))
  for(i in 1:length(remain_vars)){
    modlist[[i]] <- lm(Y ~ oldvars + remain_vars[i])
    rvec[i] <- modlist[[i]]$r.squared
  }
  bestvar_n <- which.max(rvec)
  remain_vars <- remain_vars[-bestvar_n]
  mod <- modlist[[bestvar_n]]
  selected_models[[k]] <- mod
  oldvars <- paste(oldvars, remain_vars[bestvar_n], sep = "+")
}
selected_BIC <- sapply(selected_models, BIC())
maxBIC_i <- which.max(selected_BIC)
bestmod <- selected_models[maxBIC_i]
bestmod





