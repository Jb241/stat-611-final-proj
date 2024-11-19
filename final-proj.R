# Final Project 611

# 1. Simulate data
set.seed(50)
n <- 5000
#Create predictor columns
simdata <- as.data.frame(matrix(rnorm(n * 15), nrow = n, ncol = 15))
colnames(simdata) <- c("X1", "X2", "X3", "X4", "X5", 
                       "X6", "X7", "X8", "X9", "X10", 
                       "X11", "X12", "X13", "X14", "X15")
#Create outcome column
Y <- rnorm(n)
simdata <- cbind(Y, simdata)
#Check
head(simdata)

# 2. Forward Step-wise
null <- lm(Y ~ 1, data = simdata)
vars <- colnames(simdata)[-1]
n_x <- ncol(simdata) - 1
remain_vars <- vars
oldvars <- "1"
modlist <- list()
selected_models <- list()
for(k in 1:n_x){
  rvec <- numeric(length(remain_vars))
  for(i in 1:length(remain_vars)){
      var_formula <- paste("Y ~", paste(oldvars, 
                         remain_vars[i], 
                         sep = " + ", 
                         collapse = " + "))
    modlist[[i]] <- lm(as.formula(var_formula), data=simdata)
    rvec[i] <- summary(modlist[[i]])$r.squared
  }
  bestvar_n <- which.max(rvec)
  mod <- modlist[[bestvar_n]]
  oldvars <- paste(oldvars, remain_vars[bestvar_n], sep = "+")
  remain_vars <- remain_vars[-bestvar_n]
  selected_models[[k]] <- mod
}
selected_AIC <- sapply(selected_models, function(x) AIC(x))
minAIC_i <- which.min(selected_AIC)
bestmod <- summary(selected_models[[minAIC_i]])
bestmod$coefficients[-1, 4]




