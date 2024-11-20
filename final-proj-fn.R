# Final Project 611

# 1. Simulate data function
simulate_data <- function(n, p){
  if (n <= 0) {
    stop("Error: The input 'n' must be a positive number. You provided p = ", p)
  }
  if (p < 0) {
    stop("Error: The input 'n' must not be a negative number. You provided n = ", n)
  }
  if (p==0){
    Y <- rnorm(n)
    simdata <- as.data.frame(Y)
    return(simdata)
  }
  simdata <- as.data.frame(matrix(rnorm(n * p), nrow = n, ncol = p))
  colnames(simdata) <- paste("X", 1:p, sep="")
  #Create outcome column
  Y <- rnorm(n)
  #Combine into one
  simdata <- cbind(Y, simdata)
  return(simdata)
}

# 2. Forward step-wise selection function
forward_stepwise <- function(data) {
  if (class(data) != "data.frame") {
    stop("Error: The input 'data' must be a data frame.")
  }
  #Null model
  null <- lm(Y ~ 1, data = data)
  if (ncol(data) == 1) {
    return(summary(null))
  }
  #Set up variables
  vars <- colnames(data)[-1]
  n_x <- length(vars)
  remain_vars <- vars
  oldvars <- "1"
  #Set up empty lists
  modlist <- list()
  selected_models <- list()
  #For loop adding one more variable
  for(k in 1:n_x){
    rvec <- numeric(length(remain_vars))
    #For loop trying out which one more variable to add
    for(i in 1:length(remain_vars)){
      var_formula <- paste("Y ~", paste(oldvars, 
                         remain_vars[i], 
                         sep = " + ", 
                         collapse = " + "))
      modlist[[i]] <- lm(as.formula(var_formula), data=data)
      rvec[i] <- summary(modlist[[i]])$r.squared
    }
    #Identify which model with added variable is best
    bestvar_n <- which.max(rvec)
    mod <- modlist[[bestvar_n]]
    #Update variable reservoirs by moving selected variable to old
    oldvars <- paste(oldvars, remain_vars[bestvar_n], sep = "+")
    remain_vars <- remain_vars[-bestvar_n]
    #Store selected model
    selected_models[[k]] <- mod
  }
  #Calculate AIC for all selected models
  AIC_values <- sapply(selected_models, function(x) AIC(x))
  null_AIC <- AIC(null)
  AIC_values <- c(null_AIC, AIC_values)
  #Identify final preferred model
  minAIC_i <- which.min(AIC_values)
  bestmod <- summary(selected_models[[minAIC_i]])
  return(bestmod)
}

# 3. Hypothesis testing function
hypothesis_test <- function(model) {
  if (class(model) != "summary.lm"){
    stop("Error: The input 'model' must be an lm summary object.")
  }
  if (nrow(model$coefficients) <= 1) {
    stop("Error: The model provided has no \u03B2s to test.")
  }
  ps <- model$coefficients[-1, 4]
  if (any(ps < 0.05)) {
    cat("Reject the null that all \u03B2s=0.")
    sig_vars <- rownames(model$coefficients)[ps < 0.05]
    sig_vars <- sig_vars[sig_vars != "(Intercept)"]
    invisible(sig_vars)
  }
  else{
    cat("Fail to reject the null hypothesis that all \u03B2s=0.")
  }
}
