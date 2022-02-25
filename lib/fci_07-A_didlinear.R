#' Compute the DiD Estimator with a Linear Model
#' 
#' Compute the DiD Estimator with a linear model.
#' 
#' the DiD Estimator using the scripts from section 7.2, p. 141-142.
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ A} where \code{A} 
#' is the exposure.
#' @param varsY Names of the outcome variables.
#' @R Number of bootstrap replicates. Default is 1000.
#' @conf Confidence interval. Default is 0.95.
#'
#' @return Estimates using the difference in differences.
didlinear <- function(data, formula = Y ~ A, varsY = c("VL0", "VL1"), 
                     R = 1000, conf = 0.95) {
  
  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  
  estimator <- function(data, ids, varsY) {
    nmvar <- "var"  # name of variable used to hold names
    outvar <- "outcome"  # name of variable used for outcome
    timevar <- "time"  # name of variable used for time
    
    dat <- data[ids, ]
    # make long format
    dat <- pivot_longer(dat, cols = all_of(varsY), names_to = nmvar, values_to = outvar)
    # create time variable
    dat[, timevar] <- ifelse(grepl(pattern = "0", x = dat$var), 0, 1)
    
    # fit the did model
    dformula <- paste0(outvar, "~", fvars$t, "*", timevar)
    mod.out <- glm(formula = dformula, family = "gaussian", data = dat)
    coefs <- coef(mod.out)
    
    # extract the risk difference from the interaction coefficient
    rd <- coefs[length(coefs)]
    
    # estimate E(Y(1)|A=1)
    EY1 <- mean(dat[dat[, timevar] == 1 & dat[, fvars$t] == 1, outvar, drop = TRUE])
    
    # estimate E(Y(0)|A=1)
    EY0 <- EY1 - rd
    
    # estimate the effect measures
    calc_effect_measures(val0 = EY0, val1 = EY1)
  }
  
  out <- run_boot(data = data, statistic = estimator, R = R, conf = conf,
                  varsY = varsY)

  # exponentiate the log values
  exp_effects(data = out)
}
