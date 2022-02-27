#' Compute standardized estimates with parametric outcome model
#' 
#' Compute standardized estimates with parametric outcome model.
#' 
#' The standardized estimates are computed using the outcome model.
#' IMPORTANT: The formula must be in the format \code{Y ~ T + ...} where T
#' will be used to create the data to predict the expected potential
#' outcome for each participant
#'
#' @param data Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @param family Character. Name of the distribution. Default is "binomial".
#' @param R Number of bootstrap replicates.
#' @param conf Confidence interval.
#'
#' @return Estimate using outcome-model standardization
#' @export
standout <- function(data, formula = Y ~ `T` + H, 
                     family = c("binomial", "poisson", "gaussian"), 
                     R = 1000, conf = 0.95) {
  family <- match.arg(family)
  
  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    
    lmod.out <- glm(formula = formula, family = family, data = dat)
    
    # dataset with everyone untreated
    dat0 <- dat
    dat0[, fvars$t] <- 0
    
    # dataset with everyone treated
    dat1 <- dat
    dat1[, fvars$t] <- 1
    
    # compute the expected potential outcome for
    # each participant if untreated
    # NOTE: fitted() is the same as using predict(..., type = "response")
    #       BUT fitted only use the ORIGINAL data, there is no newdata.
    EYhat0 <- predict(lmod.out, newdata = dat0, type = "response")
    # compute the expected potential outcome for
    # each participant if treated
    # NOTE: fitted() is the same as using predict(..., type = "response")
    #       BUT fitted only use the ORIGINAL data, there is no newdata.
    EYhat1 <- predict(lmod.out, newdata = dat1, type = "response")
    
    # estimate the average potential outcomes
    EY0 <- mean(EYhat0)
    EY1 <- mean(EYhat1)
    
    # estimate the effect measures
    calc_effect_measures(val0 = EY0, val1 = EY1)
  }
  
  out <- run_boot(data = data, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  exp_effects(data = out)
}
