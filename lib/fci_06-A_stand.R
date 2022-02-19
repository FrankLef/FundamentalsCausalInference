#' Compute standardized estimates
#' 
#' Compute standardized estimates.
#' 
#' The standardized estimates are computed using the outcome model.
#' IMPORTANT: The formula must be in the format \code{Y ~ T + ...}.
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...}
#' @param att if \code{FALSE} calculate the standardized (unconfounded)
#' causal effect. If \code{TRUE} calculate the average effect of treatment
#' on the treated.
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Dataframe of estimates
stand <- function(dat, formula = Y ~ `T` + H + `T`*H, att = FALSE, 
                      R = 1000, conf = 0.95) {
  
  # extract the variables names from the formula
  fvars <- formula2vars(formula)
  
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # marginal expected value of H
    if (!att) {
      # EH <- mean(dat[, h])
      EH <- mean(dat[, fvars$h])
    } else {
      # condition on treatment when ATT is requested
      EH <- mean(dat[dat[, fvars$t] == 1, fvars$h])
    }
    # fit the outcome model and extract the coefficients
    coefs <- coef(lm(formula = formula , data = dat))
    # compute the marginal expected potential outcomes
    EY0 <- coefs[fvars$x0] + coefs[fvars$h] * EH
    EY1 <- coefs[fvars$x0] + coefs[fvars$t] + sum(coefs[fvars$ht]) * EH

    # estimate the effect measures
    calc_effect_measures(val0 = EY0, val1 = EY1)
  }
  
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  exp_effects(data = out)
}
