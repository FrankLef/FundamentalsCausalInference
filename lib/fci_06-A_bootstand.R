#' Compute standardized estimates
#' 
#' Compute standardized estimates.
#' 
#' The standardized estimates are computed using the outcome model.
#' IMPORTANT: The formula must be in the format \code{Y ~ T + H + T*H}.
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + H + T*H}
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Dataframe of estimates
bootstand <- function(dat, formula = Y ~ `T` + H + `T`*H, R = 1000, conf = 0.95) {
  
  # the name of the intercept variable used by lm
  x0 <- "(Intercept)"
  # the name of the response variable
  y <- all.vars(formula[[2]])
  # name of the treatment variable
  t <- all.vars(formula[[3]])[1]
  # name of the condition variable
  h <- all.vars(formula[[3]])[2]
  # name of interaction
  th <- paste(t, h, sep = ":")
  
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # marginal expected value of H
    EH <- mean(dat[, h])
    # fit the outcome model and extract the coefficients
    coefs <- coef(lm(formula = formula , data = dat))
    # compute the marginal expected potential outcomes
    EY0 <- coefs[x0] + coefs[h] * EH
    EY1 <- coefs[x0] + coefs[t] + coefs[h] * EH  + coefs[th] * EH
    # return the effect measures
    rd <- EY1 - EY0
    logrr <- log(EY1 / EY0)
    
    out <- c(EY0, EY1, rd, logrr)
    names(out) <- c("EY0", "EY1", "rd", "logrr")
    out
  }
  
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  out <- exp_effects(data = out, vars = c("rr" = "logrr"))

  out
}
