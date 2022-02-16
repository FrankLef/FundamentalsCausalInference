#' Compute the doubly robust standardized estimates
#' 
#' Compute the doubly robust standardized estimates.
#' 
#' Compute the doubly robust standardized estimates using the code from section
#' 6.3
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @R Number of bootstrap replicates.
#' @conf Confidence interval.
#'
#' @return Dataframe of estimates
badstanddr <- function(dat, formula = Y ~ `T` + H, R = 10, conf = 0.95) {
  # the name of the intercept variable used by glm
  x0 <- "(Intercept)"
  # the name of the response variable
  y <- all.vars(formula[[2]])
  # the name of the treatment variable
  t <- all.vars(formula[[3]])[1]
  # the name of the confounding variables
  nvars <- length(all.vars(formula[[3]]))
  h <- all.vars(formula[[3]])[2:nvars]
  
  # exposure model formula
  eformula <- formula(paste(t, paste(h, collapse = "+"), sep = "~"))
  # weighted linear model formula
  lformula <- formula(paste(y, t, sep = "~"))
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    
    # estimate the parametric exposure model
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero
    
    # fit a nonparametric outcome model that we do not believe
    # i.e. a bad outcome model
    lmod <- glm(formula = lformula, family = "binomial", data = dat)
    
    # predict potential outcome for each participant
    dat0 <- dat
    dat0[, t] <- 0
    EYhat0 <- predict(lmod, newdata = dat0, type = "response")
    dat1 <- dat
    dat1[, t] <- 1
    EYhat1 <- predict(lmod, newdata = dat1, type = "response")
    
    # Use the DR estimating equation to estimate the expected
    # potential outcome
    datY <- dat[, y]
    datT <- dat[, t]
    EY0 <- mean(datY * (1 - datT) / (1 - e) + EYhat0 * (e - datT) / (1 - e))
    EY1 <- mean(datY * (datT / e) - EYhat1 * (datT - e) / e)
    
    # estimate the effect measures
    calc_effect_measures(EY0, EY1)
  }
  
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  exp_effects(data = out)
}
