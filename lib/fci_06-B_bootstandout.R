#' Compute standardized estimates with parametric outome model
#' 
#' Compute standardized estimates with parametric outome model.
#' 
#' The standardized estimates are computed using the outcome model.
#' IMPORTANT: The formula must be in the format \code{Y ~ T + ...} where T
#' will be used to create the data to predict the expected potential
#' outcome for each participant
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ T + ...} see details above.
#' @R Number of bootstrap replicates.
#' @conf Confidence interval.
#'
#' @return Dataframe of estimates
bootstandout <- function(dat, formula = Y ~ `T` + H, R = 1000, conf = 0.95) {
  # name of the treatment variable
  t <- all.vars(formula[[3]])[1]
  
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    
    lmod.out <- glm(formula = formula, family = "binomial", data = dat)
    
    # dataset with everyone untreated
    dat0 <- dat
    dat0[, t] <- 0
    
    # dataset with everyone treated
    dat1 <- dat
    dat1[, t] <- 1
    
    # compute the expected potential outcome for
    # each participant if untreated
    EYhat0 <- predict(lmod.out, newdata = dat0, type = "response")
    # compute the expected potential outcome for
    # each participant if treated
    EYhat1 <- predict(lmod.out, newdata = dat1, type = "response")
    
    # estimate the average potential outcomes
    EY0 <- mean(EYhat0)
    EY1 <- mean(EYhat1)
    
    # estimate the effect measures
    # estimate the effect measures
    out <- calc_effect_measures(val0 = EY0, val1 = EY1, log = TRUE)
    
    c("EY0" = unname(EY0), "EY1" = unname(EY1), out)
  }
  
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  out <- exp_effects(data = out)

  out
}
