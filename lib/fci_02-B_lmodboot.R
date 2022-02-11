#' Bootstrap an unconditional sampling distribution
#' 
#' Bootstrap an unconditional sampling distribution.
#' 
#  Estimate the unconditional sampling distribution by bootstrapping.
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond Formula of condition.
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Vector of summarized results
lmodboot <- function(dat, formula = Y ~ `T` + A + H,
                       cond = Y ~ `T` + A + H, 
                       R = 1000, conf = 0.95) {
  # the name of the intercept variable used by glm
  x0 <- "(Intercept)"
  # the name of the predictor variables in the condition formula
  cond <- all.vars(cond[[3]])
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = binomial, data = dat))
    # use cond to identify conditioned variables.
    # i.e. the user can decide not to use all variables from the formula
    # or, in other words, condition on some variables
    coefs <- coefs[c(x0, cond)]
    p <- sum(coefs)
    out <- c(p)
    names(out) <- c("logitP")
    out
  }
  
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)
  
  out <- invlogit_effects(out)

  out
}
