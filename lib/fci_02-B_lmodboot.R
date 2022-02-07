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
    sum(coefs)
  }
  boot.out <- boot::boot(data = dat, statistic = estimator, R = R)
  # the estimate on the logit scale
  est.logit <- boot.out$t0
  # the confidence interval on the logit scale using normal interval
  ci.logit <- boot::boot.ci(boot.out, conf = conf, type = "norm")$normal
  # the estimate and confidence interval on the natural scale
  # NOTE: the plogis() function gives the inverse logit
  c("est" = plogis(est.logit), "conf" = ci.logit[1], 
    "lci" = plogis(ci.logit[2]), "uci" =  plogis(ci.logit[3]))
}
