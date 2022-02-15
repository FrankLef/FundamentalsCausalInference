#' Compute standardized estimates with parametric exposure model
#' 
#' Compute standardized estimates with parametric exposure model.
#' 
#' The standardized estimates are computed using the exposure model and the
#' \code{geeglm} from the \code{gee} package.
#' This method requires 2 different formulas which are created from the
#' arguments \code{formula}. The 2 formulas created are for the exposure model
#' and another one for the weighted linear model.
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula must be in the form \code{Y ~ `T` + H}
#' @R Number of bootstrap replicates.
#' @conf Confidence interval.
#' 
#' @seealso standexp
#'
#' @return Dataframe of estimates
standexpgee <- function(dat, formula = Y ~ `T` + H, R = 5, conf = 0.95) {
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
  v <- paste(h, collapse = "+")
  eformula <- formula(paste(t, v, sep = "~"))
  # weighted linear model formula
  lformula <- formula(paste(y, t, sep = "~"))
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    dat$id <- seq_len(nrow(dat))  # id column used by geeglm

    # estimate the parametric exposure model
    e <- fitted(glm(formula = eformula, family = "binomial", data = dat))
    stopifnot(all(!dplyr::near(e, 0)))  # e must not equal zero

    # compute the weights
    datT <- dat[, t]
    dat$W <- (1 / e) * datT + (1 / (1 - e)) * (1 - datT)

    # fit the weighted linear model
    coefs <- coef(geepack::geeglm(formula = lformula, data = dat,
                                  id = id, , weights = W))

    # estimate the expected potential outcome
    EY0 <- coefs[x0]
    EY1 <- sum(coefs)

    # estimate the effect measures
    calc_effect_measures(EY0, EY1)
  }

  # run the bootstrapping
  out <- run_boot(data = dat, statistic = estimator, R = R, conf = conf)

  # exponentiate the log values
  exp_effects(data = out)
}
