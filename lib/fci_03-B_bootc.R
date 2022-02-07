#' Compute estimates of the conditional association measures
#' 
#' Compute estimates of the conditional association measures.
#' 
#' Estimate the expected conditional outcomes and the
#' conditional effect or association measures.
#' IMPORTANT: This is the function in chapter 3 called lmodboot.r
#'            It has been renamed bootc.r to avoid conflict with
#'            lmodboot.r of chapter 2
#' 
#' @param dat Dataframe of raw data.
#' @param formula Formula of linear model.
#' @param cond0 Formula of condition 0
#' @param cond1 Formula of condition 1
#' @param family Family used by \code{glm}
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Dataframe of summarized results
bootc <- function(dat, formula = Y ~ `T` + A + H,
                    cond0 = Y ~ A + H,
                    cond1 = Y ~ `T` + A + H, 
                    family = c("binomial", "poisson", "gaussian"), 
                    R = 1000, conf = 0.95) {
  # the family used by glm
  family <- match.arg(family)
  
  # the name of the intercept variable used by glm
  x0 <- "(Intercept)"
  # the name of the predictors from condition formulas cond0 and cond1
  cond0 <- all.vars(cond0[[3]])
  cond1 <- all.vars(cond1[[3]])
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    coefs <- coef(glm(formula = formula, family = family, data = dat))
    # use cond to identify conditioned variables.
    # i.e. the user can decide not to use all variables from the formula
    # or, in other words, condition on some variables
    xbeta0 <- sum(coefs[c(x0, cond0)])
    xbeta1 <- sum(coefs[c(x0, cond1)])
    p0 <- plogis(xbeta0)  # plogis is the inverse of logit
    p1 <- plogis(xbeta1)  # plogis is the inverse of logit
    rd <- p1 - p0
    logrr <- log(p1) - log(p0)
    logrrstar <- log(1 - p0) - log(1 - p1)
    logor <- log(p1 / (1 - p1)) - log(p0 / (1 - p0))
    
    # return the results
    out <- c(p0, p1, rd, logrr, logrrstar, logor)
    names(out) <- c("p0", "p1", "rd", "logrr", "logrrstar", "logor")
    out
  }
  
  # run the bootstrapping
  boot.out <- boot::boot(data = dat, statistic = estimator, R = R)
  
  # extract the estimated values and confidence intervals from the boot object
  out <- sapply(X = seq_along(boot.out$t0), FUN = function(i) {
    est <- boot.out$t0[i]
    ci <- boot::boot.ci(boot.out, conf = conf, type = "norm", index = i)$normal
    out <- c(est, ci)
    names(out) <- c("est", "conf", "lci", "uci")
    out
  })
  
  # create the dataframe to hold the results
  out <- data.frame(t(out))
  # add the first column as the names of the results
  out <- data.frame(name = names(boot.out$t0), out)
  
  # convert link to natural scales and add to the output
  sel <- c("rr" = "logrr","rrstar"  = "logrrstar", "or" = "logor")
  out <- within(out, {
    est[name %in% sel] <- exp(est[name %in% sel])
    lci[name %in% sel] <- exp(lci[name %in% sel])
    uci[name %in% sel] <- exp(uci[name %in% sel])
    name[name %in% sel] <- names(sel)
  })
  
  out
}
