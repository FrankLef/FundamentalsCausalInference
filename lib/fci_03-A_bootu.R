#' Compute estimates of the unconditional association measures
#' 
#' Compute estimates of the unconditional association measures.
#' 
#' Compute estimates of the unconditional association measures, 
#' hence the u in \code{bootu()}, and their confidence intervals.
#' 1- We use Gaussian, Poisson and Binomial glm to solve the 
#'    estimating equations of the measures, not their distributions
#' 2- The association measures' distributions (ci) are estimated by
#'    bootstrapping
#' Assumptions: We assume that (3.2) holds but not (3.1)
#' See p. 45 and 46 for more details.
#' @param dat Dataframe of raw data.
#' @param formula Formula of linear model.
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Dataframe of summarized results
bootu <- function(dat, formula = Y ~ `T`, R = 1000, conf = 0.95) {
  
  # get the name of dependent variable from the formula
  y <- all.vars(formula[[2]])
  # get the name of independent variables from the formula
  x <- all.vars(formula[[3]])
  # the name of the intercept variable used by glm
  x0 <- "(Intercept)"
  
  # estimate the conditional probabilities 
  # and the four association measures
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # estimate the conditional probabilities
    coefs <- coef(glm(formula = formula, family = gaussian, data = dat))
    p0 <- coefs[x0]
    p1 <- sum(coefs)
    # estimate the risk difference
    rd <- p1 - p0
    
    # use loglinear model to estimate the log relative risk
    coefs <- coef(glm(formula = formula, family = poisson, data = dat))
    logrr <- coefs[x]
    
    # prepare data to estimate the log other relative risk
    ystar <- 1 - dat[, y]
    xstar <- 1 - dat[, x]
    # use loglinear model to estimate the log other relative risk
    coefs <- coef(glm(ystar ~ xstar, family = poisson))
    logrrstar <- coefs[2]
    
    # use logistic model to estimate the log of other risk
    coefs <- coef(glm(formula = formula, family = binomial, data = dat))
    logor <- coefs[x]
    
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
