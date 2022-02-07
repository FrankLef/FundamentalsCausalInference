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
  
  # estimate bootstrap confidence intervals and point estimate
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
  
  # exponentiate the log values
  sel <- c("rr" = "logrr")
  out <- within(out, {
    est[name %in% sel] <- exp(est[name %in% sel])
    lci[name %in% sel] <- exp(lci[name %in% sel])
    uci[name %in% sel] <- exp(uci[name %in% sel])
    name[name %in% sel] <- names(sel)
  })

  out
}
