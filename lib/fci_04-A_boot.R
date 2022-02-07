#' Compute estimates of the association measures for 2 strata
#' 
#' Compute estimates of the association measures for 2 strata.
#' 
#' IMPORTANT:
#'  The formula must always be in form Y ~ `T` + M, that is with only
#'  2 predictors: T as the treatment variable and M as the modifier
#'  variable
#'
#' @param dat Dataframe of raw data.
#' @param formula Formula in format \code{Y ~ `T` + M}
#' @R Number of bootstrap replicates.
#' @conf Confidence interval
#'
#' @return Dataframe of summarized results
boot <- function(dat, formula = Y ~ `T` + M, R = 1000, conf = 0.95) {
  
  # the name of the response variable
  y <- all.vars(formula[[2]])
  # the name of the treatment variable
  t <- all.vars(formula[[3]])[1]
  # the name of the modifier variable
  m <- all.vars(formula[[3]])[2]
  
  estimator <- function(data, ids) {
    dat <- data[ids, ]
    # estimate the expected potential outcomes
    EYT0.M0 <- mean(dat[dat[, t] == 0 & dat[, m] == 0, y])
    EYT0.M1 <- mean(dat[dat[, t] == 0 & dat[, m] == 1, y])
    EYT1.M0 <- mean(dat[dat[, t] == 1 & dat[, m] == 0, y])
    EYT1.M1 <- mean(dat[dat[, t] == 1 & dat[, m] == 1, y])
    # estimate the effect measures
    RD.M0 <- EYT1.M0 - EYT0.M0
    RD.M1 <- EYT1.M1 - EYT0.M1
    logRR.M0 <- log(EYT1.M0 / EYT0.M0)
    logRR.M1 <- log(EYT1.M1 / EYT0.M1)
    logRRstar.M0 <- log((1 - EYT0.M0) / (1 - EYT1.M0))
    logRRstar.M1 <- log((1 - EYT0.M1) / (1 - EYT1.M1))
    logOR.M0 <- logRR.M0 + logRRstar.M0
    logOR.M1 <- logRR.M1 + logRRstar.M1
    # the effect measure difference
    EYT0.diff <- EYT0.M1 - EYT0.M0
    EYT1.diff <- EYT1.M1 - EYT1.M0
    RD.diff <- RD.M1 - RD.M0
    logRR.diff <- logRR.M1 - logRR.M0
    logRRstar.diff <- logRRstar.M1 - logRRstar.M0
    logOR.diff <- logOR.M1 - logOR.M0
    
    out <- c(EYT0.M0, EYT0.M1, EYT1.M0, EYT1.M1, RD.M0, RD.M1,
             logRR.M0, logRR.M1, logRRstar.M0, logRRstar.M1,
             logOR.M0, logOR.M1, EYT0.diff, EYT1.diff, 
             RD.diff, logRR.diff, logRRstar.diff, logOR.diff)
    names(out) <- c("EYT0.M0", "EYT0.M1", "EYT1.M0", "EYT1.M1", "RD.M0", "RD.M1",
                    "logRR.M0", "logRR.M1", "logRRstar.M0", "logRRstar.M1",
                    "logOR.M0", "logOR.M1", "EYT0.diff", "EYT1.diff", 
                    "RD.diff", "logRR.diff", "logRRstar.diff", "logOR.diff")
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
  
  # convert link to natural scales and add to output
  sel <- c("RR.M0" = "logRR.M0", "RR.M1" = "logRR.M1", "RR.diff"  = "logRR.diff",
           "RRstar.M0"  = "logRRstar.M0", "RRstar.M1"  = "logRRstar.M1", "RRstar.diff" = "logRRstar.diff",
           "OR.M0" = "logOR.M0", "OR.M1" = "logOR.M1", "OR.diff" = "logOR.diff")
  df <- out[match(sel, out$name), ]
  df <- as.data.frame(sapply(X = df[, c("est", "lci", "uci")], FUN = exp))
  df <- cbind(name = names(sel), conf = conf, df)
  
  out <- rbind(out, df)
  out
}
