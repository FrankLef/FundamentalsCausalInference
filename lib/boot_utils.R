#' Bootstrap and generate a dataframe of estimates with CI
#' 
#' Bootstrap and generate a dataframe of estimates with CI.
#' 
#' Generate a dataframe of estimates with the columns 
#' \code{c("est", "conf", "lci", "uci")}.
#'
#' @param data Dataframe of raw data.
#' @param statistic Function applied to data by bootstrapping.
#' @param R Number of bootstrap replicates. Default is 1000.
#' @param conf Confidence interval width. Default is 0.95.
#'
#' @return Dataframe of estimates with CI.
#' @export
#'
#' @examples
#' \dontrun{
#' }
run_boot <- function(data, statistic, R = 1000, conf = 0.95) {
  stopifnot(R >= 1, conf > 0, conf < 1)
  
  # run the bootstrapping
  boot.out <- boot::boot(data = data, statistic = statistic, R = R)
  
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
  data.frame(name = names(boot.out$t0), out)
}


#' Exponentiate the effect measures
#' 
#' Exponentiate the effect measures.
#' 
#' Exponentiate each selected measure in the dataframe and replace its name
#' accordingly.
#'
#' @param data Dataframe.
#' @param vars Character() of measure names.
#'
#' @return Dataframe of converted effects measures.
#' @export
#'
#' @examples
#' \dontrun{
#' }
exp_effects <- function(data, 
                        vars = c("rr" = "logrr","rrstar"  = "logrrstar", 
                                 "or" = "logor")) {
  pos <- match(vars, data$name)
  within(data, {
    est[pos] <- exp(est[pos])
    lci[pos] <- exp(lci[pos])
    uci[pos] <- exp(uci[pos])
    name[pos] <- names(vars)
  })
}

#' Inverse logit the effect measures
#' 
#' Inverse logit the effect measures.
#' 
#' Inverse logit each selected measure in the dataframe and replace its name
#' accordingly.
#'
#' @param data Dataframe.
#' @param vars Character() of measure names.
#'
#' @return Dataframe of converted effects measures.
#' @export
#'
#' @examples
#' \dontrun{
#' }
invlogit_effects <- function(data, vars = c("p" = "logitp")) {
  pos <- match(vars, data$name)
  within(data, {
    est[pos] <- plogis(est[pos])
    lci[pos] <- plogis(lci[pos])
    uci[pos] <- plogis(uci[pos])
    name[pos] <- names(vars)
  })
}
