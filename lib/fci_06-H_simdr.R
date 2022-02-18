#' Doubly Robust Standardization Simulation
#' 
#' Doubly robust standardization simulation.
#' 
#' This is the function used in \emph{Fundamentals of Causal Inference} by
#' B. Brumback in section 6.3 of chapter 6, p.127-128. This is the same except
#' for the addition of \code{out_choice} which is only to select what output
#' the function returns. It replaces the numerous \code(return) such as
#' \code{return(range(sumH))} found in the original script. \code{out_choice} is
#' useful to prove the algorithm to the reader. Also, \code{standdr_stats} is
#' used in the output to give more statistics.
#'
#' @param n Number of individuals (observations).
#' @param ss Number of covariates i.i.d with \code{rbinom(n, size=1, prob=probH)}
#' @param probH Probability of success on each trial.
#' @param seed Seed used for random number generation, default is \code{NULL}.
#' @param out_choice 
#'
#' @return List of values depending on \code{out_choice}.
#'
#' @examples
#' \dontrun{
#' simdr()
#' }
#' @export
simdr <- function(n = 3000, ss = 100, gamma = 20, probH = 0.05, seed = NULL,
                  out_choice = c("all", "sim", "est")) {
  out_choice <- match.arg(out_choice)
  set.seed(seed)
  
  # ss is the number of confounders
  # i.e. the number of columns of H
  H <- matrix(0, n, ss)
  # Let all components of H be independent Bernoulli variables with p=0.05
  probH <- rep(0.05, n)
  for (i in 1:ss) {
    H[, i] <- rbinom(n = 3000, size = 1, prob = probH)
  }
  # Let the treatment depend on a function of H
  sumH <- apply(H, 1, sum) * gamma / ss
  # make sure P(T=1) is between 0 and 1, i.e. positivity assumption
  probT <- 0.13 * sumH + 0.05 * rnorm(n = n, mean = 1, sd = 0.1)
  
  `T` <- rbinom(n = n, size = 1, prob = probT)
  
  # Generate the outcome depend on T and H
  probY <- 0.01 * `T` + 0.01 * sumH
  Y <- rbinom(n = n, size = 1, prob = probY)
  
  # put the simulated resuts in a list
  sim <- list("sumH" = simdr_stats(sumH),
              "probT" = simdr_stats(probT),
              "T" = simdr_stats(`T`),
              "probY" = simdr_stats(probY),
              "Y" = simdr_stats(Y))
  
  # fit the exposure model
  e <- fitted(lm(`T` ~ H))
  
  # refit the exposure model using an incorrect logistic model
  e2 <- predict(glm(`T` ~ H, family = "binomial"), type = "response")
  
  # compute the weights
  w0 <- (1 - `T`) / (1 - e)
  w1 <- `T` / e
  w02 <- (1 - `T`) / (1 - e2)
  w12 <- T / e2
  
  # fit an overspecified (saturated) outcome model
  mod.out <- lm(Y ~ `T` * H)
  
  # Estimate the expected potential outcomes using the various methods
  dat <- data.frame("Y" = Y, "T" = `T`)
  dat0 <- dat
  dat0$`T` <- 0
  dat1 <- dat
  dat1$`T` <- 1
  
  # the predicted data
  preds0 <- predict(mod.out, newdata = dat0)
  preds1 <- predict(mod.out, newdata = dat1)
  
  # calculate the estimates
  EY0out <- mean(preds0)
  EY1out <- mean(preds1)
  EY0exp <- weighted.mean(Y, w = w0)
  EY1exp <- weighted.mean(Y, w = w1)
  EY0exp2 <- weighted.mean(Y, w = w02)
  EY1exp2 <- weighted.mean(Y, w = w12)
  EY0dr <- mean(w0 * Y + preds0 * (`T` - e) / (1 - e))
  EY1dr <- mean(w1 * Y - preds1 * (`T` - e) / e)
  EYT0 <- mean(Y * (1 - `T`))
  EYT1 <- mean(Y * `T`)
  
  est <- list(
    "EY0out" = EY0out,
    "EY1out" = EY1out,
    "EY0exp" = EY0exp,
    "EY1exp" = EY1exp,
    "EY0exp2" = EY0exp2,
    "EY1exp2" = EY1exp2,
    "EY0dr" = EY0dr,
    "EY1dr" = EY1dr,
    "EYT0" = EYT0,
    "EYT1" = EYT1)
  
  if (out_choice == "all") {
    out <- append(sim, est)
  } else if(out_choice == "sim") {
    out <- sim
  } else if(out_choice == "est") {
    out <- est
  } else {
    stop(sprintf("%s is an invalid out_choice", out_choice))
  }
  out
}

#' Compute statistics from \code{simdr}. Sames as \code{standdr_est}
#'
#' @param x Vector of numeric values.
#'
#' @return list of statistics: \code{sun(x), mean(x), min(x), max(x)}.
#' 
#' @seealso standdr_stats
#'
#' @examples
#' simdr_stats(runif(20))
#' @export
simdr_stats <- function(x) {
  list("sum" = sum(x), "mean" = mean(x), "min" = min(x), "max" = max(x))
}
