#' Compute standardized averages using exposure modeling
#' 
#' Compute standardized averages using exposure modeling.
#' 
#' Compute standardized averages using exposure modeling as described in
#' section 6.1.2
#'
#' @param data Dataframe
#' @param formula Formula, must be in the format \code{Y ~ `T` + H}
#' @param weights String. Name of the columns with the weights that will
#' be used to create probabilities summing up to 1.
#'
#' @return List with 3 elements: EY1, EY0, EY0T1. See section 6.1.2
#' for more details.
#' @export
#'
#' @examples
#' \dontrun{
#' }
calc_exposure <- function(data, formula = Y ~ `T` + H, weights = "n") {
  # the name of the response variable
  y <- all.vars(formula[[2]])
  # the name of the treatment variable
  t <- all.vars(formula[[3]])[1]
  # the name of the histoy variable
  h <- all.vars(formula[[3]])[2]
  
  # compute e(H=0)
  dat0 <- data[data[, h] == 0, ]
  eH0 <- sum(dat0[dat0[, t] == 1, weights]) / sum(dat0[, weights])
  # compute e(H=1)
  dat1 <- data[data[, h] == 1, ]
  eH1 <- sum(dat1[dat1[, t] == 1, weights]) / sum(dat1[, weights])
  # compute e(H) for all participants
  eH <- eH0 * (1 - data[, h]) + eH1 * data[, h]
  # compute the summands of the estimating equations
  s1 <- data[, t] * data[, y] / eH
  s0 <- (1 - data[, t]) * data[, y] / (1 - eH)
  
  # estimate the expected values of the potential outcomes
  probs <- data[, weights] / sum(data[, weights])  # the probabilites
  stopifnot(sum(probs) == 1)
  EY1 <- sum(s1 * probs)
  EY0 <- sum(s0 * probs)
  
  # ATT calculations
  # estimate P(T = 1)
  e0 <- sum(data[, t] * probs)
  # compute the summands of the estimating equation
  s <- data[, y] * (1 - data[, t]) * eH / (e0 * (1 - eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * probs)
  
  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1)
}
