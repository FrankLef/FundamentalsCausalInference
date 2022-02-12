#' Create dataset Mortability Rate by Country
#' 
#' Create dataset Mortability Rate by Country.
#' 
#' Create dataset Mortability Rate by Country as shown in section 1.2.1.
#'
#' @return Dataframe of mortability rates
#' @export
#'
#' @examples
#' data_mortability()
data_mortability <- function() {
  out <- data.frame(
    "T" = c(TRUE, TRUE, FALSE, FALSE),
    "H" = c(FALSE, TRUE, FALSE, TRUE),
    "deaths" = c(756340, 2152660, 2923480, 7517520),
    "population" = c(282305227, 48262955, 1297258493, 133015479))
  out$Y <- out$deaths / out$population
  # verify with book
  check <- c(0.002679, 0.0446, 0.002254, 0.05652)
  stopifnot(sum(abs(out$Y - check)) < 0.0001)
  out
}

#' Create Mortability by Country dataset for exposure modeling
#' 
#' Create Mortability by Country dataset for exposure modeling.
#' 
#' Create Mortability by Country dataset for exposure modeling as described
#' in section 6.2 of chapter 6. The function also perform the ATT calculations
#' used in section 6.2.1.
#'
#' @return Dataframe of mortability rates and exposure model
#' @export
#'
#' @examples
#' data_mortability_exp()
data_mortability_exp <- function() {
  out <- data.frame(
    "H" = c(0, 0, 0, 0, 1, 1, 1, 1),
    "T" = c(0, 0, 1, 1, 0, 0, 1, 1),
    "Y" = c(0, 1, 0, 1, 0, 1, 0, 1),
    "n" = c(1297258493 - 2923480,
            2923480,
            282305227 - 756340,
            756340,
            133015479 - 7517520,
            7517520,
            48262955 - 2152660,
            2152660))
  out$p <- out$n / sum(out$n)  # compute proportion who died
  # compute e(H=0)
  dat0 <- out[out$H == 0, ]
  eH0 <- sum(dat0$n[dat0$`T` == 1]) / sum(dat0)
  # compute e(H=1)
  dat1 <- out[out$H == 1, ]
  eH1 <- sum(dat1$n[dat1$`T` == 1]) / sum(dat1)
  # compute e(H) for all participants
  out$eH <- eH0 * (1 - out$H) + eH1 * out$H
  # compute the summands of the estimating equations
  out$s1 <- out$`T` * out$Y / out$eH
  out$s0 <- (1 - out$`T`) * out$Y / (1 - out$eH)
  
  # estimate the expected values of the potential outcomes
  EY1 <- sum(out$s1 * out$p)
  EY0 <- sum(out$s0 * out$p)
  
  # include the ATT calculations
  # estimate P(T = 1)
  e0 <- sum(out$`T` * out$p)
  # compute the summands of the estimating equation
  s <- out$Y * (1 - out$`T`) * out$eH / (e0 * (1 - out$eH))
  # estimate E(Y0|T=1)
  EY0T1 <- sum(s * out$p)
  
  # test results
  stopifnot(dplyr::near(EY0, 0.0078399, tol = 1e-7),
            dplyr::near(EY1, 0.0069952, tol = 1e-7),
            dplyr::near(EY0T1, 0.010176, tol = 1e-6))
  
  # final output
  list("EY1" = EY1, "EY0" = EY0, "EY0T1" = EY0T1, "data" = out)
}
