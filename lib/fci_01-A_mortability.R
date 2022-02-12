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
#' in section 6.2 of chapter 6.
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
  # compute proportion who died
  out$p <- out$n / sum(out$n)
  stopifnot(sum(out$p) == 1)
  
  out
}
