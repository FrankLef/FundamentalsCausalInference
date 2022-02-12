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
