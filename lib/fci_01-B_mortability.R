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
    "population" = c(282305227, 48262955, 1297258493, 133015479),
    "Y" = c(0.002679, 0.044603, 0.002254, 0.056516)
  )
  stopifnot(dplyr::near(sum(out$Y), sum(out$deaths / out$population),
                        tol = 1e-6))
  out
}
