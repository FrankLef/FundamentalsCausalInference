#' Create table of probabilities with \code{gt} package
#' 
#' Create table of probabilities with \code{gt} package.
#' 
#' Create table of probabilities with \code{gt} package. The last column
#' repesents the probabilitis.
#'
#' @param df Datafframe
#' @param digits Integer, number of digits to the right of decimal
#' @param title Title of the table
#' @param subtitle Subtitle of the table
#'
#' @return A gt object create by the \code{gt} package
#' @export
#'
#' @examples
#' \dontrun{
#' }
gt_probs <- function(df, digits = 3, title, subtitle) {
  df %>%
    gt() %>%
    gt_basic(title, subtitle) %>%
    fmt_number(columns = "prob", decimals = digits)
}
