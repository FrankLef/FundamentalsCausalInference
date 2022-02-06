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
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = md(paste0("**", subtitle, "**"))) %>%
    fmt_number(columns = "prob", decimals = digits) %>%
    cols_align(align = "center", columns = everything()) %>% 
    opt_row_striping() %>%
    tab_source_note(
      source_note = "Fundamentals of Causal Inference, Babette A. Brumback, 2022"
      ) %>%
    tab_options(
      heading.background.color = "gainsboro",
      column_labels.font.weight = "bold")
}
