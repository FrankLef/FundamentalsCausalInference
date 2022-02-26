#' Basic format of table created with \code{gt}
#' 
#' Basic format of table created with \code{gt}.
#' 
#' Format of table created with \code{gt} for the heading, row stripping,
#' source note, etc.
#'
#' @param tbl Object of class \code{gt_tbl}.
#' @param title String. Table title.
#' @param subtitle String. Table subtitle.
#'
#' @return Formatted \code{gt_tbl} object.
#' @export
#'
#' @examples
#' \dontrun{
#' }
gt_basic <- function(tbl, title, subtitle) {
  
  tbl %>%
    tab_header(
      title = html(paste0("<strong>", title, "</strong>")),
      subtitle = html(paste0("<strong>", subtitle, "</strong>"))) %>%
    cols_align(align = "center", columns = !matches("name")) %>%
    opt_row_striping() %>%
    tab_style(
      style = cell_text(color = "midnightblue"),
      locations = cells_title(groups = "title")) %>%
    tab_style(
      style = cell_text(color = "midnightblue"),
      locations = cells_title(groups = "subtitle")) %>%
    tab_source_note(
      source_note = "Fundamentals of Causal Inference, Babette A. Brumback, 2022"
    ) %>%
    tab_options(
      heading.title.font.weight = "bold",
      heading.subtitle.font.weight = "bold",
      heading.background.color = "gainsboro",
      column_labels.font.weight = "bold")
}
