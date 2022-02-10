#' Create a table of effect measures with their CI
#' 
#' Create a table of effect measures with their CI.
#' 
#' The table will have 3 columns. The measure name, the estimates and the 
#' confidence interval.
#'
#' @param df Dataframe of results
#' @param conf Numeric, confidence interval
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table
#' @param subtitle Subtitle of the table
#'
#' @return A gt object create by the \code{gt} package
#' @export
#'
#' @examples
#' \dontrun{
#' }
gt_measures <- function(df, conf = df$conf[1], digits = 3, 
                        title = "Title", subtitle = "Subtitle") {
  
  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * conf)
  
  df <- df %>%
    select(name, est, lci, uci) %>%
    mutate(est = round(est, digits),
           lci = round(lci, digits),
           uci = round(uci, digits),
           ci = paste0("(", lci, ", ", uci, ")"),
           lci = NULL,
           uci = NULL)
  
  
  gt::gt(df) %>%
    gt_basic(title, subtitle) %>%
    cols_label(
      name = "Measure",
      est = "Estimate",
      ci = "CI") %>%
    tab_footnote(
      footnote = ci_label,
      locations = cells_column_labels(columns = "ci")
    )
  
  
  
  # df %>%
  #   select(name, est, lci, uci) %>%
  #   mutate(est = round(est, digits),
  #          lci = round(lci, digits),
  #          uci = round(uci, digits),
  #          ci = paste0("(", lci, ", ", uci, ")"),
  #          lci = NULL,
  #          uci = NULL) %>%
  #   gt() %>%
  #   tab_header(
  #     title = html(paste0("<strong>", title, "</strong>")),
  #     subtitle = html(paste0("<strong>", subtitle, "</strong>"))
  #   ) %>%
  #   cols_align(align = "center", columns = !matches("name")) %>% 
  #   cols_label(
  #     name = "Measure",
  #     est = "Estimate",
  #     ci = "CI") %>%
  #   tab_style(
  #     style = cell_text(color = "midnightblue"),
  #     locations = cells_title(groups = "title")
  #   ) %>%
  #   tab_style(
  #     style = cell_text(color = "midnightblue"),
  #     locations = cells_title(groups = "subtitle")
  #   ) %>%
  #   opt_row_striping() %>%
  #   tab_footnote(
  #     footnote = ci_label,
  #     locations = cells_column_labels(columns = ci)
  #     ) %>%
  #   tab_source_note(
  #     source_note = "Fundamentals of Causal Inference, Babette A. Brumback, 2022"
  #     ) %>%
  #   tab_options(
  #     heading.background.color = "gainsboro",
  #     column_labels.font.weight = "bold")
}
