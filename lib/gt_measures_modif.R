#' Create a table of effect-measure modifications with their CI
#' 
#' Create a table of effect-measure modifications with their CI.
#' 
#' The table will have 3 columns foreach of the 2 strata and the
#' difference column. The measure name, the estimates and the 
#' confidence interval.
#'
#' @param df Dataframe of results
#' @param conf Numeric, confidence interval
#' @param digits Integer, number of digits to the right of the decimal.
#' @param title The title of the table
#' @param subtitle Subtitle of the table
#'
#' @return A gt object create by the `gt` package
#' @export
#'
#' @examples
#' \dontrun{
#' }
gt_measures_modif <- function(df, conf = df$conf[1], digits = 3, 
                       title = "Title", subtitle = "Effect-measure Modification") {
  
  # confidence interval label used in footnote
  ci_label <- sprintf("%.0f%% confidence interval", 100 * conf)
  
  # prepare the dataframe to create the table
  df <- df %>%
    filter(!grepl(pattern = "log.+", name)) %>%
    select(-conf) %>%
    mutate(est = round(est, digits = digits),
           lci = round(lci, digits = digits),
           uci = round(uci, digits = digits)) %>%
    tidyr::pivot_longer(cols = c(est, lci, uci), names_to = "point", values_to = "value") %>%
    tidyr::separate(col = "name", into = c("name", "stratum"), sep = "[.]") %>%
    tidyr::unite(col = "estimate", stratum, point, sep = "_") %>%
    pivot_wider(names_from = "estimate", values_from = "value") %>%
    mutate(M0_ci = paste0("(", M0_lci, ", ", M0_uci, ")"),
           M1_ci = paste0("(", M1_lci, ", ", M1_uci, ")"),
           diff_ci = paste0("(", diff_lci, ", ", diff_uci, ")"),
           M0_lci = NULL, M0_uci = NULL,
           M1_lci = NULL, M1_uci = NULL,
           diff_lci = NULL, diff_uci = NULL)
  
  # create the table with gt
  gt::gt(df, rowname_col = "name") %>%
    tab_header(title = md(paste0("**", title, "**")), 
               subtitle = md(paste0("**", subtitle, "**"))) %>%
    tab_spanner_delim(delim = "_", columns = everything(), split = "last") %>%
    cols_align(align = "center", columns = everything()) %>% 
    cols_label(
      name = "Measure",
      M0_est = "Estimate", M0_ci = "CI",
      M1_est = "Estimate", M1_ci = "CI",
      diff_est = "Estimate", diff_ci = "CI") %>%
    opt_row_striping() %>%
    tab_style(
      style = cell_borders(sides = "left", color = "grey60", 
                           weight = px(1.5), style = "solid"),
      locations = cells_body( columns = c("M1_est", "diff_est"))
    ) %>%
    tab_style(
      style = cell_borders(sides = "left", color = "grey60", 
                           weight = px(1.5), style = "solid"),
      locations = cells_column_labels(columns = c("M1_est", "diff_est"))
    ) %>%
    tab_footnote(
      footnote = ci_label,
      locations = cells_column_labels(columns = vars(M0_ci, M1_ci, diff_ci))
    ) %>%
    tab_source_note(
      source_note = "Fundamentals of Causal Inference, Babette A. Brumback, 2022"
    ) %>%
    tab_options(
      heading.title.font.weight = "bold", 
      heading.subtitle.font.weight = "bold",
      heading.background.color = "gainsboro",
      column_labels.font.weight = "bold")
  
  # df
}
