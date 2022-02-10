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
