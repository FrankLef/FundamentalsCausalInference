#' Heatmap of a matrix from \code{betasim_effect_measures()}
#' 
#' Heatmap of a matrix from \code{betasim_effect_measures()}.
#' 
#' Heatmap of a matrix from \code{betasim_effect_measures()} with the
#' possibilitiy of changing the color.
#'
#' @param sim Monte Carlo sim data from \code{betasim_effect_measures()}
#' @param var Variable from the Monte Carlo sim to plot.
#' @param colr Vector of colors for the heatmap.
#' @param title Title of the plot
#'
#' @return ggplot object
#' @export
#' 
#' @seealso betasim_effect_measures
#'
#' @examples
#' \dontrun{
#' }
ggp_betasim <- function(sim, var = "RD_RR_RRstar_OR",
                         colr = list("low" = "cadetblue1", "high" = "cadetblue4"),
                         title = "Monte Carlo simulation with beta distribution") {
  mat <- sim[[var]]
  df <- mat %>%
    as.data.frame() %>%
    mutate(shape2 = rownames(.)) %>%
    pivot_longer(cols = starts_with("s1"), names_to = "shape1", values_to = "value")
  
  ggplot(df, aes(x = shape1, y = shape2, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "floralwhite",
              fontface = "bold", size = 5) +
    scale_fill_gradient(low = colr$low, high = colr$high) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(title = title, subtitle = sprintf("Event =  %s", var))
}
