#' Create a plot of effect-measures modifications
#' 
#' Create a plot of effect-measures modifications
#' 
#' Create a plot of effect-measures modifications. The measures are shoen
#' as solid lines. The actual effects are shown in dashed lines
#'
#' @param df Dataframe to plot
#' @param title Title of the plot
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' }
ggp_measures_modif <- function(df, title = "Change in effect measures",
                               subtitle = "Effect-Measure Modification and Statistical Interaction") {
  
  df <- df %>%
    filter(group != "diff")
  
  ggplot(df, 
         aes(x = group, y = est, color = estimator, linetype = estimator, group = estimator)) +
    geom_line(size = 1) +
    geom_point(mapping = aes(fill = estimator), size = 3, shape = 21) +
    geom_text(mapping = aes(label = round(est, 1)),
              nudge_x = 0.05, nudge_y = 0.05) +
    scale_linetype_manual(values = c("EYT0" = "dashed", "EYT1" = "dashed", 
                                     "OR" = "solid", "RD" = "solid", 
                                     "RR" = "solid", "RR*" = "solid")) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(title = title, subtitle = subtitle, y = "estimated expected value")
}
