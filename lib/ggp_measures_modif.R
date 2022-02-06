

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
ggp_measures_modif <- function(df, title = "Change in effect measures") {
  # prepare the dataframe used to pcreate the plot
  df <- df %>%
    filter(!grepl(pattern = "log.+", name)) %>%
    select(-conf) %>%
    tidyr::separate(col = "name", into = c("name", "stratum"), sep = "[.]") %>%
    filter(stratum != "diff")
  
  # create the plot
  ggplot(df, 
         aes(x = stratum, y = est, color = name, linetype = name, group = name)) +
    geom_line(size = 1) +
    geom_point(mapping = aes(fill = name), size = 3, shape = 21) +
    geom_text(mapping = aes(label = round(est, 1)),
              nudge_x = 0.05, nudge_y = 0.05) +
    scale_linetype_manual(values = c("EYT0" = "dashed", "EYT1" = "dashed", 
                                     "OR" = "solid", "RD" = "solid", 
                                     "RR" = "solid", "RRstar" = "solid")) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank()) +
    labs(title = title,
         subtitle = "Effect-Measure Modification and Statistical Interaction",
         y = "estimated expected value")
}
