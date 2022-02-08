#' Create Venn diagram of the Monte Carlo with beta distribution
#' 
#' Create Venn diagram of the Monte Carlo with beta distribution.
#' 
#' Create Venn diagram of the Monte Carlo. The colors can be changed.
#'
#' @param data Monte Carlo data from \code{mc_beta_effect_measures()}
#' @param n total nb of elements in the whole diagram
#' @param fill_colr Vector of 4 fill colors
#' @param title Title of the diagram
#'
#' @return ggplot object
#' @export
#'
#' @seealso mc_beta_effect_measures
#'
#' @examples
#' \dontrun{
#' }
ggp_venn_sim <- function(data, n = 1000, 
                         fill_colr = c("blue", "yellow", "green", "red"),
                         title = "Venn diagram of effect measure modifications") {
  # the_events <- c("RD_RR", "RD_RRstar", "RD_OR", "RR_RRstar", "RR_OR",
  #               "RRstar_OR", "RD_RR_RRstar", "RD_RR_OR", "RD_RRstar_OR",
  #               "RR_RRstar_OR", "RD_RR_RRstar_OR")
  # dfs <- vector(mode = "list", length = length(the_events))
  # names(dfs) <- the_events
  dfs <- list()
  
  id <- "RD_RR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = T, "RRstar" = F, "OR" = F)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RRstar"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = F, "RRstar" = T, "OR" = F)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = F, "RRstar" = F, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RR_RRstar"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = F, "RR" = T, "RRstar" = T, "OR" = F)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RR_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = F, "RR" = T, "RRstar" = F, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RRstar_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = F, "RR" = F, "RRstar" = T, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RR_vs_RRstar_OR"
  nreps <- data[[id]] * n * 0.5
  df <- data.frame("RD" = T, "RR" = T, "RRstar" = F, "OR" = F)
  dfs[["RD_RR_vs_RRstar_OR_1"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  df <- data.frame("RD" = F, "RR" = F, "RRstar" = T, "OR" = T)
  dfs[["RD_RR_vs_RRstar_OR_2"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RRstar_vs_RR_OR"
  nreps <- data[[id]] * n * 0.5
  df <- data.frame("RD" = T, "RR" = F, "RRstar" = T, "OR" = F)
  dfs[["RD_RRstar_vs_RR_OR_1"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  df <- data.frame("RD" = F, "RR" = T, "RRstar" = F, "OR" = T)
  dfs[["RD_RRstar_vs_RR_OR_2"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_OR_vs_RR_RRstar"
  p <- data[[id]]
  nreps <- data[[id]] * n * 0.5
  df <- data.frame("RD" = T, "RR" = F, "RRstar" = F, "OR" = T)
  dfs[["RD_OR_vs_RR_RRstar_1"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  df <- data.frame("RD" = F, "RR" = T, "RRstar" = T, "OR" = F)
  dfs[["RD_OR_vs_RR_RRstar_2"]] <- 
    do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RR_RRstar"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = T, "RRstar" = T, "OR" = F)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RR_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = T, "RRstar" = F, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RD_RRstar_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = F, "RRstar" = T, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  id <- "RR_RRstar_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = F, "RR" = T, "RRstar" = T, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  
  id <- "RD_RR_RRstar_OR"
  nreps <- data[[id]] * n
  df <- data.frame("RD" = T, "RR" = T, "RRstar" = T, "OR" = T)
  dfs[[id]] <- do.call(rbind, replicate(nreps, df, simplify = FALSE))
  
  df <- do.call(rbind, dfs)
  
  # create the venn diagram
  ggplot(df) +
    ggvenn::geom_venn(aes(A = RD, B = RR, C = RRstar, D = OR), show_percentage = TRUE,
              fill_color = fill_colr) +
    theme_void() +
    labs(title = title, subtitle = sprintf("%d items", n))
}
