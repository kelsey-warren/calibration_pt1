#' compute_annual_highflow_days
#'
#' calculates average high flow days per year and summary stats.
#'
#' @param m Numeric vector of modeled streamflows
#' @param o Numeric vector of observed streamflows
#' @param wy Numeric vector of water years
#' @param threshold Streamflow threshold for high flow (default = 0.3)
#'
#' @return A list with avg_obs_days, avg_mod_days, diff_days, and correlation

compute_annual_highflow_days <- function(m, o, wy, threshold = 0.3) {
  
  data <- data.frame(model = m, obs = o, wy = wy)
  
  data_summary <- data %>%
    group_by(wy) %>%
    summarize(
      obs_days = sum(obs > threshold, na.rm = TRUE),
      mod_days = sum(model > threshold, na.rm = TRUE),
      .groups = "drop"
    )
  
  avg_obs_days <- mean(data_summary$obs_days, na.rm = TRUE)
  avg_mod_days <- mean(data_summary$mod_days, na.rm = TRUE)
  diff_days <- avg_mod_days - avg_obs_days
  corr <- cor(data_summary$mod_days, data_summary$obs_days, use = "complete.obs")
  
  return(list(
    avg_obs_days = avg_obs_days,
    avg_mod_days = avg_mod_days,
    diff_days = diff_days,
    corr = corr
  ))
}


