library(ggplot2)
library(dplyr)
library(zoo)

plot_line <- function(df, title, y_label) {
  ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "red") +
    labs(title = title, x = "Time", y = y_label) +
    theme_bw()
}

plot_smooth <- function(df, title, y_label, window_size) {
  df <- df %>%
    arrange(time) %>%
    mutate(moving_avg = zoo::rollmean(value, k = window_size, fill = NA, align = "right"))
  
  ggplot(df, aes(x = time, y = value)) +
    geom_line(color = "red") +
    geom_line(aes(y = moving_avg), color = "blue") +
    labs(title = title, x = "Time", y = y_label) +
    theme_bw()
}

plot_sd <- function(df, title, y_label, include_sd = TRUE, sd_factor = 1) {
  df <- df %>%
    mutate(year = format(time, "%Y")) %>%
    group_by(year) %>%
    summarise(
      mean_value = mean(value, na.rm = TRUE),
      sd_value = sd(value, na.rm = TRUE)
    )
  
  if (include_sd) {
    df <- df %>%
      mutate(
        ymin = mean_value - sd_factor * sd_value,
        ymax = mean_value + sd_factor * sd_value
      )
  }
  
  p <- ggplot(df, aes(x = as.numeric(year), y = mean_value)) +
    geom_line(color = "blue") +
    labs(title = title, x = "Year", y = y_label) +
    theme_bw()
  
  if (include_sd) {
    p <- p + 
      geom_ribbon(aes(ymin = ymin, ymax = ymax), alpha = 0.2, fill = "blue") +
      geom_line(aes(y = ymin), color = "blue", linetype = "dashed") +
      geom_line(aes(y = ymax), color = "blue", linetype = "dashed")
  }
  
  return(p)
}