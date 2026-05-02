
pull_fit_stats <- function(y_test_input, y_hat_test_input) {
  
  rmse <- sqrt(mean((y_test_input - y_hat_test_input)^2))
  
  rss <- sum((y_test_input - y_hat_test_input)^2)
  tss <- sum((y_test_input - mean(y_test_input))^2)
  r2 <- (1-(rss/tss))
  
  return(list(RMSE = rmse, R2 = r2))
  
}


create_diag_plots <- function(y_train_input, y_hat_train_input, y_test_input, y_hat_test_input) {
  
  resid_plot_df <- data.frame(
    y = y_train_input,
    y_hat = y_hat_train_input,
    resid = y_train_input - y_hat_train_input
  )
  
  resid_plot <- ggplot(resid_plot_df, aes(x = y_hat, y = resid)) +
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed") +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", span = 1, se = FALSE, color = "brown3") + 
    labs(x = "Predicted value",
         y = "Residual") +
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
  
  fitted_plot_df <- data.frame(
    y = y_test_input,
    y_hat = y_hat_test_input
  )
  
  fitted_plot <- ggplot(fitted_plot_df, aes(x = y_hat, y = y)) +
    geom_point(alpha = 0.4) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") + 
    labs(x = "Predicted surface fuel load (Mg/ha)",
         y = "Measured surface fuel load (Mg/ha)") +
    theme_minimal() + 
    theme(panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) + 
    scale_y_continuous(limits = c(0, 250)) + 
    scale_x_continuous(limits = c(0, 250))
  
  return(list(residual_plot = resid_plot, fitted_line_plot = fitted_plot))
  
}
