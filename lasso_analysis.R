
library(grpreg)
library(rsample)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
source("helper_functions.R")

################################################################################
# Data prep
################################################################################

fuels_data <- read_csv("fuels_data.csv", col_types = cols(site = col_factor()))
#head(fuels_data)

## training and testing data split
set.seed(230)
split_data <- initial_split(fuels_data, prop = 0.75, strata = site)
train_data <- training(split_data)
test_data <- testing(split_data)

# create model matrices 
x_train <- model.matrix(fuel_load_Mg_ha ~ ., data = train_data)[,-1]
x_test <- model.matrix(fuel_load_Mg_ha ~ ., data = test_data)[,-1]

y_train <- train_data$fuel_load_Mg_ha
y_test <- test_data$fuel_load_Mg_ha


################################################################################
# Fit regression 
################################################################################

# fit lasso regression with cross-validation 
group <- c(rep(1, 5), seq_len(ncol(x_train) - 5) + 1)
cv_fit <- cv.grpreg(X = x_train, y = y_train, group = group, penalty = "grLasso")

# test and train set predictions 
y_hat_train <- as.numeric(predict(cv_fit, X = x_train, lambda = cv_fit$lambda.min))
y_hat_test <- as.numeric(predict(cv_fit, X = x_test, lambda = cv_fit$lambda.min))

# pull out names of non-zero coefficients 
coefs <- coef(cv_fit, lambda = cv_fit$lambda.min)
nonzero_coef_names <- setdiff(names(coefs)[coefs != 0], "(Intercept)")
nonzero_coef_names


################################################################################
# Regression performance 
################################################################################

# test-set performance
lasso_performance <- pull_fit_stats(y_test, y_hat_test)
lasso_performance$RMSE
lasso_performance$R2

# diagnostic plots
lasso_diags <- create_diag_plots(y_train, y_hat_train, y_test, y_hat_test)
lasso_diags$residual_plot
lasso_diags$fitted_line_plot


################################################################################
# Coefficient path plot 
################################################################################

fit <- cv_fit$fit
beta_matrix <- as.matrix(coef(fit))

coef_path_df <- as.data.frame(beta_matrix) %>%
  rownames_to_column("term")

colnames(coef_path_df)[-1] <- fit$lambda

coef_path_df_long <- coef_path_df %>%
  pivot_longer(cols = -term,
               names_to = "lambda",
               values_to = "estimate") %>%
  mutate(lambda = as.numeric(lambda)) %>%
  filter(term != "(Intercept)",
         lambda <= 10)

coef_path_plot <- ggplot(coef_path_df_long, aes(x = lambda, y = estimate, group = term, color = term)) +
  geom_line(linewidth = 0.6) +
  geom_vline(xintercept = cv_fit$lambda.min, linetype = "dashed", linewidth = 0.9) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.2),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  labs(x = expression(lambda),
       y = "Coefficient") + 
  scale_x_continuous(limits = c(0, 10), breaks = 0:10)

coef_path_plot
