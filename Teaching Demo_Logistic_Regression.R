# =============================================================================
# LOGISTIC REGRESSION ANALYSIS: BINARY OUTCOME (TURNOVER)
# =============================================================================

# Install required libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("broom")
#install.packages("gridExtra")
#install.packages("MASS")
#install.packages("purrr")
#install.packages("ResourceSelection")
#install.packages("corrplot")
#install.packages("here")
#install.packages("readr")

# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)
library(gridExtra)
library(MASS)
library(purrr)
library(ResourceSelection)
library(corrplot)
library(here)
library(readr)


# Load and prepare data
data <- readr::read_csv(here::here("employee_turnover_data.csv"))


data1 <- data %>% dplyr::mutate(Turnover = ifelse(Turnover == "Yes", 1, 0))

# --- Center numeric predictors for modeling (keep original columns for plots) ---
center_cols <- c("Age","Tenure_Years","Avg_Hours_Week","Training_Hours_Year","Job_Satisfaction")
means_list  <- sapply(data1[, center_cols], function(x) mean(x, na.rm = TRUE))

# data_c will be used in *all modeling* steps
data_c <- data1
for (nm in center_cols) {
  data_c[[paste0(nm, "_c")]] <- data_c[[nm]] - means_list[[nm]]
}


cat("=== LOGISTIC REGRESSION ANALYSIS: BINARY OUTCOME ===\n\n")

# =============================================================================
# EXPLORATORY LOGISTIC REGRESSION FLOW 
# -----------------------------------------------------------------------------
# Flow: 
#   (1) CIs for turnover rate by Department, 
#   (2) Correlation table among continuous predictors,
#   (3) Simple (unadjusted) logistic regressions: one predictor at a time
#   (4) Multiple (adjusted) logistic regression with main effects
#   (5) Add theory-motivated interactions and compare via LRT/AIC
#   (6) Interpret using ORs + 95% CIs, LRT/AIC, pseudo-R^2, ROC AUC, calibration
# -----------------------------------------------------------------------------

# ---- 1. Turnover rate by Department with 95% CIs (Clopper–Pearson) ----
dept_agg <- data1 %>%
  dplyr::count(Department, Turnover) %>%
  tidyr::pivot_wider(names_from = Turnover, values_from = n, values_fill = 0) %>%
  dplyr::rename(Stayed = `0`, Left = `1`) %>%
  dplyr::mutate(total = Stayed + Left,
                rate = Left / total)

dept_ci <- dept_agg %>%
  dplyr::rowwise() %>%
  dplyr::mutate(ci_low  = binom.test(Left, total)$conf.int[1],
                ci_high = binom.test(Left, total)$conf.int[2]) %>%
  dplyr::ungroup()

print(dept_ci %>% dplyr::select(Department, Stayed, Left, total, rate, ci_low, ci_high))

ggplot(dept_ci, aes(x = Department, y = rate, fill = Department)) +
  geom_col(alpha = 0.8) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, size = 1.2) +
  # Add values at the ends of error bars
  geom_text(aes(y = ci_low, label = paste0(round(ci_low * 100, 1), "%")), 
            vjust = 1.5, size = 5, color = "black", fontface = "bold") +
  geom_text(aes(y = ci_high, label = paste0(round(ci_high * 100, 1), "%")), 
            vjust = -0.5, size = 5, color = "black", fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7")) +  # Bright vibrant colors
  labs(title = "Turnover Rate by Department (95% CIs)",
       x = "Department", y = "Turnover rate") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend since x-axis shows departments
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.6  # Make plot wider (landscape orientation)
  ) -> plot_dept_ci
print(plot_dept_ci)



# ---- 2. Pearson correlation matrix (continuous predictors) ----
# Normal correlation table among continuous predictors, with p-value significance stars
# Signif. codes: '****' < 1e-4, '***' < 0.001, '**' < 0.01, '*' < 0.05
 
# Columns assumed to exist based on earlier analysis sections
num_vars <- c("Age", "Tenure_Years", "Avg_Hours_Week", "Training_Hours_Year", "Job_Satisfaction")
 
# Correlation coefficients (Pearson)
corr_mat <- stats::cor(data1[, num_vars], use = "pairwise.complete.obs", method = "pearson")
 
# Matrix of p-values via pairwise cor.test
k <- length(num_vars)
p_mat <- matrix(NA_real_, nrow = k, ncol = k, dimnames = list(num_vars, num_vars))
for (i in seq_len(k)) {
  for (j in seq_len(k)) {
    if (i == j) {
      p_mat[i, j] <- NA_real_
    } else {
      ct <- suppressWarnings(stats::cor.test(data1[[ num_vars[i] ]], data1[[ num_vars[j] ]], method = "pearson"))
      p_mat[i, j] <- ct$p.value
    }
  }
}
 
# Significance stars
star_fun <- function(p) {
  if (is.na(p)) return("")
  if (p < 1e-4) return("****")
  if (p < 1e-3) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("")
}
 
# Display matrix with r and stars (e.g., 0.32**)
disp <- matrix("", nrow = k, ncol = k, dimnames = list(num_vars, num_vars))
for (i in seq_len(k)) {
  for (j in seq_len(k)) {
    disp[i, j] <- paste0(sprintf("%.2f", corr_mat[i, j]), star_fun(p_mat[i, j]))
  }
}
 
corr_table <- data.frame(Variable = rownames(disp), disp, check.names = FALSE, row.names = NULL)
cat("\n--- Pearson correlation matrix (continuous predictors) ---\n")
print(corr_table, row.names = FALSE)
cat("Signif. codes: '****' < 1e-4, '***' < 0.001, '**' < 0.01, '*' < 0.05\n\n")

# ---- 2.1 Correlation Visualization Plots ----

# Custom variable names for better display (must align with num_vars)
custom_names <- c("Age", "Tenure", "Work", "Training", "Satisfaction")

# --- Plot 1: Correlation matrix (LOWER TRIANGLE + DIAGONAL) using ggplot2 ---
var_levels  <- num_vars
name_levels <- custom_names
n <- length(var_levels)

# Build indices for LOWER triangle INCLUDING diagonal: i >= j (row index >= column index)
idx <- expand.grid(i = seq_len(n), j = seq_len(n))
idx <- subset(idx, i >= j)

corr_df <- idx %>%
  mutate(
    Var1     = var_levels[i],   # row variable
    Var2     = var_levels[j],   # column variable
    Var1_lab = name_levels[i],
    Var2_lab = name_levels[j],
    r = mapply(function(a,b) corr_mat[a,b], Var1, Var2),
    p = mapply(function(a,b) p_mat[a,b],   Var1, Var2)
  ) %>%
  mutate(
    sig = dplyr::case_when(
      is.na(p)        ~ "",      # diagonal
      p < 0.001       ~ "***",
      p < 0.01        ~ "**",
      p < 0.05        ~ "*",
      TRUE            ~ ""
    )
  )

# Enforce axis orders; put column names at top, row names on left (reversed)
corr_df$Var2_lab <- factor(corr_df$Var2_lab, levels = name_levels)
corr_df$Var1_lab <- factor(corr_df$Var1_lab, levels = rev(name_levels))

plot_corr <- ggplot(corr_df, aes(Var2_lab, Var1_lab, fill = r)) +
  geom_tile(color = "white", size = 0.8) +
  geom_text(aes(label = sprintf("%.2f%s", r, sig)), size = 5, fontface = "bold") +
  scale_fill_gradient2(low = "#FF0000", mid = "white", high = "#00BFFF",
                       midpoint = 0, limits = c(-1, 1), name = "Correlation",
                       guide = guide_colorbar(barheight = 20, barwidth = 1.5)) +
  scale_x_discrete(position = "top", drop = FALSE, expand = c(0, 0)) +
  scale_y_discrete(drop = FALSE, expand = c(0, 0)) +
  coord_fixed() +
  labs(title = "Correlation Matrix (Numeric)", x = NULL, y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, margin = margin(b = 3)),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 20, margin = margin(b = 2)),
    axis.text.y = element_text(size = 18, margin = margin(r = 2)),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 12),
    legend.position = "right"
  )

print(plot_corr)

# Plot 2: Correlation heatmap with custom names
# Apply custom axis labels by changing dimnames
corr_mat_named <- corr_mat
colnames(corr_mat_named) <- rownames(corr_mat_named) <- custom_names

par(mar = c(0, 0, 2, 0))
corrplot(
  corr_mat_named,
  method = "color",
  type   = "full",
  order  = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.2,
  cl.cex = 1.0,
  addCoef.col = "black",
  number.cex  = 0.9,
  col = colorRampPalette(c("#FF6B6B", "white", "#4ECDC4"))(200)
)
title("Correlation Heatmap", line = 0.5)

# Plot 3: Correlation matrix with circles
par(mar = c(0, 0, 2, 0))
corrplot(
  corr_mat_named,
  method = "circle",
  type   = "lower",
  order  = "hclust",
  tl.col = "black",
  tl.srt = 45,
  tl.cex = 1.2,
  cl.cex = 1.0,
  col = colorRampPalette(c("#FF6B6B", "white", "#4ECDC4"))(200)
)
title("Correlation Matrix (Circle Size = Strength)", line = 0.5)

# Reset par settings
par(mar = c(5, 4, 4, 2) + 0.1)


# =============================================================================

# =============================================================================
# 1. SIMPLE LOGISTIC REGRESSION (ONE VARIABLE AT A TIME)
# =============================================================================

cat("--- SIMPLE LOGISTIC REGRESSION RESULTS ---\n\n")

# Function to perform simple logistic regression (centered for modeling, original for plot)
perform_logistic_regression <- function(data_model, data_plot, model_var, plot_var, variable_label, x_label) {
  # Fit logistic regression using centered predictor
  formula <- as.formula(paste("Turnover ~", model_var))
  model <- glm(formula, data = data_model, family = binomial(link = "logit"))

  # Summaries from the centered model
  model_summary <- summary(model)
  coefficients <- model_summary$coefficients
  intercept <- coefficients[1, 1]
  slope <- coefficients[2, 1]
  p_value <- coefficients[2, 4]
  odds_ratio <- exp(slope)

  # McFadden's R2
  null_model <- glm(Turnover ~ 1, data = data_model, family = binomial)
  mcfadden_r2 <- 1 - (model$deviance / null_model$deviance)

  results <- data.frame(
    Variable = variable_label,
    Coefficient = round(slope, 4),
    Odds_Ratio = round(odds_ratio, 4),
    P_Value = round(p_value, 4),
    McFadden_R2 = round(mcfadden_r2, 4),
    Significance = ifelse(p_value < 0.001, "***", ifelse(p_value < 0.01, "**", ifelse(p_value < 0.05, "*", "ns")))
  )

  cat(paste("Variable:", variable_label, "\n"))
  cat(paste("Coefficient (log-odds):", round(slope, 4), "\n"))
  cat(paste("Odds Ratio:", round(odds_ratio, 4), "\n"))
  cat(paste("P-value:", round(p_value, 4), "\n"))
  cat(paste("McFadden's R²:", round(mcfadden_r2, 4), "\n"))
  cat(paste("Significance:", results$Significance, "\n\n"))

  # Plot on the original (uncentered) x-variable for interpretability
  plot <- data_plot %>%
    ggplot(aes_string(x = plot_var, y = "Turnover")) +
    geom_point(alpha = 0.6, size = 2, color = "#2E86AB") +
    geom_smooth(method = "glm", method.args = list(family = "binomial"),
                se = TRUE, color = "#A23B72", size = 1.2) +
    labs(
      title = paste("Turnover vs", variable_label, "(Logistic Regression)"),
      subtitle = paste("Coef =", round(slope, 4),
                       "| OR =", round(odds_ratio, 4),
                       "| p =", round(p_value, 4),
                       "| (model uses centered ", variable_label, ")"),
      x = x_label,
      y = "Turnover Probability"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1))

  list(results = results, plot = plot, model = model)
}

# Run simple logistic regressions for each continuous variable (centered for modeling, original for plotting)
age_logistic <- perform_logistic_regression(data_model = data_c, data_plot = data1,
  model_var = "Age_c", plot_var = "Age", variable_label = "Age", x_label = "Age (Years)")

tenure_logistic <- perform_logistic_regression(data_model = data_c, data_plot = data1,
  model_var = "Tenure_Years_c", plot_var = "Tenure_Years", variable_label = "Tenure", x_label = "Tenure (Years)")

hours_logistic <- perform_logistic_regression(data_model = data_c, data_plot = data1,
  model_var = "Avg_Hours_Week_c", plot_var = "Avg_Hours_Week", variable_label = "Work Hours", x_label = "Average Hours per Week")

training_logistic <- perform_logistic_regression(data_model = data_c, data_plot = data1,
  model_var = "Training_Hours_Year_c", plot_var = "Training_Hours_Year", variable_label = "Training Hours", x_label = "Training Hours per Year")

satisfaction_logistic <- perform_logistic_regression(data_model = data_c, data_plot = data1,
  model_var = "Job_Satisfaction_c", plot_var = "Job_Satisfaction", variable_label = "Job Satisfaction", x_label = "Job Satisfaction (1-5)")

# Run simple logistic regressions for categorical variables
# Department (using dummy coding)
dept_model <- glm(Turnover ~ Department, data = data1, family = binomial(link = "logit"))
dept_summary <- summary(dept_model)
dept_coef <- dept_summary$coefficients
dept_odds <- exp(dept_coef[,1])
dept_pvals <- dept_coef[,4]

# Calculate McFadden's R² for Department
null_model <- glm(Turnover ~ 1, data = data1, family = binomial)
dept_mcfadden <- 1 - (dept_model$deviance / null_model$deviance)

# Create Department results (excluding intercept)
dept_results <- data.frame(
  Variable = c("Department (HR vs Sales)", "Department (Finance vs Sales)", 
               "Department (IT vs Sales)", "Department (Operations vs Sales)"),
  Coefficient = round(dept_coef[2:5,1], 4),  # Exclude intercept
  Odds_Ratio = round(dept_odds[2:5], 4),     # Exclude intercept
  P_Value = round(dept_pvals[2:5], 4),       # Exclude intercept
  McFadden_R2 = round(rep(dept_mcfadden, 4), 4),
  Significance = ifelse(dept_pvals[2:5] < 0.001, "***", 
                       ifelse(dept_pvals[2:5] < 0.01, "**", 
                              ifelse(dept_pvals[2:5] < 0.05, "*", "")))
)

# Salary Level (using dummy coding)
salary_model <- glm(Turnover ~ Salary_Level, data = data1, family = binomial(link = "logit"))
salary_summary <- summary(salary_model)
salary_coef <- salary_summary$coefficients
salary_odds <- exp(salary_coef[,1])
salary_pvals <- salary_coef[,4]

# Calculate McFadden's R² for Salary Level
salary_mcfadden <- 1 - (salary_model$deviance / null_model$deviance)

# Create Salary Level results (excluding intercept)
salary_results <- data.frame(
  Variable = c("Salary Level (Medium vs Low)", "Salary Level (High vs Low)"),
  Coefficient = round(salary_coef[2:3,1], 4),  # Exclude intercept
  Odds_Ratio = round(salary_odds[2:3], 4),     # Exclude intercept
  P_Value = round(salary_pvals[2:3], 4),       # Exclude intercept
  McFadden_R2 = round(rep(salary_mcfadden, 2), 4),
  Significance = ifelse(salary_pvals[2:3] < 0.001, "***", 
                       ifelse(salary_pvals[2:3] < 0.01, "**", 
                              ifelse(salary_pvals[2:3] < 0.05, "*", "")))
)

# Combine all results
all_logistic_results <- rbind(
  age_logistic$results,
  tenure_logistic$results,
  hours_logistic$results,
  training_logistic$results,
  satisfaction_logistic$results,
  dept_results,
  salary_results
)

# Display combined results table
cat("--- SUMMARY OF LOGISTIC REGRESSION RESULTS ---\n")
print(all_logistic_results)

# Create publication-ready tables using tab_model
if (!require(sjPlot, quietly = TRUE)) {
  install.packages("sjPlot", repos = "https://cran.r-project.org")
  library(sjPlot)
}


cat("\n--- INDIVIDUAL LOGISTIC REGRESSION MODELS (tab_model) ---\n")

# Continuous variables
sjPlot::tab_model(age_logistic$model, 
                  title = "Age vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

sjPlot::tab_model(tenure_logistic$model,
                  title = "Tenure vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

sjPlot::tab_model(hours_logistic$model,
                  title = "Work Hours vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

sjPlot::tab_model(training_logistic$model,
                  title = "Training Hours vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

sjPlot::tab_model(satisfaction_logistic$model,
                  title = "Job Satisfaction vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

# Categorical variables
sjPlot::tab_model(dept_model,
                  title = "Department vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

sjPlot::tab_model(salary_model,
                  title = "Salary Level vs Turnover", show.ci = TRUE, show.se = TRUE, show.p = TRUE,
                  p.style = "stars", digits = 4)

cat("\n--- COMBINED SIMPLE LOGISTIC REGRESSION RESULTS (tab_model) ---\n")

cat("\n--- Testing individual models ---\n")

# Combined table for continuous predictors
sjPlot::tab_model(
  age_logistic$model, tenure_logistic$model, hours_logistic$model,
  training_logistic$model, satisfaction_logistic$model,
  title = "Simple Logistic Regression: Age, Tenure, Work Hours, Training Hours, Job Satisfaction",
  show.ci = FALSE, show.se = TRUE, show.p = TRUE,
  p.style = "stars", digits = 2
)

# Combined table for categorical predictors
sjPlot::tab_model(
  dept_model, salary_model,
  title = "Simple Logistic Regression: Department, Salary Level",
  show.ci = FALSE, show.se = TRUE, show.p = TRUE,
  p.style = "stars", digits = 2
)

# For categorical variables separately
cat("\n--- CATEGORICAL VARIABLES ---\n")
# print(dept_table)
# print(salary_table)

# =============================================================================
# 2. MULTIPLE LOGISTIC REGRESSION
# =============================================================================

cat("\n--- MULTIPLE LOGISTIC REGRESSION (ALL VARIABLES) ---\n")

# Multiple logistic regression with ALL variables together (centered numeric variables)
multiple_model <- glm(Turnover ~ Age_c + Tenure_Years_c + Avg_Hours_Week_c + Training_Hours_Year_c +
                      Job_Satisfaction_c + Department + Salary_Level,
                      data = data_c, family = binomial(link = "logit"))

# Display multiple regression results using summary and custom table
cat("--- MULTIPLE LOGISTIC REGRESSION RESULTS ---\n")
multiple_summary <- summary(multiple_model)
print(multiple_summary)

# Create a clean results table
multiple_coef <- multiple_summary$coefficients
multiple_results <- data.frame(
  Variable = rownames(multiple_coef),
  Coefficient = round(multiple_coef[,1], 4),
  Odds_Ratio = round(exp(multiple_coef[,1]), 4),
  Std_Error = round(multiple_coef[,2], 4),
  Z_Value = round(multiple_coef[,3], 4),
  P_Value = round(multiple_coef[,4], 4),
  Significance = ifelse(multiple_coef[,4] < 0.001, "***", 
                       ifelse(multiple_coef[,4] < 0.01, "**", 
                              ifelse(multiple_coef[,4] < 0.05, "*", "")))
)

cat("\n--- MULTIPLE LOGISTIC REGRESSION COEFFICIENTS ---\n")
print(multiple_results)

# Model fit statistics
cat("\n--- MODEL FIT STATISTICS ---\n")
cat(paste("AIC:", round(multiple_model$aic, 4), "\n"))
cat(paste("BIC:", round(BIC(multiple_model), 4), "\n"))

# McFadden's R²
null_model <- glm(Turnover ~ 1, data = data1, family = binomial)
mcfadden_r2 <- 1 - (multiple_model$deviance / null_model$deviance)
cat(paste("McFadden's R²:", round(mcfadden_r2, 4), "\n"))

# Try tab_model for multiple regression (simplified approach)
cat("\n--- MULTIPLE LOGISTIC REGRESSION (tab_model) ---\n")
sjPlot::tab_model(
  multiple_model,
  title = "Multiple Logistic Regression: All Variables",
  show.ci = FALSE, show.se = TRUE, show.p = TRUE,
  p.style = "stars", digits = 3
)

# Display all plots
cat("\n--- LOGISTIC REGRESSION PLOTS ---\n")
print(age_logistic$plot)
print(tenure_logistic$plot)
print(hours_logistic$plot)
print(training_logistic$plot)
print(satisfaction_logistic$plot)

# Create plots for categorical variables
# Department plot
dept_plot <- data1 %>%
  ggplot(aes(x = Department, y = Turnover, fill = Department)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7")) +
  labs(title = "Turnover by Department (Logistic Regression)",
       subtitle = paste("McFadden's R² =", round(dept_mcfadden, 4)),
       x = "Department", y = "Turnover") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

# Salary Level plot
salary_plot <- data1 %>%
  ggplot(aes(x = Salary_Level, y = Turnover, fill = Salary_Level)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "red") +
  scale_fill_manual(values = c("#FF6B6B", "#4ECDC4", "#45B7D1")) +
  labs(title = "Turnover by Salary Level (Logistic Regression)",
       subtitle = paste("McFadden's R² =", round(salary_mcfadden, 4)),
       x = "Salary Level", y = "Turnover") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

print(dept_plot)
print(salary_plot)

# Create combined plot
combined_logistic_plot <- gridExtra::grid.arrange(
  age_logistic$plot,
  tenure_logistic$plot,
  hours_logistic$plot,
  training_logistic$plot,
  ncol = 2, nrow = 2
)

print(combined_logistic_plot)



# =============================================================================
# 3. LOGISTIC REGRESSION WITH INTERACTION EFFECTS (INDIVIDUAL TESTING)
# =============================================================================

cat("\n--- LOGISTIC REGRESSION WITH INTERACTION EFFECTS (INDIVIDUAL TESTING) ---\n\n")

# =============================================================================
# STEP 1: TENURE × DEPARTMENT INTERACTION
# =============================================================================

cat("STEP 1: TENURE × DEPARTMENT INTERACTION\n")

# Main effects model for Tenure × Department
tenure_dept_main <- glm(Turnover ~ Tenure_Years_c + Department, 
                        data = data_c, family = binomial(link = "logit"))

# Interaction model for Tenure × Department
tenure_dept_interaction <- glm(Turnover ~ Tenure_Years_c + Department + Tenure_Years_c:Department, 
                               data = data_c, family = binomial(link = "logit"))

# Calculate statistics
tenure_dept_main_aic <- AIC(tenure_dept_main)
tenure_dept_interaction_aic <- AIC(tenure_dept_interaction)
tenure_dept_main_bic <- BIC(tenure_dept_main)
tenure_dept_interaction_bic <- BIC(tenure_dept_interaction)
tenure_dept_main_r2 <- 1 - (tenure_dept_main$deviance / tenure_dept_main$deviance)
tenure_dept_interaction_r2 <- 1 - (tenure_dept_interaction$deviance / tenure_dept_main$deviance)

# LRT test
tenure_dept_lrt <- anova(tenure_dept_main, tenure_dept_interaction, test = "Chisq")
tenure_dept_lrt_p <- tenure_dept_lrt$`Pr(>Chi)`[2]

cat("Main Effects Model: Turnover ~ Tenure_Years_c + Department\n")
cat("Interaction Model: Turnover ~ Tenure_Years_c + Department + Tenure_Years_c:Department\n")
cat("AIC Change:", round(tenure_dept_interaction_aic - tenure_dept_main_aic, 2), "\n")
cat("BIC Change:", round(tenure_dept_interaction_bic - tenure_dept_main_bic, 2), "\n")
cat("R² Change:", round(tenure_dept_interaction_r2 - tenure_dept_main_r2, 4), "\n")
cat("LRT p-value:", round(tenure_dept_lrt_p, 4), "\n\n")

# Display model comparison
cat("--- TENURE × DEPARTMENT COMPARISON ---\n")
cat("MAIN EFFECTS MODEL:\n")
print(summary(tenure_dept_main)$coefficients)
cat("\nINTERACTION MODEL:\n")
print(summary(tenure_dept_interaction)$coefficients)

# Create comparison table
tenure_dept_comparison <- data.frame(
  Model = c("Main Effects", "Main + Interaction"),
  AIC = c(tenure_dept_main_aic, tenure_dept_interaction_aic),
  BIC = c(tenure_dept_main_bic, tenure_dept_interaction_bic),
  R2 = c(tenure_dept_main_r2, tenure_dept_interaction_r2),
  LRT_p = c(NA, tenure_dept_lrt_p)
)
cat("\nMODEL COMPARISON:\n")
print(tenure_dept_comparison)

# Tab_model for Tenure × Department
cat("\n--- TENURE × DEPARTMENT TAB_MODEL ---\n")
sjPlot::tab_model(
  tenure_dept_main, tenure_dept_interaction,
  title = "Tenure × Department: Main Effects vs Main + Interaction",
  show.ci = FALSE, show.se = TRUE, show.p = TRUE,
  p.style = "stars", digits = 2
)

# Interaction plot for Tenure × Department
cat("\n--- TENURE × DEPARTMENT INTERACTION PLOT ---\n")

# Create a new model using original tenure values for the plot
tenure_dept_interaction_original <- glm(Turnover ~ Tenure_Years + Department + Tenure_Years:Department, 
                                       data = data1, family = binomial(link = "logit"))

# Get the original tenure values for proper tick marks
original_tenure <- data1$Tenure_Years
cat("Original tenure range:", round(range(original_tenure, na.rm = TRUE), 1), "\n")

interactions::interact_plot(
  tenure_dept_interaction_original,
  pred = Tenure_Years,
  modx = Department,
  plot.points = FALSE,
  colors = c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A"),
  main.title = "Interaction: Tenure × Department",
  x.label = "Tenure (Years)",
  y.label = "Predicted Probability of Turnover",
  legend.main = "Department",
  line.thickness = 2,
  vary.lty = FALSE
) +
  theme(
    text = element_text(size = 20),
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20),
    plot.title = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  theme(plot.margin = margin(20, 40, 20, 20)) +
  coord_fixed(ratio = 10)


