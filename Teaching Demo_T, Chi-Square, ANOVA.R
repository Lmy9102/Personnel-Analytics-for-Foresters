# Employee Turnover Analysis
# This script analyzes employee turnover data to understand why some employees stay while others leave
# and to identify areas for intervention to reduce turnover.

# Install required libraries
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("here")
#install.packages("readr")
#install.packages("flextable")
#install.packages("ggplot2")
#install.packages("tidyr")

# Load required libraries 
library(ggplot2)
library(dplyr)
library(here) 
library(readr)
library(flextable)
library(ggplot2)
library(tidyr)


# Load data
data <- readr::read_csv(here("employee_turnover_data.csv"))

# Check for missing data
missing_summary <- sapply(data, function(x) sum(is.na(x)))
print(missing_summary)


# Check for duplicates (EmployeeIDs)
duplicates <- data %>%
  group_by(EmployeeID) %>%
  filter(n() > 1) %>%
  arrange(EmployeeID)
duplicates

# View the data structure (e.g., sample size, number of variables, variable type)
dplyr::glimpse(data)

# Recode dependent variable (Turnover)
data1 <- data %>%
  dplyr::mutate(Turnover = ifelse(Turnover == "Yes", 1, 0))
dplyr::glimpse(data1)
View(data)



# ============================================================================
# T-TEST ANALYSIS: NUMERIC VARIABLES BY TURNOVER GROUP
# ============================================================================

# Function to perform t-test and create visualization for any variable
perform_t_test_analysis <- function(data, variable_name, variable_label, y_label) {
  
  cat(paste0("\n=== ", toupper(variable_label), " BY TURNOVER GROUP ===\n"))
  
  # 1. DESCRIPTIVE STATISTICS
  summary_stats <- data %>%
    group_by(Turnover) %>%
    summarise(
      Count = n(),
      Mean_Value = round(mean(!!sym(variable_name)), 2),
      Median_Value = round(median(!!sym(variable_name)), 2),
      SD_Value = round(sd(!!sym(variable_name)), 2),
      Min_Value = min(!!sym(variable_name)),
      Max_Value = max(!!sym(variable_name)),
      .groups = 'drop'
    ) %>%
    mutate(Turnover_Group = ifelse(Turnover == 1, "Left", "Stayed"))
  
  # Display summary table
  summary_table <- flextable(summary_stats) %>%
    autofit() %>%
    align(j = 2:8, align = "center") %>%
    set_header_labels(
      Turnover = "Turnover Code",
      Count = "Count",
      Mean_Value = paste("Mean", variable_label),
      Median_Value = paste("Median", variable_label),
      SD_Value = "Standard Deviation",
      Min_Value = paste("Min", variable_label),
      Max_Value = paste("Max", variable_label),
      Turnover_Group = "Turnover Group"
    )
  print(summary_table)
  
  # 2. VISUALIZATION
  cat(paste0("\n--- ", variable_label, " Distribution by Turnover Group ---\n"))
  
  # Calculate means for labeling
  stayed_mean <- mean(data[[variable_name]][data$Turnover == 0])
  left_mean <- mean(data[[variable_name]][data$Turnover == 1])
  
  # Create data frame for mean labels
  mean_labels <- data.frame(
    x = factor(c("Stayed", "Left"), levels = c("Stayed", "Left")),
    y = c(stayed_mean, left_mean),
    label = c(paste0("Mean=", round(stayed_mean, 1)), paste0("Mean=", round(left_mean, 1)))
  )
  
  # Create box plot
  p <- ggplot(data, aes(x = factor(Turnover, labels = c("Stayed", "Left")), 
                        y = !!sym(variable_name), 
                        fill = factor(Turnover, labels = c("Stayed", "Left")))) +
    geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
    geom_text(data = mean_labels, aes(x = x, y = y, label = label), 
              inherit.aes = FALSE,
              vjust = -1.5, hjust = 0.5, size = 10, color = "blue", fontface = "bold") +
    labs(title = paste(variable_label, "by Turnover Group"),
         subtitle = paste("Comparison of", tolower(variable_label), "between stayed vs. left (Blue diamond = Mean)"),
         x = "Turnover Group",
         y = y_label) +
    theme_minimal(base_size = 24) +
    theme(legend.position = "none",
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          plot.title = element_text(size = 28, face = "bold"),
          plot.subtitle = element_text(size = 18, color = "gray50"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.margin = margin(t = 20, r = 20, b = 20, l = 20)) +
    scale_fill_manual(values = c("Stayed" = "#4ECDC4", "Left" = "#FF6B6B")) +
    coord_cartesian(clip = "off")
  
  print(p)
  
  # 3. T-TEST ANALYSIS
  cat(paste0("\n=== ", toupper(variable_label), " T-TEST RESULTS ===\n"))
  
  # Extract values for each group
  stayed_values <- data[[variable_name]][data$Turnover == 0]
  left_values <- data[[variable_name]][data$Turnover == 1]
  
  # Perform t-test
  t_test_result <- t.test(stayed_values, left_values, 
                         alternative = "two.sided", 
                         var.equal = FALSE)
  
  # Display results
  cat("T-Test Results:\n")
  cat("================\n")
  cat("Test Type: Independent Samples t-test (Welch's)\n")
  cat("Null Hypothesis: No difference in", tolower(variable_label), "between groups\n")
  cat("Alternative Hypothesis: Difference in", tolower(variable_label), "between groups\n\n")


  cat("T-Test Results:\n")
  cat("  t-statistic =", round(t_test_result$statistic, 3), "\n")
  cat("  Degrees of freedom =", round(t_test_result$parameter, 2), "\n")
  cat("  p-value =", round(t_test_result$p.value, 4), "\n")
  cat("  95% Confidence Interval: [", round(t_test_result$conf.int[1], 3), ",", round(t_test_result$conf.int[2], 3), "]\n\n")
  
  # 4. INTERPRETATION
  cat("=== INTERPRETATION ===\n")
  if(t_test_result$p.value < 0.05) {
    cat("CONCLUSION: Reject the null hypothesis (p < 0.05)\n")
    cat("There IS a statistically significant difference in", tolower(variable_label), "between turnover groups.\n")
    
    if(mean(stayed_values) > mean(left_values)) {
      cat("Employees who STAYED have HIGHER", toupper(variable_label), "than those who LEFT.\n")
    } else {
      cat("Employees who LEFT have HIGHER", toupper(variable_label), "than those who STAYED.\n")
    }
  } else {
    cat("CONCLUSION: Fail to reject the null hypothesis (p >= 0.05)\n")
    cat("There is NO statistically significant difference in", tolower(variable_label), "between turnover groups.\n")
  }
  

  
  cat(paste0("\n=== ", toupper(variable_label), " ANALYSIS COMPLETE ===\n"))
  
  return(list(
    t_test = t_test_result,
    stayed_mean = mean(stayed_values),
    left_mean = mean(left_values)
  ))
}


# Perform t-test analysis for all variables
cat("\n\n=== COMPREHENSIVE T-TEST ANALYSIS FOR NUMERIC VARIABLES ===\n")

# 1. JOB SATISFACTION
js_results <- perform_t_test_analysis(data1, "Job_Satisfaction", "Job Satisfaction", "Job Satisfaction (1-5)")

# 2. AGE
age_results <- perform_t_test_analysis(data1, "Age", "Age", "Age (Years)")

# 3. TENURE YEARS
tenure_results <- perform_t_test_analysis(data1, "Tenure_Years", "Tenure", "Tenure (Years)")

# 4. AVERAGE HOURS PER WEEK
hours_results <- perform_t_test_analysis(data1, "Avg_Hours_Week", "Average Hours", "Average Hours per Week")

# 5. TRAINING HOURS PER YEAR
training_results <- perform_t_test_analysis(data1, "Training_Hours_Year", "Training Hours", "Training Hours per Year")



# ============================================================================
# CHI-SQUARE TEST ANALYSIS: CATEGORICAL VARIABLES BY TURNOVER GROUP
# ============================================================================

# 5. SALARY LEVEL (Chi-square test instead of t-test for categorical variable)
cat("\n=== SALARY LEVEL BY TURNOVER GROUP ===\n")

# Create contingency table
salary_contingency <- table(data1$Salary_Level, data1$Turnover)
colnames(salary_contingency) <- c("Stayed", "Left")
rownames(salary_contingency) <- c("Low", "Medium", "High")

# Display contingency table using flextable
salary_contingency_df <- as.data.frame.matrix(salary_contingency)
salary_contingency_df$Salary_Level <- rownames(salary_contingency_df)
salary_contingency_df <- salary_contingency_df[, c("Salary_Level", "Stayed", "Left")]

# Add total row
salary_contingency_df <- rbind(salary_contingency_df, 
                              data.frame(Salary_Level = "Total", 
                                        Stayed = sum(salary_contingency_df$Stayed),
                                        Left = sum(salary_contingency_df$Left)))

salary_contingency_table <- flextable(salary_contingency_df) %>%
  autofit() %>%
  bold(i = nrow(salary_contingency_df), bold = TRUE) %>%   # bold the total row
  align(j = 2:3, align = "center") %>%                     # center numeric columns
  set_header_labels(
    Salary_Level = "Salary Level",
    Stayed = "Stayed",
    Left = "Left"
  )

print(salary_contingency_table)

# Choose appropriate test
choose_test <- function(contingency_table, test_name) {
  # Check expected frequencies
  expected <- chisq.test(contingency_table)$expected
  min_expected <- min(expected)
  
  cat(paste0("\n=== ", toupper(test_name), " ANALYSIS ===\n"))
  cat("Minimum expected frequency:", round(min_expected, 2), "\n")
  
  if(min_expected < 5) {
    cat("Using Fisher's Exact Test (expected frequencies < 5)\n")
    result <- fisher.test(contingency_table)
    cat("Fisher's exact test p-value =", round(result$p.value, 4), "\n")
  } else {
    cat("Using Chi-Square Test (expected frequencies ≥ 5)\n")
    result <- chisq.test(contingency_table)
    cat("Chi-square test p-value =", round(result$p.value, 4), "\n")
  }
  
  return(result)
}

# Perform appropriate test for Salary Level
salary_result <- choose_test(salary_contingency, "Salary Level")

# Interpretation for salary level
cat("=== INTERPRETATION ===\n")
if(salary_result$p.value < 0.05) {
  cat("CONCLUSION: Reject the null hypothesis (p < 0.05)\n")
  cat("There IS a statistically significant association between salary level and turnover.\n")
} else {
  cat("CONCLUSION: Fail to reject the null hypothesis (p >= 0.05)\n")
  cat("There is NO statistically significant association between salary level and turnover.\n")
}


# 6. DEPARTMENT (Chi-square test for categorical variable)
cat("\n=== DEPARTMENT BY TURNOVER GROUP ===\n")

# Create contingency table for Department
dept_contingency <- table(data1$Department, data1$Turnover)
colnames(dept_contingency) <- c("Stayed", "Left")
rownames(dept_contingency) <- c("HR", "Finance", "IT", "Operations", "Sales")

# Display contingency table using flextable
dept_contingency_df <- as.data.frame.matrix(dept_contingency)
dept_contingency_df$Department <- rownames(dept_contingency_df)
dept_contingency_df <- dept_contingency_df[, c("Department", "Stayed", "Left")]

# Add total row
dept_contingency_df <- rbind(dept_contingency_df, 
                            data.frame(Department = "Total", 
                                      Stayed = sum(dept_contingency_df$Stayed),
                                      Left = sum(dept_contingency_df$Left)))

dept_contingency_table <- flextable(dept_contingency_df) %>%
  autofit() %>%
  bold(i = nrow(dept_contingency_df), bold = TRUE) %>%   # bold the total row
  align(j = 2:3, align = "center") %>%                   # center numeric columns
  set_header_labels(
    Department = "Department",
    Stayed = "Stayed",
    Left = "Left"
  )

print(dept_contingency_table)

# Calculate percentages for better interpretation
dept_percentages <- prop.table(dept_contingency, margin = 1) * 100
dept_percentages_df <- as.data.frame.matrix(dept_percentages)
dept_percentages_df$Department <- rownames(dept_percentages_df)
dept_percentages_df <- dept_percentages_df[, c("Department", "Stayed", "Left")]

dept_percentages_table <- flextable(dept_percentages_df) %>%
  autofit() %>%
  align(j = 2:3, align = "center") %>%
  set_header_labels(
    Department = "Department",
    Stayed = "Stayed (%)",
    Left = "Left (%)"
  ) %>%
  add_header_row(values = c("", "Turnover Status"), colwidths = c(1, 2)) %>%
  theme_box()

cat("\nDepartment Turnover Percentages (by row):\n")
print(dept_percentages_table)

# Perform appropriate test for Department
dept_result <- choose_test(dept_contingency, "Department")

# Interpretation for department
cat("=== INTERPRETATION ===\n")
if(dept_result$p.value < 0.05) {
  cat("CONCLUSION: Reject the null hypothesis (p < 0.05)\n")
  cat("There IS a statistically significant association between department and turnover.\n")
  
  # Show which departments have highest turnover rates
  turnover_rates <- dept_percentages[,2]  # "Left" column
  highest_turnover_dept <- names(which.max(turnover_rates))
  lowest_turnover_dept <- names(which.min(turnover_rates))
  
  cat("\nDepartment Turnover Rates:\n")
  for(i in 1:length(turnover_rates)) {
    cat(sprintf("  %s: %.1f%%\n", names(turnover_rates)[i], turnover_rates[i]))
  }
  
  cat(sprintf("\nHighest turnover: %s (%.1f%%)\n", highest_turnover_dept, max(turnover_rates)))
  cat(sprintf("Lowest turnover: %s (%.1f%%)\n", lowest_turnover_dept, min(turnover_rates)))
  
} else {
  cat("CONCLUSION: Fail to reject the null hypothesis (p >= 0.05)\n")
  cat("There is NO statistically significant association between department and turnover.\n")
}

# Calculate Cramér's V for effect size
dept_n <- sum(dept_contingency)
dept_cramers_v <- sqrt(dept_chi_square_result$statistic / (dept_n * (min(nrow(dept_contingency), ncol(dept_contingency)) - 1)))
cat("\nEffect Size (Cramér's V):\n")
cat("Cramér's V =", round(dept_cramers_v, 3), "\n")

if(dept_cramers_v < 0.1) {
  cat("Effect size: Negligible\n")
} else if(dept_cramers_v < 0.3) {
  cat("Effect size: Small\n")
} else if(dept_cramers_v < 0.5) {
  cat("Effect size: Medium\n")
} else {
  cat("Effect size: Large\n")
}

# 6. SUMMARY OF ALL RESULTS
cat("\n\n=== SUMMARY OF ALL T-TEST RESULTS ===\n")
cat("Variable\t\t\tp-value\t\tSignificant\tDirection\n")
cat("--------\t\t\t-------\t\t-----------\t---------\n")

# Job Satisfaction
js_sig <- ifelse(js_results$t_test$p.value < 0.05, "Yes", "No")
js_direction <- ifelse(js_results$stayed_mean > js_results$left_mean, "Stayed > Left", "Left > Stayed")
cat(sprintf("Job Satisfaction\t\t%.4f\t\t%s\t\t%s\n", 
            js_results$t_test$p.value, js_sig, js_direction))

# Age
age_sig <- ifelse(age_results$t_test$p.value < 0.05, "Yes", "No")
age_direction <- ifelse(age_results$stayed_mean > age_results$left_mean, "Stayed > Left", "Left > Stayed")
cat(sprintf("Age\t\t\t%.4f\t\t%s\t\t%s\n", 
            age_results$t_test$p.value, age_sig, age_direction))

# Tenure
tenure_sig <- ifelse(tenure_results$t_test$p.value < 0.05, "Yes", "No")
tenure_direction <- ifelse(tenure_results$stayed_mean > tenure_results$left_mean, "Stayed > Left", "Left > Stayed")
cat(sprintf("Tenure\t\t\t%.4f\t\t%s\t\t%s\n", 
            tenure_results$t_test$p.value, tenure_sig, tenure_direction))

# Hours
hours_sig <- ifelse(hours_results$t_test$p.value < 0.05, "Yes", "No")
hours_direction <- ifelse(hours_results$stayed_mean > hours_results$left_mean, "Stayed > Left", "Left > Stayed")
cat(sprintf("Average Hours\t\t%.4f\t\t%s\t\t%s\n", 
            hours_results$t_test$p.value, hours_sig, hours_direction))

# Training
training_sig <- ifelse(training_results$t_test$p.value < 0.05, "Yes", "No")
training_direction <- ifelse(training_results$stayed_mean > training_results$left_mean, "Stayed > Left", "Left > Stayed")
cat(sprintf("Training Hours\t\t%.4f\t\t%s\t\t%s\n", 
            training_results$t_test$p.value, training_sig, training_direction))

# Salary Level
salary_sig <- ifelse(salary_result$p.value < 0.05, "Yes", "No")
cat(sprintf("Salary Level\t\t%.4f\t\t%s\t\tAssociation\n", 
            salary_result$p.value, salary_sig))

# Department
dept_sig <- ifelse(dept_result$p.value < 0.05, "Yes", "No")
cat(sprintf("Department\t\t%.4f\t\t%s\t\tAssociation\n", 
            dept_result$p.value, dept_sig))

# ============================================================================
# ANOVA ANALYSIS: IDENTIFYING TURNOVER CAUSES
# ============================================================================

cat("\n\n=== ANOVA ANALYSIS FOR TURNOVER CAUSES ===\n")

# 1. ONE-WAY ANOVA: NUMERIC VARIABLES BY DEPARTMENT
cat("\n1. ONE-WAY ANOVA: NUMERIC VARIABLES BY DEPARTMENT\n")
cat("Research Question: Do numeric variables differ significantly across departments?\n\n")

# 1.1 AGE BY DEPARTMENT
cat("\n--- AGE BY DEPARTMENT ---\n")
age_anova <- aov(Age ~ Department, data = data1)
age_summary <- summary(age_anova)
print(age_summary)

age_p_value <- age_summary[[1]]$`Pr(>F)`[1]
if(age_p_value < 0.05) {
  cat("SIGNIFICANT: Age differs across departments (p =", round(age_p_value, 4), ")\n")
  # Post-hoc test using TukeyHSD from base R
  age_tukey <- TukeyHSD(age_anova)
  print(age_tukey)
} else {
  cat("NOT SIGNIFICANT: No age differences across departments (p =", round(age_p_value, 4), ")\n")
}

# 1.2 TENURE BY DEPARTMENT
cat("\n--- TENURE BY DEPARTMENT ---\n")
tenure_anova <- aov(Tenure_Years ~ Department, data = data1)
tenure_summary <- summary(tenure_anova)
print(tenure_summary)

tenure_p_value <- tenure_summary[[1]]$`Pr(>F)`[1]
if(tenure_p_value < 0.05) {
  cat("SIGNIFICANT: Tenure differs across departments (p =", round(tenure_p_value, 4), ")\n")
  tenure_tukey <- TukeyHSD(tenure_anova)
  print(tenure_tukey)
} else {
  cat("NOT SIGNIFICANT: No tenure differences across departments (p =", round(tenure_p_value, 4), ")\n")
}

# 1.3 JOB SATISFACTION BY DEPARTMENT
cat("\n--- JOB SATISFACTION BY DEPARTMENT ---\n")
satisfaction_anova <- aov(Job_Satisfaction ~ Department, data = data1)
satisfaction_summary <- summary(satisfaction_anova)
print(satisfaction_summary)

satisfaction_p_value <- satisfaction_summary[[1]]$`Pr(>F)`[1]
if(satisfaction_p_value < 0.05) {
  cat("SIGNIFICANT: Job satisfaction differs across departments (p =", round(satisfaction_p_value, 4), ")\n")
  satisfaction_tukey <- TukeyHSD(satisfaction_anova)
  print(satisfaction_tukey)
} else {
  cat("NOT SIGNIFICANT: No job satisfaction differences across departments (p =", round(satisfaction_p_value, 4), ")\n")
}

# 1.4 AVERAGE HOURS BY DEPARTMENT
cat("\n--- AVERAGE HOURS BY DEPARTMENT ---\n")
hours_anova <- aov(Avg_Hours_Week ~ Department, data = data1)
hours_summary <- summary(hours_anova)
print(hours_summary)

hours_p_value <- hours_summary[[1]]$`Pr(>F)`[1]
if(hours_p_value < 0.05) {
  cat("SIGNIFICANT: Average hours differ across departments (p =", round(hours_p_value, 4), ")\n")
  hours_tukey <- TukeyHSD(hours_anova)
  print(hours_tukey)
} else {
  cat("NOT SIGNIFICANT: No average hours differences across departments (p =", round(hours_p_value, 4), ")\n")
}

# 1.5 TRAINING HOURS BY DEPARTMENT
cat("\n--- TRAINING HOURS BY DEPARTMENT ---\n")
training_anova <- aov(Training_Hours_Year ~ Department, data = data1)
training_summary <- summary(training_anova)
print(training_summary)

training_p_value <- training_summary[[1]]$`Pr(>F)`[1]
if(training_p_value < 0.05) {
  cat("SIGNIFICANT: Training hours differ across departments (p =", round(training_p_value, 4), ")\n")
  training_tukey <- TukeyHSD(training_anova)
  print(training_tukey)
} else {
  cat("NOT SIGNIFICANT: No training hours differences across departments (p =", round(training_p_value, 4), ")\n")
}

# 2. TWO-WAY ANOVA: INTERACTION EFFECTS
cat("\n\n2. TWO-WAY ANOVA: INTERACTION EFFECTS\n")
cat("Research Question: Do variables interact with department and turnover status?\n\n")

# 2.1 JOB SATISFACTION BY DEPARTMENT × TURNOVER
cat("\n--- JOB SATISFACTION BY DEPARTMENT × TURNOVER ---\n")
satisfaction_twoway <- aov(Job_Satisfaction ~ Department * Turnover, data = data1)
satisfaction_twoway_summary <- summary(satisfaction_twoway)
print(satisfaction_twoway_summary)

# Check p-values for each effect
dept_p <- satisfaction_twoway_summary[[1]]$`Pr(>F)`[1]        # Department main effect
turnover_p <- satisfaction_twoway_summary[[1]]$`Pr(>F)`[2]    # Turnover main effect
interaction_p <- satisfaction_twoway_summary[[1]]$`Pr(>F)`[3] # Interaction effect

cat("\nInterpretation:\n")
if(interaction_p < 0.05) {
  cat("SIGNIFICANT INTERACTION: Department and Turnover interact (p =", round(interaction_p, 4), ")\n")
} else {
  cat("NO SIGNIFICANT INTERACTION: Effects are independent (p =", round(interaction_p, 4), ")\n")
}

if(dept_p < 0.05) {
  cat("SIGNIFICANT DEPARTMENT EFFECT (p =", round(dept_p, 4), ")\n")
} else {
  cat("NO DEPARTMENT EFFECT (p =", round(dept_p, 4), ")\n")
}

if(turnover_p < 0.05) {
  cat("SIGNIFICANT TURNOVER EFFECT (p =", round(turnover_p, 4), ")\n")
} else {
  cat("NO TURNOVER EFFECT (p =", round(turnover_p, 4), ")\n")
}

# 2.2 AGE BY DEPARTMENT × TURNOVER
cat("\n--- AGE BY DEPARTMENT × TURNOVER ---\n")
age_twoway <- aov(Age ~ Department * Turnover, data = data1)
age_twoway_summary <- summary(age_twoway)
print(age_twoway_summary)

age_dept_p <- age_twoway_summary[[1]]$`Pr(>F)`[1]
age_turnover_p <- age_twoway_summary[[1]]$`Pr(>F)`[2]
age_interaction_p <- age_twoway_summary[[1]]$`Pr(>F)`[3]

cat("\nInterpretation:\n")
if(age_interaction_p < 0.05) {
  cat("SIGNIFICANT INTERACTION: Department and Turnover interact (p =", round(age_interaction_p, 4), ")\n")
} else {
  cat("NO SIGNIFICANT INTERACTION: Effects are independent (p =", round(age_interaction_p, 4), ")\n")
}

# 2.3 TENURE BY DEPARTMENT × TURNOVER
cat("\n--- TENURE BY DEPARTMENT × TURNOVER ---\n")
tenure_twoway <- aov(Tenure_Years ~ Department * Turnover, data = data1)
tenure_twoway_summary <- summary(tenure_twoway)
print(tenure_twoway_summary)

tenure_dept_p <- tenure_twoway_summary[[1]]$`Pr(>F)`[1]
tenure_turnover_p <- tenure_twoway_summary[[1]]$`Pr(>F)`[2]
tenure_interaction_p <- tenure_twoway_summary[[1]]$`Pr(>F)`[3]

cat("\nInterpretation:\n")
if(tenure_interaction_p < 0.05) {
  cat("SIGNIFICANT INTERACTION: Department and Turnover interact (p =", round(tenure_interaction_p, 4), ")\n")
} else {
  cat("NO SIGNIFICANT INTERACTION: Effects are independent (p =", round(tenure_interaction_p, 4), ")\n")
}

# 3. SUMMARY OF ANOVA FINDINGS
cat("\n\n3. SUMMARY OF ANOVA FINDINGS FOR TURNOVER CAUSES\n")
cat("================================================\n")

cat("KEY INSIGHTS:\n")
cat("• One-Way ANOVA identifies which variables differ across departments\n")
cat("• Two-Way ANOVA reveals interaction effects between department and turnover\n")
cat("• Significant interactions suggest department-specific turnover patterns\n")
cat("• This helps identify which departments are most at risk for turnover\n\n")

cat("RECOMMENDATIONS:\n")
cat("• Focus on departments with significant differences in key variables\n")
cat("• Investigate interaction effects to understand department-specific risks\n")
cat("• Use post-hoc tests to identify specific department pairs with differences\n")
cat("• Consider department-specific interventions based on ANOVA results\n")


