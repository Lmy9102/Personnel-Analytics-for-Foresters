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
# Department Turnover and Visualization
# ============================================================================

# Overall turnover rate
turnover_rate <- mean(data1$Turnover == 1)
cat("Overall Turnover Rate:", round(turnover_rate * 100, 1), "%\n")
cat("Employees who left:", sum(data1$Turnover == 1), "\n")
cat("Employees who stayed:", sum(data1$Turnover == 0), "\n")


# Turnover by department
dept_turnover <- data1 %>%
  group_by(Department) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = paste0(round(mean(Turnover == 1) * 100, 1), "%"),
    .groups = 'drop'
  ) %>%
  arrange(desc(Turnover_Rate))

# Add total row
overall <- data1 %>%
  summarise(
    Department = "Total",
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = paste0(round(mean(Turnover == 1) * 100, 1), "%")
  )
dept_turnover <- bind_rows(dept_turnover, overall)

# Create a table
dept_turnover_table <- flextable(dept_turnover) %>%
  autofit() %>%
  bold(i = nrow(dept_turnover), bold = TRUE) %>%   # bold the total row
  align(j = 2:4, align = "center") %>%             # center numeric columns
  set_header_labels(
    Department = "Department",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
dept_turnover_table

# Histogram: Turnover Rate by Department
dept_turnover_plot <- dept_turnover %>%
  filter(Department != "Total") %>%  # Remove total row for plotting
  mutate(Department = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales"))) %>%
  ggplot(aes(x = Department, y = as.numeric(gsub("%", "", Turnover_Rate)), fill = Department)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = Turnover_Rate), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Department",
    subtitle = "Percentage of employees who left by department",
    x = "Department",
    y = "Turnover Rate (%)",
    fill = "Department"
    #caption = "Data: Employee Turnover Dataset"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    plot.caption = element_text(size = 12),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("HR" = "#E31A1C", "Finance" = "#1F78B4", "IT" = "#33A02C", 
                               "Operations" = "#FF7F00", "Sales" = "#6A3D9A")) +
  scale_y_continuous(limits = c(0, max(as.numeric(gsub("%", "", dept_turnover$Turnover_Rate[dept_turnover$Department != "Total"]))) * 1.1))

print(dept_turnover_plot)

# 2. TURNOVER RATE BY AGE GROUPS
cat("\n--- Turnover Rate by Age Groups ---\n")

# Create age groups
data1 <- data1 %>%
  mutate(Age_Group = case_when(
    Age < 30 ~ "Under 30",
    Age >= 30 & Age < 40 ~ "30-39",
    Age >= 40 & Age < 50 ~ "40-49",
    Age >= 50 ~ "50+"
  ))

# Calculate turnover rate by age group
age_turnover <- data1 %>%
  group_by(Age_Group) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Age_Group = factor(Age_Group, levels = c("Under 30", "30-39", "40-49", "50+"))) %>%
  arrange(Age_Group)

print(age_turnover)

# Create flextable for age turnover with total row
age_turnover_with_total <- age_turnover %>%
  mutate(Turnover_Rate = paste0(Turnover_Rate, "%")) %>%
  bind_rows(
    age_turnover %>%
      summarise(
        Age_Group = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

age_turnover_table <- age_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(age_turnover_with_total), part = "body") %>%
  set_header_labels(
    Age_Group = "Age Group",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
age_turnover_table

# Histogram for age groups
age_turnover_plot <- age_turnover %>%
  ggplot(aes(x = Age_Group, y = Turnover_Rate, fill = Age_Group)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Age Group",
    subtitle = "Percentage of employees who left by age group",
    x = "Age Group",
    y = "Turnover Rate (%)",
    fill = "Age Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("Under 30" = "#FF6B6B", "30-39" = "#4ECDC4", "40-49" = "#45B7D1", "50+" = "#96CEB4")) +
  scale_y_continuous(limits = c(0, max(age_turnover$Turnover_Rate) * 1.1))

print(age_turnover_plot)

# 3. TURNOVER RATE BY TENURE GROUPS
cat("\n--- Turnover Rate by Tenure Groups ---\n")

# Create tenure groups
data1 <- data1 %>%
  mutate(Tenure_Group = case_when(
    Tenure_Years < 1 ~ "<1 year",
    Tenure_Years >= 1 & Tenure_Years < 3 ~ "1-3 years",
    Tenure_Years >= 3 & Tenure_Years < 5 ~ "3-5 years",
    Tenure_Years >= 5 ~ "5+ years"
  ))

# Calculate turnover rate by tenure group
tenure_turnover <- data1 %>%
  group_by(Tenure_Group) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Tenure_Group = factor(Tenure_Group, levels = c("<1 year", "1-3 years", "3-5 years", "5+ years")))

print(tenure_turnover)

# Create flextable for tenure turnover with total row
tenure_turnover_with_total <- tenure_turnover %>%
  mutate(Turnover_Rate = paste0(Turnover_Rate, "%")) %>%
  bind_rows(
    tenure_turnover %>%
      summarise(
        Tenure_Group = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

tenure_turnover_table <- tenure_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(tenure_turnover_with_total), part = "body") %>%
  set_header_labels(
    Tenure_Group = "Tenure Group",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
tenure_turnover_table

# Histogram for tenure groups
tenure_turnover_plot <- tenure_turnover %>%
  ggplot(aes(x = Tenure_Group, y = Turnover_Rate, fill = Tenure_Group)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Tenure Group",
    subtitle = "Percentage of employees who left by tenure group",
    x = "Tenure Group",
    y = "Turnover Rate (%)",
    fill = "Tenure Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("<1 year" = "#FF9F43", "1-3 years" = "#FF6348", 
                               "3-5 years" = "#FF4757", "5+ years" = "#C44569")) +
  scale_y_continuous(limits = c(0, max(tenure_turnover$Turnover_Rate) * 1.1))

print(tenure_turnover_plot)

# 4. TURNOVER RATE BY JOB SATISFACTION LEVELS
cat("\n--- Turnover Rate by Job Satisfaction Levels ---\n")

# Calculate turnover rate by job satisfaction
satisfaction_turnover <- data1 %>%
  group_by(Job_Satisfaction) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(Job_Satisfaction)

print(satisfaction_turnover)

# Create flextable for satisfaction turnover with total row
satisfaction_turnover_with_total <- satisfaction_turnover %>%
  mutate(
    Job_Satisfaction = as.character(Job_Satisfaction),
    Turnover_Rate = paste0(Turnover_Rate, "%")
  ) %>%
  bind_rows(
    satisfaction_turnover %>%
      summarise(
        Job_Satisfaction = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

satisfaction_turnover_table <- satisfaction_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(satisfaction_turnover_with_total), part = "body") %>%
  set_header_labels(
    Job_Satisfaction = "Job Satisfaction Level",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
satisfaction_turnover_table

# Histogram for job satisfaction
satisfaction_turnover_plot <- satisfaction_turnover %>%
  ggplot(aes(x = factor(Job_Satisfaction), y = Turnover_Rate, fill = factor(Job_Satisfaction))) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Job Satisfaction Level",
    subtitle = "Percentage of employees who left by job satisfaction level",
    x = "Job Satisfaction Level",
    y = "Turnover Rate (%)",
    fill = "Satisfaction"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("1" = "#E74C3C", "2" = "#F39C12", "3" = "#F1C40F", 
                               "4" = "#2ECC71", "5" = "#27AE60"),
                    labels = c("1" = "Very Unsatisfied", "2" = "Unsatisfied", 
                              "3" = "Neither Satisfied nor Unsatisfied", 
                              "4" = "Satisfied", "5" = "Very Satisfied")) +
  scale_x_discrete(limits = c("1", "2", "3", "4", "5")) +
  scale_y_continuous(limits = c(0, max(satisfaction_turnover$Turnover_Rate) * 1.1))

print(satisfaction_turnover_plot)

# 5. TURNOVER RATE BY WORK HOURS GROUPS
cat("\n--- Turnover Rate by Work Hours Groups ---\n")

# Create work hours groups
data1 <- data1 %>%
  mutate(Hours_Group = case_when(
    Avg_Hours_Week < 40 ~ "< 40",
    Avg_Hours_Week >= 40 & Avg_Hours_Week < 45 ~ "40-44",
    Avg_Hours_Week >= 45 & Avg_Hours_Week < 50 ~ "45-49",
    Avg_Hours_Week >= 50 ~ "50+"
  ))

# Calculate turnover rate by work hours group
hours_turnover <- data1 %>%
  filter(!is.na(Hours_Group)) %>%
  group_by(Hours_Group) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Hours_Group = factor(Hours_Group, levels = c("< 40", "40-44", "45-49", "50+"))) %>%
  arrange(Hours_Group)

print(hours_turnover)

# Create flextable for hours turnover with total row
hours_turnover_with_total <- hours_turnover %>%
  mutate(Turnover_Rate = paste0(Turnover_Rate, "%")) %>%
  bind_rows(
    hours_turnover %>%
      summarise(
        Hours_Group = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

hours_turnover_table <- hours_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(hours_turnover_with_total), part = "body") %>%
  set_header_labels(
    Hours_Group = "Work Hours Group",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
hours_turnover_table

# Histogram for work hours groups
hours_turnover_plot <- hours_turnover %>%
  ggplot(aes(x = Hours_Group, y = Turnover_Rate, fill = Hours_Group)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Work Hours Group",
    subtitle = "Percentage of employees who left by work hours group",
    x = "Work Hours Group",
    y = "Turnover Rate (%)",
    fill = "Hours Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("< 40" = "#FF6B6B", "40-44" = "#4ECDC4", 
                               "45-49" = "#45B7D1", "50+" = "#96CEB4"), 
                    drop = FALSE) +
  scale_y_continuous(limits = c(0, max(hours_turnover$Turnover_Rate) * 1.1))

print(hours_turnover_plot)

# 6. TURNOVER RATE BY SALARY LEVEL
cat("\n--- Turnover Rate by Salary Level ---\n")

# Calculate turnover rate by salary level
salary_turnover <- data1 %>%
  group_by(Salary_Level) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Salary_Level = factor(Salary_Level, levels = c("Low", "Medium", "High")))

print(salary_turnover)

# Create flextable for salary turnover with total row
salary_turnover_with_total <- salary_turnover %>%
  mutate(Turnover_Rate = paste0(Turnover_Rate, "%")) %>%
  bind_rows(
    salary_turnover %>%
      summarise(
        Salary_Level = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

salary_turnover_table <- salary_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(salary_turnover_with_total), part = "body") %>%
  set_header_labels(
    Salary_Level = "Salary Level",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
salary_turnover_table

# Histogram for salary level
salary_turnover_plot <- salary_turnover %>%
  ggplot(aes(x = Salary_Level, y = Turnover_Rate, fill = Salary_Level)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Salary Level",
    subtitle = "Percentage of employees who left by salary level",
    x = "Salary Level",
    y = "Turnover Rate (%)",
    fill = "Salary Level"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("Low" = "#E74C3C", "Medium" = "#3498DB", "High" = "#2ECC71")) +
  scale_x_discrete(limits = c("Low", "Medium", "High")) +
  scale_y_continuous(limits = c(0, max(salary_turnover$Turnover_Rate) * 1.1))

print(salary_turnover_plot)

# 7. TURNOVER RATE BY TRAINING HOURS GROUPS
cat("\n--- Turnover Rate by Training Hours Groups ---\n")

# Create training hours groups
data1 <- data1 %>%
  mutate(Training_Group = case_when(
    Training_Hours_Year < 10 ~ "< 10",
    Training_Hours_Year >= 10 & Training_Hours_Year < 20 ~ "10-19",
    Training_Hours_Year >= 20 & Training_Hours_Year < 30 ~ "20-29",
    Training_Hours_Year >= 30 & Training_Hours_Year <= 40 ~ "30-40",
    Training_Hours_Year > 40 ~ "> 40",
    TRUE ~ NA_character_
  ))

# Debug: Check Training_Group values
cat("Training_Group values after creation:\n")
print(table(data1$Training_Group, useNA = "always"))

# Calculate turnover rate by training hours group
training_turnover <- data1 %>%
  filter(!is.na(Training_Group)) %>%
  group_by(Training_Group) %>%
  summarise(
    Total_Employees = n(),
    Turnover_Count = sum(Turnover == 1),
    Turnover_Rate = round(mean(Turnover == 1) * 100, 1),
    .groups = 'drop'
  ) %>%
  mutate(Training_Group = factor(Training_Group, levels = c("< 10", "10-19", "20-29", "30-40", "> 40"))) %>%
  arrange(Training_Group)

print(training_turnover)

# Create flextable for training turnover with total row
training_turnover_with_total <- training_turnover %>%
  mutate(Turnover_Rate = paste0(Turnover_Rate, "%")) %>%
  bind_rows(
    training_turnover %>%
      summarise(
        Training_Group = "Total",
        Total_Employees = sum(Total_Employees),
        Turnover_Count = sum(Turnover_Count),
        Turnover_Rate = paste0(round(sum(Turnover_Count) / sum(Total_Employees) * 100, 1), "%")
      )
  )

training_turnover_table <- training_turnover_with_total %>%
  flextable() %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  bold(part = "header") %>%
  bold(i = nrow(training_turnover_with_total), part = "body") %>%
  set_header_labels(
    Training_Group = "Training Hours Group",
    Total_Employees = "Total Count",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
training_turnover_table

# Histogram for training hours groups
training_turnover_plot <- training_turnover %>%
  ggplot(aes(x = Training_Group, y = Turnover_Rate, fill = Training_Group)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Turnover_Rate, "%")), vjust = -0.5, size = 8, fontface = "bold") +
  labs(
    title = "Turnover Rate by Training Hours Group",
    subtitle = "Percentage of employees who left by training hours group",
    x = "Training Hours Group",
    y = "Turnover Rate (%)",
    fill = "Training Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 20),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20),
    legend.position = "right",
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.5,
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20))
  ) +
  scale_fill_manual(values = c("< 10" = "#E74C3C", "10-19" = "#F39C12", 
                               "20-29" = "#F1C40F", "30-40" = "#2ECC71", "> 40" = "#3498DB"), 
                    drop = FALSE) +
  scale_y_continuous(limits = c(0, max(training_turnover$Turnover_Rate) * 1.1))

print(training_turnover_plot)

