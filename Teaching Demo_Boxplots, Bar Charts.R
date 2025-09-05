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
# HEADCOUNT BY DEPARTMENT
# ============================================================================

# Headcount by department (Pie chart)
dept_pct <- data1 %>%
  group_by(Department) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(
    Percent = Count / sum(Count) * 100,  # percentage of 100
    Fraction = Count / sum(Count)        # fraction adding to 1
  )

ggplot(dept_pct, aes(x = "", y = Percent, fill = Department)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Percent, 1), "%")), 
            position = position_stack(vjust = 0.5),
            size = 8,      
            fontface = "bold") +
  labs(title = "Employee Distribution by Department (Percent of Total)",
       fill = "Department") +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),     
    legend.title = element_text(size = 22, face = "bold"),    
    legend.text = element_text(size = 22)                     
  )


# ============================================================================
# OVERALL TURNOVER AND DEPARTMENT TURNOVER
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
    Total_Employees = "Total Employees",
    Turnover_Count = "Turnover Count",
    Turnover_Rate = "Turnover Rate"
  )
dept_turnover_table

# Stacked bar chart by department 
## Remove Total row and convert Turnover_Rate to numeric
dept_order <- dept_turnover %>%
  filter(Department != "Total") %>%
  mutate(Turnover_Rate_Num = as.numeric(gsub("%", "", Turnover_Rate))) %>%
  arrange(Turnover_Rate_Num) %>%
  pull(Department)

## Prepare data for stacked bar chart
data2 <- data1 %>%
  mutate(
    Turnover_Flag = factor(Turnover, levels = c(0,1), labels = c("Stayed","Left")),
    Department = factor(Department, levels = dept_order)  # order departments by ascending turnover
  ) %>%
  group_by(Department, Turnover_Flag) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Department) %>%
  mutate(Percent = Count / sum(Count) * 100)

## Create stacked bar chart
ggplot(data2, aes(x = Department, y = Count, fill = Turnover_Flag)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Turnover_Flag == "Left", paste0(round(Percent,1),"%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 6,
            fontface = "bold") +
  scale_fill_manual(values = c("Stayed" = "#0e8825", "Left" = "#c70909")) +
  labs(title = "Turnover by Department (Ascending Order of Turnover Rate)",
       x = "Department",
       y = "Number of Employees",
       fill = "Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 22, face = "bold"),
    axis.text.y = element_text(size = 22, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

summary(data1)

department_summary <- data1 %>%
  group_by(Department) %>%
  summarise(
    Mean_Age = round(mean(Age), 1),
    Mean_Tenure = round(mean(Tenure_Years), 1),
    Mean_Job_Satisfaction = round(mean(Job_Satisfaction), 1),
    Mean_Hours = round(mean(Avg_Hours_Week), 1),
    Mean_Training = round(mean(Training_Hours_Year), 1)
  ) %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

# Display the entire table using flextable format
department_summary_table <- flextable(department_summary) %>%
  autofit() %>%
  align(j = 2:6, align = "center") %>%             # center numeric columns
  set_header_labels(
    Department = "Department",
    Mean_Age = "Mean Age",
    Mean_Tenure = "Mean Tenure",
    Mean_Job_Satisfaction = "Mean Job Satisfaction",
    Mean_Hours = "Mean Hours",
    Mean_Training = "Mean Training"
  )
department_summary_table

# Create median table for each department
department_median <- data1 %>%
  group_by(Department) %>%
  summarise(
    Median_Age = round(median(Age), 1),
    Median_Tenure = round(median(Tenure_Years), 1),
    Median_Job_Satisfaction = round(median(Job_Satisfaction), 1),
    Median_Hours = round(median(Avg_Hours_Week), 1),
    Median_Training = round(median(Training_Hours_Year), 1)
  ) %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

# Display median table using flextable format
department_median_table <- flextable(department_median) %>%
  autofit() %>%
  align(j = 2:6, align = "center") %>%             # center numeric columns
  set_header_labels(
    Department = "Department",
    Median_Age = "Median Age",
    Median_Tenure = "Median Tenure",
    Median_Job_Satisfaction = "Median Job Satisfaction",
    Median_Hours = "Median Hours",
    Median_Training = "Median Training"
  )
department_median_table


# ============================================================================
# SEPARATE BOXPLOTS FOR EACH VARIABLE
# ============================================================================

# 1. AGE BOXPLOT
cat("\n--- Age Boxplot ---\n")
age_data <- data_long %>% filter(Variable == "Age")
age_stats <- age_data %>%
  summarise(
    Mean = round(mean(Value), 1),
    Median = round(median(Value), 1),
    Min = round(min(Value), 1),
    Max = round(max(Value), 1),
    Q1 = round(quantile(Value, 0.25), 1),
    Q3 = round(quantile(Value, 0.75), 1)
  )

p_age <- ggplot(age_data, aes(x = 1, y = Value, fill = "Age")) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = min, geom = "point", shape = 24, size = 4, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 4, color = "orange", fill = "orange") +
  geom_text(data = data.frame(x = 1, y = age_stats$Mean, label = paste0("Mean=", age_stats$Mean)), 
            aes(x = x, y = y, label = label), vjust = -2.5, hjust = 0.5, size = 12, color = "blue", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = age_stats$Median, label = paste0("Med=", age_stats$Median)), 
            aes(x = x, y = y, label = label), vjust = 3.0, hjust = 0.5, size = 12, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = age_stats$Min, label = paste0("Min=", age_stats$Min)), 
            aes(x = x, y = y, label = label), vjust = 1.8, hjust = 0.5, size = 12, color = "darkgreen", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = age_stats$Max, label = paste0("Max=", age_stats$Max)), 
            aes(x = x, y = y, label = label), vjust = -0.8, hjust = 0.5, size = 12, color = "orange", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = age_stats$Q1, label = paste0("Q1=", age_stats$Q1)), 
            aes(x = x, y = y, label = label), vjust = 2.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = age_stats$Q3, label = paste0("Q3=", age_stats$Q3)), 
            aes(x = x, y = y, label = label), vjust = -1.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  labs(title = "Box Plot: Age", subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max", x = "Age", y = "Value") +
  theme_minimal(base_size = 24) + theme(legend.position = "none", axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
                                        plot.title = element_text(size = 32, face = "bold"), plot.subtitle = element_text(size = 20, color = "gray50"),
                                        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") + scale_fill_manual(values = c("Age" = "#FF9999"))

print(p_age)

# 2. TENURE BOXPLOT
cat("\n--- Tenure Boxplot ---\n")
tenure_data <- data_long %>% filter(Variable == "Tenure_Years")
tenure_stats <- tenure_data %>%
  summarise(
    Mean = round(mean(Value), 1),
    Median = round(median(Value), 1),
    Min = round(min(Value), 1),
    Max = round(max(Value), 1),
    Q1 = round(quantile(Value, 0.25), 1),
    Q3 = round(quantile(Value, 0.75), 1)
  )

p_tenure <- ggplot(tenure_data, aes(x = 1, y = Value, fill = "Tenure_Years")) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = min, geom = "point", shape = 24, size = 4, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 4, color = "orange", fill = "orange") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Mean, label = paste0("Mean=", tenure_stats$Mean)), 
            aes(x = x, y = y, label = label), vjust = -2.5, hjust = 0.5, size = 12, color = "blue", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Median, label = paste0("Med=", tenure_stats$Median)), 
            aes(x = x, y = y, label = label), vjust = 3.0, hjust = 0.5, size = 12, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Min, label = paste0("Min=", tenure_stats$Min)), 
            aes(x = x, y = y, label = label), vjust = 1.8, hjust = 0.5, size = 12, color = "darkgreen", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Max, label = paste0("Max=", tenure_stats$Max)), 
            aes(x = x, y = y, label = label), vjust = -0.8, hjust = 0.5, size = 12, color = "orange", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Q1, label = paste0("Q1=", tenure_stats$Q1)), 
            aes(x = x, y = y, label = label), vjust = 2.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = tenure_stats$Q3, label = paste0("Q3=", tenure_stats$Q3)), 
            aes(x = x, y = y, label = label), vjust = -1.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  labs(title = "Box Plot: Tenure", subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max", x = "Tenure (Years)", y = "Value") +
  theme_minimal(base_size = 24) + theme(legend.position = "none", axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
                                        plot.title = element_text(size = 32, face = "bold"), plot.subtitle = element_text(size = 20, color = "gray50"),
                                        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") + scale_fill_manual(values = c("Tenure_Years" = "#66B2FF"))

print(p_tenure)

# 3. JOB SATISFACTION BOXPLOT
cat("\n--- Job Satisfaction Boxplot ---\n")
satisfaction_data <- data_long %>% filter(Variable == "Job_Satisfaction")
satisfaction_stats <- satisfaction_data %>%
  summarise(
    Mean = round(mean(Value), 1),
    Median = round(median(Value), 1),
    Min = round(min(Value), 1),
    Max = round(max(Value), 1),
    Q1 = round(quantile(Value, 0.25), 1),
    Q3 = round(quantile(Value, 0.75), 1)
  )

p_satisfaction <- ggplot(satisfaction_data, aes(x = 1, y = Value, fill = "Job_Satisfaction")) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = min, geom = "point", shape = 24, size = 4, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 4, color = "orange", fill = "orange") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Mean, label = paste0("Mean=", satisfaction_stats$Mean)), 
            aes(x = x, y = y, label = label), vjust = -2.5, hjust = 0.5, size = 12, color = "blue", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Median, label = paste0("Med=", satisfaction_stats$Median)), 
            aes(x = x, y = y, label = label), vjust = 3.0, hjust = 0.5, size = 12, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Min, label = paste0("Min=", satisfaction_stats$Min)), 
            aes(x = x, y = y, label = label), vjust = 1.8, hjust = 0.5, size = 12, color = "darkgreen", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Max, label = paste0("Max=", satisfaction_stats$Max)), 
            aes(x = x, y = y, label = label), vjust = -0.8, hjust = 0.5, size = 12, color = "orange", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Q1, label = paste0("Q1=", satisfaction_stats$Q1)), 
            aes(x = x, y = y, label = label), vjust = 3.5, hjust = -0.3, size = 12, color = "purple", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = satisfaction_stats$Q3, label = paste0("Q3=", satisfaction_stats$Q3)), 
            aes(x = x, y = y, label = label), vjust = -1.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  labs(title = "Box Plot: Job Satisfaction", subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max", x = "Job Satisfaction", y = "Value") +
  theme_minimal(base_size = 24) + theme(legend.position = "none", axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
                                        plot.title = element_text(size = 32, face = "bold"), plot.subtitle = element_text(size = 20, color = "gray50"),
                                        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") + scale_fill_manual(values = c("Job_Satisfaction" = "#99FF99"))

print(p_satisfaction)

# 4. HOURS BOXPLOT
cat("\n--- Hours Boxplot ---\n")
hours_data <- data_long %>% filter(Variable == "Avg_Hours_Week")
hours_stats <- hours_data %>%
  summarise(
    Mean = round(mean(Value), 1),
    Median = round(median(Value), 1),
    Min = round(min(Value), 1),
    Max = round(max(Value), 1),
    Q1 = round(quantile(Value, 0.25), 1),
    Q3 = round(quantile(Value, 0.75), 1)
  )

p_hours <- ggplot(hours_data, aes(x = 1, y = Value, fill = "Avg_Hours_Week")) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = min, geom = "point", shape = 24, size = 4, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 4, color = "orange", fill = "orange") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Mean, label = paste0("Mean=", hours_stats$Mean)), 
            aes(x = x, y = y, label = label), vjust = -2.5, hjust = 0.5, size = 12, color = "blue", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Median, label = paste0("Med=", hours_stats$Median)), 
            aes(x = x, y = y, label = label), vjust = 3.0, hjust = 0.5, size = 12, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Min, label = paste0("Min=", hours_stats$Min)), 
            aes(x = x, y = y, label = label), vjust = 1.8, hjust = 0.5, size = 12, color = "darkgreen", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Max, label = paste0("Max=", hours_stats$Max)), 
            aes(x = x, y = y, label = label), vjust = -0.8, hjust = 0.5, size = 12, color = "orange", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Q1, label = paste0("Q1=", hours_stats$Q1)), 
            aes(x = x, y = y, label = label), vjust = 2.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = hours_stats$Q3, label = paste0("Q3=", hours_stats$Q3)), 
            aes(x = x, y = y, label = label), vjust = -1.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  labs(title = "Box Plot: Average Hours per Week", subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max", x = "Average Hours per Week", y = "Value") +
  theme_minimal(base_size = 24) + theme(legend.position = "none", axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
                                        plot.title = element_text(size = 32, face = "bold"), plot.subtitle = element_text(size = 20, color = "gray50"),
                                        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") + scale_fill_manual(values = c("Avg_Hours_Week" = "#FFCC99"))

print(p_hours)

# 5. TRAINING BOXPLOT
cat("\n--- Training Hours Boxplot ---\n")
training_data <- data_long %>% filter(Variable == "Training_Hours_Year")
training_stats <- training_data %>%
  summarise(
    Mean = round(mean(Value), 1),
    Median = round(median(Value), 1),
    Min = round(min(Value), 1),
    Max = round(max(Value), 1),
    Q1 = round(quantile(Value, 0.25), 1),
    Q3 = round(quantile(Value, 0.75), 1)
  )

p_training <- ggplot(training_data, aes(x = 1, y = Value, fill = "Training_Hours_Year")) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 6, color = "blue", fill = "blue") +
  stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "black") +
  stat_summary(fun = min, geom = "point", shape = 24, size = 4, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 4, color = "orange", fill = "orange") +
  geom_text(data = data.frame(x = 1, y = training_stats$Mean, label = paste0("Mean=", training_stats$Mean)), 
            aes(x = x, y = y, label = label), vjust = -2.5, hjust = 0.5, size = 12, color = "blue", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = training_stats$Median, label = paste0("Med=", training_stats$Median)), 
            aes(x = x, y = y, label = label), vjust = 3.0, hjust = 0.5, size = 12, color = "black", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = training_stats$Min, label = paste0("Min=", training_stats$Min)), 
            aes(x = x, y = y, label = label), vjust = 1.8, hjust = 0.5, size = 12, color = "darkgreen", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = training_stats$Max, label = paste0("Max=", training_stats$Max)), 
            aes(x = x, y = y, label = label), vjust = -0.8, hjust = 0.5, size = 12, color = "orange", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = training_stats$Q1, label = paste0("Q1=", training_stats$Q1)), 
            aes(x = x, y = y, label = label), vjust = 2.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  geom_text(data = data.frame(x = 1, y = training_stats$Q3, label = paste0("Q3=", training_stats$Q3)), 
            aes(x = x, y = y, label = label), vjust = -1.2, hjust = 0.2, size = 12, color = "purple", fontface = "bold") +
  labs(title = "Box Plot: Training Hours per Year", subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max", x = "Training Hours per Year", y = "Value") +
  theme_minimal(base_size = 24) + theme(legend.position = "none", axis.text.x = element_text(size = 24), axis.text.y = element_text(size = 24),
                                        plot.title = element_text(size = 32, face = "bold"), plot.subtitle = element_text(size = 20, color = "gray50"),
                                        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(clip = "off") + scale_fill_manual(values = c("Training_Hours_Year" = "#CC99FF"))

print(p_training)



# ============================================================================
# COMBINED BOXPLOT
# ============================================================================
# Select the required columns and reshape to long format for faceted plotting
data_long <- data1 %>%
  select(Age, Tenure_Years, Job_Satisfaction, Avg_Hours_Week, Training_Hours_Year) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Calculate comprehensive summary statistics for each variable
summary_stats <- data_long %>%
  group_by(Variable) %>%
  summarise(
    Mean = round(mean(Value), 2),
    Median = round(median(Value), 2),
    Std_Dev = round(sd(Value), 2),
    Min = round(min(Value), 2),
    Max = round(max(Value), 2),
    Q1 = round(quantile(Value, 0.25), 2),
    Q3 = round(quantile(Value, 0.75), 2),
    .groups = 'drop'
  )
print(summary_stats)

# Generate box plots with values displayed at their exact locations
ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  # Add mean points with values
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "blue", fill = "blue") +
  stat_summary(fun = mean, geom = "text", aes(label = paste0("Mean=", round(..y.., 1))), 
               vjust = -1.0, hjust = -0.2, size = 3, color = "blue", fontface = "bold") +
  # Add median values (displayed on the box plot line)
  stat_summary(fun = median, geom = "text", aes(label = paste0("Med=", round(..y.., 1))), 
               vjust = 2.0, hjust = 1.2, size = 3, color = "black", fontface = "bold") +
  # Add min/max points with values
  stat_summary(fun = min, geom = "point", shape = 24, size = 2, color = "darkgreen", fill = "darkgreen") +
  stat_summary(fun = min, geom = "text", aes(label = paste0("Min=", round(..y.., 1))), 
               vjust = 2.5, hjust = 0.5, size = 3, color = "darkgreen", fontface = "bold") +
  stat_summary(fun = max, geom = "point", shape = 25, size = 2, color = "orange", fill = "orange") +
  stat_summary(fun = max, geom = "text", aes(label = paste0("Max=", round(..y.., 1))), 
               vjust = -1.5, hjust = 0.5, size = 3, color = "orange", fontface = "bold") +
  # Add Q1 and Q3 values
  stat_summary(fun = function(x) quantile(x, 0.25), geom = "text", 
               aes(label = paste0("Q1=", round(..y.., 1))), 
               vjust = 1.8, hjust = -0.3, size = 3, color = "purple", fontface = "bold") +
  stat_summary(fun = function(x) quantile(x, 0.75), geom = "text", 
               aes(label = paste0("Q3=", round(..y.., 1))), 
               vjust = -0.8, hjust = -0.3, size = 3, color = "purple", fontface = "bold") +
  labs(title = "Box Plots with Values at Exact Locations",
       subtitle = "Blue diamond = Mean, Green triangle = Min, Orange triangle = Max",
       x = "Variable",
       y = "Value") +
  theme_minimal(base_size = 16) +  
  theme(legend.position = "none",  # Hide legend since Variable names are on x-axis
        axis.text.x = element_text(size = 14, angle = 15, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50"),
        panel.grid.major.x = element_blank(),  # Remove vertical grid lines
        panel.grid.minor = element_blank()) +  # Remove minor grid lines
  # Ensure all data points and labels are visible
  coord_cartesian(clip = "off") +
  # Add very granular y-axis breaks in units of 5
  scale_y_continuous(breaks = function(x) {
    min_val <- min(x)
    max_val <- max(x)
    # Create breaks in units of 5
    seq(round(min_val/5)*5, round(max_val/5)*5 + 5, by = 5)
  })



# ============================================================================
# HISTOGRAMS
# ============================================================================

# 1. JOB SATISFACTION HISTOGRAM
cat("\n--- Job Satisfaction Histogram ---\n")
p_satisfaction_hist <- ggplot(data1, aes(x = Job_Satisfaction, fill = factor(Job_Satisfaction))) +
  geom_bar(stat = "count", color = "black", linewidth = 0.5) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 8, fontface = "bold") +
  labs(title = "Distribution of Job Satisfaction Scores",
       subtitle = "Count of employees for each satisfaction level",
       x = "Job Satisfaction Score",
       y = "Count of Employees") +
  scale_fill_manual(values = c("1" = "#FF6B6B", "2" = "#FFA07A", "3" = "#FFD93D", "4" = "#6BCF7F", "5" = "#4ECDC4"),
                    labels = c("1" = "1 (Very Unsatisfied)", "2" = "2 (Unsatisfied)", "3" = "3 (Neutral)", "4" = "4 (Satisfied)", "5" = "5 (Very Satisfied)"),
                    name = "Satisfaction Level") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 20, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_satisfaction_hist)

# 2. SALARY LEVEL HISTOGRAM
cat("\n--- Salary Level Histogram ---\n")
p_salary_hist <- ggplot(data1, aes(x = factor(Salary_Level, levels = c("Low", "Medium", "High")), fill = factor(Salary_Level, levels = c("Low", "Medium", "High")))) +
  geom_bar(stat = "count", color = "black", linewidth = 0.5) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 8, fontface = "bold") +
  labs(title = "Distribution of Salary Levels",
       subtitle = "Count of employees for each salary level",
       x = "Salary Level",
       y = "Count of Employees") +
  scale_fill_manual(values = c("Low" = "#FF6B6B", "Medium" = "#FFD93D", "High" = "#4ECDC4"),
                    name = "Salary Level") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "right",
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 32, face = "bold"),
        plot.subtitle = element_text(size = 20, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_salary_hist)


# ============================================================================
# CONSOLIDATED BOXPLOTS BY DEPARTMENT
# ============================================================================

# 1. AGE BY DEPARTMENT
cat("\n--- Age Distribution by Department ---\n")
# Calculate means for each department
age_means <- data1 %>%
  group_by(Department) %>%
  summarise(Mean_Age = round(mean(Age), 1), .groups = 'drop') %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

p_age_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                 y = Age, fill = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "blue", fill = "blue") +
  geom_text(data = age_means, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                  y = Mean_Age, label = paste0("Mean=", Mean_Age)), 
            vjust = -1.5, hjust = 0.5, size = 6, color = "blue", fontface = "bold") +
  labs(title = "Age Distribution by Department",
       subtitle = "Box plots showing age spread across different departments (Blue diamond = Mean)",
       x = "Department",
       y = "Age") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("HR" = "#FF6B6B", "Finance" = "#FFEAA7", "IT" = "#4ECDC4", 
                               "Operations" = "#45B7D1", "Sales" = "#96CEB4")) +
  coord_cartesian(clip = "off")

print(p_age_dept)

# 2. TENURE BY DEPARTMENT
cat("\n--- Tenure Distribution by Department ---\n")
# Calculate means for each department
tenure_means <- data1 %>%
  group_by(Department) %>%
  summarise(Mean_Tenure = round(mean(Tenure_Years), 1), .groups = 'drop') %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

p_tenure_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                    y = Tenure_Years, fill = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "blue", fill = "blue") +
  geom_text(data = tenure_means, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                     y = Mean_Tenure, label = paste0("Mean=", Mean_Tenure)), 
            vjust = -1.5, hjust = 0.5, size = 6, color = "blue", fontface = "bold") +
  labs(title = "Tenure Distribution by Department",
       subtitle = "Box plots showing years of service across different departments (Blue diamond = Mean)",
       x = "Department",
       y = "Tenure (Years)") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("HR" = "#FF6B6B", "Finance" = "#FFEAA7", "IT" = "#4ECDC4", 
                               "Operations" = "#45B7D1", "Sales" = "#96CEB4")) +
  coord_cartesian(clip = "off")

print(p_tenure_dept)

# 3. HOURS BY DEPARTMENT
cat("\n--- Average Hours per Week by Department ---\n")
# Calculate means for each department
hours_means <- data1 %>%
  group_by(Department) %>%
  summarise(Mean_Hours = round(mean(Avg_Hours_Week), 1), .groups = 'drop') %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

p_hours_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                   y = Avg_Hours_Week, fill = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "blue", fill = "blue") +
  geom_text(data = hours_means, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                    y = Mean_Hours, label = paste0("Mean=", Mean_Hours)), 
            vjust = -1.5, hjust = 0.5, size = 6, color = "blue", fontface = "bold") +
  labs(title = "Average Hours per Week by Department",
       subtitle = "Box plots showing work hours across different departments (Blue diamond = Mean)",
       x = "Department",
       y = "Average Hours per Week") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("HR" = "#FF6B6B", "Finance" = "#FFEAA7", "IT" = "#4ECDC4", 
                               "Operations" = "#45B7D1", "Sales" = "#96CEB4")) +
  coord_cartesian(clip = "off")

print(p_hours_dept)

# 4. TRAINING HOURS BY DEPARTMENT
cat("\n--- Training Hours per Year by Department ---\n")
# Calculate means for each department
training_means <- data1 %>%
  group_by(Department) %>%
  summarise(Mean_Training = round(mean(Training_Hours_Year), 1), .groups = 'drop') %>%
  arrange(factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))

p_training_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                      y = Training_Hours_Year, fill = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")))) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 4, color = "blue", fill = "blue") +
  geom_text(data = training_means, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                       y = Mean_Training, label = paste0("Mean=", Mean_Training)), 
            vjust = -1.5, hjust = 0.5, size = 6, color = "blue", fontface = "bold") +
  labs(title = "Training Hours per Year by Department",
       subtitle = "Box plots showing training investment across different departments (Blue diamond = Mean)",
       x = "Department",
       y = "Training Hours per Year") +
  theme_minimal(base_size = 24) +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = c("HR" = "#FF6B6B", "Finance" = "#FFEAA7", "IT" = "#4ECDC4", 
                               "Operations" = "#45B7D1", "Sales" = "#96CEB4")) +
  coord_cartesian(clip = "off")

print(p_training_dept)



# ============================================================================
# CONSOLIDATED BAR CHARTS BY DEPARTMENT
# ============================================================================

# 1. JOB SATISFACTION BY DEPARTMENT
cat("\n--- Job Satisfaction Distribution by Department ---\n")
p_satisfaction_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                          fill = factor(Job_Satisfaction))) +
  geom_bar(position = "dodge", color = "black", linewidth = 0.5) +
  labs(title = "Job Satisfaction by Department",
       subtitle = "Distribution of satisfaction levels across departments",
       x = "Department",
       y = "Count of Employees",
       fill = "Satisfaction Level") +
  scale_fill_manual(values = c("1" = "#FF6B6B", "2" = "#FFA07A", "3" = "#FFD93D", "4" = "#6BCF7F", "5" = "#4ECDC4"),
                    labels = c("1" = "1 (Very Unsatisfied)", "2" = "2 (Unsatisfied)", "3" = "3 (Neutral)", 
                               "4" = "4 (Satisfied)", "5" = "5 (Very Satisfied)")) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_satisfaction_dept)

# 2. SALARY LEVEL BY DEPARTMENT
cat("\n--- Salary Level Distribution by Department ---\n")
p_salary_dept <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                    fill = factor(Salary_Level, levels = c("Low", "Medium", "High")))) +
  geom_bar(position = "dodge", color = "black", linewidth = 0.5) +
  labs(title = "Salary Level by Department",
       subtitle = "Distribution of salary levels across departments",
       x = "Department",
       y = "Count of Employees",
       fill = "Salary Level") +
  scale_fill_manual(values = c("Low" = "#FF6B6B", "Medium" = "#FFD93D", "High" = "#4ECDC4")) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_salary_dept)

# 3. JOB SATISFACTION BY DEPARTMENT - STACKED BAR CHART
cat("\n--- Job Satisfaction Distribution by Department (Stacked) ---\n")
p_satisfaction_stacked <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                           fill = factor(Job_Satisfaction))) +
  geom_bar(position = "stack", color = "black", linewidth = 0.5) +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), 
            size = 4, fontface = "bold", color = "white") +
  labs(title = "Job Satisfaction by Department (Stacked)",
       subtitle = "Stacked distribution of satisfaction levels across departments",
       x = "Department",
       y = "Count of Employees",
       fill = "Satisfaction Level") +
  scale_fill_manual(values = c("1" = "#FF6B6B", "2" = "#FFA07A", "3" = "#FFD93D", "4" = "#6BCF7F", "5" = "#4ECDC4"),
                    labels = c("1" = "1 (Very Unsatisfied)", "2" = "2 (Unsatisfied)", "3" = "3 (Neutral)", 
                               "4" = "4 (Satisfied)", "5" = "5 (Very Satisfied)")) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_satisfaction_stacked)

# 4. SALARY LEVEL BY DEPARTMENT - STACKED BAR CHART
cat("\n--- Salary Level Distribution by Department (Stacked) ---\n")
p_salary_stacked <- ggplot(data1, aes(x = factor(Department, levels = c("HR", "Finance", "IT", "Operations", "Sales")), 
                                      fill = factor(Salary_Level, levels = c("Low", "Medium", "High")))) +
  geom_bar(position = "stack", color = "black", linewidth = 0.5) +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), 
            size = 5, fontface = "bold", color = "white") +
  labs(title = "Salary Level by Department (Stacked)",
       subtitle = "Stacked distribution of salary levels across departments",
       x = "Department",
       y = "Count of Employees",
       fill = "Salary Level") +
  scale_fill_manual(values = c("Low" = "#FF6B6B", "Medium" = "#FFD93D", "High" = "#4ECDC4")) +
  theme_minimal(base_size = 24) +
  theme(axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 20),
        plot.title = element_text(size = 28, face = "bold"),
        plot.subtitle = element_text(size = 18, color = "gray50"),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 18, face = "bold"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

print(p_salary_stacked)



