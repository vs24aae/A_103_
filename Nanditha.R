# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(ggplot2)     # For visualizations

# Load the dataset
patientData <- read_csv("PatientInfo.csv", show_col_types = FALSE)

# Clean column names to make them syntactically valid
patientData <- patientData %>% rename_with(~make.names(.), everything())

# Print the head of the dataset
head(patientData)

# Print the columns of the dataset
colnames(patientData)

# Check missng values in dataset
colSums(is.na(patientData))

# Impute 'contact_number' with the median
patientData <- patientData %>%
  mutate(contact_number = ifelse(is.na(contact_number), 
                                 median(contact_number, na.rm = TRUE), 
                                 contact_number))

# Impute 'sex' and 'infection_case' with the mode
calculateMode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

patientData <- patientData %>%
  mutate(sex = ifelse(is.na(sex), calculateMode(sex), sex),
         infection_case = ifelse(is.na(infection_case), calculateMode(infection_case), infection_case))

# Remove missing values in 'state'
patientData <- patientData %>%
  filter(!is.na(state))

# Remove irrelevant or highly missing columns
patientData <- patientData %>%
  select(-c(symptom_onset_date, deceased_date))

# Handle missing 'city' with "Unknown"
patientData <- patientData %>%
  mutate(city = ifelse(is.na(city), "Unknown", city))

# Replace remaining missing values in dates with placeholders
patientData <- patientData %>%
  mutate(across(ends_with("_date"), ~ ifelse(is.na(.), as.Date("1900-01-01"), as.Date(.))))

# Replace missing values with in infected_by with "Unknown"
patientData <- patientData %>%
  mutate(infected_by = ifelse(is.na(infected_by), "Unknown", infected_by))

# Verify Missing Values After Cleaning
colSums(is.na(patientData))

# Print summary of the Cleaned Dataset
summary(patientData)

# Print structure of the Cleaned Dataset
str(patientData)

# Remove non-finite values from age column and convert age to numeric
patientData <- patientData %>% 
  filter(!is.na(age) & is.finite(as.numeric(gsub("s", "", age)))) %>%
  mutate(age = as.numeric(gsub("s", "", age)))

# Convert sex to numeric
patientData <- patientData %>% mutate(sex_numeric = ifelse(sex == "male", 1, 0))

# Plot the Histogram of Age with Bell Curve
ggplot(patientData, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = "skyblue", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(patientData$age, na.rm = TRUE), 
                            sd = sd(patientData$age, na.rm = TRUE)), 
                color = "blue", linewidth = 1) +
  labs(title = "Histogram of Patient Age with Bell Curve",
       x = "Age", y = "Density") +
  theme_minimal()

# Plot the Histogram of Sex with Density Plot
ggplot(patientData, aes(x = sex_numeric)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightgreen", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(title = "Histogram of Sex with Density Plot",
       x = "Sex (0 = Female, 1 = Male)", y = "Density") +
  theme_minimal()

# Plot Scatterplot of sex vs Age with Trend Line
ggplot(patientData, aes(x = sex, y = as.numeric(as.factor(age)))) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatterplot of Patient sex vs Patient Age",
       x = "sex", y = "Age") +
  theme_minimal()

# Perform Correlation between Sex and Age
cor.test(patientData$sex_numeric, patientData$age)