# Load packages 
library(readxl)
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(stringr)
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(corrplot)
library(dplyr)
library(car)

# Load life expectancy data
LE_data<- read_excel("LE_at_birth_2015_to_2019.01.xlsx", skip = 3)

# Load income estimates data
MSOAd_income<- read_excel("MSOA_disposable income_2020.xlsx")

# Load housing affordability data
Housing_Affordabilty_Ratio_s <- read_excel("Housing Affordabilty Ratio's.xlsx")
# rename columns
colnames(LE_data)[colnames(LE_data) %in% c("Code", "Label", "Life expectancy at birth for males", "Life expectancy at birth for females")] <- c("MSOA code","MSOA name","LE males", "LE Females")

# merge data sets on common data

merged_df <- LE_data %>%
  inner_join(MSOAd_income, by = "MSOA code","MSOA name")

# clean data

df<-merged_df %>%
  select("MSOA code","LE males","LE Females", "Net annual income (£)","Local authority code", "Region code", "Region name") #  removing rows 

df$`LE males` <- as.numeric(merged_df$`LE males`)# convert column to numeric 
df$`LE Females` <- as.numeric(merged_df$`LE Females`)

colnames(Housing_Affordabilty_Ratio_s)

housing_afford<-Housing_Affordabilty_Ratio_s %>% select("MSOA code","All properties")

df <- housing_afford %>%
  inner_join(df, by = "MSOA code")

names(housing_afford)[names(housing_afford) == "All properties"] <- "Housing Affordability Ratios"

str(df)



# Convert character columns to numeric
df$`housing affordability ratios` <- as.numeric(merged_df2$`housing affordability ratios`)
df$`LE males` <- as.numeric(merged_df2$`LE males`)
df$`LE Females` <- as.numeric(merged_df2$`LE Females`)

# check for NA's
na_males <- sum(is.na(df$`LE males`))
na_females <- sum(is.na(df$`LE Females`))
print(paste("NAs in LE males:", na_males))
print(paste("NAs in LE Females:", na_females))

sum(is.na(df))

df$`housing affordability ratios`[is.na(df$`housing affordability ratios`)] <- 
  mean(df$`housing affordability ratios`, na.rm = TRUE)

# Impute missing values with column mean
if (any(is.na(df$`LE males`))) {
  merged_df2$`LE males`[is.na(df$`LE males`)] <- mean(df$`LE males`, na.rm = TRUE)
}

if (any(is.na(df$`LE Females`))) {
  df$`LE Females`[is.na(df$`LE Females`)] <- mean(df$`LE Females`, na.rm = TRUE)
}

write.xlsx(df, file = "dataoutput.xlsx", sheetName = "Sheet1", rowNames = FALSE)

df$Life_Expectancy <- (df$`LE males` + df$`LE Females`) / 2 # Add average life expectancy column

# MLR model
model <- lm(`Life_Expectancy` ~ `housing affordability ratios` + `Net annual income (£)` + `Region name`, data = df)

summary(model)

# Calculate VIF for the mode

vif_values <- vif(model)

print(vif_values)



# Most significant variables plot

# Extracting coefficients from the model
coefficients <- summary(model)$coefficients
coef_names <- rownames(coefficients)
coef_values <- coefficients[, 1]  # The first column contains the estimates

# Creating a data frame for plotting
coef_data <- data.frame(
  Variable = coef_names,
  Coefficient = coef_values
)


# Plotting the coefficients
ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +  # Flip coordinates for horizontal bars
  theme_minimal() +
  labs(
    title = "Coefficients from the Linear Model",
    x = "Variable",
    y = "Coefficient Value"
  )
# correlation
predictors <- df[, c("housing affordability ratios", "Net annual income (£)", "Life_Expectancy")]  


# Correlation between Life Expectancy and each predictor
correlations <- cor(predictors, df$Life_Expectancy, use = "complete.obs")
print(correlations)

# Phase 2 analysis 

# Filter the data for the West Midlands 
west_midlands_data <- subset(df, `Region name` == "West Midla")

# Calculate the average income in the West Midlands
average_income_west_midlands <- mean(west_midlands_data$`Net annual income (£)`, na.rm = TRUE)

# Print the result
print(average_income_west_midlands)

region_trends <- df %>%
  group_by(`Region name`) %>%
  summarise(
    `Housing Affordability Ratio` = mean(`housing affordability ratios`, na.rm = TRUE),
    `Net Annual Income (£)` = mean(`Net annual income (£)`, na.rm = TRUE)
  )

print(region_trends)


# Data Visulisations 


ggplot(df, aes(x = `Net annual income (£)`, y = Life_Expectancy)) +
  geom_point(color = "blue", alpha = 0.5) +  # Scatter plot
  geom_smooth(method = "lm", color = "red") +  # Linear regression line
  labs(title = "Correlation between Net Annual Income and Life Expectancy",
       x = "Net Annual Income (£)",
       y = "Life_Expectancy (Years)") +
  theme_minimal()

# Plot Housing Affordability Ratios by Region
ggplot(region_trends, aes(x = `Housing Affordability Ratio`, y = reorder(`Region name`, `Housing Affordability Ratio`))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Housing Affordability Ratios by Region",
       x = "Housing Affordability Ratio",
       y = "Region")

# Plot Net Annual Income by Region
ggplot(region_trends, aes(x = `Net Annual Income (£)`, y = reorder(`Region name`, `Net Annual Income (£)`))) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Net Annual Income by Region",
       x = "Net Annual Income (£)",
       y = "Region")

#Calculate mean life expectancy for males
mean_le_males <- mean(data$LE_males, na.rm = TRUE)

# Calculate mean life expectancy for females
mean_le_females <- mean(data$LE_Females, na.rm = TRUE)

# Calculate mean life expectancy for both genders
mean_le_aggregated <- mean(c(data$LE_males, data$LE_Females), na.rm = TRUE)

cat("Mean Life Expectancy for Males:", mean_le_males, "\n")
cat("Mean Life Expectancy for Females:", mean_le_females, "\n")
cat("Aggregated Mean Life Expectancy:", mean_le_aggregated, "\n")
