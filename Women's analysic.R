library(rpart)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(corrplot)
library(plm)
library(lmtest)
library(sandwich)
library(zoo)
library(broom)
library(missForest)

#Combine the WHR data
setwd("/Users/Main/Women\'s\ education\ analysis")
xls_files <- list.files(pattern = "\\.xls$", full.names = TRUE)
combined_file <- bind_rows(lapply(xls_files, function(file) {
  df <- read_excel(file)
  year <- str_extract(basename(file), "\\d{4}")
  df$Year <- as.numeric(year)
  return(df)
}))
combined_whr <- combined_file %>%
  group_by(Country) %>%
  arrange(Year, .by_group = TRUE)

write_xlsx(combined_whr, "combined_whr.xlsx")

#Data Wrangling for Education
primary_female <- read.csv("/Users/Main/Women\'s\ education\ analysis/primary_female.csv")
primary_male <- read.csv("/Users/Main/Women\'s\ education\ analysis/primary_male.csv")

secondary_female <- read.csv("/Users/Main/Women\'s\ education\ analysis/secondary_female.csv")
secondary_male<- read.csv("/Users/Main/Women\'s\ education\ analysis/secondary_male.csv")

bachelor_female <- read.csv("/Users/Main/Women\'s\ education\ analysis/bachelor_female.csv")
bachelor_male <- read.csv("/Users/Main/Women\'s\ education\ analysis/bachelor_male.csv")

master_female <- read.csv("/Users/Main/Women\'s\ education\ analysis/master_female.csv")
master_male <- read.csv("/Users/Main/Women\'s\ education\ analysis/master_male.csv")

doctoral_female <- read.csv("/Users/Main/Women\'s\ education\ analysis/doctoral_female.csv")
doctoral_male <- read.csv("/Users/Main/Women\'s\ education\ analysis/doctoral_male.csv")

pivot_edu_data <- function(df, new_value_name) {
  df %>%
    pivot_longer(cols = starts_with("X"), names_to = "Year", values_to = new_value_name) %>%
    mutate(Year = as.integer(sub("X", "", Year)))
}

primary_female_long <- pivot_edu_data(primary_female, "Primary_Female")
secondary_female_long <- pivot_edu_data(secondary_female, "Secondary_Female")
bachelor_female_long <- pivot_edu_data(bachelor_female, "Bachelor_Female")
master_female_long <- pivot_edu_data(master_female, "Master_Female")
doctoral_female_long <- pivot_edu_data(doctoral_female, "Doctoral_Female")

education_female <- primary_female_long %>%
  left_join(secondary_female_long, by = c("Country", "Year")) %>%
  left_join(bachelor_female_long, by = c("Country", "Year")) %>%
  left_join(master_female_long, by = c("Country", "Year")) %>%
  left_join(doctoral_female_long, by = c("Country", "Year"))

full_data <- education_female %>%
  inner_join(combined_whr, by = c("Country", "Year"))

#Initial EDA
colSums(is.na(full_data))

years_per_country <- full_data %>%
  group_by(Country) %>%
  summarize(n_years = n_distinct(Year)) %>%
  arrange(n_years)

#Data Cleaning
countries_with_enough_years <- years_per_country %>%
  filter(n_years >= 3) %>%
  pull(Country)

full_data_clean <- full_data %>%
  filter(Country %in% countries_with_enough_years)

education_columns <- c("Primary_Female", "Secondary_Female", "Bachelor_Female",
                       "Master_Female", "Doctoral_Female")

missing_rate_edu <- full_data_clean %>%
  group_by(Country) %>%
  summarize(across(all_of(education_columns), ~ mean(is.na(.)), .names = "missing_{.col}"))

countries_to_remove <- missing_rate_edu %>%
  filter(if_any(starts_with("missing_"), ~ .x > 0.8)) %>%
  pull(Country)

final_data <- full_data_clean %>%
  filter(!Country %in% countries_to_remove)

#Imputation
impute_data <- final_data %>%
  select(all_of(education_columns))
impute_data_df <- as.data.frame(impute_data)

set.seed(123)
imputed <- missForest(impute_data_df)
final_data_filled <- final_data %>%
  select(-all_of(education_columns)) %>%
  bind_cols(as_tibble(imputed$ximp))

#EDA 
# Calculate the mean values for each country
mean_data <- final_data_filled %>%
  group_by(Country) %>%
  summarize(
    Mean_Happiness = mean(Happiness, na.rm = TRUE),
    Mean_GDP = mean(GDP_per_capita, na.rm = TRUE),
    Mean_Bachelor = mean(Bachelor_Female, na.rm = TRUE),
    .groups = "drop"
  )

# Select top and bottom 10 countries by mean happiness
top10_happiness <- mean_data %>%
  arrange(desc(Mean_Happiness)) %>%
  slice_head(n = 10) %>%
  mutate(Group = "Top 10")

bottom10_happiness <- mean_data %>%
  arrange(Mean_Happiness) %>%
  slice_head(n = 10) %>%
  mutate(Group = "Bottom 10")

# Combine top and bottom happiness
combined_happiness <- bind_rows(top10_happiness, bottom10_happiness) %>%
  mutate(Group = factor(Group, levels = c("Top 10", "Bottom 10")))

# Select top and bottom 10 countries by mean GDP per capita
top10_gdp <- mean_data %>%
  arrange(desc(Mean_GDP)) %>%
  slice_head(n = 10) %>%
  mutate(Group = "Top 10")

bottom10_gdp <- mean_data %>%
  arrange(Mean_GDP) %>%
  slice_head(n = 10) %>%
  mutate(Group = "Bottom 10")

# Combine top and bottom GDP
combined_gdp <- bind_rows(top10_gdp, bottom10_gdp) %>%
  mutate(Group = factor(Group, levels = c("Top 10", "Bottom 10")))

# Create combined happiness plot
happiness_plot <- ggplot(combined_happiness, aes(x = reorder(Country, Mean_Happiness), y = Mean_Happiness)) +
  geom_col(aes(fill = Group)) +
  geom_point(aes(y = Mean_Bachelor/10), color = "red", size = 3) +
  scale_fill_manual(values = c("Top 10" = "lightblue", "Bottom 10" = "lightpink")) +
  scale_y_continuous(
    name = "Mean Happiness Score",
    sec.axis = sec_axis(~.*10, name = "Mean Bachelor's Completion (%)")
  ) +
  coord_flip() +
  labs(title = "Top vs Bottom 10 Countries - Mean Happiness and Bachelor Completion",
       x = "Country",
       fill = "Group") +
  theme_minimal() +
  theme(legend.position = "top")

# Create combined GDP plot
gdp_plot <- ggplot(combined_gdp, aes(x = reorder(Country, Mean_GDP), y = Mean_GDP)) +
  geom_col(aes(fill = Group)) +
  geom_point(aes(y = Mean_Bachelor/10), color = "red", size = 3) +
  scale_fill_manual(values = c("Top 10" = "lightblue", "Bottom 10" = "lightpink")) +
  scale_y_continuous(
    name = "Mean GDP per Capita",
    sec.axis = sec_axis(~.*10, name = "Mean Bachelor's Completion (%)")
  ) +
  coord_flip() +
  labs(title = "Top vs Bottom 10 Countries - Mean GDP per Capita and Bachelor Completion",
       x = "Country",
       fill = "Group") +
  theme_minimal() +
  theme(legend.position = "top")

# Display plots
print(happiness_plot)
print(gdp_plot)

#Correlation
selected_data <- final_data_filled %>%
  select(Primary_Female, Secondary_Female, Bachelor_Female, Master_Female, Doctoral_Female,
         Happiness, Dystopia, `GDP per capita`, `Social support`, `Healthy life expectancy`,
         `Freedom to make life choices`, Generosity, `Perceptions of corruption`)

corr_matrix <- cor(selected_data, use = "complete.obs")

education_vars <- c("Primary_Female", "Secondary_Female", "Bachelor_Female", "Master_Female", "Doctoral_Female")
whr_vars <- c("Happiness", "Dystopia", "GDP per capita", "Social support", "Healthy life expectancy",
              "Freedom to make life choices", "Generosity", "Perceptions of corruption")

corr_submatrix <- corr_matrix[education_vars, whr_vars]

corrplot(corr_submatrix, method = "color", type = "full", addCoef.col = "black", tl.col = "black")

#Column rename
final_data_filled <- final_data_filled %>%
  rename(
    GDP_per_capita = `GDP per capita`,
    Social_support = `Social support`,
    Healthy_life_expectancy = `Healthy life expectancy`
  )

#t-test
final_data_filled$edu_group <- ifelse(final_data$Bachelor_Female > median(final_data_filled$Bachelor_Female) 
                                      & final_data$Master_Female > median(final_data_filled$Master_Female) ,
                                      "High", "Low")
t.test(Happiness ~ edu_group, data = final_data_filled)
t.test(GDP_per_capita ~ edu_group, data = final_data_filled)
t.test(Social_support ~ edu_group, data = final_data_filled)
t.test(Healthy_life_expectancy ~ edu_group, data = final_data_filled)

#ARIMAX
library(forecast)
library(tseries)    
library(ggplot2)

# Convert your Happiness variable into a time series
happiness_ts <- ts(final_data_filled$Happiness, frequency = 1)

# 1. Plot the time series
plot(happiness_ts, main = "Happiness Time Series", ylab = "Happiness Score", col = "blue")

# 2. Augmented Dickey-Fuller Test for stationarity
adf_result <- adf.test(happiness_ts)
print(adf_result)

# 3. Plot ACF and PACF
acf(happiness_ts, main = "ACF of Happiness")
pacf(happiness_ts, main = "PACF of Happiness")

# Prepare the data: convert to time series format
happiness_ts <- ts(final_data_filled$Happiness, frequency = 1)  
primary_female_ts <- ts(final_data_filled$Primary_Female, frequency = 1)
secondary_female_ts <- ts(final_data_filled$Secondary_Female, frequency = 1)
bachelor_female_ts <- ts(final_data_filled$Bachelor_Female, frequency = 1)
master_female_ts <- ts(final_data_filled$Master_Female, frequency = 1)

# Fit ARIMAX model (Happiness with exogenous variables Primary_Female and Secondary_Female)
arimax_model <- arima(happiness_ts, order = c(5, 0, 0),
                      xreg = cbind(primary_female_ts, secondary_female_ts, bachelor_female_ts, master_female_ts))

# Print model summary
summary(arimax_model)





