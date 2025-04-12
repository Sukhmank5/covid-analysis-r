# ---------------------------------------------
# COVID-19 Global Spread Analysis
# Data Source: Our World in Data (OWID)
# 
# This project analyzes COVID-19 trends in India, the United States, 
# and Brazil. It uses data visualization to explore daily new cases, 
# total cases, and mortality rates. Key findings include the U.S. 
# having the highest total case count, with India showing a lower 
# mortality rate over time.
# ---------------------------------------------

# Install packages (only need to do this once)
install.packages("tidyverse")

# Load libraries
library(tidyverse)
library(ggplot2)

# Read the CSV directly from the web
covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Filter Data for a Country (India)
india <- covid_data %>%
  filter(location == "India")
# Plot Daily New Cases in India
ggplot(india, aes(x = as.Date(date), y = new_cases)) +
  geom_line(color = "steelblue") +
  labs(title = "Daily COVID-19 Cases in India",
       x = "Date",
       y = "New Cases") +
  theme_minimal()
# Plot Mortality Rate (Deaths / Cases) in India
india <- india %>%
  mutate(mortality_rate = ifelse(total_cases > 0, (total_deaths / total_cases) * 100, NA))
# Plot the Mortality Rate
ggplot(india, aes(x = as.Date(date), y = mortality_rate)) +
  geom_line(color = "darkred") +
  labs(title = "COVID-19 Mortality Rate in India",
       x = "Date",
       y = "Mortality Rate (%)") +
  theme_minimal()
# Compare Multiple Countries (India, United States, Brazil)
countries <- covid_data %>%
  filter(location %in% c("India", "United States", "Brazil"))

ggplot(countries, aes(x = as.Date(date), y = new_cases, color = location)) +
  geom_line() +
  labs(title = "Daily COVID-19 Cases Comparison",
       x = "Date",
       y = "New Cases",
       color = "Country") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# Optional: Filter out 0s or negative values from new_cases to avoid log scale issues
countries <- countries %>%
  filter(new_cases > 0)
# Plotting the data
ggplot(countries, aes(x = as.Date(date), y = new_cases, color = location)) +
  geom_line(linewidth = 1) +  # Use `linewidth` instead of `size`
  scale_y_log10() +  # Log scale transformation for Y-axis
  labs(title = "Daily COVID-19 Cases (Log Scale)",
       x = "Date",
       y = "New Cases (log scale)",
       color = "Country") +
  theme_minimal()


# Optional: Filter out 0s or negative values from new_cases to avoid log scale issues
countries <- countries %>%
  filter(new_cases > 0)
# Plotting the data
ggplot(countries, aes(x = as.Date(date), y = new_cases, color = location)) +
  geom_line(linewidth = 1) +  # Use `line width` instead of `size`
  scale_y_log10() +  # Log scale transformation for Y-axis
  labs(title = "Daily COVID-19 Cases (Log Scale)",
       x = "Date",
       y = "New Cases (log scale)",
       color = "Country") +
  theme_minimal()
# Bar chart with formatted total cases
# Calculate total cases for each country
total_cases_comparison <- countries %>%
  group_by(location) %>%
  summarize(total_cases = max(total_cases, na.rm = TRUE))
ggplot(total_cases_comparison, aes(x = reorder(location, -total_cases), y = total_cases, fill = location)) +
  geom_bar(stat = "identity") +
  labs(title = "Total COVID-19 Cases by Country",
       x = "Country",
       y = "Total Cases") +
  theme_minimal() +
  coord_flip() +  # Flip for better readability
  scale_y_continuous(labels = scales::comma)  # Adding comma formatting to Y-axis
