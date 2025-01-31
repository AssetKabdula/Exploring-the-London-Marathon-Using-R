install.packages("tidytuesdayR")  # Install if not already installed

# Importing Necessary Libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidytuesdayR)           

# Data Loading
winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/winners.csv')
london_marathon <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/london_marathon.csv')

## Data Exploration
summary(winners)
summary(london_marathon)

# Converting to data.table
london_marathon <- as.data.table(london_marathon)
winners <- as.data.table(winners)

# Filtering out marathons after 2020
london_recent <- london_marathon[Year <= 2020]

# Average time by categories (men, women, wheelchair men, wheelchair women)
winners[, avg_time := mean(Time), by = Category]

## Merging Datasets
marathon_data <- merge(london_recent, winners, by = "Year", all.x = TRUE)

### Growth in Participation
ggplot(marathon_data, aes(x = Year)) +
  geom_line(aes(y = Applicants, color = 'Applicants')) +
  geom_line(aes(y = Accepted, color = 'Accepted')) +
  geom_line(aes(y = Starters, color = 'Starters')) +
  geom_line(aes(y = Finishers, color = 'Finishers')) +
  labs(
    title = "London Marathon Participation Trends",
    x = "Year",
    y = "Number of Participants",
    color = "Category"
  ) +
  theme_minimal()

### Finish rate
marathon_data[, Finish_Rate := Finishers / Starters]

ggplot(marathon_data, aes(x = Year, y = Finish_Rate)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "London Marathon Finish Rate Over Time",
       x = "Year",
       y = "Finish Rate") +
  theme_minimal()

### Charity Fundraising Over Time
ggplot(marathon_data, aes(x = Year, y = Raised)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(
    title = "Charity Fundraising Over Time",
    x = "Year",
    y = "Amount Raised (£ millions)"
  ) +
  theme_minimal()

### Fundraising Distribution by Official Charity
marathon_data[!is.na(Raised)] %>%
  ggplot(aes(x = reorder(`Official charity`, Raised), y = Raised)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(
    title = "Fundraising Distribution by Official Charity",
    x = "Official Charity",
    y = "Amount Raised (£ millions)"
  ) +
  theme_minimal()

### Nationality Distribution of Winners
ggplot(marathon_data, aes(x = reorder(Nationality, table(Nationality)[Nationality]))) +
  geom_bar(fill = "coral") +
  coord_flip() +
  labs(
    title = "Most Successful Nationalities in the London Marathon",
    x = "Nationality",
    y = "Number of Wins"
  ) +
  theme_minimal()

### Winning Time Trends
ggplot(marathon_data, aes(x = Year, y = Time, color = Category)) +
  geom_point() +
  geom_smooth(method = "loess") +
  geom_hline(aes(yintercept = avg_time, color = Category), linetype = "dashed") +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Winning Time Trends with Average Times", x = "Year", y = "Time (hh:mm:ss)") +
  theme_minimal()

### Boxplot of Winning Times by Category
ggplot(marathon_data, aes(x = Category, y = Time, fill = Category)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribution of Winning Times", x = "Category", y = "Time (hh:mm:ss)") +
  theme_minimal()

