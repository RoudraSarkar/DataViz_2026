
#Question1

# Load required libraries
library(tidyverse)
# Load CES 2015 data from local file
ces2015 <- read_csv("/Users/whiz/Desktop/whiz1/DataViz_2026/problemSets/PS03/my_answers/CES2015.csv")
# Keep only high-quality respondents
ces2015 <- ces2015 |>
  filter(discard == "Good quality")
# Check the dataset
glimpse(ces2015)

#Question2
# Filter and recode voting variable
ces2015 <- ces2015 |>
  mutate(
    voted = case_when(
      p_voted == "Yes" ~ 1,
      p_voted == "No" ~ 0,
      p_voted %in% c("Don't know", "Refused") ~ NA_real_,
      TRUE ~ NA_real_
    )
  ) |>
  filter(!is.na(voted))
# Check results
table(ces2015$voted)

#Question3
ces2015 <- ces2015 |>
  mutate(
    age = as.numeric(age),          # convert birth year to numeric
    age_years = 2015 - age,
    
    age_group = case_when(
      age_years < 30 ~ "<30",
      age_years >= 30 & age_years <= 44 ~ "30-44",
      age_years >= 45 & age_years <= 64 ~ "45-64",
      age_years >= 65 ~ "65+",
      TRUE ~ NA_character_
    )
  )

table(ces2015$age_group)


#Data visualization 
#Question1
ces2015 |>
  group_by(age_group) |>
  summarise(turnout_rate = mean(voted, na.rm = TRUE)) |>
  ggplot(aes(x = age_group, y = turnout_rate)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Turnout Rate by Age Group (CES 2015)",
    x = "Age Group",
    y = "Turnout Rate"
  ) +
  theme_minimal()

#Question2
ces2015 |>
  filter(!is.na(p_selfplace),
         p_selfplace >= 0 & p_selfplace <= 10,
         partyid %in% c("Liberal", "Conservative", "NDP", "Bloc Québécois", "Green")) |>
  ggplot(aes(x = p_selfplace, fill = partyid)) +
  geom_density(alpha = 0.6, color = "black") +
  facet_wrap(~partyid) +
  labs(
    title = "Ideology Distribution by Party (CES 2015)",
    x = "Left–Right Self-Placement (0 = Left, 10 = Right)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # legend not needed with facets

#Question3

ces2015 |>
  filter(!is.na(voted), !is.na(income_full)) |>
  ggplot(aes(x = income_full, fill = factor(voted))) +
  geom_bar(position = "stack", color = "black") +
  facet_wrap(~province) +
  scale_fill_manual(
    values = c("0" = "red", "1" = "blue"),
    labels = c("Did not vote", "Voted")
  ) +
  labs(
    title = "Turnout by Income, Faceted by Province (CES 2015)",
    x = "Income",
    y = "Count",
    fill = "Turnout"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Question4
install.packages("ggrepel")
library(ggrepel)  

# Aggregate turnout by age group
age_turnout <- ces2015 |>
  filter(!is.na(age_group), !is.na(voted)) |>
  group_by(age_group) |>
  summarise(turnout_rate = mean(voted, na.rm = TRUE), .groups = "drop")

annotation_data <- age_turnout |>
  filter(turnout_rate == max(turnout_rate))

my_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50", hjust = 0),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank()
    )
}

age_turnout |>
  ggplot(aes(x = age_group, y = turnout_rate)) +
  geom_col(fill = "#9d02d7") +
  geom_text_repel(
    data = annotation_data,
    aes(x = age_group, y = turnout_rate,
        label = paste0("Highest turnout: ", scales::percent(turnout_rate))),
    inherit.aes = FALSE,
    nudge_y = 0.05,
    size = 5,
    color = "#ffb14e",
    segment.color = "gray50"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  labs(
    title = "Turnout Increases with Age",
    subtitle = "CES 2015 respondents: proportion of participants who voted by age group",
    x = "Age Group",
    y = "Turnout Rate",
    caption = "Data: Canadian Election Study 2015; only good-quality respondents; turnout coded 1 = voted, 0 = did not vote"
  ) +
  my_theme()

































