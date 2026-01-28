install.packages(c("readxl", "readr"))  
library(readxl)
library(readr)
library(tidyverse)

setwd("~/Desktop/whiz1/DataViz_2026/problemSets/PS01/my_answers")
mep_info <- read_excel("mep_info_26Jul11.xls", sheet = "EP1", col_names = TRUE)
head(mep_info)
str(mep_info)
rcv_ep1 <- read_table2("rcv_ep1.txt")  
head(rcv_ep1)
str(rcv_ep1)

#Question 3
rcv_ep1 <- read_delim("rcv_ep1.txt")
colnames(rcv_ep1)
rcv_ep1_long <- rcv_ep1 %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "vote_id",
    values_to = "decision"
  )
rcv_ep1_long <- rcv_ep1_long %>%
  mutate(
    decision_label = case_when(
      decision == 1 ~ "Yes",
      decision == 2 ~ "No",
      decision == 3 ~ "Abstain",
      decision == 4 ~ "Absent",
      decision == 5 ~ "Present but did not vote",
      decision == 0 ~ "Not a member",
      TRUE ~ "Other"
    )
  )

decision_summary <- rcv_ep1_long %>%
  count(decision_label) %>%
  arrange(desc(n))

decision_summary




#Question 4
meta_cols <- c("MEPID", "MEPNAME", "MS", "NP", "EPG")
ep1_combined <- rcv_ep1_long %>%
  select(all_of(meta_cols), vote_id, decision, decision_label)
head(ep1_combined)
str(ep1_combined)
missing_summary <- ep1_combined %>%
  summarise(across(everything(), ~sum(is.na(.))))

missing_summary


#Question 5.1
#Filter only relevant votes (Yes, No, Abstain)
votes_filtered <- ep1_combined %>%
  filter(decision_label %in% c("Yes", "No", "Abstain"))

#Compute Yes rate per roll call per EP group
yes_rate_per_vote <- votes_filtered %>%
  group_by(EPG, vote_id) %>%   # each vote within each EP group
  summarise(
    yes_count = sum(decision_label == "Yes"),
    total_votes = n(),
    yes_rate = yes_count / total_votes,
    .groups = "drop"
  )

#Compute mean Yes rate across all roll calls for each EP group
mean_yes_rate_epg <- yes_rate_per_vote %>%
  group_by(EPG) %>%
  summarise(mean_yes_rate = mean(yes_rate), .groups = "drop") %>%
  arrange(desc(mean_yes_rate))

mean_yes_rate_epg


#Question5.2
#Filter only relevant votes (Yes, No, Abstain)
votes_filtered <- ep1_combined %>%
  filter(decision_label %in% c("Yes", "No", "Abstain"))

#Compute Abstain rate per roll call per EP group
abstain_rate_per_vote <- votes_filtered %>%
  group_by(EPG, vote_id) %>%
  summarise(
    abstain_count = sum(decision_label == "Abstain"),
    total_votes = n(),
    abstain_rate = abstain_count / total_votes,
    .groups = "drop"
  )

#Compute mean Abstain rate across all roll calls for each EP group
mean_abstain_rate_epg <- abstain_rate_per_vote %>%
  group_by(EPG) %>%
  summarise(mean_abstain_rate = mean(abstain_rate), .groups = "drop") %>%
  arrange(desc(mean_abstain_rate))

# View the result
mean_abstain_rate_epg

#Question5.3
library(dplyr)
mep_info <- read_excel("mep_info_26Jul11.xls", sheet = "EP1", col_names = TRUE)

#clean column names
colnames(mep_info) <- colnames(mep_info) %>%
  trimws() %>%
  gsub(" ", ".", .)  # replace spaces with dots

#Ensure the key columns exist
colnames(mep_info)
head(mep_info)


#Fix column types and trim spaces
mep_info <- mep_info %>%
  mutate(
    MEP.id = trimws(as.character(MEP.id)),   # ensure same type as ep1_combined$MEPID
    `NOM-D1` = as.numeric(`NOM-D1`),
    `NOM-D2` = as.numeric(`NOM-D2`)
  )

ep1_combined <- ep1_combined %>%
  mutate(MEPID = trimws(as.character(MEPID)))


#Merge NOM-D1 and NOM-D2 into long-format vote dataset
ep1_combined_nom <- ep1_combined %>%
  left_join(
    mep_info %>% select(MEP.id, `NOM-D1`, `NOM-D2`),
    by = c("MEPID" = "MEP.id")
  )


#Compute mean NOM-D1 and NOM-D2 per EP group
mean_nominate_epg <- ep1_combined_nom %>%
  group_by(EPG) %>%
  summarise(
    mean_nom_d1 = mean(`NOM-D1`, na.rm = TRUE),
    mean_nom_d2 = mean(`NOM-D2`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_nom_d1)  


#View the result
mean_nominate_epg

#Data Visualization 
#Question 1
library(ggplot2)
#Keep only observations with valid NOM-D1 values
plot_data <- ep1_combined_nom %>%
  filter(!is.na(`NOM-D1`))

#Boxplot of NOM-D1 by EP group
ggplot(plot_data, aes(x = EPG, y = `NOM-D1`)) +
  geom_boxplot() +
  labs(
    title = "Distribution of NOMINATE Dimension 1 by EP Group (EP1)",
    x = "European Parliament Group",
    y = "NOMINATE Dimension 1"
  ) +
  theme_minimal()



#Question 2
#Create a unique MEP-level dataset
mep_level <- ep1_combined_nom %>%
  select(MEPID, EPG, `NOM-D1`, `NOM-D2`) %>%
  distinct() %>%                      # one row per MEP
  filter(!is.na(`NOM-D1`), !is.na(`NOM-D2`))

#Scatterplot
ggplot(mep_level, aes(x = `NOM-D1`, y = `NOM-D2`, color = EPG)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(
    title = "MEP Ideal Points in NOMINATE Space (EP1)",
    x = "NOMINATE Dimension 1",
    y = "NOMINATE Dimension 2",
    color = "EP Group"
  ) +
  theme_minimal()


#Question 3 
# Step 1: Compute proportion of Yes votes per MEP
mep_yes_rate <- ep1_combined %>%
  filter(decision_label %in% c("Yes", "No", "Abstain")) %>%  # denominator
  group_by(MEPID, EPG) %>%
  summarise(
    yes_rate = mean(decision_label == "Yes"),
    .groups = "drop"
  )

#Boxplot by EP group
ggplot(mep_yes_rate, aes(x = EPG, y = yes_rate)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Yes Vote Proportions by EP Group (EP1)",
    x = "European Parliament Group",
    y = "Proportion of Votes Cast as Yes"
  ) +
  theme_minimal()

#Question4
ep1_combined %>%
  filter(decision_label %in% c("Yes", "No", "Abstain")) %>%
  group_by(NP) %>%
  summarise(
    yes_rate = mean(decision_label == "Yes"),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(factor(NP), yes_rate), y = yes_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Proportion Voting Yes by National Party (EP1)",
    x = "National Party",
    y = "Proportion of Yes Votes"
  ) +
  theme_minimal()








