# Load the required package
install.packages("dplyr")
library(dplyr)
library(stringdist)
library(fuzzyjoin)
library(tidyr)
library(stringr)
library(tidyverse)
library(lubridate)
library(readr)

# Load all data sets to combine into final dataset-----
# All data has been matched/handcoded, this process just merges everything together
# All data in this .R file is in the "final data after matching folder" in the git hub repository

# Load perfect matches that did not need to be hand checked (3853 final)
# Perf and district check are in the same dataset, which why it is loaded twice but just filtered differently
perf.final <- read.csv('perf_match_one_distinct.csv')
perf.final <- perf.final %>%
  filter(court_mismatch == 1)
perf.final$climate_count <- as.character(perf.final$climate_count)

# Load perfect matches that had to be hand checked because of district mismatch (84 final with one magistrate)
district.check.final <- read.csv('perf_match_one_distinct.csv')
district.check.final <- district.check.final %>%
  filter(keep == 1) %>%
  filter(court_mismatch != 1 | is.na(court_mismatch))

# Load matches that were perfect but needed to be checked (63 final)
perf.check.final <- read.csv('perf_match_check_distinct.csv')
perf.check.final <- perf.check.final %>%
  filter(keep == 1)
perf.check.final$climate_count <- as.character(perf.check.final$climate_count)
perf.check.final$housing_count <- as.character(perf.check.final$housing_count)
perf.check.final$Degree.Year..2. <- as.character(perf.check.final$Degree.Year..2.)
perf.check.final$district <- as.character(perf.check.final$district)

# Load matches that were not perfect and found to be matches (149 final)
nonperf.check.final <- read.csv('matches.to.check.csv')
nonperf.check.final <- nonperf.check.final %>%
  filter(keep == 1)
nonperf.check.final$Degree.Year..2. <- as.character(nonperf.check.final$Degree.Year..2.)

# Load matches that were not perfect and first pass called for reevaluation (60)
double.check.final <- read.csv('reevaluate.nonperf.match.csv')
double.check.final$climate_count <- as.character(double.check.final$climate_count)
double.check.final$housing_count <- as.character(double.check.final$housing_count)
double.check.final$Degree.Year..2. <- as.character(double.check.final$Degree.Year..2.)
double.check.final$district <- as.character(double.check.final$district)

# Load handcoded matches (643)
handcode.final <- read.csv('new.data.no.matches.csv')
handcode.final$ej_count <- as.integer(handcode.final$ej_count)
handcode.final$Degree.Year..2. <- as.character(handcode.final$Degree.Year..2.)

# Merge together
final.judge.matches <- bind_rows(perf.final, district.check.final, perf.check.final, nonperf.check.final, double.check.final, handcode.final)

# Replace NAs with 0 if not coded for magistrate
final.judge.matches <- final.judge.matches %>%
  mutate(magistrate = replace_na(magistrate, 0))

# Replace NAs with 1 if not checked for match
final.judge.matches <- final.judge.matches %>%
  mutate(matched = replace_na(matched, 1))

# Download CSV
write.csv(final.judge.matches, "final.judge.matches.csv")

n_distinct(final.judge.matches$ID)

# Data matching/joining is complete! YAY

# Start of exploratory visualizations-----
# Remove magistrates, add in a few extra variables for clarity
final.judge.matches.ex <- final_judge_matches
final.judge.matches.ex$district <- NULL

final.judge.matches.ex <- final.judge.matches.ex %>%
  filter(magistrate == 0) %>%
  filter(matched == 1)

final.judge.matches.ex <- final.judge.matches.ex %>%
  mutate(decision_year = str_extract(ID, "\\d{4}"))

final.judge.matches.ex <- final.judge.matches.ex %>%
  left_join(court_crosswalk, by = "court")

# Judge by number of decisions/partisanship and other various visualization------
judge_partisanship <- final.judge.matches.ex %>%
  group_by(Party.of.Appointing.President..1., decision_year) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  arrange(decision_year)

judge_partisanship$decision_year <- as.numeric(judge_partisanship$decision_year)

ggplot(judge_partisanship, aes(x = decision_year, y = decision_count, color = Party.of.Appointing.President..1.)) +
  geom_line() +
  labs(title = "Number of Decisions by Party of Judge's Appointing President Over Time",
       x = "Year",
       y = "Number of Decisions",
       color = "Party of Judge's Appointing President") +
  scale_color_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()

judge_jud_chair_median <- final.judge.matches.ex %>%
  group_by(Party.of.Appointing.President..1., decision_year) %>%
  summarise(median_jud_chair_dw = median(jud.chair.dw, na.rm = TRUE)) %>%
  arrange(decision_year)

judge_jcs_median <- final.judge.matches.ex %>%
  group_by(Party.of.Appointing.President..1., decision_year, ID) %>%
  summarise(jcs.score.dw.median = median(jcs.score.dw, na.rm = TRUE)) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  arrange(decision_year)

judge_jud_chair_median <- judge_jud_chair_median %>%
  filter(decision_year >= 1971)

ggplot(judge_jud_chair_median, aes(x = decision_year, y = median_jud_chair_dw, color = Party.of.Appointing.President..1., group = Party.of.Appointing.President..1.)) +
  geom_line() +
  labs(title = "Median Jud.chair.dw Ideology Score Over Time",
       x = "Year",
       y = "Median",
       color = "Party of Judge's Appointing President") +
  scale_x_discrete(breaks = seq(1971, max(judge_partisanship_median$decision_year), by = 5)) +
  theme_minimal()

judge_jcs_median <- judge_jcs_median %>%
  filter(decision_year >= 1971)

ggplot(judge_jcs_median, aes(x = decision_year, y = jcs.score.dw.median, color = Party.of.Appointing.President..1., group = Party.of.Appointing.President..1.)) +
  geom_line() +
  labs(title = "Median JCS Ideology Score Over Time",
       x = "Year",
       y = "Median",
       color = "Party of Judge's Appointing President") +
  scale_x_discrete(breaks = seq(1971, max(judge_partisanship_median$decision_year), by = 5)) +
  scale_color_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()


dime_median <- final.judge.matches.ex %>%
  group_by(Party.of.Appointing.President..1., decision_year) %>%
  summarise(dime.median = median(imputed.dime.cfscore, na.rm = TRUE)) %>%
  arrange(decision_year)

dime_median <- dime_median %>%
  filter(decision_year >= 1971)

ggplot(dime_median, aes(x = decision_year, y = dime.median, color = Party.of.Appointing.President..1., group = Party.of.Appointing.President..1.)) +
  geom_line() +
  labs(title = "Median Dime Ideology Score Over Time",
       x = "Year",
       y = "Median",
       color = "Party of Judge's Appointing President") +
  scale_x_discrete(breaks = seq(1971, max(judge_partisanship_median$decision_year), by = 5)) +
  scale_color_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()

# Visualize by outcome and then by party-----------
# Visualize how often nonprofits win comparing democrats versus republican appointed judges
nonprofit_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(outcome, Party.of.Appointing.President..1.) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(nonprofit_plaintiff, aes(x = outcome, y = percentage_wins, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Party of Judge's Appointing President") +
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()

# Visualize how often the federal government wins comparing democrats versus republican appointed judges
federal_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(outcome, Party.of.Appointing.President..1.) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(federal_plaintiff, aes(x = outcome, y = percentage_wins, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in Federal Gov Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Party of Judge's Appointing President") +
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()



# Visualize how often industry wins comparing democrats versus republican appointed judges
industry_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(outcome, Party.of.Appointing.President..1.) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(industry_plaintiff, aes(x = outcome, y = percentage_wins, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in Industry Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Party of Judge's Appointing President") +
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()

# Visualize by party and then by outcome-------
# Try to different way of viewing the data with nonprofits
nonprofit_plaintiff_outcome <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(nonprofit_plaintiff_outcome, aes(x = Party.of.Appointing.President..1., y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Try to different way of viewing the data with federal
fed_plaintiff_outcome <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(fed_plaintiff_outcome, aes(x = Party.of.Appointing.President..1., y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in Federal Gov Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Try to different way of viewing the data with industry (excluding industry v. industry)
industry_filtered_outcome <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(industry_filtered_outcome, aes(x = Party.of.Appointing.President..1., y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in Industry Cases (Excluding Industry v. Industry Cases)",
       x = "Outcome",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize with ideology scores------
# Nonprofits
nonprofit_plaintiff_score <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(jcs.score.dw, outcome) %>%
  mutate(plaintiff.win = if_else(outcome == "plaintiff", 1, 0)) %>%
  ungroup()

model <- lm(plaintiff.win ~ jcs.score.dw, data = nonprofit_plaintiff_score)

# Extract the coefficients
coefficients <- coef(model)

# Access the slope
slope <- coefficients["jcs.score.dw"]


ggplot(nonprofit_plaintiff_score, aes(x = jcs.score.dw, y = plaintiff.win)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(
    title = "Relationship Between JCS Ideology Score and Case Outcome in Nonprofit Cases",
    x = "Judge Ideology Score",
    y = "Case Outcome (1 = Plaintiff Win, 0 = Defendant Win)"
  ) +
  theme_minimal() 

# Federal
federal_plaintiff_score <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(jcs.score.dw, outcome) %>%
  mutate(plaintiff.win = if_else(outcome == "plaintiff", 1, 0)) %>%
  ungroup()

model <- lm(plaintiff.win ~ jcs.score.dw, data = federal_plaintiff_score)

# Extract the coefficients
coefficients <- coef(model)

# Access the slope
slope <- coefficients["jcs.score.dw"]


ggplot(federal_plaintiff_score, aes(x = jcs.score.dw, y = plaintiff.win)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(
    title = "Relationship Between JCS Ideology Score and Case Outcome in Federal Gov Cases",
    x = "Judge Ideology Score",
    y = "Case Outcome (1 = Plaintiff Win, 0 = Defendant Win)"
  ) +
  theme_minimal() 

# Industry
industry_plaintiff_score <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(jcs.score.dw, outcome) %>%
  mutate(plaintiff.win = if_else(outcome == "plaintiff", 1, 0)) %>%
  ungroup()

model <- lm(plaintiff.win ~ jcs.score.dw, data = industry_plaintiff_score)

# Extract the coefficients
coefficients <- coef(model)

# Access the slope
slope <- coefficients["jcs.score.dw"]


ggplot(industry_plaintiff_score, aes(x = jcs.score.dw, y = plaintiff.win)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(
    title = "Relationship Between JCS Ideology Score and Case Outcome in Industry Cases",
    x = "Judge Ideology Score",
    y = "Case Outcome (1 = Plaintiff Win, 0 = Defendant Win)"
  ) +
  theme_minimal() 

# Industry (remove industry v. industry)
industry_filtered_plaintiff_score <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(jcs.score.dw, outcome) %>%
  mutate(plaintiff.win = if_else(outcome == "plaintiff", 1, 0)) %>%
  ungroup()

model <- lm(plaintiff.win ~ jcs.score.dw, data = industry_filtered_plaintiff_score)

# Extract the coefficients
coefficients <- coef(model)

# Access the slope
slope <- coefficients["jcs.score.dw"]

ggplot(industry_filtered_plaintiff_score, aes(x = jcs.score.dw, y = plaintiff.win)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(
    title = "Relationship Between JCS Ideology Score and Case Outcome in Industry Case (Excluding Industry v. Industry)",
    x = "Judge Ideology Score",
    y = "Case Outcome (1 = Plaintiff Win, 0 = Defendant Win)"
  ) +
  theme_minimal() 


# Visualize the geography--------

# Load crosswalk data to identify corresponding appeals court for each decision
appeal_crosswalk <- read.csv('/Users/Cordeliavanderveer/Downloads/Judge Matching, New Data/appeal_court_crosswalk.csv')

appeal_crosswalk <- appeal_crosswalk %>%
  rename(
    "district" = "District_FJCBonica"
  )

appeal_crosswalk$Court.Name..1. <- NULL

appeal_crosswalk <- head(appeal_crosswalk,-6)

final.judge.matches.ex <- final.judge.matches.ex %>%
  left_join(appeal_crosswalk, by = "district")


# Democratic judges in nonprofit cases
geography.nonprofit <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Court_of_Appeals, Party.of.Appointing.President..1.) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  mutate(total_count = sum(decision_count, na.rm = TRUE),
       percentage = (decision_count / total_count) * 100) %>%
  ungroup()#%>%
  #filter(Party.of.Appointing.President..1. == "Democratic")

ggplot(geography.nonprofit, aes(x = Court_of_Appeals, y = percentage, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Democratic Judges in NGO or Civic Association Cases",
       x = "District Court (Grouped by Circuit Court Number) with D.C. = 0",
       y = "Percentage of Democratic Judges") +
    scale_fill_manual(values = c("Republican" = "red","Democratic" = "blue")) #+
  #theme(legend.position="none")


geography.all.cases <- final.judge.matches.ex %>%
  group_by(Court_of_Appeals, Party.of.Appointing.President..1.) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  mutate(total_count = sum(decision_count, na.rm = TRUE),
         percentage = (decision_count / total_count) * 100) %>%
  ungroup()%>%
  filter(Party.of.Appointing.President..1. == "Democratic")

ggplot(geography.all.cases, aes(x = Court_of_Appeals, y = percentage, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Democratic Judges in all RESL Decisions by Court of Appeals District",
       x = "District Court (Grouped by Appeals Court Number) with D.C. = 0",
       y = "Percentage of Democratic Judges") +
  scale_fill_manual(values = c("Democratic" = "blue")) +
  theme(legend.position="none")

geography.all.cases.both <- final.judge.matches.ex %>%
  group_by(Court_of_Appeals, Party.of.Appointing.President..1.) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  mutate(total_count = sum(decision_count, na.rm = TRUE),
         percentage = (decision_count / total_count) * 100) %>%
  ungroup()

ggplot(geography.all.cases.both, aes(x = Court_of_Appeals, y = percentage, fill = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percentage of Democratic/Republican Judges in all RESL Decisions by Court of Appeals District",
       x = "District Court (Grouped by Appeals Court Number) with D.C. = 0",
       y = "Percentage of Democratic Judges") +
  scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
  theme_minimal()

# Try weighting by number of judges---------
# Try making one for each district first? and then make new variable that is a weighted score

weighted_scores <- final.judge.matches.ex %>%
  group_by(Court_of_Appeals, Party.of.Appointing.President..1.) %>%
  summarise(decision_count = n_distinct(ID)) %>%
  mutate(total_count = sum(decision_count, na.rm = TRUE),
         percentage = (decision_count / total_count) * 100) %>%
  ungroup()

weighted_nonprofit_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  #filter(str_detect(Court_of_Appeals, "1")) %>%
  group_by(Party.of.Appointing.President..1., Court_of_Appeals, outcome) %>%
  summarize(wins = n()) %>%
  #mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  # create new variable with weighted percentage_wins
  ungroup()

ggplot(weighted_nonprofit_plaintiff, aes(x = Party.of.Appointing.President..1., y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

weighted_nonprofit_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Court_of_Appeals, Party.of.Appointing.President..1., outcome) %>%
  summarise(wins = n(),
            decision_count = n_distinct(ID)) %>%
  mutate(total_count = sum(decision_count, na.rm = TRUE),
         weight = decision_count / total_count,
         weighted_wins = wins * weight,
         total_weighted_wins = sum(weighted_wins),
         percentage_wins = (weighted_wins / total_weighted_wins) * 100) %>%
  ungroup()

ggplot(weighted_nonprofit_plaintiff, aes(x = Court_of_Appeals, y = percentage_wins, fill = outcome, group = Party.of.Appointing.President..1.)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Weighted Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize the data
ggplot(weighted_nonprofit_plaintiff, aes(x = Party.of.Appointing.President..1., y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Weighted Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Green generation-----------
# Facet wrap for each generation

final.judge.matches.ex <- final.judge.matches.ex %>%
  mutate(green.generation = case_when(Birth.Year >= 1945 & Birth.Year <= 1955 ~ 1,
                                      TRUE ~ 0))

final.judge.matches.ex$green.generation = as.factor(final.judge.matches.ex$green.generation)

nonprofit_generation <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(green.generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(nonprofit_generation, aes(x = green.generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge in NGO or Civic Association Cases",
       x = "Judge Generation (1 if came of age during first Earth Day in 1970, 0 if not)",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

industry_generation <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry"))  %>%
  group_by(green.generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()


ggplot(industry_generation, aes(x = green.generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge in Industry Cases",
       x = "Judge Generation (1 if came of age during first Earth Day in 1970, 0 if not)",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) 
  theme_minimal()

industry_ex_generation <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry"))  %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(Party.of.Appointing.President..1., green.generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()


ggplot(industry_ex_generation, aes(x = green.generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge in Industry Cases (Excluding Industry v. Industry)",
       x = "Judge Generation (1 if came of age during first Earth Day in 1970, 0 if not)",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

federal_generation <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(green.generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(federal_generation, aes(x = green.generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge in Federal Government Cases",
       x = "Judge Generation (1 if came of age during first Earth Day in 1970, 0 if not)",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()


# Gender ---------

# nonprofits
gender.nonprofits <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Gender, Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(gender.nonprofits, aes(x = Gender, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender of Judge and Decision Outcome in Environmental Nonprofit Cases",
       x = "Gender",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# federal government

gender.gov <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(Gender, Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(gender.gov, aes(x = Gender, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender of Judge and Decision Outcome in Environmental Federal Gov Cases",
       x = "Gender",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# industry
gender.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(Party.of.Appointing.President..1. %in% c("Republican")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(Gender, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(gender.industry, aes(x = Gender, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender of Judge and Decision Outcome in Industry Cases",
       x = "Gender",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

  

# Facet wrap------
nonprofit_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Court_of_Appeals,Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(nonprofit_plaintiff, aes(x = Party.of.Appointing.President..1., y = wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Partisanship of Appointing President in NGO or Civic Association Cases",
       x = "Outcome",
       y = "Weighted Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  facet_wrap(vars(Court_of_Appeals)) +
  theme_minimal()



# RESL Regions-----
# Load regions data
regions <- read.csv('/Users/Cordeliavanderveer/Downloads/RESL_Regions.csv')
regions <- regions %>%
  rename(
    "location" = "State"
  )
regions <- regions %>%
  mutate(location = tolower(location))

# Add regions to dataset
final.judge.matches.ex <- final.judge.matches.ex %>%
  left_join(regions, by = "location")

# Visualize region differences for nonprofits
region.nonprofits <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(region.nonprofits, aes(x = Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region of Judge and Decision Outcome in Environmental Nonprofit Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize region differences for industry
region.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(region.industry, aes(x = Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region of Judge and Decision Outcome in Industry Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

region.industry.filter <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(region.industry.filter, aes(x = Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region of Judge and Decision Outcome in Industry Cases (Excluding Industry v. Industry)",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize region differences for federal government
region.gov <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(region.gov, aes(x = Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region of Judge and Decision Outcome in Federal Gov Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Region, industry, partisanship
region.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(Region, Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(region.industry, aes(x = Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Region of Judge and Decision Outcome in Industry Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# Judge birth state-----
# Load regions data
regions <- read.csv('/Users/Cordeliavanderveer/Downloads/RESL_Regions.csv')
regions$State <- NULL

regions <- regions %>%
  rename(
    "Birth.State" = "Abbreviation"
  )


regions <- regions %>%
  rename(
    "Birth.Region" = "Region"
  )

# Add regions to dataset
final.judge.matches.ex <- final.judge.matches.ex %>%
  left_join(regions, by = "Birth.State")

# Visualize region differences for nonprofits
birth.nonprofits <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(Birth.Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(birth.nonprofits, aes(x = Birth.Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Birth Region of Judge and Decision Outcome in Environmental Nonprofit Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize region differences for industry
birth.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(Birth.Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(birth.industry, aes(x = Birth.Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Birth Region of Judge and Decision Outcome in Industry Cases",
       x = "Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

birth.industry.filter <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(Birth.Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(birth.industry.filter, aes(x = Birth.Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Birth Region of Judge and Decision Outcome in Industry Cases (Excluding Industry v. Industry)",
       x = "Birth.Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# Visualize birth region differences for federal government
birth.gov <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(Birth.Region, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

ggplot(birth.gov, aes(x = Birth.Region, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Birth Region of Judge and Decision Outcome in Federal Gov Cases",
       x = "Birth.Region",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()

# All generations-------
get_generation <- function(year) {
  if (year >= 1900 & year <= 1924) {
    return("Greatest Generation")
  } else if (year >= 1928 & year <= 1945) {
    return("Silent Generation")
  } else if (year >= 1946 & year <= 1964) {
    return("Baby Boomers")
  } else if (year >= 1965 & year <= 1980) {
    return("Generation X")
  } else if (year >= 1981 & year <= 1996) {
    return("Millennials")
  } else if (year >= 1997 & year <= 2012) {
    return("Generation Z")
  } else if (year >= 2013 & year <= 2023) {
    return("Generation Alpha")
  } else {
    return(NA)
  }
}

# For nonprofits
final.judge.matches.ex$generation <- sapply(final.judge.matches.ex$Birth.Year, get_generation)
generation.nonprofits <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn")) %>%
  group_by(generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.nonprofits  %>%
arrange(percentage_wins) %>%
mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Nonprofit Cases",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# For industry
generation.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.industry  %>%
  arrange(percentage_wins) %>%
  mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
  ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Industry Cases",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# For industry (excluding industry)
generation.industry.ex <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  filter(!str_detect(def_typ, "industry")) %>%
  group_by(generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.industry.ex  %>%
  arrange(percentage_wins) %>%
  mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
  ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Industry Cases (Excluding Industry Defendants)",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# For federal gov
generation.gov <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.gov  %>%
  arrange(percentage_wins) %>%
  mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
  ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Federal Gov Cases",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# Subsetting data example------
# No subset
generation.industry <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(generation, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.industry  %>%
  arrange(percentage_wins) %>%
  mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
  ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Industry Cases",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

#Subset
generation.industry.subset <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "industry")) %>%
  group_by(generation, Party.of.Appointing.President..1., outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()

generation.industry.subset  %>%
  arrange(percentage_wins) %>%
  mutate(generation = factor(generation, levels=c("Greatest Generation", "Silent Generation", "Baby Boomers", "Generation X"))) %>%
  ggplot(generation.nonprofits, mapping = aes(x = generation, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Generation of Judge and Outcome in Industry Cases (Subset)",
       x = "Generation",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  #facet_wrap(vars(Party.of.Appointing.President..1.)) +
  theme_minimal()

# Create subset of dataset to verify------
verification.judges <- final_judge_matches

verification.judges <- sample_n(verification.judges, 100)
write.csv(verification.judges, "verification.judges.csv")

# Break it down by decades to see how partisanship evolves over time-----
decades <- function(year) {
  if (year >= 1971 & year <= 1979) {
    return("1970s")
  } else if (year >= 1980 & year <= 1989) {
    return("1980s")
  } else if (year >= 1990 & year <= 1999) {
    return("1990s")
  } else if (year >= 2000 & year <= 2009) {
    return("2000s")
  } else if (year >= 2010 & year <= 2019) {
    return("2010s")
  } else if (year >= 2020 & year <= 2029) {
  } else {
    return(NA)
  }
}

final.judge.matches.ex$decade <- sapply(final.judge.matches.ex$decision_year, decades)

# Time series------

time_partisan <- final.judge.matches.ex %>%
  filter(decision_year >= 1971) %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "fed")) %>%
  group_by(decade, outcome) %>%
  summarize(wins = n()) %>%
  mutate(total_matches = sum(wins), percentage_wins = (wins / total_matches) * 100) %>%
  ungroup()


ggplot(time_partisan, aes(x = decade, y = percentage_wins, fill = outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Gender of Judge and Decision Outcome in Environmental Nonprofit Cases",
       x = "Decade",
       y = "Percentage",
       fill = "Decision Outcome") +
  scale_fill_manual(values = c("plaintiff" = "green", "defendant" = "blue")) +
  theme_minimal()


#How many plaintiffs?-------
ngo_plaintiff <- final.judge.matches.ex %>%
  filter(outcome %in% c("defendant", "plaintiff")) %>%
  filter(str_detect(plt_typ, "ngo") | str_detect(plt_typ, "civic_assn"))

biz_plaintiff <- final.judge.matches.ex %>%
  filter(str_detect(plt_typ, "industry"))

#Random statistical stuff for interest---
hist(final.judge.matches.ex$jcs.score.dw[final.judge.matches.ex$Party.of.Appointing.President..1.=="Republican"], freq=FALSE)
hist(final.judge.matches.ex$jcs.score.dw[final.judge.matches.ex$Party.of.Appointing.President..1.=="Democratic"], freq=FALSE)

plot(density(final.judge.matches.ex$jcs.score.dw[final.judge.matches.ex$Party.of.Appointing.President..1.=="Republican"], na.rm = TRUE))

plot(density(final.judge.matches.ex$jcs.score.dw[final.judge.matches.ex$Party.of.Appointing.President..1.=="Democratic"], na.rm = TRUE))

boxplot(final.judge.matches.ex$jcs.score.dw, horizontal = T)

hist(final.judge.matches.ex$Death.Year)

var(final.judge.matches.ex$jcs.score.dw, na.rm = TRUE)

summary(final.judge.matches.ex, na.rm = TRUE)

help("hist")



