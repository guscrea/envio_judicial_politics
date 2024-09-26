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
#install.packages("fuzzyjoin")
# All the data is in the "raw data for matching" folder in the git hub repository

# Load bonica data
bonicajudge <- read.csv('bonica_sen_fed_judges.csv')

# Load fjc data
fjc <- read.csv('US_Federal_Judges.csv')

# Load docket data
final.resl <- read.csv('07-Val_Cor_Clean_Meta.csv')

# Clean bonica/sen data------
fjc_clean <- fjc %>%
  select(
    nid:`Nomination.Date..1.`,`School..1.`:`Other.Nominations.Recess.Appointments`
  )

fjc_clean <- fjc_clean %>%
  filter(
    str_detect(Court.Type..1., "U.S. District Court")
  )

bonicajudge <- bonicajudge %>%
  rename(
    "jid" = "fjc.judge.idno"
  )

bonicajudge <- bonicajudge %>%
  filter(
    str_detect(court.type, "USDC")
  )

bonicajudge$jid <- as.integer(bonicajudge$jid)

fjc_bonica <- left_join(
  fjc_clean, #this is the fjc data
  bonicajudge, # this is the bonica data
  by = "jid" # this is the variable we're using to match the data
)

fjc_bonica <- fjc_bonica %>%
  select(
    -c(judge.last.name, judge.first.name, judge.middle.name, suffix)
  )

# Make sure all data is lowercase
fjc_bonica <- fjc_bonica %>%
  mutate_at(vars(First.Name, Middle.Name, Last.Name), as.character) %>%
  mutate(across(c(First.Name, Middle.Name, Last.Name), tolower))

# Clean fjc_bonica to include extra column with just middle initial
fjc_bonica <- fjc_bonica %>%
  mutate(Middle.Initial = str_extract(Middle.Name,  "\\b[A-Za-z]{1}"))

fjc_bonica <- fjc_bonica %>%
  group_by(jid) %>%
  mutate(
    n = n()
  )


fjc_bonica <- fjc_bonica %>%
  distinct()

write.csv(fjc_bonica, "fjc_bonica_joined.csv")


# Start cleaning RESL data-----
# Remove triplicates
final.resl <- final.resl %>%
  group_by(ID) %>%
  mutate(
    n = row_number() # this labels every row number IN A GROUP. 
  ) %>%
  filter(
    n == 1 # this only keeps the first row of every group. 
  )

# Drop "jr." and "," from RESL data
final.resl$judge_clean <- gsub("( jr\\.| sr\\.| iii| ii| sr| jr| ,|,)", "", final.resl$judge, ignore.case = TRUE)

# Custom function to split docket Judges into first, middle, and last names from docket data
split_names <- function(fullname) {
  words <- strsplit(fullname, " ")[[1]]
  n <- length(words)
  if (n >= 3) {
    return(c(words[1], words[2], paste(words[3:n], collapse = " ")))
  } else if (n == 2) {
    return(c(words[1], "", words[2]))
  } else {
    return(rep(NA, 3))
  }
}

final.resl <- final.resl %>%
  rowwise() %>%
  mutate(NameSplit = list(split_names(judge_clean))) %>%
  ungroup() %>%
  tidyr::unnest_wider(NameSplit, names_sep = "_") %>%
  rename(
    First_Name = NameSplit_1,
    Middle = NameSplit_2,
    Last_Name = NameSplit_3
  )

# Make sure all data is lowercase
final.resl <- final.resl %>%
  mutate(across(c(First_Name, Middle, Last_Name), tolower))

# Clean fjc_bonica to include extra column with just middle initial
final.resl <- final.resl %>%
  mutate(Middle_Initial = str_extract(Middle,  "\\b[A-Za-z]{1}"))

# Let the fuzzy matching commence----
# Define small_str_distance function
small_str_distance <- function(left, right) {
  # Replace NA values with an empty string
  left[is.na(left)] <- ""
  right[is.na(right)] <- ""
  
  # Use stringdist for non-NA values
  stringdist(left, right) <= 2
}

# Fuzzy match
matched_docket <-
  fuzzy_left_join(
    final.resl, fjc_bonica,
    by = c(
      "First_Name"= "First.Name",
      "Middle_Initial"= "Middle.Initial",
      "Last_Name" =  "Last.Name"
    ),
    match_fun =c(
      "First_Name"= small_str_distance,
      "Middle_Initial"= small_str_distance,
      "Last_Name" = small_str_distance
    )
  )


# Mutate to show string distance
matched_docket <- matched_docket %>%
  mutate(
    first_dist = stringdist(First_Name, First.Name),
    middle_dist = stringdist(Middle_Initial, Middle.Initial),
    last_dist = stringdist(Last_Name, Last.Name), 
    total_dist = ifelse(is.na(middle_dist), first_dist + last_dist, first_dist + middle_dist + last_dist),
    matched = ifelse(total_dist == 0, 1, 0)
  )

# Do some initial cleaning of all the matches-----------

# Remove other matches if another match to the same id is perfect
filtered_matches <- matched_docket %>%
  group_by(First_Name, Middle_Initial, Last_Name, ID) %>%
  mutate(has_perfect_match = any(matched == 1)) %>%
  filter(!(has_perfect_match & matched == 0)) %>%
  ungroup() %>%
  select(-has_perfect_match)

# Check to see if district in the RESL and Bonica/Sen data matches

# Load crosswalk data and join to RESL data
court_crosswalk <- read.csv('unique_court_values_crosswalk.csv')
court_crosswalk <- court_crosswalk %>%
  rename(
    "court" = "Court"
  )

filtered_matches <- filtered_matches %>%
  left_join(court_crosswalk, by = "court")

# Extract court names to make a crosswalk codebook
#manual_fjc_crosswalk <- filtered_matches[, c("Court.Name..1.")]

#manual_fjc_crosswalk <- manual_fjc_crosswalk %>%
#unique()

#write.csv(manual_fjc_crosswalk, "manual_fjc_crosswalk.csv")

# Load new crosswalk data, drop notes, and join by Court.Name
court_crosswalk_fjcbonica <- read.csv('manual_fjc_crosswalk.csv')

court_crosswalk_fjcbonica$Notes <- NULL

filtered_matches <- filtered_matches %>%
  left_join(court_crosswalk_fjcbonica, by = "Court.Name..1.")

filtered_matches <- filtered_matches %>%
  mutate(court_mismatch = ifelse(is.na(district) | is.na(District_FJCBonica), NA,
                                 ifelse(district != District_FJCBonica, 0, 1)))

# Group filtered matches for hand coding
filtered_matches$...1 <- NULL

filtered_matches <- filtered_matches %>%
  group_by(ID) %>%
  group_by(judge) %>%
  mutate(
    n = n())

write.csv(filtered_matches, "filtered_matches.csv")

# Split the data into three dataframe for further cleaning-----
# Show perfect matches (no difference between docket first name, middle initial, and last name and judge irst name, middle initial, and last name)
perf_match <- filtered_matches %>%
  filter(total_dist == 0) %>%
  group_by(ID) %>%
  mutate(
    n = n()
  ) %>%
  group_by(judge) %>%
  arrange(ID, judge)

# Find multiple perfect matches for hand code eliminating ---------
perf_match_check <- perf_match %>%
  filter(n > 1)
write.csv(perf_match_check, "perf_match_check_distinct.csv")
n_distinct(perf_match_check$ID) # Should have 63 matches for 63 unique IDs

# Make sure to evaluate matches that have mismatched districts even if there is only one perfect match
perf_match_one <- perf_match %>%
  filter(n == 1)
write.csv(perf_match_one, "perf_match_one_distinct.csv")


# Find what matches (or non matches) need to be checked/coded by hand ---------
# add variable for ids with multiple matches and sort from shortest to largest distance
matches_to_check <- filtered_matches %>%
  group_by(First_Name, Middle_Initial, Last_Name, ID) %>%
  mutate(has_perfect_match = any(total_dist == 0)) %>%
  filter(!(has_perfect_match & total_dist == 0)) %>%
  ungroup() %>%
  select(-has_perfect_match) %>%
  group_by(ID) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()
write.csv(matches_to_check, "matches.to.check.csv")
n_distinct(matches_to_check$ID) # Should have 209 matches/flags for 209 unique IDs

# Find what judges algorithm could not match and do by hand
no_match <- matched_docket %>%
  filter(is.na(total_dist))
write.csv(no_match, "new.data.no.matches.csv")

# Find what not perfect matches need to be reevaluated
reevaluate <- read.csv('matches.to.check.csv')
reevaluate <- reevaluate %>%
  filter(keep == 3)
write.csv(reevaluate, "reevaluate.nonperf.match.csv")