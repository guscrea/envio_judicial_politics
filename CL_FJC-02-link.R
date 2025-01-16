# FJC and Court Listener Data
# 02 - Link/Join Court Listener Data to Itself and to FJC Data
# Script by Chris Rea

library(tidyverse)
library(tidytext)
library(patchwork)
library(viridis)
library(broom) # for tidying regression results
library(corrplot) # for visial correlation matricies
library(vtable)
library(glm2) # for logistic regression
library(car) # for vif scores
library(sjPlot) # for reg. forest plots
library(lubridate)

# Read in Court Listener Data ####

# read in cl environmentally-focused docket data
cl_e <- read_csv(
  #"/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/NOS_893_dockets-2024-08-31.csv"
  "/Users/chrisrea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/NOS_893_dockets-2024-08-31.csv"
)

# read in cl court data
cl_court <- read_csv(
  #"/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/courts-2024-08-31.csv",
  "/Users/chrisrea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/courts-2024-08-31.csv",
  quote = "`"
) %>%
  rename(
    "court_id" = "id"
  ) %>%
  select(
    court_id, 
    pacer_court_id,
    fjc_court_id
  )

# read in cl people - people data
cl_people_peo <- read_csv(
  #"/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-people-2024-08-31.csv",
  "/Users/chrisrea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-people-2024-08-31.csv",
  quote = "`"
  ) %>%
  # check for duplicates
  group_by(
    id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()
 #--> none. each ID is unique

# read in cl people - political affiliation data
cl_people_pol <- read_csv(
  #"/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-political-affiliations-2024-08-31.csv",
  "/Users/chrisrea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-political-affiliations-2024-08-31.csv",
  quote = "`"
  )

# Join CL data to itself ####

# join court data to docket data
cl_e <- left_join(
  cl_e,
  cl_court,
  by = "court_id"
  )

# join person data to docket and court data - assigned to
cl_e <- left_join(
  cl_e,
  cl_people_peo %>%
    rename(
      "assigned_to_id" = "id"
    ) %>%
    select(
      -c(
        date_created,
        date_modified,
        slug,
        n
      )
    ),
  by = "assigned_to_id"
  )

# join person data to docket and court data - referred to
cl_e <- left_join(
  cl_e,
  cl_people_peo %>%
    rename(
      "referred_to_id" = "id"
    ) %>%
    select(
      c(
        referred_to_id,
        fjc_id,
        name_first,
        name_middle,
        name_last,
        name_suffix,
        date_dob,
        gender
        ) 
      ) %>%
    rename_with(
      ~ paste0("ref_", .x, recycle0 = TRUE),
      starts_with("name") | starts_with("date") | starts_with("gender") | starts_with("fjc_id")
    ),
  by = "referred_to_id"
)

check <- cl_e %>%
  select(
    id,
    date_filed,
    date_terminated,
    docket_number,
    docket_number_core,
    case_name,
    assigned_to_str,
    assigned_to_id,
    fjc_court_id,
    fjc_id,
    name_first,
    name_middle,
    name_last,
    gender,
    ref_fjc_id,
    ref_name_first,
    ref_name_middle,
    ref_name_last,
    ref_gender
  )

rm(check)

# join political affiliation to docket, court, and person data - assigned to
cl_e <- left_join(
  cl_e,
  cl_people_pol %>%
    select(
      c(
        id,
        political_party,
        source
        )
      ) %>%
    rename(
      "assigned_to_id" = "id",
      "source_pa" = "source"
      ),
  by = "assigned_to_id"
  )

# join political affiliation to docket, court, and person data - referred to
cl_e <- left_join(
  cl_e,
  cl_people_pol %>%
    select(
      c(
        id,
        political_party,
        source
      )
    ) %>%
    rename(
      "assigned_to_id" = "id",
      "source_pa" = "source"
    ) %>%
    rename_with(
      ~ paste0("ref_", .x, recycle0 = TRUE),
      starts_with("poli") | starts_with("sour")
    ),
  by = "assigned_to_id"
)

# consolidate assigned to and reassigned to columns
# --> replace assigned to judge info with referred to judge info for purposes of
# analysis, since the referred to judge is the one who will make decisions on
# the case
cl_e <- cl_e %>%
  ungroup() %>%
  mutate(
    name_first = case_when(
      !is.na(ref_name_first) ~ ref_name_first,
      TRUE ~ name_first
    ),
    name_middle = case_when(
      !is.na(ref_name_middle) ~ ref_name_middle,
      TRUE ~ name_middle
    ),
    name_last = case_when(
      !is.na(ref_name_last) ~ ref_name_last,
      TRUE ~ name_last
    ),
    name_suffix = case_when(
      !is.na(ref_name_suffix) ~ ref_name_suffix,
      TRUE ~ name_suffix
    ),
    date_dob = case_when(
      !is.na(ref_date_dob) ~ ref_date_dob,
      TRUE ~ date_dob
    ),
    gender = case_when(
      !is.na(ref_gender) ~ ref_gender,
      TRUE ~ gender
    ),
    political_party = case_when(
      !is.na(ref_political_party) ~ ref_political_party,
      TRUE ~ political_party
    ),
    source_pa = case_when(
      !is.na(ref_source_pa) ~ ref_source_pa,
      TRUE ~ source_pa
    ),
    assigned_to_id = case_when(
      !is.na(referred_to_id) ~ referred_to_id,
      TRUE ~ assigned_to_id
    ),
    fjc_id = case_when(
      !is.na(ref_fjc_id) ~ ref_fjc_id,
      TRUE ~ fjc_id
    )
  ) %>%
  # now drop all referred to data, since it has now replaced original assigned 
  # to data.
  select(
    -starts_with("ref_"),
    -c(referred_to_id, referred_to_str)
  )

check <- cl_e %>%
  select(
    id,
    date_filed,
    date_terminated,
    docket_number,
    docket_number_core,
    case_name,
    assigned_to_str,
    assigned_to_id,
    fjc_court_id,
    fjc_id,
    name_first,
    name_middle,
    name_last,
    gender,
    political_party,
    source_pa
  )

rm(check)

# get file year; make dist-docket-yr_file number for joining to FJC
cl_e <- cl_e %>%
  mutate(
    yr_file = year(date_filed),
    # clean case name
    case_name_clean = str_to_lower(case_name),
    case_name_clean = str_remove_all(case_name_clean, "<b>|</b>|<font color=\"red\">|</font>|(ps)|(ss)"),
    plt_fl = str_sub(trimws(case_name_clean),1,1),
    join_id = str_c(fjc_court_id,"-",docket_number_core,"-", yr_file,"-",plt_fl)
  ) %>%
  # assess uniqueness of join_id
  group_by(
    join_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()

# drop component dfs
rm(cl_court, #cl_people_peo,
   cl_people_pol)

# Examine & Clean Court Listener Data ####

# distribution of years for all CL data
cl_e %>%
  ggplot(
    aes(
      x = yr_file,
    )
  ) +
  geom_bar() + 
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,2000)
  )

# simple full cl_e df
cl_e_simp <- cl_e %>%
  select(
    id, yr_file
  ) %>%
  mutate(
    data_type = "All raw data from Court Listener\n(n = 34,263)"
  )

# IMC Global and BP Deepwater Horizon cases immediately pop out. Drop those
# cases.
cl_e_clean <- cl_e %>%
  filter(
    !(court_id == "laed" & str_detect(case_name, "BP") & yr_file >= 2010),
    !(court_id == "scd" & str_detect(case_name, "IMC")),
    #!is.na(assigned_to_id)
  )

# make simple clean df
cl_e_clean_simp <- cl_e_clean %>%
  select(
    id, yr_file
  ) %>%
  mutate(
    data_type = "Base Data (No BP or IMC)\n(n = 29,174)"
  )

# plot distribution of years for all and for cleaned data, for comparison
plot_all_clean <- bind_rows(
  cl_e_clean_simp,
  cl_e_simp
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      group = data_type,
      fill = data_type
    )
  ) +
  geom_bar(
    position = "identity",
    alpha = .8,
    #color = "#777777"
  ) +
  scale_fill_grey() + 
  #scale_fill_viridis_d() +
  labs(
    x = NULL,
    y = "Count of Cases",
    fill = "Data"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,2000)
  )

# plot comparison
plot_all_clean

# after dropping BP and IMC Global cases, how many observations do we have with
# judges recorded?
# --> starting with 29,174 cases
# count observations with judges observed:
cl_e_clean_j <- cl_e_clean %>%
  filter(
    !is.na(assigned_to_id)
  )

# --> 21,973
# --> So we lose ~25% for our original observations (7,274). That's a lot!
# Certainly need to investigate if those cases for which we do not observe the
# the judge are systematically different from those where we do.

# make simple clean df with judges
cl_e_clean_j_simp <- cl_e_clean_j %>%
  select(
    id, yr_file
  ) %>%
  mutate(
    data_type = "Observations with judge recorded\n(n = 21,973)"
  )

# plot distribution of years for cleaned data and cleaned data with judges, for
# comparison
plot_clean_j <- bind_rows(
  cl_e_clean_simp %>%
    mutate(
      data_type = "Clean data (No BP or IMC)\n(n = 29,174)"
    ),
  cl_e_clean_j_simp
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      group = data_type,
      fill = data_type
    )
  ) +
  geom_bar(
    position = "identity",
    alpha = .7,
    #color = "#777777"
  ) +
  scale_fill_grey() + 
  #scale_fill_viridis_d() +
  labs(
    x = NULL,
    y = "Count of Cases",
    fill = "Data"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,1250)
  )
  
# plot the plot
 plot_clean_j

# check uniqueness of join_id in cl_e_clean_j; for now, drop non-unique
# observations (21,900 --> 21,730)
 cl_e_clean_j <- cl_e_clean_j %>%
   group_by(
     join_id
   ) %>%
   mutate(
     n = n()
   ) %>%
   filter(
     n == 1
   )

# retain just judge info for join to FJC data
cl_e_clean_j <- cl_e_clean_j %>%
  select(
    id,
    join_id,
    case_name,
    court_id,
    #assigned_to_str,
    assigned_to_id,
    fjc_id,
    #referred_to_str,
    #referred_to_id,
    name_first:plt_fl
    )

# Read in Bonica and Sen Judicial Ideology Data ######

b_s <- read_csv(
  "raw data for matching/bonica_sen_fed_judges.csv"
  ) %>%
  filter(
    court.type == "USDC"
  ) %>%
  rename(
    "fjc_id" = "fjc.judge.idno"
  ) %>%
  select(
   fjc_id,
   judge.first.name,
   judge.middle.name,
   judge.last.name,
   suffix,
   party.affiliation.of.president,
   president.name,
   court.type,
   court.name,
   nomination.date.senate.executive.journal,
   enter.year,
   dime.cid,
   dime.cfscore,
   imputed.dime.cfscore,
   jcs.score.dw,
   jcs.cfscore.cf,
   pres.dw,
   pres.dime.cfscore,
   senscore.dw,
   senscore.dime.cfscore,
   state.delegation.dw,
   state.delegation.dime.cfscore,
   birth.year,
   race.or.ethnicity,
   jd.rank,
   clerked.for.con,
   clerked.for.lib
  ) %>%
  # check for duplicate rows
  group_by(
    fjc_id
  ) %>%
  mutate(
    # make nomination date a date
    nomination.date.senate.executive.journal = mdy(nomination.date.senate.executive.journal),
    n = n(),
    row_n = row_number(),
    # make data_source variable
    data_source = "b_s"
  ) %>%
  filter(
    row_n == 1
  ) %>%
  select(
    -c(n, row_n)
  ) %>%
  ungroup()

# note: although the b-s data technically go to 2016, they are effectively
# truncated after 2014, with only a handful of judges appointed in 2015 and none
# in 2016. We keep these data and use the "data_source" variable to flag were
# they came from, retaining these observations over the supplementary ones below
# where there are overlaps.

# Read in and join later-date supplementary DIME (and prez party) judicial ideology scores ####

# the b_s data above ends in 2014, which means that we lack ideology scores for
# any judges appointed at the end of the Obama administration and all of the
# Trump and Biden administrations after. The data below add more recent ideology
# data for some indicators from some years.

# read in s_b DIME score updates
s_b <- read_csv(
  "data/supplemental_judicial_ideology/fjc_jep.csv"
) %>%
  # keep only post-2016 appointments to District Courts
  # note: note 100% the b_s
  filter(
    enter.year >= 2014, # we allow for one year of overlap between b_s and s_b data
    court.type == "USDC"
  ) %>%
  # make judge ID var name compatible with fjc data
  rename(
    "fjc_id" = "fjc.judge.idno",
    # rename "dime.score" to "dime.cfscore" for compatibility with b_s data;
    # same with following variable names
    "dime.cfscore" = "dime.score",
    "president.name" = "pres"
  ) %>%
  mutate(
    data_source = "s_b",
    # make imputed.dime.cfscore the same as dime.cfscore, since this is the
    # variable we use for analysis.
    imputed.dime.cfscore = dime.cfscore
  ) %>%
  # drop variables not retained in b_s data
  select(
    -c(exit.year)
  )

# append supplementary data to original b_s data; look for duplicates; keep
# older data (b_s) in favor of newer data (s_b) where there are overlaps/ this
# grows the data set from 1799 observations (the original b_s data set) to 1948
# observations - an increase of 149 judges. Most of these - 136 - are Trump
# appointees.
b_s <- bind_rows(
  b_s,
  s_b
) %>%
  group_by(
    fjc_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  filter(
    n == 1 |
      (n == 2 & data_source == "b_s")
  ) %>%
  # drop counting column
  select(
    -n
  )


# Read in Boyd JCS DW Nominate scores (through 2022) and join as well #### 

# The Boyd data use nid, not the jid (which we use, above) to link across data
# sources. So, we have to read in the U.S. federal judges data course to
# cross-walk between Boyd's data and the other data sources we call on. This
# data also includes a range of other variables not in Boyd's data that are
# useful (gender, race, appointing president, etc.).

# read in US judges data.
us_judges <- read_csv(
  "raw data for matching/US_Federal_Judges.csv"
)

# read in Boyd data
boyd <- read_csv(
  "data/supplemental_judicial_ideology/Boyd-district-court-ideology-scores-through-117th-Cong-Feb2023.csv"
  #"data/supplemental_judicial_ideology/Boyd-district-court-ideology-scores-through-117th-Cong-Feb2023_GPT_names_parsed.csv"
) %>%
  rename(
    "judgename_boyd" = "judgename",
    "ideology_score_boyd" = "ideology_score",
    "district1_boyd" = "district1",
    "district2_boyd" = "district2",
    "district3_boyd" = "district3"
  ) %>%
  mutate(
    data_source_boyd = "boyd"
  )

# join the boyd data to the U.S. Judges data. Note here that we (re)name the
# joined data frame "boyd", but in fact we are left joining boyd to the U.S.
# courts data.

boyd <- left_join(
  us_judges,
  boyd,
  by = "nid"
) %>%
  # keep only observations that boyd has data for
  filter(
    !is.na(data_source_boyd)
  ) %>%
  # retain important observations
  select(
    nid,
    jid,
    `Birth Year`,
    Gender,
    `Race or Ethnicity`,
    `Appointing President (1)`,
    `Party of Appointing President (1)`,
    `Nomination Date (1)`,
    `Commission Date (1)`,
    judgename_boyd,
    ideology_score_boyd,
    data_source_boyd
  ) %>%
  # rename jid as fjc_id
  rename(
    "fjc_id" = "jid",
    "birth.year.boyd" = "Birth Year",
    "race.or.ethnicity.boyd" = "Race or Ethnicity",
    "president.name.boyd" = "Appointing President (1)",
    "party.affiliation.of.president.boyd" = "Party of Appointing President (1)",
    "jcs.score.dw.boyd" = "ideology_score_boyd",
    "nomination.date.boyd" = "Nomination Date (1)",
    "enter.year.boyd" = "Commission Date (1)"
  ) %>%
  # make commission date into enter.year
  mutate(
    enter.year.boyd = mdy(enter.year.boyd),
    enter.year.boyd = year(enter.year.boyd),
    birth.year.boyd = as.numeric(birth.year.boyd),
    nomination.date.boyd = mdy(nomination.date.boyd)
  )

# the Boyd data includes both judges already in the b_s (And supplemented s_b)
# data, as well as later appointments not included in either (up through 2022).
# Thus, we use a full join between the data sets, to capture all of the Boyd
# data and compare it to the b_s data. Where there is overlap, we retain the b_s
# data. Where there is not, we bring the Boyd data into the b_s data.

b_s <- full_join(
  b_s,
  boyd,
  by = "fjc_id"
) %>%
  # order data for ease of comparison
  select(
    fjc_id,
    judge.first.name,
    judge.middle.name,
    judge.last.name,
    judgename_boyd,
    suffix,
    party.affiliation.of.president,
    party.affiliation.of.president.boyd,
    president.name,
    president.name.boyd,
    court.type,
    court.name,
    nomination.date.senate.executive.journal,
    nomination.date.boyd,
    enter.year,
    enter.year.boyd,
    dime.cid,
    dime.cfscore,
    imputed.dime.cfscore,
    jcs.score.dw,
    jcs.score.dw.boyd,
    jcs.cfscore.cf,
    pres.dw,
    pres.dime.cfscore,
    senscore.dw,
    senscore.dime.cfscore,
    state.delegation.dw,
    state.delegation.dime.cfscore,
    birth.year,
    birth.year.boyd,
    race.or.ethnicity,
    race.or.ethnicity.boyd,
    jd.rank,
    clerked.for.con,
    clerked.for.lib,
    data_source,
    data_source_boyd
  ) %>%
  mutate(
    overlap = case_when(
      !is.na(data_source) & !is.na(data_source_boyd) ~ 1,
      TRUE ~ 0
    )
  )

# remove unecessary dfs
rm(
  boyd,
  us_judges,
  s_b
)

# where Boyd data is present and other b_s or s_b data is not, use Boyd
# scores/info (some of which comes from U.S. Courts). Then drop "boyd" columns

b_s <- b_s %>%
  mutate(
    jcs.score.dw = case_when(
      is.na(jcs.score.dw) & !is.na(jcs.score.dw.boyd) ~ jcs.score.dw.boyd,
      TRUE ~ jcs.score.dw
    ),
    race.or.ethnicity = case_when(
      is.na(race.or.ethnicity) & !is.na(race.or.ethnicity.boyd) ~ race.or.ethnicity.boyd,
      TRUE ~ race.or.ethnicity
    ),
    birth.year = case_when(
      is.na(birth.year) & !is.na(birth.year.boyd) ~ birth.year.boyd,
      TRUE ~ birth.year
    ),
    nomination.date.senate.executive.journal = case_when(
      is.na(nomination.date.senate.executive.journal) & !is.na(nomination.date.boyd) ~ nomination.date.boyd,
      TRUE ~ nomination.date.senate.executive.journal
    ),
    enter.year = case_when(
      is.na(enter.year) & !is.na(enter.year.boyd) ~ enter.year.boyd,
      TRUE ~ enter.year
    )
  ) %>%
  select(
    -contains("boyd"),
    -overlap
  )


# Plot judicial ideology (DIME) scores by president ####

# make plot df
plot_df_dime <- b_s %>%
  # nominating presidents
  filter(
    president.name %in% c(
      "Donald J. Trump",
      "George W. Bush",
      "Ronald Reagan",
      "George H.W. Bush",
      "William J. Clinton",
      "Jimmy Carter",
      "Barack Obama",
      "Richard M. Nixon"
    )
  ) %>%
  ungroup() %>%
  # select relevant variables
  select(
    president.name, party.affiliation.of.president, imputed.dime.cfscore,jcs.score.dw
  ) %>%
  # first, count number of appointments by president; make prez name with n
  group_by(
    president.name
  ) %>%
  mutate(
    appt_n = n(),
    prez_name_n = str_c(president.name, " (n = ", appt_n, ")")
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(imputed.dime.cfscore,jcs.score.dw),
    names_to = "ideology_var",
    values_to = "ideology_score"
  ) %>%
  filter(
    !is.na(party.affiliation.of.president),
    ideology_var == "imputed.dime.cfscore"
  ) %>%
  # calculate average and mdeia ideology scores by president
  group_by(
    president.name, ideology_var
  ) %>%
  mutate(
    mean_score = mean(ideology_score, na.rm = T),
    med_score = median(ideology_score, na.rm = T)
  )

# make factor levels and labels
lev_lab <- plot_df_dime %>%
  ungroup() %>%
  select(
    prez_name_n, med_score
  ) %>%
  group_by(
    prez_name_n
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  arrange(
    med_score
  )

f_labs <- lev_lab$prez_name_n
f_levs <- lev_lab$med_score
  
# plot ideology of appointments plot 
plot_dime <- plot_df_dime %>%
  mutate(
    prez_name_n_f = factor(
      med_score,
      levels = f_levs,
      labels = f_labs
    )
  ) %>%
  ggplot(
    aes(
      y = prez_name_n_f,
      x = ideology_score,
      color = med_score
    )
  ) +
  scale_color_viridis() +
  labs(
    y = "President",
    x = "Judicial Ideology\n-  <-- liberal     conservative -->  +",
    color = "Median Judge Ideology"
    ) +
  geom_boxplot() +
  facet_wrap(
    vars(ideology_var)
  ) +
  theme_linedraw()

# Plot judicial ideology (DW) scores by president ####

# make plot df
plot_df_dw <- b_s %>%
  # nominating presidents
  filter(
    president.name %in% c(
      "Donald J. Trump",
      "George W. Bush",
      "Ronald Reagan",
      "George H.W. Bush",
      "William J. Clinton",
      "Jimmy Carter",
      "Barack Obama",
      "Richard M. Nixon"
    )
  ) %>%
  ungroup() %>%
  # select relevant variables
  select(
    president.name, party.affiliation.of.president, imputed.dime.cfscore,jcs.score.dw
  ) %>%
  # first, count number of appointments by president; make prez name with n
  group_by(
    president.name
  ) %>%
  mutate(
    appt_n = n(),
    prez_name_n = str_c(president.name, " (n = ", appt_n, ")")
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(imputed.dime.cfscore,jcs.score.dw),
    names_to = "ideology_var",
    values_to = "ideology_score"
  ) %>%
  filter(
    !is.na(party.affiliation.of.president),
    ideology_var == "jcs.score.dw"
  ) %>%
  # calculate average and mdeia ideology scores by president
  group_by(
    president.name, ideology_var
  ) %>%
  mutate(
    mean_score = mean(ideology_score, na.rm = T),
    med_score = median(ideology_score, na.rm = T)
  )

# make factor levels and labels
lev_lab <- plot_df_dw %>%
  ungroup() %>%
  select(
    prez_name_n, med_score
  ) %>%
  group_by(
    prez_name_n
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  ungroup() %>%
  arrange(
    med_score
  )

f_labs <- lev_lab$prez_name_n
f_levs <- lev_lab$med_score

# plot ideology of appointments plot 
plot_dw <- plot_df_dw %>%
  mutate(
    prez_name_n_f = factor(
      med_score,
      levels = f_levs,
      labels = f_labs
    )
  ) %>%
  ggplot(
    aes(
      y = prez_name_n_f,
      x = ideology_score,
      color = med_score
    )
  ) +
  scale_color_viridis() +
  labs(
    y = "President",
    x = "Judicial Ideology\n-  <-- liberal     conservative -->  +",
    color = "Median Judge Ideology"
  ) +
  geom_boxplot() +
  facet_wrap(
    vars(ideology_var)
  ) +
  theme_linedraw()

# Plot judicial both ideology (DIME and DW) scores; write out DIME ####

plot_dw | plot_dime +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

plot_dime

ggsave(
  "Prez_appointment_ideology.png",
  units = "mm",
  width = 250,
  height =  200,
  path = "Figures"
)

# Join judicial ideology data to CL data ####

# join Bonica and Sen data to Court Listener data
cl_e_clean_j <- left_join(
  cl_e_clean_j,
  b_s,
  by = "fjc_id"
  )


# examine matched data
check <- cl_e_clean_j %>%
  select(
    case_name,
    assigned_to_id,
    fjc_id,
    yr_file,
    name_first,
    name_middle,
    name_last,
    name_suffix,
    judge.first.name,
    judge.middle.name,
    judge.last.name,
    suffix,
    political_party,
    party.affiliation.of.president,
    date_dob,
    birth.year
  )

rm(check)

check <- cl_e_clean_j %>%
  select(
    case_name,
    name_first,
    name_middle,
    name_last,
    political_party,
    party.affiliation.of.president
  ) %>%
  mutate(
    t = case_when(
      is.na(party.affiliation.of.president) &
        !is.na(political_party) ~ 1,
      TRUE ~ 0
    )
  )

# for handful of instances where CL data has partisan affiliation and B&S do
# not, take either R or D from CL and add it to B&S data.
cl_e_clean_j <- cl_e_clean_j %>%
  ungroup() %>%
  mutate(
    party.affiliation.of.president = case_when(
      is.na(party.affiliation.of.president) & political_party == "d" ~ "Democratic",
      TRUE ~ party.affiliation.of.president
    ),
    party.affiliation.of.president = case_when(
      is.na(party.affiliation.of.president) & political_party == "r" ~ "Republican",
      TRUE ~ party.affiliation.of.president
    )
  )
  
# Read in FJC data ####

# read in cleaned fjc_idb data; drop BP and IMC cases
fjc_e <- read_csv(
  #"/Users/crea/Dropbox\ (Personal)/Professional/Research/_RESL/Environmental_Law_Research/Nature_Sustainability_2024/Data/FJC_postprocessed/District/fjc_e_post_processed_clean.csv"
  "/Users/chrisrea/Dropbox\ (Personal)/Professional/Research/_RESL/Environmental_Law_Research/Nature_Sustainability_2024/Data/FJC_postprocessed/District/fjc_e_post_processed_clean.csv"
  
  ) %>%
  filter(
    !(DISTRICT == "3L" & (str_detect(PLT,"BP")==T | str_detect(DEF,"BP")==T)), # drop BP cases in LA
    !(DISTRICT == "20" & str_detect(DEF,"IMC")==T) # drop IMC cases in SC
    )

# distribution of years (again)
fjc_e %>%
  ggplot(
    aes(
      x = yr_file,
    )
  ) +
  geom_bar() + 
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,1250)
  )

# make dist-docket number for join to CL data
fjc_e <- fjc_e %>%
  mutate(
    # clean case name
    PLT_clean = str_to_lower(PLT),
    PLT_clean = str_remove_all(PLT_clean, "<b>|</b>|<font color=\"red\">|</font>|(ps)|(ss)"),
    plt_fl = str_sub(trimws(PLT_clean),1,1),
    join_id = str_c(DISTRICT,"-",DOCKET,"-", yr_file,"-",plt_fl)
  ) %>%
  # assess uniqueness
  group_by(
    join_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup() %>%
  filter(
    n == 1 # drops  from 29.9 to 29.3
  )

# Join  CL data to FJC data ####

# try join
fjc_cl <- left_join(
  fjc_e,
  cl_e_clean_j %>%
    select(
      -yr_file
    ),
  by = "join_id"
)


# create df with only those observations that have judges
fjc_cl_j <- fjc_cl %>%
  filter(
    !is.na(assigned_to_id)
  )

# make simple df of joined cl-fjc data
fjc_cl_simp <- fjc_cl_j %>%
  select(
    id, yr_file
  ) %>%
  mutate(
    data_type = "Data matched to FJC\n(n = 19,253)"
  )


# call function for generating simple input dfs
source("functions/simple_filter.R")

# make simple df of joined-cl-fjc data TO BE USED IN REGRESSIONS
fjc_cl_j_reg_simp <- simp_filt(
  df = "fjc_cl_j",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE
  ) %>%
  filter(
    !is.na(PLT_typ),
    !is.na(REGION),
    !is.na(yr_file),
    !is.na(fjc_id),
    !is.na(state.delegation.dime.cfscore)
  ) %>%
  select(
    id, yr_file, DISP
  ) %>%
  mutate(
    data_type = "Cleaned Data \n(n = 13,341)"
  )


# plot distribution of years for cleaned data and cleaned data with judges, for
# comparison
plot_cl_fjc <- bind_rows(
  cl_e_clean_simp %>%
    mutate(
      data_type = "Base data (No BP or IMC)\n(n = 29,174)"
    ),
  # cl_e_clean_j_simp %>%
  #   mutate(
  #     data_type = "Clean data with judge recorded\n(n = 21,973)"
  #   ),
  #fjc_cl_simp,
  fjc_cl_j_reg_simp,
  fjc_cl_j_reg_simp %>%
    filter(
      DISP %in% c(2,3,14,6,7,8,9,17,19,20)
    ) %>%
    mutate(
      data_type = "Final data used for analysis\n(n = 4,535)"
    )
  ) %>%
  ggplot(
    aes(
      x = yr_file,
      group = data_type,
      fill = data_type
    )
  ) +
  geom_bar(
    position = "identity",
    alpha = .8,
    #color = "#777777"
  ) +
  scale_fill_grey() + 
  #scale_fill_viridis_d() +
  labs(
    x = NULL,
    y = "Count of Cases",
    fill = "Data"
  ) +
  theme_linedraw() +
  coord_cartesian(
    ylim = c(0,1250)
  )

# plot the plot
plot_cl_fjc

# Plot distributions of data together ####

# plot distributions in relation
plot_all_clean / plot_cl_fjc +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "Data_cleaning_distributions.png",
  units = "mm",
  width = 250,
  height =  200,
  path = "Figures"
)

# Code outcomes; add districts; build age and generation vars ######

# read back in coded district crosswalk and format
dist_crosswalk <- read_csv("crosswalks/district_crosswalk_coded.csv") %>%
  mutate(
    FJC_dist_code = str_pad(FJC_dist_code,2,side = "left", pad = 0)
  ) %>%
  rename(
    "DISTRICT" = "FJC_dist_code"
  ) %>%
  mutate(
    DISTRICT = str_pad(DISTRICT,2,side = "left", pad = 0)
  )

# Code outcomes; add districts; create green generation indicator and age vars
fjc_cl_j <- fjc_cl_j %>%
  ungroup() %>%
  mutate(
    jud_or_set = case_when(
      # below, we make different determinations of what kinds of cases we want
      # to include in an analysis. See FJC codebook for DISP codes.
      # 
      # most permissive:
      # call "in" for analysis all cases that are dismissed, settled, or reach
      # judgement.
      # note -8 signals missing disposition;it is excluded
      #DISP %in% c(2,3,12,13,14,4,5,6,7,8,9,15,16,17,18,19,20) ~ 1,
      # 
      # middle ground:
      # exclude vol. dismissals (12), judgement on default (4), statistical
      # closings (18), awards of arbitrator (15), and stays pending
      # bankruptcy (16) from analysis, since judges have less role in such
      # cases. We also exclude settlements (13) and judgements on consent 
      # (5; essentially, court-enforceable settlement agreements) since
      # judges also play a minimal role in such cases. 
      DISP %in% c(2,3,14,6,7,8,9,17,19,20) ~ 1,
      #
      # most aggressive exclusions:
      # in addition to above, also exclude dismissals because of want of
      # prosecution (2) and lack of jurisdiction (3), settlements (13) "other"
      # dismissals (14) and "other" judgements (17)
      #DISP %in% c(2,3,5,6,7,8,9,19,20) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  # next, code for plt/def wins and losses
  # PLT wins include:
  # - all judgements for plaintiff or both
  # - all settlements
  # PLT loses include:
  # - all judgements for defendant
  # - all dismissals except settlements
  # DEF wins include
  # - all judgements for defendant or both
  # - all dismissals except settlements
  # DEF losses include:
  # - all judgements for plaintiff
  # - all settlements
  mutate(
    PLT_wl = case_when(
      JUDGMENT %in% c(1,3) | DISP == 13 ~ "w",
      JUDGMENT == 2 | DISP %in% c(2,3,12,14) ~ "l",
      TRUE ~ "n"
    ),
    DEF_wl = case_when(
      JUDGMENT %in% c(2,3) | DISP %in% c(2,3,12,14) ~ "w",
      JUDGMENT %in% c(1) | DISP == 13 ~ "l",
      TRUE ~ "n"
    )
  ) %>%
  # green generation: pree green, green, after green
  mutate(
    date_dob = ymd(date_dob),
    TERMDATE = ymd(TERMDATE),
    age_at_term = as.numeric((TERMDATE-date_dob)/365.25),
    green_gen = case_when(
      date_dob < ymd("1935-01-01") ~ "PG",
      date_dob >= ymd("1935-01-01") & date_dob <= ymd("1950-12-31") ~ "GG",
      date_dob > ymd("1950-12-31") ~ "AG"
    ),
    green_gen = as.factor(green_gen)
  )

fjc_cl_j$age_at_term[1]

# Plot FJC heatmaps ######

# call function for generating graphics input dfs
source("functions/simple_filter.R")

# call heat map plotting function
source("functions/heat_map.R")

# set plaintiff types for heat map
l_typs <- c("BIZ",
            #"CIVIC",
            "FED",
            "IND",
            "LOC",
            "NGO",
            #"NGO_O",
            #"OTHER",
            #"TRIBE",
            "STA"
)

# Plot heat maps of litigant types - FJC-CL combined data
plot_fjc_cl_judges <-  plot_PD_combo_heatmap(
  df = simp_filt(
    df = "fjc_cl_j",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE
  ) %>% filter(
    !is.na(PLT_typ),
    !is.na(REGION),
    !is.na(yr_file),
    !is.na(fjc_id),
    DISP %in% c(2,3,14,6,7,8,9,17,19,20),
    !is.na(state.delegation.dime.cfscore)
  ) ,
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - FJC Data for Analysis (with intra-type cases) - District Courts 1980-2022",
  court_level = "D"
)

# Plot heat maps of litigant types - FJC data alone
plot_fjc_raw <- plot_PD_combo_heatmap(
  df = simp_filt(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = TRUE
  ),
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - FJC Data Alone - District Courts 1980-2022",
  court_level = "D"
)

# plot plt-def distributions in relation
plot_fjc_raw / plot_fjc_cl_judges +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "PLT-DEF_matrix_compare.png",
  units = "mm",
  width = 180,
  height =  300,
  path = "Figures"
)

# NOW WITH NO DIAG

# Plot heat maps of litigant types - FJC-CL combined data
plot_fjc_cl_judges_nodiag <-  plot_PD_combo_heatmap(
  df = simp_filt(
    df = "fjc_cl_j",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE
    ) %>% filter(
      !is.na(PLT_typ),
      !is.na(REGION),
      !is.na(yr_file),
      !is.na(fjc_id),
      DISP %in% c(2,3,14,6,7,8,9,17,19,20),
      !is.na(state.delegation.dime.cfscore)
    ) ,
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - FJC Data for Analysis (w/o intra-type suits) - District Courts 1980-2022",
  court_level = "D"
  )

# Plot heat maps of litigant types - FJC data alone
plot_fjc_raw <- plot_PD_combo_heatmap(
  df = simp_filt(
    df = "fjc_e",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE
  ),
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - FJC Data Alone - District Courts 1980-2022",
  court_level = "D"
  )

# plot plt-def distributions in relation
plot_fjc_raw / plot_fjc_cl_judges_nodiag +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "PLT-DEF_matrix_compare_no_diag.png",
  units = "mm",
  width = 180,
  height =  300,
  path = "Figures"
  )





# Plot geographic (district) distributions ####

# list of state names - lower case
state_names_lower <- c("Alabama", "Alaska", "American Samoa", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Minor Outlying Islands", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "U.S. Virgin Islands", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

state_name_lower_orlist <- paste(state_names_lower, collapse = "|")

# Geographic distribution of cases for all FJC data
plot_dist_fjc <- simp_filt(
  df = "fjc_e",
  jud = FALSE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = TRUE
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

plot_dist_fjc <-  plot_dist_fjc %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2,
      fill = PLT_typ2
    )
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = "Plaitiff Types"
  ) +
  #facet_wrap(
  #  vars(PLT_typ),
  #  ncol = 2
  #) +
  scale_color_viridis_d(option="mako",) +
  scale_fill_viridis_d(option="mako",) +
  #guides(colour = "none", fill = "none") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Geographic distribution of FJC-CL data with judges - USED FOR ANALYSIS
plot_dist_fjc_cl_j <- simp_filt(
  df = "fjc_cl_j",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE
  ) %>%
  filter(
    !is.na(PLT_typ),
    !is.na(REGION),
    !is.na(yr_file),
    !is.na(fjc_id),
    !is.na(state.delegation.dime.cfscore),
    DISP %in% c(2,3,14,6,7,8,9,17,19,20)
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

plot_dist_fjc_cl_j <-  plot_dist_fjc_cl_j %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2,
      fill = PLT_typ2
    )
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = "Plaitiff Types"
  ) +
  #facet_wrap(
  #  vars(PLT_typ),
  #  ncol = 2
  #) +
  scale_color_viridis_d(option="mako",) +
  scale_fill_viridis_d(option="mako",) +
  #guides(colour = "none", fill = "none") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot plt-def distributions in relation
plot_dist_fjc / plot_dist_fjc_cl_j +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "District_dist_compare.png",
  units = "mm",
  width = 400,
  height =  300,
  path = "Figures"
  )

# Read in and format RESL data ####

# Read in RESL data with judges

resl <- read.csv("final data after matching/perf_match_one_distinct.csv") %>%
  # drop some variables
  select(
    -gender
  ) %>%
  # clean up some variables
  mutate(
    case_date = mdy(case_date), # this causes problems, though: cases from 1922
    # to 1968 are transformed to 2022-2068. We need to correct.
    yr = year(case_date),
    yr = case_when(
      yr >= 2022 & yr <= 2068 ~ yr-100,
      TRUE ~ yr
    ),
    case_date = ymd(
      str_c(
        yr,
        month(case_date),
        day(case_date),
        sep = "-"
      )
    )
  ) %>%
  # makes names match fjc_cl_j data
  rename(
    "CIRCUIT" = "circuit",
    "DISTRICT" = "district",
    "DOCKET" = "docket",
    "TERMDATE" = "case_date",
    "PLT" = "plt",
    "PLT_typ" = "plt_typ",
    "DEF" = "def",
    "DEF_typ" = "def_typ",
    "gender" = "Gender",
  ) %>%
  rowwise() %>%
  mutate(
    # make placeholder variables for data compatibility
    jud_or_set = 1,
    # modify or build needed extra variables
    date_dobA = ymd(as.character(str_c(Birth.Year, Birth.Month, Birth.Day, sep = "-"))),
    TERMDATE = ymd(TERMDATE),
    yr_file = year(TERMDATE),
    FILEDATE = TERMDATE, # duration is zero since we don't know when cases
    # started in the RESL data
    PLT_wl = case_when(
      outcome == "plaintiff" | 
        outcome == "mixed" ~ "w",
      outcome == "defendant" ~ "l",
      outcome == "none" | outcome == "unknown" ~ "n"
    ),
    gender = case_when(
      gender == "Male" ~ "m",
      gender == "Female" ~ "f"
    )
  ) %>%
  rowwise() %>%
  mutate(
    # capture old PLT_typ and DEF_typ with multiple types (for use later)
    PLT_typ_all = PLT_typ,
    DEF_typ_all = DEF_typ,
    # simplify and standardize lists of plaintiff type and defendnat types
    PLT_typ_all = str_replace_all(PLT_typ_all, "fed", "FED"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "ngo", "NGO"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "individual", "IND"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "state", "STA"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "industry", "BIZ"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "trade_assn", "BIZ"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "local", "LOC"),
    PLT_typ_all = str_replace_all(PLT_typ_all, "public_org", "LOC"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "fed", "FED"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "ngo", "NGO"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "individual", "IND"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "state", "STA"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "industry", "BIZ"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "trade_assn", "BIZ"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "local", "LOC"),
    DEF_typ_all = str_replace_all(DEF_typ_all, "public_org", "LOC"),
    # create binary variables for different plaintiff types
    PLT_typ_IND = case_when(
      str_detect(PLT_typ_all,"IND") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_STA = case_when(
      str_detect(PLT_typ_all,"STA") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_BIZ = case_when(
      str_detect(PLT_typ_all,"BIZ") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_NGO = case_when(
      str_detect(PLT_typ_all,"NGO") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_FED = case_when(
      str_detect(PLT_typ_all,"FED") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_LOC = case_when(
      str_detect(PLT_typ_all,"LOC") ~ 1,
      TRUE ~ 0
    ),
    PLT_typ_OTH = case_when(
      (PLT_typ_IND == 0 &
        PLT_typ_STA == 0 &
        PLT_typ_BIZ == 0 &
        PLT_typ_NGO == 0 &
        PLT_typ_LOC == 0 &
        PLT_typ_FED == 0) ~ 1,
      TRUE ~ 0
    ),
    # create binary variables for different defendant types
    DEF_typ_IND = case_when(
      str_detect(DEF_typ_all,"IND") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_STA = case_when(
      str_detect(DEF_typ_all,"STA") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_BIZ = case_when(
      str_detect(DEF_typ_all,"BIZ") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_NGO = case_when(
      str_detect(DEF_typ_all,"NGO") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_FED = case_when(
      str_detect(DEF_typ_all,"FED") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_LOC = case_when(
      str_detect(DEF_typ_all,"LOC") ~ 1,
      TRUE ~ 0
    ),
    DEF_typ_OTH = case_when(
      (DEF_typ_IND == 0 &
         DEF_typ_STA == 0 &
         DEF_typ_BIZ == 0 &
         DEF_typ_NGO == 0 &
         DEF_typ_LOC == 0 &
         DEF_typ_FED == 0) ~ 1,
      TRUE ~ 0
    ),
    # transform PLT_typ and DEF_typ into just the leading plaintiff/def type
    # as specified in FJC data
    PLT_typ = str_split(PLT_typ, "%"),
    DEF_typ = str_split(DEF_typ, "%"),
    PLT_typ = PLT_typ[1],
    DEF_typ = DEF_typ[1],
    PLT_typ = str_to_upper(PLT_typ),
    DEF_typ = str_to_upper(DEF_typ),
    PLT_typ = case_when(
      PLT_typ == "INDIVIDUAL" ~ "IND",
      PLT_typ == "STATE" ~ "STA",
      PLT_typ == "INDUSTRY" ~ "BIZ",
      PLT_typ == "TRADE_ASSN" ~ "BIZ",
      PLT_typ == "LOCAL" ~ "LOC",
      PLT_typ == "PUBLIC_ORG" ~ "LOC",
      TRUE ~ PLT_typ
    ),
    DEF_typ = case_when(
      DEF_typ == "INDIVIDUAL" ~ "IND",
      DEF_typ == "STATE" ~ "STA",
      DEF_typ == "INDUSTRY" ~ "BIZ",
      DEF_typ == "TRADE_ASSN" ~ "BIZ",
      DEF_typ == "LOCAL" ~ "LOC",
      DEF_typ == "PUBLIC_ORG" ~ "LOC",
      TRUE ~ DEF_typ
    )
  ) %>%
  ungroup() %>%
  #join region info
  left_join(
    dist_crosswalk,
    by = "DISTRICT"
  ) %>%
  # join CL judge demographics
  left_join(
    cl_people_peo %>%
      select(
        fjc_id, date_dob
      ) %>%
      rename(
        "jid" = "fjc_id"
      ) %>%
      # get rid of NAs in jid
      filter(
        !is.na(jid)
      ),
    by = "jid"
  ) %>%
  mutate(
    age_at_term = as.numeric((TERMDATE-date_dob)/365.25),
    green_gen = case_when(
      date_dob < ymd("1935-01-01") ~ "PG",
      date_dob >= ymd("1935-01-01") & date_dob <= ymd("1950-12-31") ~ "GG",
      date_dob > ymd("1950-12-31") ~ "AG"
    ),
    green_gen = as.factor(green_gen),
    ooc_mc1 = case_when(
      ooc_mc1 == "military" ~ "Other Topic",
      ooc_mc1 == "unknown" ~ "Other Topic",
      ooc_mc1 == "disaster recovery" ~ "Other Topic",
      ooc_mc1 == "legal & procedural" ~ "Other Topic",
      ooc_mc1 == "non-environmental" ~ "Other Topic",
      ooc_mc1 == "recreation" ~ "Other Topic",
      TRUE ~ ooc_mc1
    ),
    ooc_mc1 = str_to_title(ooc_mc1),
    ooc_mc1 = as.factor(ooc_mc1)
  )

# make sure that PLT_typ_X variables are being appropriately recoded.
# test <- resl %>%
#   select(
#     ID,
#     case_name,
#     PLT,
#     PLT_typ,
#     PLT_typ_all,
#     PLT_typ_IND,
#     PLT_typ_STA,
#     PLT_typ_BIZ,
#     PLT_typ_NGO,
#     PLT_typ_FED,
#     PLT_typ_LOC,
#     PLT_typ_OTH
#   )

# Plot FJC-RESL heatmap comparison ######

# prep RESL data for heatmap function
resl_heatmap <- resl %>%
  rename(
    "fjc_id" = "ID"
    ) %>%
  mutate(
    # make up disposition value that will be counted "in"
    DISP = 6
  )

# Plot heat maps of litigant types - FJC-CL combined data
plot_resl_heatmap <-  plot_PD_combo_heatmap(
  df = simp_filt(
    df = "resl_heatmap",
    jud = TRUE,
    disp_drop = NULL,
    noBP_IMC = TRUE,
    diag = FALSE
    ) %>% filter(
      !is.na(PLT_typ),
      !is.na(REGION),
      !is.na(yr_file),
      !is.na(fjc_id),
      DISP %in% c(2,3,14,6,7,8,9,17,19,20),
      !is.na(state.delegation.dime.cfscore)
      ),
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - RESL Data (w/o intra-type suits) - District Courts 1980-2022",
  court_level = "D"
  )

# plot plt-def distributions in relation
plot_fjc_cl_judges_nodiag / plot_resl_heatmap +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "PLT-DEF_matrix_compare_fjc_resl.png",
  units = "mm",
  width = 180,
  height =  300,
  path = "Figures"
)




# Plot FJC-RESL geographic distribution comparison #####

# Geographic distribution of FJC-CL data with judges - USED FOR ANALYSIS
plot_dist_resl <- simp_filt(
  df = "resl_heatmap",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE
) %>%
  filter(
    !is.na(PLT_typ),
    !is.na(REGION),
    !is.na(yr_file),
    !is.na(fjc_id),
    !is.na(state.delegation.dime.cfscore),
    DISP %in% c(2,3,14,6,7,8,9,17,19,20)
  ) %>%
  mutate(
    PLT_typ1 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Civic Association",
      PLT_typ == "TRIBE" ~ "Tribe",
      PLT_typ == "NGO_O" ~ "Non-Environmental Non-Proft",
      PLT_typ == "PUB_ORG" ~ "Public Organizations",
      TRUE ~ PLT_typ
    ),
    PLT_typ2 = case_when(
      PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
      PLT_typ == "BIZ" ~ "Firms and Trade Associations",
      PLT_typ == "IND" ~ "Individuals",
      PLT_typ == "FED" ~ "Federal Government",
      PLT_typ == "STA" ~ "State Government",
      PLT_typ == "LOC" ~ "Local Government",
      PLT_typ == "CIVIC" ~ "Others",
      PLT_typ == "TRIBE" ~ "Others",
      PLT_typ == "NGO_O" ~ "Others",
      PLT_typ == "PUB_ORG" ~ "Others",
      TRUE ~ "Others"
    )
  )

plot_dist_resl <-  plot_dist_resl %>%
  mutate(
    dist_name_flag = case_when(
      str_detect(Judicial_2,state_name_lower_orlist) == TRUE ~ 1,
      TRUE ~ 0
    ),
    dist_name = case_when(
      dist_name_flag == 1 ~ Judicial_2,
      TRUE ~ str_c(Judicial_2,STATE_TERR, sep = " of ")
    )
  ) %>%
  filter(
    !is.na(dist_name)
  ) %>%
  ggplot() +
  geom_bar(
    aes(
      x = reorder(dist_name,dist_name,function(x)-length(x)),
      group = PLT_typ2,
      fill = PLT_typ2
    )
  ) +
  labs(
    x = NULL,
    y = "No. Cases",
    fill = "Plaitiff Types"
  ) +
  #facet_wrap(
  #  vars(PLT_typ),
  #  ncol = 2
  #) +
  scale_color_viridis_d(option="mako",) +
  scale_fill_viridis_d(option="mako",) +
  #guides(colour = "none", fill = "none") +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# plot plt-def distributions in relation
plot_dist_fjc_cl_j / plot_dist_resl +
  plot_annotation(tag_levels = 'A')  & 
  theme(plot.tag = element_text(face = "bold"))

# save combo plot 
ggsave(
  "District_dist_compare_FJC_RESL.png",
  units = "mm",
  width = 400,
  height =  300,
  path = "Figures"
)

# Run regressions - FJC / CL data ####


# call win-loss regression
source("functions/win_loss_logit_reg.R")

# regression of plaintiff wins and losses
# --> excluding BP and IMC
# --> testing for association with party of president (partisan effect)

# note: judicial ideology variable (judge_pv) can take on one of the following 
# values. See Bonica and Sen (2021), Journal of Economic Perspectives, and
# associated papers referenced in that overview paper.

# --> "prez_party": the political party of the appointing president
# --> "dime": the DIME score of the judge (based on political contributions;
#.     imputed where missing.)
# --> "jcs_dw": Judicial Common Space score based on D-W NOMINATE 
# --> "jcs_cf":
# --> "prez_dw":
# --> "prez_dime":
# --> "sen_dw":
# --> "sen_dime":
# --> "del_dw":
# --> "del_dime"

# loop through each judge_pv var with win_loss_reg 
judge_pv_list <- c("dime", "jcs_dw", "jcs_cf", "prez_party", "prez_dw",
                   "prez_dime", "sen_dw", "sen_dime", "del_dw", "del_dime")

lapply(
  judge_pv_list,
  win_los_reg,
  df = "fjc_cl_j",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1980,
  yr_f = 2022,
  party_or_admin = "PARTY",
  judges = TRUE,
  RESL = FALSE
  )


# Run regressions - RESL data ####

# call win-loss regression
source("functions/win_loss_logit_reg.R")

# loop through each judge_pv var with win_loss_reg 
judge_pv_list <- c("dime", "jcs_dw", "jcs_cf", "prez_party", "prez_dw",
                   "prez_dime", "sen_dw", "sen_dime", "del_dw", "del_dime")

# resl data of just ENGOs

lapply(
  judge_pv_list,
  win_los_reg,
  df = "resl",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1980,
  yr_f = 2022,
  party_or_admin = "PARTY",
  judges = TRUE,
  RESL = TRUE
)

# Reformat ideology OR tables - FJC ####
fjc_OR_table <- read_csv(
  "regressions/ideology_OR_tables/ideology_OR_fjc.csv"
)

# ideology of ruling judge
jud_chunk <- fjc_OR_table %>%
  filter(
    ideology_var == "dime" | ideology_var == "jcs_dw" | ideology_var == "jcs_cf"
  )
jud_head <- jud_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Ruling Judge"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of president
prez_chunk <- fjc_OR_table %>%
  filter(
    ideology_var == "prez_party" | ideology_var == "prez_dw" | ideology_var == "prez_dime"
  )
prez_head <- prez_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Appointing President"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of senators
sen_chunk <- fjc_OR_table %>%
  filter(
    ideology_var == "sen_dw" | ideology_var == "sen_dime" 
  )
sen_head <- sen_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Home State Senators"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of state del
del_chunk <- fjc_OR_table %>%
  filter(
    ideology_var == "del_dw" | ideology_var == "del_dime"
  )
del_head <- del_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Full State Dellegation"
  ) %>%
  filter(
    row_number() == 1
  )

# assemble chunks and headers
fjc_OR_table <- bind_rows(
  jud_head,
  jud_chunk,
  prez_head,
  prez_chunk,
  sen_head,
  sen_chunk,
  del_head,
  del_chunk
)

# convert NAs to blanks
fjc_OR_table <- fjc_OR_table %>%
  mutate(
    `Full Model` = as.character(`Full Model`),
    `Full Model` = case_when(
      is.na(`Full Model`) ~ "",
      TRUE ~ `Full Model`
      ),
    `ENGO Plaintiffs` = as.character(`ENGO Plaintiffs`),
    `ENGO Plaintiffs` = case_when(
      is.na(`ENGO Plaintiffs`) ~ "",
      TRUE ~ `ENGO Plaintiffs`
      ),
    `Federal Plaintiffs` = as.character(`Federal Plaintiffs`),
    `Federal Plaintiffs` = case_when(
      is.na(`Federal Plaintiffs`) ~ "",
      TRUE ~ `Federal Plaintiffs`
      ),
    `Firm Plaintiffs` = as.character(`Firm Plaintiffs`),
    `Firm Plaintiffs` = case_when(
      is.na(`Firm Plaintiffs`) ~ "",
      TRUE ~ `Firm Plaintiffs`
      )
  ) %>%
  # drop ideology_var
  select(
    -ideology_var
  )


write_csv(
  fjc_OR_table,
  file = str_c("regressions/ideology_OR_tables/ideology_OR_fjc_pretty.csv")
)

# Reformat ideology OR tables - RESL ####
resl_OR_table <- read_csv(
  "regressions/ideology_OR_tables/ideology_OR_resl.csv"
)

# ideology of ruling judge
jud_chunk <- resl_OR_table %>%
  filter(
    ideology_var == "dime" | ideology_var == "jcs_dw" | ideology_var == "jcs_cf"
  )
jud_head <- jud_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Ruling Judge"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of president
prez_chunk <- resl_OR_table %>%
  filter(
    ideology_var == "prez_party" | ideology_var == "prez_dw" | ideology_var == "prez_dime"
  )
prez_head <- prez_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Appointing President"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of senators
sen_chunk <- resl_OR_table %>%
  filter(
    ideology_var == "sen_dw" | ideology_var == "sen_dime"
  )
sen_head <- sen_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Home State Senators"
  ) %>%
  filter(
    row_number() == 1
  )

# ideology of state del
del_chunk <- resl_OR_table %>%
  filter(
    ideology_var == "del_dw" | ideology_var == "del_dime"
  )
del_head <- del_chunk %>%
  select(
    `Indicator of Judicial Ideology`
  ) %>%
  mutate(
    `Indicator of Judicial Ideology` = "Ideology of Full State Dellegation"
  ) %>%
  filter(
    row_number() == 1
  )

# assemble chunks and headers
resl_OR_table <- bind_rows(
  jud_head,
  jud_chunk,
  prez_head,
  prez_chunk,
  sen_head,
  sen_chunk,
  del_head,
  del_chunk
)

# convert NAs to blanks
resl_OR_table <- resl_OR_table %>%
  mutate(
    `Full Model` = as.character(`Full Model`),
    `Full Model` = case_when(
      is.na(`Full Model`) ~ "",
      TRUE ~ `Full Model`
    ),
    `ENGO Plaintiffs` = as.character(`ENGO Plaintiffs`),
    `ENGO Plaintiffs` = case_when(
      is.na(`ENGO Plaintiffs`) ~ "",
      TRUE ~ `ENGO Plaintiffs`
    ),
    `Federal Plaintiffs` = as.character(`Federal Plaintiffs`),
    `Federal Plaintiffs` = case_when(
      is.na(`Federal Plaintiffs`) ~ "",
      TRUE ~ `Federal Plaintiffs`
    ),
    `Firm Plaintiffs` = as.character(`Firm Plaintiffs`),
    `Firm Plaintiffs` = case_when(
      is.na(`Firm Plaintiffs`) ~ "",
      TRUE ~ `Firm Plaintiffs`
    ),
    `Conservation Conflicts` = as.character(`Conservation Conflicts`),
    `Conservation Conflicts` = case_when(
      is.na(`Conservation Conflicts`) ~ "",
      TRUE ~ `Conservation Conflicts`
    ),
    `Waste and Pollution Conflicts` = as.character(`Waste and Pollution Conflicts`),
    `Waste and Pollution Conflicts` = case_when(
      is.na(`Waste and Pollution Conflicts`) ~ "",
      TRUE ~ `Waste and Pollution Conflicts`
    )
  ) %>%
  # drop ideology_var
  select(
    -ideology_var
  )

write_csv(
  resl_OR_table,
  file = str_c("regressions/ideology_OR_tables/ideology_OR_resl_pretty.csv")
)

# plot count of cases by substantive focus by plaintiff type in RESL data ####

  resl %>%
    ungroup() %>%
    pivot_longer(
      cols = PLT_typ_IND:PLT_typ_OTH,
      names_to = "PLT_typ2",
      values_to = "PLT_typ_indicator",
      names_prefix = "PLT_typ_",
      values_drop_na = TRUE
    ) %>%
    filter(
      PLT_typ_indicator ==1
    ) %>%
    #select(
    #  ID, case_name,PLT_typ2, PLT_typ_indicator
    #) %>%
    mutate(
      PLT_typ2 = case_when(
        PLT_typ == "CIVIC" ~ "Other",
        PLT_typ == "TRIBE" ~ "Other",
        PLT_typ == "NGO_O" ~ "Other",
        PLT_typ == "PUB_ORG" ~ "Other",
        PLT_typ == "NGO" ~ "Environmental Advocacy Groups",
        PLT_typ == "BIZ" ~ "Firms and Trade Associations",
        PLT_typ == "IND" ~ "Individuals",
        PLT_typ == "FED" ~ "Federal Government",
        PLT_typ == "STA" ~ "State Government",
        PLT_typ == "LOC" ~ "Local Government",
        TRUE ~ "Other"
      ),
      PLT_typ2 = factor(PLT_typ2)
    ) %>%
    group_by(
      PLT_typ2
    ) %>%
    mutate(
      tot = n()
    ) %>%
    group_by(
      PLT_typ2, ooc_mc1
    ) %>%
    mutate(
      n = n(),
      pct = str_c(round(n/tot*100,1),"%"),
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    ungroup() %>%
    ggplot(
      aes(
        x = ooc_mc1,
        y = n,
        group = ooc_mc1,
        fill = ooc_mc1
      )
    ) +
    geom_bar(
      stat = "identity"
    ) + 
    geom_text(
      aes(
        label = pct
      ),
      hjust = .5,
      vjust = -0.2
    ) +
    scale_fill_viridis_d() + 
    labs(
      x = "Substantive Focus of Legal Conflict"
    ) +
    facet_wrap(
      vars(PLT_typ2)
    ) +
    guides(
      fill = "none"
    ) +
    theme_linedraw() + 
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) +
    coord_cartesian(
      ylim = c(0,900)
    )
  
  ggsave(
    str_c("case_focus_by_plaintiff_type.png"),
    plot = last_plot(),
    width = 10,
    height = 7,
    path = "figures"
  )

# write out random sample of conservation-focused cases and waste and pollution-focused cases

set.seed(1234)

# conservation cases
resl %>%
  mutate(
    n_rand = runif(n =3937, max = 1000)
  ) %>%
  filter(
    ooc_mc1 == "Conservation",
    n_rand <= 160
  ) %>%
  mutate(
    n = row_number()
  ) %>%
  write_csv(
    "data/decisions_to_investigate/random_resl_cons_decisions.csv"
  )

set.seed(1234)
# waste and pollution casess
resl %>%
  mutate(
    n_rand = runif(n =3937, max = 1000)
  ) %>%
  filter(
    ooc_mc1 == "Waste & Pollution",
    n_rand <= 112.5
  ) %>%
  mutate(
    n = row_number()
  ) %>%
  write_csv(
    "data/decisions_to_investigate/random_resl_waste_pol_decisions.csv"
  )

set.seed(1234)
# waste and pollution (non-conservation) brought by ENGOs
resl %>%
  mutate(
    n_rand = runif(n =3937, max = 1000)
  ) %>%
  filter(
    #ooc_mc1 == "Waste & Pollution",
    ooc_mc1 != "Conservation",
    PLT_typ_NGO == 1,
    #n_rand <= 112.5
  ) %>%
  mutate(
    n = row_number()
  ) %>%
  write_csv(
    "data/decisions_to_investigate/resl_non-conservation_ENGO_plt_decisions.csv",
  )

set.seed(1234)
# conservation cases brought by ENGOs
resl %>%
  mutate(
    n_rand = runif(n =3937, max = 1000)
  ) %>%
  filter(
    #ooc_mc1 == "Waste & Pollution",
    ooc_mc1 == "Conservation",
    PLT_typ_NGO == 1,
    #n_rand <= 112.5
  ) %>%
  mutate(
    n = row_number()
  ) %>%
  write_csv(
    "data/decisions_to_investigate/resl_conservation_ENGO_plt_decisions.csv"
  )

# compare non-conservation cases brought by ENGOs to other cases
resl <- resl %>%
  mutate(
    PLT_typ_ooc_simp = case_when(
      PLT_typ_NGO == 1 & ooc_mc1 != "Conservation" ~ "Non-conservation conflicts brought by ENGOs",
      TRUE ~ "All other conflicts"
    ),
    PLT_typ_ooc = case_when(
      PLT_typ_NGO == 1 & ooc_mc1 != "Conservation" ~ "Non-conservation conflicts brought by ENGOs",
      PLT_typ_NGO == 1 & ooc_mc1 == "Conservation" ~ "Conservation conflicts brought by ENGOs",
      PLT_typ_NGO == 0 & ooc_mc1 != "Conservation" ~ "Non-conservation conflicts brought by all other plaintiff types",
      PLT_typ_NGO == 0 & ooc_mc1 == "Conservation" ~ "Conservation conflicts brought by all other plaintiff types"
    )
  )

# plot wins and losses by judicial ideology for non-conservation ENGO-plaintiff
# cases. These are the cases driving statistical effects
unique(resl$outcome)

outcome_plot <- resl %>%
  mutate(
    outcome_num = case_when(
      outcome == "plaintiff" ~ 1,
      outcome == "defendant" ~ 0,
      outcome == "mixed" ~ 1
    ),
    outcome_fac = as.factor(outcome_num)
  ) %>%
  filter(
    #(PLT_typ_ooc == "Non-conservation conflicts brought by ENGOs"),# |
      #PLT_typ_ooc == "Conservation conflicts brought by ENGOs"),
    #PLT_typ_ooc == "Conservation conflicts brought by all other plaintiff types",
    !is.na(outcome_fac)
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  mutate(
    PLT_typ_ooc_n = n(),
    PLT_typ_ooc_n_lab = str_c("n = ", PLT_typ_ooc_n)
  ) %>%
  ungroup()

outcome_plot %>%
  ggplot(
    aes(
      x = imputed.dime.cfscore,
      color = imputed.dime.cfscore
    )
  ) +
  geom_point(
    aes(
      y = outcome_num,
    ),
    position = position_jitter(
      seed = 1234,
      height = 0.05
    ),
    alpha = 0.7
  ) +
  geom_density(
    aes(
      group = outcome_fac,
      fill = outcome_fac
    ),
    alpha = 0.5
  ) + 
  geom_smooth(
    aes(
      y = outcome_num
    ),
    method = "lm"
  ) +
  geom_text(
    data = outcome_plot %>%
      group_by(PLT_typ_ooc) %>%
      filter(
        row_number() == 1
      ),
    aes(
      x = 1,
      y = .825,
      label = PLT_typ_ooc_n_lab
    ),
    color = "#000000"
  ) +
  scale_color_viridis_c() +
  #scale_color_grey()
  scale_fill_grey() +
  labs(
    x = "Judicial Ideology (imputed DIME score)\n(- <-- liberal  conservative --> +)",
    y = "Outcome (1 = plaintiff win)",
    color = "Judicial Ideology\n(DIME score)",
    fill = "Outcome\n(1 = plaintiff win)",
    caption = "Note: Mixed outcomes coded as PLT wins."
  ) + 
  facet_wrap(
    vars(PLT_typ_ooc)
  ) +
  theme_linedraw()

ggsave(
  "Bivariate_plt_wins_by_plt_typ_and_ideology_color.png",
  units = "mm",
  width = 300,
  height =  200,
  path = "Figures"
)
  

# plot most common statutes in conflicts brought by different plaintiff types/
# focused on different kinds of nature 
resl %>%
  # drop biz-biz conflicts
  filter(
    !(str_detect(PLT_typ, "BIZ") & str_detect(DEF_typ, "BIZ"))
  ) %>%
  select(PLT_typ_ooc, statute) %>%
  filter(
    !is.na(PLT_typ_ooc)
  ) %>%
  # convert stautes to list
  mutate(
    statute = str_split(statute,"%")
  ) %>%
  # spread list values into unique columns
  unnest_wider(
    statute,
    names_sep = "_"
  ) %>%
  # trim and leading or trailing white space from statutes
  mutate(
    across(
        statute_1:statute_11,
        str_trim
    )
  ) %>%
  pivot_longer(
    cols = statute_1:statute_11,
    names_to = "number",
    values_to = "statute",
    values_drop_na = TRUE
    ) %>%
  select(
    -number
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  mutate(
    tot = n()
  ) %>%
  group_by(
    PLT_typ_ooc, statute
  ) %>%
  mutate(
    n = n(),
    pct = round(n/tot*100,1)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  arrange(
    PLT_typ_ooc, desc(pct)
  ) %>%
  mutate(
    rank = row_number()
  ) %>%
  filter(
    rank <= 15
  ) %>%
  arrange(
    PLT_typ_ooc, pct
  ) %>%
  mutate(
    rank = row_number(),
    rank = factor(
      x = rank,
      labels = statute
    )
  ) %>%
  ggplot(
    aes(
      y = reorder_within(rank, by = pct, within = PLT_typ_ooc),
      x = pct
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(
      label = pct
    ),
    hjust = -.2
  ) + 
  scale_y_reordered() + # remove facet name appending
  labs(
    x = "Percent of Decisions",
    y = "Statute"
  ) + 
  facet_wrap(
    vars(PLT_typ_ooc),
    scales = "free_y"
  ) +
  coord_cartesian(xlim = c(0, 41)) +
  theme_linedraw()

ggsave(
  "most_common_statutes_by_plt_and_nature.png",
  units = "mm",
  width = 300,
  height =  200,
  path = "Figures"
)


# plot most common agencies in conflicts brought by different plaintiff types/
# focused on different kinds of nature 
resl %>%
  # drop biz-biz conflicts
  filter(
    !(str_detect(PLT_typ, "BIZ") & str_detect(DEF_typ, "BIZ"))
  ) %>%
  select(PLT_typ_ooc, agy) %>%
  filter(
    !is.na(PLT_typ_ooc)
  ) %>%
  # convert stautes to list
  mutate(
    agy = str_split(agy,"%")
  ) %>%
  # spread list values into unique columns
  unnest_wider(
    agy,
    names_sep = "_"
  ) %>%
  # trim and leading or trailing white space from statutes
  mutate(
    across(
      agy_1:agy_5,
      str_trim
    )
  ) %>%
  pivot_longer(
    cols = agy_1:agy_5,
    names_to = "number",
    values_to = "agency",
    values_drop_na = TRUE
  ) %>%
  select(
    -number
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  mutate(
    tot = n()
  ) %>%
  group_by(
    PLT_typ_ooc, agency
  ) %>%
  mutate(
    n = n(),
    pct = round(n/tot*100,1)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  arrange(
    PLT_typ_ooc, desc(pct)
  ) %>%
  mutate(
    rank = row_number()
  ) %>%
  filter(
    rank <= 15
  ) %>%
  arrange(
    PLT_typ_ooc, pct
  ) %>%
  mutate(
    rank = row_number(),
    rank = factor(
      x = rank,
      labels = agency
    )
  ) %>%
  ggplot(
    aes(
      y = reorder_within(rank, by = pct, within = PLT_typ_ooc),
      x = pct
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(
      label = pct
    ),
    hjust = -.2
  ) + 
  scale_y_reordered() + # remove facet name appending
  labs(
    x = "Percent of Decisions",
    y = "Agency"
  ) + 
  facet_wrap(
    vars(PLT_typ_ooc),
    scales = "free_y"
  ) +
  coord_cartesian(xlim = c(0, 55)) +
  theme_linedraw()

ggsave(
  "most_common_agencies_by_plt_and_nature.png",
  units = "mm",
  width = 300,
  height =  200,
  path = "Figures"
)
 

  # plot most common defendants in conflicts brought by different plaintiff types/
  # focused on different kinds of nature 
  resl %>%
    # drop biz-biz conflicts
    filter(
      !(str_detect(PLT_typ, "BIZ") & str_detect(DEF_typ, "BIZ"))
    ) %>%
    select(PLT_typ_ooc, DEF_typ_all) %>%
    filter(
      !is.na(PLT_typ_ooc)
    ) %>%
    # convert defendant types to list
    mutate(
      DEF_typ_all = str_split(DEF_typ_all,"%")
    ) %>%
    # spread list values into unique columns
    unnest_wider(
      DEF_typ_all,
      names_sep = "_"
    ) %>%
    # trim and leading or trailing white space from statutes
    mutate(
      across(
        DEF_typ_all_1:DEF_typ_all_6,
        str_trim
      )
    ) %>%
    pivot_longer(
      cols = DEF_typ_all_1:DEF_typ_all_6,
      names_to = "number",
      values_to = "DEF_typ",
      values_drop_na = TRUE
    ) %>%
    select(
      -number
    ) %>%
    group_by(
      PLT_typ_ooc
    ) %>%
    mutate(
      tot = n()
    ) %>%
    group_by(
      PLT_typ_ooc, DEF_typ
    ) %>%
    mutate(
      n = n(),
      pct = round(n/tot*100,1)
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    group_by(
      PLT_typ_ooc
    ) %>%
    arrange(
      PLT_typ_ooc, desc(pct)
    ) %>%
    mutate(
      rank = row_number()
    ) %>%
    filter(
      rank <= 15
    ) %>%
    arrange(
      PLT_typ_ooc, pct
    ) %>%
    mutate(
      rank = row_number(),
      rank = factor(
        x = rank,
        labels = DEF_typ
      )
    ) %>%
    ggplot(
      aes(
        y = reorder_within(rank, by = pct, within = PLT_typ_ooc),
        x = pct
      )
    ) +
    geom_bar(
      stat = "identity"
    ) +
    geom_text(
      aes(
        label = pct
      ),
      hjust = -.2
    ) + 
    scale_y_reordered() + # remove facet name appending
    labs(
      x = "Percent of Decisions",
      y = "Defendant Type"
    ) + 
    facet_wrap(
      vars(PLT_typ_ooc),
      scales = "free_y"
    ) +
    coord_cartesian(xlim = c(0, 90)) +
    theme_linedraw()
  
  ggsave(
    "most_common_defendnat types_by_plt_and_nature.png",
    units = "mm",
    width = 300,
    height =  200,
    path = "Figures"
  )


# Generate list of most common plaintiff names brought by different plaintiff types/
# focused on different kinds of nature 
test <- resl %>%
  # drop biz-biz conflicts
  filter(
    !(str_detect(PLT_typ, "BIZ") & str_detect(DEF_typ, "BIZ"))
  ) %>%
  select(PLT_typ_ooc, PLT) %>%
  filter(
    !is.na(PLT)
  ) %>%
  # convert PLT to list
  mutate(
    PLT = str_split(PLT,"%")
  ) %>%
  # spread list values into unique columns
  unnest_wider(
    PLT,
    names_sep = "_"
  ) %>%
  # trim and leading or trailing white space from statutes
  mutate(
    across(
      PLT_1:PLT_220,
      str_trim
    )
  ) %>%
  pivot_longer(
    cols = PLT_1:PLT_220,,
    names_to = "number",
    values_to = "PLT",
    values_drop_na = TRUE
  ) %>%
  select(
    -number
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  mutate(
    tot = n()
  ) %>%
  group_by(
    PLT_typ_ooc, PLT
  ) %>%
  mutate(
    n = n(),
    pct = round(n/tot*100,1)
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    PLT_typ_ooc
  ) %>%
  arrange(
    PLT_typ_ooc, desc(pct)
  ) %>%
  mutate(
    rank = row_number()
  ) %>%
  filter(
    rank <= 100
  ) %>%
  arrange(
    PLT_typ_ooc, pct
  ) %>%
  mutate(
    rank = row_number(),
    rank = factor(
      x = rank,
      labels = DEF_typ
    )
  ) %>%
  ggplot(
    aes(
      y = reorder_within(rank, by = pct, within = PLT_typ_ooc),
      x = pct
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(
      label = pct
    ),
    hjust = -.2
  ) + 
  scale_y_reordered() + # remove facet name appending
  labs(
    x = "Percent of Decisions",
    y = "Defendant Type"
  ) + 
  facet_wrap(
    vars(PLT_typ_ooc),
    scales = "free_y"
  ) +
  coord_cartesian(xlim = c(0, 90)) +
  theme_linedraw()

ggsave(
  "most_common_defendnat types_by_plt_and_nature.png",
  units = "mm",
  width = 300,
  height =  200,
  path = "Figures"
)
