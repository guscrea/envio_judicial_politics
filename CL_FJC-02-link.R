# FJC and Court Listener Data
# 02 - Link/Join Court Listener Data to Itself and to FJC Data
# Script by Chris Rea

library(tidyverse)
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
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/NOS_893_dockets-2024-08-31.csv"
)

# read in cl court data
cl_court <- read_csv(
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/courts-2024-08-31.csv",
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
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-people-2024-08-31.csv",
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
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-political-affiliations-2024-08-31.csv",
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
    data_type = "Clean Data (No BP or IMC)\n(n = 29,174)"
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
    #assigned_to_str,
    assigned_to_id,
    fjc_id,
    #referred_to_str,
    #referred_to_id,
    name_first:plt_fl
    )

# Read in Bonica and Sen Judicial Ideology Data; join to CL data ####

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
    n = n(),
    row_n = row_number()
  ) %>%
  filter(
    row_n == 1
  ) %>%
  select(
    -c(n, row_n)
  ) %>%
  ungroup()

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
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_RESL/Environmental_Law_Research/Nature_Sustainability_2024/Data/FJC_postprocessed/District/fjc_e_post_processed_clean.csv"
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
    id, yr_file
  ) %>%
  mutate(
    data_type = "Final data used for analysis\n(n = 13,341)"
  )


# plot distribution of years for cleaned data and cleaned data with judges, for
# comparison
plot_cl_fjc <- bind_rows(
  cl_e_clean_simp %>%
    mutate(
      data_type = "Clean data (No BP or IMC)\n(n = 29,174)"
    ),
  cl_e_clean_j_simp %>%
    mutate(
      data_type = "Clean data with judge recorded\n(n = 21,973)"
    ),
  fjc_cl_simp,
  fjc_cl_j_reg_simp
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

# Plot heatmaps ######

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
      !is.na(state.delegation.dime.cfscore)
    ) ,
  yr_start = 1980,
  yr_end = 2022,
  l_typs = l_typs,
  title = "Plaintiff and Defendant Type Combinations - Data for Analysis (with intra-type cases) - District Courts 1980-2022",
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
    !is.na(state.delegation.dime.cfscore)
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
  yr_f = 2020,
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

lapply(
  judge_pv_list,
  win_los_reg,
  df = "resl",
  jud = TRUE,
  disp_drop = NULL,
  noBP_IMC = TRUE,
  diag = FALSE,
  yr_i = 1980,
  yr_f = 2020,
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
    str_detect(`Indicator of Judicial Ideology`, "DIME|JCS")
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
    str_detect(`Indicator of Judicial Ideology`, "Republican|President")
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
    str_detect(`Indicator of Judicial Ideology`, "Senate")
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
    str_detect(`Indicator of Judicial Ideology`, "State")
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
  )


write_csv(
  fjc_OR_table,
  file = str_c("regressions/ideology_OR_tables/ideology_OR_fjc_pretty.csv")
)

# reformat ideology OR tables - RESL ####
resl_OR_table <- read_csv(
  "regressions/ideology_OR_tables/ideology_OR_resl.csv"
)

# ideology of ruling judge
jud_chunk <- resl_OR_table %>%
  filter(
    str_detect(`Indicator of Judicial Ideology`, "DIME|JCS")
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
    str_detect(`Indicator of Judicial Ideology`, "Republican|President")
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
    str_detect(`Indicator of Judicial Ideology`, "Senate")
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
    str_detect(`Indicator of Judicial Ideology`, "State")
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
  )

write_csv(
  resl_OR_table,
  file = str_c("regressions/ideology_OR_tables/ideology_OR_resl_pretty.csv")
)

