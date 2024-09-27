# FJC and Court Listener Data
# 01 - Compliel Court Listener Data
# Script by Chris Rea

library(tidyverse)

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

# join court data to docket data
cl_e <- left_join(
  cl_e,
  cl_court,
  by = "court_id"
)

# get file year; make dist-docket number
cl_e <- cl_e %>%
  mutate(
    yr_file = year(date_filed),
    join_id = str_c(fjc_court_id,"-",docket_number_core,"-", yr_file)
  ) %>%
  # assess uniqueness of join_id
  group_by(
    join_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()







# how many judges do we have?
cl_e_j <- cl_e %>%
  filter(
    !is.na(assigned_to_id)
  )
# --> 25,991

# distribution of years
cl_e_j %>%
  ggplot(
    aes(
      x = yr_file,
    )
  ) +
  geom_bar()

# drop BP Deepwater cases; with judges
cl_e_no_BP_No_IMC <- cl_e %>%
  filter(
    !(court_id == "laed" & str_detect(case_name, "BP")),
    !(court_id == "scd" & str_detect(case_name, "IMC")),
    #!is.na(assigned_to_id)
  )

# distribution of years
cl_e_no_BP_No_IMC %>%
  ggplot(
    aes(
      x = yr_file,
    )
  ) +
  geom_bar()


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
  geom_bar()

# make dist-docket number
fjc_e <- fjc_e %>%
  mutate(
    join_id = str_c(DISTRICT,"-",DOCKET,"-", yr_file)
  ) %>%
  # assess uniqueness
  group_by(
    join_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()

# get just judge info for join
cl_e_for_join <- cl_e_no_BP_No_IMC %>%
  select(
    join_id,
    n,
    case_name,
    assigned_to_str,
    assigned_to_id,
    referred_to_str,
    referred_to_id
  ) %>%
  filter(
    n == 1 # drops observations from 29.2k to 22.2k
  )

fjc_e_for_join <- fjc_e %>%
  filter(
    n == 1
  ) # drops  from 29.9 to 29.2

# try join
fjc_cl <- left_join(
  fjc_e_for_join,
  cl_e_for_join,
  by = "join_id"
)

# examine join results
fjc_cl_look <- fjc_cl %>%
  select(
    CIRCUIT,
    DISTRICT,
    DOCKET,
    ORIGIN,
    FILEDATE,
    PLT,
    DEF,
    case_name,
    yr_file,
    yr_term,
    new,
    PLT_typ,
    DEF_typ,
    PLT_wl,
    DEF_wl,
    REGION,
    join_id,
    assigned_to_str,
    assigned_to_id,
    referred_to_str,
    referred_to_id
  )

fjc_cl_look_j <- fjc_cl_look %>%
  filter(
    !is.na(assigned_to_str)
  )


# read in cl people - position data
j_pos <- read_csv(
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-positions-2024-08-31.csv",
  quote = "`"
) %>%
  filter(
    #!is.na(court_id),
    #str_detect(organization_name, "District") # keep only district court roles
    str_detect(job_title, "Judge") # keep only district court roles
  ) %>%
  group_by(
    person_id
  ) %>%
  mutate(
    n = n()
  ) %>%
  ungroup()


# read in cl people - political affiliation data
j_pol <- read_csv(
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/people-db-political-affiliations-2024-08-31.csv",
  quote = "`"
)
