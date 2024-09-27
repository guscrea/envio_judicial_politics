# FJC and Court Listener Data
# 02 - Link/Join Court Listener Data to Itself and to FJC Data
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

# join person data to docket and court data
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

# join political affiliation data to docket, court, and person data
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
      "assigned_to_id" = "id"
      ),
  by = "assigned_to_id"
  )

# get file year; make dist-docket-yr_file number for joining to FJC
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

# drop component dfs
rm(cl_court, cl_people_peo, cl_people_pol)

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

# IMC Global and BP Deepwater Horizon cases immediately pop out. Drop those
# cases.
cl_e_clean <- cl_e %>%
  filter(
    !(court_id == "laed" & str_detect(case_name, "BP") & yr_file >= 2010),
    !(court_id == "scd" & str_detect(case_name, "IMC")),
    #!is.na(assigned_to_id)
  )

# distribution of years for cleaned data
cl_e_clean %>%
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

# after dropping BP and IMC Global cases, how many observations do we have with
# judges recorded?
# --> starting with 29,174 cases
# count observations with judges observed:
cl_e_clean_j <- cl_e_clean %>%
  filter(
    !is.na(assigned_to_id)
  ) %>%
  # assess uniqueness
  group_by(
    join_id
  ) %>%
  mutate(
    n = row_number() # don't count number of duplicates; label by row numbers
  ) %>%
  ungroup()

# --> 21,900
# --> So we lose ~25% for our original observations (7,274). That's a lot!
# Certainly need to investigate if those cases for which we do not observe the
# the judge are systematically different from those where we do.

# distribution of years for 
cl_e_clean_j %>%
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

# for now, drop non-unique join IDs
cl_e_clean_j <- cl_e_clean_j %>%
  filter(
    n == 1
    )

# get just judge info for join
cl_e_clean_j <- cl_e_clean_j %>%
  select(
    join_id,
    case_name,
    assigned_to_str,
    assigned_to_id,
    referred_to_str,
    referred_to_id,
    name_first:source.y
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
    ylim = c(0,2000)
  )

# make dist-docket number for join to CL data
fjc_e <- fjc_e %>%
  mutate(
    join_id = str_c(DISTRICT,"-",DOCKET,"-", yr_file)
  ) %>%
  # assess uniqueness
  group_by(
    join_id
  ) %>%
  mutate(
    n = row_number()
  ) %>%
  ungroup() %>%
  filter(
    n == 1
  )

# Winnow down FJC data for join ####

# drop fjc observations with duplicate join IDs
fjc_e_for_join <- fjc_e %>%
  filter(
    n == 1
  ) # drops  from 29.9 to 29.2

# try join
fjc_cl <- left_join(
  fjc_e_for_join,
  cl_e_clean_j,
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
    referred_to_id,
    name_first:source.y
  )

fjc_cl_look_j <- fjc_cl_look %>%
  filter(
    !is.na(assigned_to_str)
  )


