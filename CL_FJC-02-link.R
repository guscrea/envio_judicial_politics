# FJC and Court Listener Data
# 02 - Link/Join Court Listener Data to Itself and to FJC Data
# Script by Chris Rea

library(tidyverse)
library(patchwork)

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
      "assigned_to_id" = "id",
      "source_pa" = "source"
      ),
  by = "assigned_to_id"
  )

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

# --> 21,900
# --> So we lose ~25% for our original observations (7,274). That's a lot!
# Certainly need to investigate if those cases for which we do not observe the
# the judge are systematically different from those where we do.

# make simple clean df with judges
cl_e_clean_j_simp <- cl_e_clean_j %>%
  select(
    id, yr_file
  ) %>%
  mutate(
    data_type = "Observations with judge recorded\n(n = 21,900)"
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
    assigned_to_str,
    assigned_to_id,
    referred_to_str,
    referred_to_id,
    name_first:plt_fl
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

# clean up join results
fjc_cl <- fjc_cl %>%
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
    id,
    assigned_to_str,
    assigned_to_id,
    referred_to_str,
    referred_to_id,
    name_first:plt_fl.y
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
    data_type = "Final data matched to FJC\n(n = 19,202)"
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
      data_type = "Clean data with judge recorded\n(n = 21,900)"
    ),
  fjc_cl_simp
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