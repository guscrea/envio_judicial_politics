# FJC and Court Listener Data
# 01 - Compliel Court Listener Data
# Script by Chris Rea

library(tidyverse)

# Assemble Court Listener Data ####

# function to read in court listener docket data in n-row chunks, keeping only
# specified NOS code (e.g. 893)

# get the number of rows in the cl docket file:
count_rows <- read_csv(
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/dockets-2024-08-31.csv",
  quote = "`",
  #n_max = 10,
  show_col_types = FALSE,
  col_select = c(id)
  )
# --> answer: 67,509,733

# call function to parse court listener data
source("functions/assemble_cl_cases.R")

# set number of iterations for lapply to run through - 68 given the 67M+ rows
# above
iter <- seq(0,68,1) # 68 iterations, 1M rows each

cl_893 <- lapply(
  iter,
  read_in_cl_cases,
  n_rows = 1000000,
  NOS = "893"
  )

# create data frame of all elements
# --> 34,263 NOS = 893 entries
cl_e <- bind_rows(cl_893, .id = "iteration")

#write out data so that data do not have to be compiled anew each time.
write_csv(
  cl_e,
  "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/NOS_893_dockets-2024-08-31.csv"
)

# drop list of lists; keep just single compiled dataframe
rm(cl_893)

# the end

