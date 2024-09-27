# Function to read in and assemmle Court Listeneer Docket Data for given NOS code(s)

read_in_cl_cases <- function(i,n_rows,NOS){
  
  #start timer
  start.time <- Sys.time()
  
  # get column names
  c_names <- read_csv(
    "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/dockets-2024-08-31.csv",
    quote = "`",
    n_max = 0,
    show_col_types = FALSE
  )
  c_names <- names(c_names)
  
  # now read in data after getting column names
  df <- read_csv(
    "/Users/crea/Dropbox\ (Personal)/Professional/Research/_Basic_Data/US_Legal_Data/court_listener/dockets-2024-08-31.csv",
    quote = "`",
    col_names = c_names,
    col_types = cols(
      .default = col_character(),
      date_created = col_datetime(),
      date_modified = col_datetime(),
      date_last_index = col_datetime(),
      date_cert_granted = col_date(),
      date_cert_denied = col_date(),
      date_argued = col_date(),
      date_reargued = col_date(),
      date_reargument_denied = col_date(),
      date_filed = col_date(),
      date_terminated = col_date(),
      date_last_filing = col_date(),
      date_blocked = col_date(),
    ),
    skip = i*n_rows, # rows to skip before starting to read in file
    n_max = n_rows, # number of rows to read in
    #show_col_types = FALSE
  ) %>%
    filter(
      str_detect(nature_of_suit, NOS)
    )
  
  # end timer
  end.time <- Sys.time()
  
  # calcuate total time taken
  time.taken <- end.time - start.time
  message(str_c("Iteration no.: ",i))
  print(time.taken)
  
  #print(time.taken)
  gc()
  
  return(df)
  
  
}