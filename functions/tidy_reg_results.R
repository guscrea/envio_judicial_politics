# function for tidily assembling regression results

# inputs
# m = model results to tidy
# m_name = readable name of model

# for testing
# m = log_wl_model
# m_name = "Full Model"
# judges = TRUE
# judge_pv = "del_dime"

tidy_results <- function(m, m_name,judges,judge_pv = NULL){
  df <- tidy(m) %>%
    mutate(
      # clean up variable names
      var_name_pretty = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "dplyr::lead(Prez, n = 0)R" ~ "Party of President",
        term == "yr_file" ~ "File Year",
        term == "PLT_typ2Federal Government" ~ "Federal Government",
        term == "PLT_typ2Firms and Trade Associations" ~ "Firms and Trade Associations",
        term == "PLT_typ2Individuals" ~ "Individuals",
        term == "PLT_typ2Local Government" ~ "Local Government",
        term == "PLT_typ2Other" ~ "Other",
        term == "PLT_typ2State Government" ~ "State Government",
        term == "REGIONNORTHEAST/MID-ATLANTIC" ~ "Northeast & Mid-Atlantic",
        term == "REGIONPACIFIC" ~ "Pacific",
        term == "REGIONPLAINS" ~ "Plains",
        term == "REGIONSOUTH" ~ "South",
        term == "REGIONWESTERN INTERIOR" ~ "Western Interior",
        term == "prez_nameCarter" ~ "Carter",
        term == "prez_nameReagan" ~ "Reagan",
        term == "prez_nameH.W. Bush" ~ "H.W. Bush",
        term == "prez_nameClinton" ~ "Clinton",
        term == "prez_nameW. Bush" ~ "W. Bush",
        term == "prez_nameObama" ~ "Obama",
        term == "prez_nameTrump" ~ "Trump",
        term == "prez_nameBiden" ~ "Biden",
        term == "genderm" ~ "Male",
        term == "green_genGG" ~ "Green Generation (1935-1950)",
        term == "green_genPG" ~ "Pre-Green Generation (Pre-1935)",
        term == "party.affiliation.of.presidentDemocratic" ~ "Democratic",
        term == "party.affiliation.of.presidentRepublican" ~ "Republican",
        term == "imputed.dime.cfscore" ~ "DIME Score (CF)",
        term == "jcs.score.dw" ~ "JCS Score (DW)",
        term == "jcs.cfscore.cf" ~ "JCS Score (CF)",
        term == "pres.dw" ~ "President Score (DW)",
        term == "pres.dime.cfscore" ~ "President Score (CF)",
        term == "senscore.dw" ~ "Senate Del. Score (DW)",
        term == "senscore.dime.cfscore" ~ "Senate Del. Score (CF)",
        term == "state.delegation.dw" ~ "State Del. Score (DW)",
        term == "state.delegation.dime.cfscore" ~ "State Del. Score (CF)",
      ),
      # round estimates, std errors to third, fourth decimal
      estimate_r = round(estimate, 4),
      std.error_r = round(std.error, 4),
      # build p value signs
      p_symbol = case_when(
        p.value <= 0.001 ~ "***",
        p.value > 0.001 & p.value <= 0.01 ~ "**",
        p.value > 0.01 & p.value <= 0.05 ~ "*",
        p.value > 0.05 & p.value <= 0.1 ~ "†",
        TRUE ~ ""
      ),
      # build estimates with p-value symbols
      est_pretty = str_c(estimate_r,p_symbol, sep = ""),
      # build standard errors with parentheses
      std_error_pretty = str_c("(",std.error_r,")", sep = "")
    ) %>%
    select(
      var_name_pretty, est_pretty, std_error_pretty
    ) %>%
    # make into conventional reg results format
    pivot_longer(
      cols = est_pretty:std_error_pretty,
      names_to = "val_name",
      values_to = "val"
    ) %>%
    # make variable names ready for joins
    mutate(
      var_name_pretty = case_when(
        val_name == "std_error_pretty" ~ str_c(var_name_pretty," - stderr", sep = ""),
        val_name == "est_pretty" ~ str_c(var_name_pretty," - est", sep = ""),
        TRUE ~ var_name_pretty
      )
    ) %>%
    select(
      -val_name
    )
  
  # determine if model is with prez party or admin
  party_or_admin <- df %>%
    filter(
      str_detect(var_name_pretty, "Party") == TRUE
    )
  # make into value: party_or_admin will be 2 if the model is for presidential 
  # party; will be 0 if it's for prez admin.
  party_or_admin = length(party_or_admin$var_name_pretty)
  
  # create header rows for output
  
  # prez header
  if(party_or_admin > 0){
    prez_row <- df %>%
      filter(
        row_number() == 1
      ) %>%
      mutate(
        var_name_pretty = "Presidential Party (D = ref)",
        val = ""
      )
  } else {
    # note: for models specified 1988-2020, Clinton (the first prez,
    # alphabetically, in that time range) will be the ref. If the time
    # range changes, the ref might too. E.g., shifting to 1988-2021 would
    # include the first year of the Biden Administration; Biden is before
    # Clinton alphabetically, so Biden will shift to the ref category.
    prez_row <- df %>%
      filter(
        row_number() == 1
      ) %>%
      mutate(
        var_name_pretty = "Presidential Administration (Clinton = ref)",
        val = ""
      )
  }
  
  # case timing header
  timing_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Case Timing",
      val = ""
    )
  
  # p_typ header
  p_typ_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Plaintiff Type (ENGO = ref)",
      val = ""
    )
  
  # region header
  reg_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Region (Midwest = ref)",
      val = ""
    )
  
  # judge sex header
  sex_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Sex (Female = ref)",
      val = ""
    )
  
  # judge generation header
  gen_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Generation (Post-Green/Post-1950 = ref)",
      val = ""
    )
  
  # judge party header
  if(judges == TRUE & judge_pv == "prez_party"){
    jparty_row <- df %>%
      filter(
        row_number() == 1
      ) %>%
      mutate(
        var_name_pretty = "Appointing President's Party (Democratic = ref)",
        val = ""
      )
  } else if (judges == TRUE & judge_pv != "prez_party"){
    jparty_row <- df %>%
      filter(
        row_number() == 1
      ) %>%
      mutate(
        var_name_pretty = "Judicial Ideology",
        val = ""
      )
  } else{
    .
  }
  
  
  # Intercept header
  int_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Intercept",
      val = ""
    )
  
  # parameters and fit header
  pandf_row <- df %>%
    filter(
      row_number() == 1
    ) %>%
    mutate(
      var_name_pretty = "Model Parameters and Fit",
      val = ""
    )
  
  # break output into chunks for reassembly
  
  # prez chunk
  if(party_or_admin > 0){
    prez_chunk <- df %>%
      filter(
        str_detect(var_name_pretty, "Party")
      )
  } else {
    prez_list <- c("Biden|Trump|Obama|Bush|Clinton|Reagan|Carter")
    prez_chunk <- df %>%
      filter(
        str_detect(var_name_pretty, prez_list)
      ) %>%
      mutate(
        prez_order = case_when(
          str_detect(var_name_pretty,"Carter") ~ 1,
          str_detect(var_name_pretty,"Reagan") ~ 2,
          str_detect(var_name_pretty,"H.W. Bush") ~ 3,
          str_detect(var_name_pretty,"Clinton") ~ 4,
          str_detect(var_name_pretty,"W. Bush") ~ 5,
          str_detect(var_name_pretty,"Obama") ~ 6,
          str_detect(var_name_pretty,"Trump") ~ 7,
          str_detect(var_name_pretty,"Biden") ~ 8,
          TRUE ~ 9
        )
      ) %>%
      arrange(
        prez_order
      ) %>%
      select(
        -prez_order
      )
  }
  
  #timing chunk
  timing_chunk <- df %>%
    filter(
      str_detect(var_name_pretty, "File Year")
    )
  
  #plaintiff type chunk
  p_typ_list <- c("Federal|Firms|Individuals|Local|Other|State Gov")
  p_typ_chunk <- df %>%
    filter(
      str_detect(var_name_pretty, p_typ_list)
    )
    
  #region chunk
  region_list <- c("Northeast|Pacific|Plains|South|Western")
  region_chunk <- df %>%
  filter(
    str_detect(var_name_pretty, region_list)
    )
  
  #sex chunk
  sex_list <- c("Male")
  sex_chunk <- df %>%
    filter(
      str_detect(var_name_pretty, sex_list)
    )
  
  #gen chunk
  gen_list <- c("Green")
  gen_chunk <- df %>%
    filter(
      str_detect(var_name_pretty, gen_list)
    )
  
  #judge ideology chunk
  if(judges == TRUE & judge_pv == "prez_party"){
    jparty_list <- c("Democratic|Republican")
    jparty_chunk <- df %>%
      filter(
        str_detect(var_name_pretty, jparty_list)
      )
  } else if(judges == TRUE & judge_pv != "prez_party"){
    jparty_list <- c("DIME Score|JCS Score|President Score|Senate Del|State Del")
    jparty_chunk <- df %>%
      filter(
        str_detect(var_name_pretty, jparty_list)
      )
  }
    
  # intercept chunk
  int_chunk <- df %>%
    filter(
      str_detect(var_name_pretty, "Intercept")
    )
    
  # assemble chunks with headers
  if(party_or_admin > 0){
    df <- bind_rows(
      prez_row,
      prez_chunk,
      timing_row,
      timing_chunk,
      p_typ_row,
      p_typ_chunk,
      reg_row,
      region_chunk,
      sex_row,
      sex_chunk,
      gen_row,
      gen_chunk,
      jparty_row,
      jparty_chunk,
      int_row,
      int_chunk,
      pandf_row
      )
    } else {
    df <- bind_rows(
      prez_row,
      prez_chunk,
      p_typ_row,
      p_typ_chunk,
      reg_row,
      region_chunk,
      sex_row,
      sex_chunk,
      gen_row,
      gen_chunk,
      jparty_row,
      jparty_chunk,
      int_row,
      int_chunk,
      pandf_row
      )
    }
    
  # now get model summary stats and make pretty for joining to model estimates
  g <- glance(m) %>%
    pivot_longer(
      cols = everything(),
      names_to = "var_name_pretty",
      values_to = "val"
    ) %>%
    # make names pretty
    mutate(
      var_name_pretty = case_when(
        var_name_pretty == "null.deviance" ~ "Null deviance: ",
        var_name_pretty == "deviance" ~ "Residual deviance: ",
        var_name_pretty == "nobs" ~ "No. Obs.: ",
        var_name_pretty == "BIC" ~ "BIC: ",
        var_name_pretty == "AIC" ~ "AIC: ",
        var_name_pretty == "logLik" ~ "Log Likelihood: ",
        TRUE ~ var_name_pretty
      ),
      # round specified values
      val = case_when(
        var_name_pretty == "Null deviance: " ~ round(val,0),
        var_name_pretty == "Residual deviance: " ~ round(val,0),
        var_name_pretty == "df.null" ~ round(val,0),
        var_name_pretty == "df.residual" ~ round(val,0),
        var_name_pretty == "Log Likelihood: " ~ round(val,2),
        var_name_pretty == "AIC: " ~ round(val,0),
        var_name_pretty == "BIC: " ~ round(val,0),
        TRUE ~ val
      ),
      # make no. obs pretty; change other values to characters
      val = case_when(
        var_name_pretty == "No. Obs.: " ~ prettyNum(val,","),
        TRUE ~ as.character(val)
      ),
      val = case_when(
        var_name_pretty == "Null deviance: " ~ 
          str_c(val[var_name_pretty=="Null deviance: "],
                " on ",
                val[var_name_pretty=="df.null"],
                " degrees of freedom",
                sep = ""),
        var_name_pretty == "Residual deviance: " ~ 
          str_c(val[var_name_pretty=="Residual deviance: "],
                " on ",
                val[var_name_pretty=="df.residual"],
                " degrees of freedom",
                sep = ""),
        TRUE ~ val
      )
    ) %>%
    # drop rows no longer needed
    filter(
      var_name_pretty != "df.null",
      var_name_pretty != "df.residual"
    ) %>%
    # order rows
    mutate(
      row_ord = case_when(
        var_name_pretty == "Null deviance: " ~ 1,
        var_name_pretty == "Residual deviance: " ~ 2,
        var_name_pretty == "Log Likelihood: " ~ 3,
        var_name_pretty == "AIC: " ~ 4,
        var_name_pretty == "BIC: " ~ 5,
        var_name_pretty == "No. Obs.: " ~ 6,
        TRUE ~ 99
      )
    ) %>%
    # drop any no-labeled rows
    filter(
      row_ord <= 6
    ) %>%
    arrange(
      row_ord
    ) %>%
    select(
      -row_ord
    )
    
  # join model reutls to model stats
  df <- bind_rows(df, g)
  
  # rename vals colum to model name
  df <- df %>%
      rename(
        {{m_name}} := "val"
        )
  
}
