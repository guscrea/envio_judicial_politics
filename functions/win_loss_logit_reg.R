# Function for running win-loss regressions, collecting and plotting results

# inputs
# judge_pv: variable specifying which indicator of judge partisanship/ideology
#           will be modeled. Can be "prez_party", "dime", "jcs_dw", "jcs_cf",
#           "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", "del_dime"
#           See Bonica and Sen (2013[?]) for details.


# for testing
# df = "fjc_cl_j"
# jud = TRUE
# disp_drop = NULL
# noBP_IMC = TRUE
# diag = FALSE
# yr_i = 1980
# yr_f = 2020
# party_or_admin = "PARTY"
# judges = TRUE
# judge_pv = "del_dime"
# name_append = NULL
# recode_set = NULL
# recode_mixed = NULL

win_los_reg <- function(
    df,
    jud,
    disp_drop,
    noBP_IMC,
    diag,
    yr_i,
    yr_f,
    party_or_admin,
    judges,
    judge_pv = NULL,
    name_append = NULL,
    recode_set = NULL,
    recode_mixed = NULL
    ){
  
  # build save_name base text based on input parameters ####
  save_name = "_"
  
  # jud
  if(jud == TRUE) {
    save_name = str_c(save_name,"")
  } else if (jud == FALSE) {
    save_name = str_c(save_name,"jud_and_non_jud_")
  } else {
    stop(cat('Cannot tell if cases should be limited to those with judgments and other legal endings. Did you set jud as TRUE or FALSE?'))
  }
  
  # additional dispositions dropped 
  if(is.null(disp_drop)) {
    save_name = str_c(save_name,"")
  } else if (!is.null(disp_drop)) {
    save_name = str_c(save_name,"addtl_disp_drops_",str_c(disp_drop, collapse = "_"),"_")
  } else {
    stop(cat('Cannot tell if diagonal (itra-type) cases should be included. Did you set diag as TRUE or FALSE?'))
  }
  
  # noBP_IMC
  if(noBP_IMC == TRUE) {
    save_name = str_c(save_name,"noBP_IMC_")
  } else if (noBP_IMC == FALSE) {
    save_name = str_c(save_name,"withBP_IMC_")
  } else {
    stop(cat('Cannot tell if BP and IMC cases should be included. Did you set noBP_IMC as TRUE or FALSE?'))
  }
  
  # diagonal
  if(diag == TRUE) {
    save_name = str_c(save_name,"")
  } else if (diag == FALSE) {
    save_name = str_c(save_name,"no_diag_")
  } else {
    stop(cat('Cannot tell if diagonal (itra-type) cases should be included. Did you set diag as TRUE or FALSE?'))
  }
  
  # append year start and end
  save_name = str_c(save_name,yr_i,"-",yr_f,"_")
  
  # party_or_admin
  if(party_or_admin == "PARTY") {
    save_name = str_c(save_name,"PrezPARTY")
  } else if (party_or_admin == "ADMIN") {
    save_name = str_c(save_name,"PrezADMIN")
  } else {
    stop(cat('Cannot tell if the model should control for prez party or prez administration. Did you set party_or_admin to "PARTY" or "ADMIN"?'))
  }
  
  # judge characteristics in or not
  if(judges == TRUE) {
    save_name = str_c(save_name,"_judge_",judge_pv)
  } else if (judges != TRUE) {
    save_name = str_c(save_name,"_nojudge")
  } else {
    stop(cat('Cannot tell if the model should include or exclude judge characteristics. Did you set judges to TRUE or FALSE and set judge_pv to an appropriate value (e.g. "prez_aprty"?)'))
  }
  
  # add name append
  if(is.null(name_append)) {
    save_name = str_c(save_name,"")
  } else if (!is.null(name_append)) {
    save_name = str_c(save_name,"_",name_append)
  } else {
    save_name = str_c(save_name,"")
  }
  
  # recode settlements as not wins 
  if(is.null(recode_set)) {
    save_name = str_c(save_name,"")
  } else if (!is.null(recode_set)) {
    save_name = str_c(save_name,"_SET_not_win")
  } else {
    save_name = str_c(save_name,"")
  }
  
  # recode mixed as not wins 
  if(is.null(recode_mixed)) {
    save_name = str_c(save_name,"")
  } else if (!is.null(recode_mixed)) {
    save_name = str_c(save_name,"_MIXED_not_win")
  } else {
    save_name = str_c(save_name,"")
  }
  
  # read in data on party in power ####
  
  # Note: 0 = Dem; 1 = Rep
  party <- read.csv("data/partisan_control/party_control_prez_sen_house.csv")
  # recode party 1's and 0's as R and D
  party <- party %>%
    mutate(
      Prez = case_when(
        # in regression, R will factorize these character and the first
        # in alphabetical order will be the reference category (D).
        Prez == 1 ~ "R",
        Prez == 0 ~ "D",
        TRUE ~ "Unknown"
      ),
      Sen = case_when(
        Sen == 1 ~ "R",
        Sen == 0 ~ "D",
        TRUE ~ "Unknown"
      ),
      Hou = case_when(
        Hou == 1 ~ "R",
        Hou == 0 ~ "D",
        TRUE ~ "Unknown"
      )
    ) %>%
    rename(
      # for joining to fjc data
      "yr_file" = "Year"
    )
  
  # build df for win-not-win logistic regression ####
  
  # build df - this function starts by calling the simp_filt() function, which
  # takes care of filtering the data in terms of keeping only cases that include
  # judgements and settlements (jud), any additional disposition drops
  # (disp_drop), BP and IMC cases (noBP_IMC), and intra-type cases, like fed
  # suing fed or firms suing firms (diag).
  reg_df <- simp_filt(
    df = df,
    jud = jud,
    disp_drop = disp_drop,
    noBP_IMC = noBP_IMC,
    diag = diag
    ) %>%
    # drop cases before yr_i and after yr_f
    filter(
      yr_file >= yr_i,
      yr_file <= yr_f
    ) %>%
    # if recode_set != NULL, recode settlements (DISP == 13) as "l" (losses)
    {
      if(!is.null(recode_set))
        mutate(
          .,
          PLT_wl = case_when(
            DISP == 13 ~ "l",
            TRUE ~ PLT_wl
          ),
          DEF_wl = case_when(
            DISP == 13 ~ "l",
            TRUE ~ DEF_wl
          )
        )
      else
        .
      } %>%
    # if recode_mixed != NULL, recode mixed outcomes (JUDGEMENT == 3) as "l"
    # (losses)
    {
      if(!is.null(recode_mixed))
        mutate(
          .,
          PLT_wl = case_when(
            JUDGMENT == 3 ~ "l",
            TRUE ~ PLT_wl
          ),
          DEF_wl = case_when(
            JUDGMENT == 3 ~ "l",
            TRUE ~ DEF_wl
          )
        )
      else
        .
      } %>%
    # drop "neither" wins nor losses (n) - see comment ~ line 1727 in
    # 07-sm_figs.R.
    filter(
      PLT_wl != "n"
    ) %>%
    # make win-loss numeric; This code will produce NAs for observations that
    # are neither "w" nor "l"; those observations will be dropped automatically
    # in the regression
    mutate(
      PLT_wl = case_when(
        PLT_wl == "w" ~ 1,
        PLT_wl == "l" ~ 0,
      )
    ) %>%
    # re-categorize plaintiff types for regression
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
    # join party data
    left_join(
      party, by = "yr_file"
    ) %>%
    # make individual president factor variable
    mutate(
      prez_name = case_when(
        yr_file <= 1988 ~ "Reagan",
        yr_file > 1988 & yr_file <= 1992 ~ "H.W. Bush",
        yr_file > 1992 & yr_file <= 2000 ~ "Clinton",
        yr_file > 2000 & yr_file <= 2008 ~ "W. Bush",
        yr_file > 2008 & yr_file <= 2016 ~ "Obama",
        yr_file > 2016 & yr_file <= 2020 ~ "Trump",
        yr_file > 2020 & yr_file <= 2024 ~ "Biden"
      ),
      prez_name = as.factor(prez_name)
    ) %>%
    # make REGION a factor for regression
    mutate(
      REGION = as.factor(REGION),
      duration = as.numeric(TERMDATE - FILEDATE)
      ) %>%
    {
      if(judges == TRUE){
        mutate(
          .,
          gender = as.factor(gender),
          party.affiliation.of.president = as.factor(party.affiliation.of.president),
        )
      } else {
        .
      }
    } %>%
    ungroup()
  
  # build and plot correlation matrix ####
  
  # first, select variables of interest
  if(judges == TRUE){
    cor_vars <- c(
      "PLT_wl",
      "yr_file",
      "Prez",
      "prez_name",
      "PLT_typ2",
      "REGION",
      "gender",
      "party.affiliation.of.president",
      "imputed.dime.cfscore",
      "jcs.score.dw",
      "jcs.cfscore.cf",
      "pres.dw",
      "pres.dime.cfscore",
      "senscore.dw",
      "senscore.dime.cfscore",
      "state.delegation.dw",
      "state.delegation.dime.cfscore")
    } else {
      cor_vars <- c(
      "PLT_wl",
      "yr_file",
      "Prez",
      "prez_name",
      "PLT_typ2",
      "REGION")
    }
   
  cor_df <- reg_df %>%
    ungroup() %>%
    select(
      all_of(cor_vars)
    ) %>%
    mutate(
      rowID = row_number(),
      value = 1,
      #PLT_typ2 = as.character(PLT_typ2),
      REGION = as.character(REGION),
      REGION = str_to_title(REGION)
    ) %>%
    filter(
      !is.na(REGION)
    ) %>%
    pivot_wider(
      names_from = PLT_typ2,
      values_from = value,
      values_fill = 0
    ) %>%
    mutate(
      value = 1,
    ) %>%
    pivot_wider(
      names_from = REGION,
      values_from = value,
      values_fill = 0
    ) %>%
    mutate(
      value = 1,
    ) %>%
    pivot_wider(
      names_from = prez_name,
      values_from = value,
      values_fill = 0
    ) %>%
    mutate(
      value = 1,
    ) %>%
    pivot_wider(
      names_from = Prez,
      values_from = value,
      values_fill = 0
    ) %>%
    rename(
      "Prez_R" = "R",
      "Prez_D" = "D"
    ) %>%
    {
      if(judges == TRUE){
        mutate(
          .,
          value = 1,
        ) %>%
        pivot_wider(
          .,
          names_from = gender,
          values_from = value,
          values_fill = 0
          ) %>%
          rename(
            .,
            "male" = "m",
            "female" = "f"
          ) %>%
          mutate(
            .,
            # make male and female NA if NA column (from pivot_wider, above) is 1
            male = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ male
              ),
            female = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ female
            ),
            # add value column for next pivot wider
            value = 1,
            ) %>%
          # drop the NA column
          select(
            .,
            -`NA`
          ) %>%
          pivot_wider(
            .,
            names_from = party.affiliation.of.president,
            values_from = value,
            values_fill = 0
            ) %>%
          mutate(
            .,
            # make judge partisan columns NA if NA column (from pivot_wider,
            # above) is 1
            Republican = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ Republican
            ),
            Democratic = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ Democratic
            )
          ) %>%
          # drop the NA column
          select(
            .,
            -`NA`
          ) 
        } else{
          .
          }
      } %>%
    # drop ROW ID
    select(
      -rowID
      )
  
  cor_mat <- cor(
    cor_df
  )
  
  # make correlation matrix name excluding name elements that do not effect obs.
  corr_save_name = str_remove_all(save_name,"_PrezADMIN")
  corr_save_name = str_remove_all(corr_save_name,"_PrezPARTY")
  
  # note: corrplot() does not produce a gg-object that can be saved
  # with ggsave(). So we use standard base R. First, open a png device...
  png(
    filename = str_c("regressions/cor_matrices/corr_mat", corr_save_name, ".png"),
    width = 2000,
    height = 2000,
    pointsize = 44
    )
  
  # ... plot pretty correlation matrix
  corrplot(
    cor_mat,
    method = 'color',
    type = 'lower',
    #tl.srt = 45,
    addCoef.col = 'grey50',
    #col = viridis::viridis(100),
    col = COL2('PuOr', 100),
    number.cex = 10/ncol(cor_df),
    # variable label color and size
    tl.col="black",
    tl.cex = .7,
    # number label size
    cl.cex = .7
    )
  
  # then close the device
  dev.off()
  
  # summary statistics ####
  
  st(
    cor_df,
    out = "csv",
    file = str_c("regressions/summary_stats/summary_stats", corr_save_name, ".csv")
    )
  
  # fit models ####
  
  # call function fo building tidy results
  source("functions/tidy_reg_results.R")
  
  # call different model depending on whether controlling for prez party
  # (testing for partisan effect) or the specific administration of each
  # president (presidential effect).
  
  if(party_or_admin == "PARTY" & judges == TRUE){
    
    # fit model with all plaintiff types
    if(judge_pv == "prez_party"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          party.affiliation.of.president,
        data = reg_df,
        family = 'binomial'
        )
    } else if(judge_pv == "dime"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          imputed.dime.cfscore,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          jcs.score.dw,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          jcs.cfscore.cf,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          pres.dw,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          pres.dime.cfscore,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          senscore.dw,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          senscore.dime.cfscore,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          state.delegation.dw,
        data = reg_df,
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          state.delegation.dime.cfscore,
        data = reg_df,
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # look at VIF scores (for manual insepction)
    vif(log_wl_model)
    
    # collect tidy results
    log_wl_model_tidy <- tidy_results(log_wl_model,"Full Model", judges = judges, judge_pv = judge_pv)
    
    # fit model with only federal plaintiffs
    if(judge_pv == "prez_party"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          party.affiliation.of.president,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "dime"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          imputed.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.score.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.cfscore.cf,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # collect tidy results
    log_wl_model_fed_tidy <- tidy_results(log_wl_model_fed, "Federal Plaintiffs", judges = judges, judge_pv = judge_pv)
    
    # fit model with only ENGO plaintiffs
    if(judge_pv == "prez_party"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          party.affiliation.of.president,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "dime"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          imputed.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.score.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.cfscore.cf,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # collect tidy results
    log_wl_model_NGO_tidy <- tidy_results(log_wl_model_NGO, "ENGO Plaintiffs", judges = judges, judge_pv = judge_pv)
    
    # fit model with only BIZ plaintiffs
    if(judge_pv == "prez_party"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          party.affiliation.of.president,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "dime"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          imputed.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.score.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          jcs.cfscore.cf,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          pres.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          senscore.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dw,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION +
          gender +
          state.delegation.dime.cfscore,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # collect tidy results
    log_wl_model_BIZ_tidy <- tidy_results(log_wl_model_BIZ, "Firm Plaintiffs", judges = judges, judge_pv = judge_pv)
    
    # build axis labels for plot figure based on model results
    # reverse order of list
    axis_labels_list <- rev(labels(log_wl_model$coefficients)[-1]) # exclude first item (intercept)
    
    # clean up axis labels list
    axis_labels_list <- axis_labels_list %>%
      str_remove_all(
        "REGION|PLT_typ2|party.affiliation.of.president|dplyr::lead\\(|, n = 0\\)R"
      ) %>%
      str_to_title() %>%
      str_replace_all(
        "And", "and"
      ) %>%
      str_replace_all(
        "Yr_file", "File Year"
      ) %>%
      str_replace_all(
        "Prez", "Pres. Party"
      ) %>%
      str_replace_all(
        "Other", "Other Plaintiffs"
      ) %>%
      str_replace_all(
        "Genderm", "Male"
      ) %>%
      str_replace_all(
        "Imputed.dime.cfscore", "DIME Score (CF)"
      ) %>%
      str_replace_all(
        "Jcs.score.dw", "JCS Score (DW)"
      ) %>%
      str_replace_all(
        "Jcs.cfscore.cf", "JCS Score (CF)"
      ) %>%
      str_replace_all(
        "Pres.dw", "President Score (DW)"
      ) %>%
      str_replace_all(
        "Pres.dime.cfscore", "President Score (CF)"
      ) %>%
      str_replace_all(
        "Senscore.dw", "Senate Del. Score (DW)"
      ) %>%
      str_replace_all(
        "Senscore.dime.cfscore", "Senate Del. Score (CF)"
      ) %>%
      str_replace_all(
        "State.delegation.dw", "State Del. Score (DW)"
      ) %>%
      str_replace_all(
        "State.delegation.dime.cfscore", "State Del. Score (CF)"
      )
    
    # plot model results with sjPlot
    all_models <- plot_models(
      log_wl_model, 
      log_wl_model_fed, log_wl_model_NGO, log_wl_model_BIZ,
      #grid = TRUE,
      show.values = TRUE,
      axis.lim = c(0.5,2),
      colors = "viridis",
      value.size = 2.5,
      spacing = 0.65,
      legend.title = "Model",
      vline.color = "grey",
      title = NULL,
      axis.labels = axis_labels_list,
      m.labels = c("All Plaintiffs","Federal Government Only", "ENGOs Only", "Firms Only")
      #m.labels = c("Federal Government Only", "ENGOs Only", "Firms Only")
    ) + theme_linedraw()
    
    # plot
    all_models
    
    #save
    ggsave(
      str_c("Logit_Odds_Ratios", save_name, ".png"),
      plot = last_plot(),
      width = 7,
      height = 14,
      path = "regressions/forest_plots"
      )
    
    } else if (party_or_admin == "ADMIN" & judges == TRUE) {
      
      # fit model with all plaintiff types
      if(judge_pv == "prez_party"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            party.affiliation.of.president,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "dime"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            imputed.dime.cfscore,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_dw"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            jcs.score.dw,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_cf"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            jcs.cfscore.cf,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dw"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            pres.dw,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dime"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            pres.dime.cfscore,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dw"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            senscore.dw,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dime"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            senscore.dime.cfscore,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "del_dw"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            state.delegation.dw,
          data = reg_df,
          family = 'binomial'
        )
      } else if(judge_pv == "del_dime"){
        log_wl_model <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            PLT_typ2 +
            REGION +
            gender +
            state.delegation.dime.cfscore,
          data = reg_df,
          family = 'binomial'
        )
      } else {
        stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
      }
      
      # look at VIF scores (for manual insepction)
      vif(log_wl_model)
      
      # collect tidy results
      log_wl_model_tidy <- tidy_results(log_wl_model,"Full Model", judges = judges, judge_pv = judge_pv)
      
      # fit model with only federal plaintiffs
      if(judge_pv == "prez_party"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            party.affiliation.of.president,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "dime"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            imputed.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_dw"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.score.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_cf"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.cfscore.cf,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dw"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dime"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dw"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dime"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dw"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dime"){
        log_wl_model_fed <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "FED"
            ),
          family = 'binomial'
        )
      } else {
        stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
      }
      
      # collect tidy results
      log_wl_model_fed_tidy <- tidy_results(log_wl_model_fed, "Federal Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # fit model with only ENGO plaintiffs
      if(judge_pv == "prez_party"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            party.affiliation.of.president,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "dime"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            imputed.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_dw"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.score.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_cf"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.cfscore.cf,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dw"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dime"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dw"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dime"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dw"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dime"){
        log_wl_model_NGO <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "NGO"
            ),
          family = 'binomial'
        )
      } else {
        stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
      }
      
      # collect tidy results
      log_wl_model_NGO_tidy <- tidy_results(log_wl_model_NGO, "ENGO Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # fit model with only BIZ plaintiffs
      if(judge_pv == "prez_party"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            party.affiliation.of.president,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "dime"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            imputed.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_dw"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.score.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "jcs_cf"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            jcs.cfscore.cf,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dw"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "prez_dime"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            pres.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dw"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "sen_dime"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            senscore.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dw"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dw,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else if(judge_pv == "del_dime"){
        log_wl_model_BIZ <- glm(
          PLT_wl ~ #dplyr::lead(Prez, n = 0) +
            prez_name + 
            yr_file +
            #PLT_typ2 +
            REGION +
            gender +
            state.delegation.dime.cfscore,
          data = reg_df %>%
            filter(
              PLT_typ == "BIZ"
            ),
          family = 'binomial'
        )
      } else {
        stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
      }
      
      # collect tidy results
      log_wl_model_BIZ_tidy <- tidy_results(log_wl_model_BIZ, "Firm Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # build axis labels for plot figure based on model results
      # reverse order of list
      axis_labels_list <- rev(labels(log_wl_model$coefficients)[-1]) # exclude first item (intercept)
      
      # clean up axis labels list
      axis_labels_list <- axis_labels_list %>%
        str_remove_all(
          "REGION|PLT_typ2|party.affiliation.of.president|prez_name"
        ) %>%
        str_to_title() %>%
        str_replace_all(
          "And", "and"
        ) %>%
        str_replace_all(
          "H.w.", "H.W."
        ) %>%
        str_replace_all(
          "Other", "Other Plaintiffs"
        ) %>%
        str_replace_all(
          "Genderm", "Male"
        ) %>%
        str_replace_all(
          "Imputed.dime.cfscore", "DIME Score (CF)"
        ) %>%
        str_replace_all(
          "Jcs.score.dw", "JCS Score (DW)"
        ) %>%
        str_replace_all(
          "Jcs.cfscore.cf", "JCS Score (CF)"
        ) %>%
        str_replace_all(
          "Pres.dw", "President Score (DW)"
        ) %>%
        str_replace_all(
          "Pres.dime.cfscore", "President Score (CF)"
        ) %>%
        str_replace_all(
          "Senscore.dw", "Senate Del. Score (DW)"
        ) %>%
        str_replace_all(
          "Senscore.dime.cfscore", "Senate Del. Score (CF)"
        ) %>%
        str_replace_all(
          "State.delegation.dw", "State Del. Score (DW)"
        ) %>%
        str_replace_all(
          "State.delegation.dime.cfscore", "State Del. Score (CF)"
        )
      
      # plot model results with sjPlot
      all_models <- plot_models(
        log_wl_model, 
        log_wl_model_fed, log_wl_model_NGO, log_wl_model_BIZ,
        #grid = TRUE,
        show.values = TRUE,
        axis.lim = c(0.5,2),
        colors = "viridis",
        value.size = 2.5,
        spacing = 0.65,
        legend.title = "Model",
        vline.color = "grey",
        title = NULL,
        axis.labels = axis_labels_list,
        m.labels = c("All Plaintiffs","Federal Government Only", "ENGOs Only", "Firms Only")
        #m.labels = c("Federal Government Only", "ENGOs Only", "Firms Only")
      ) + theme_linedraw()
      
      # plot
      all_models
      
      #save
      ggsave(
        str_c("Logit_Odds_Ratios", save_name, ".png"),
        plot = last_plot(),
        width = 7,
        height = 14,
        path = "regressions/forest_plots"
        )
    } else if(party_or_admin == "PARTY" & judges != TRUE){
      # fit model with all plaintiff types
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION,
        data = reg_df,
        family = 'binomial'
      )
      
      # look at VIF scores (for manual insepction)
      vif(log_wl_model)
      
      # collect tidy results
      log_wl_model_tidy <- tidy_results(log_wl_model,"Full Model", judges = judges, judge_pv = judge_pv)
      
      # fit model with only federal plaintiffs
      log_wl_model_fed <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_fed_tidy <- tidy_results(log_wl_model_fed, "Federal Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # fit model with only ENGO plaintiffs
      log_wl_model_NGO <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_NGO_tidy <- tidy_results(log_wl_model_NGO, "ENGO Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      
      # fit model with only BIZ plaintiffs
      log_wl_model_BIZ <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),,
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_BIZ_tidy <- tidy_results(log_wl_model_BIZ, "Firm Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # build axis labels for plot figure based on model results
      # reverse order of list
      axis_labels_list <- rev(labels(log_wl_model$coefficients)[-1]) # exclude first item (intercept)
      
      # clean up axis labels list
      axis_labels_list <- axis_labels_list %>%
        str_remove_all(
          "REGION|PLT_typ2|dplyr::lead\\(|, n = 0\\)R"
        ) %>%
        str_to_title() %>%
        str_replace_all(
          "And", "and"
        ) %>%
        str_replace_all(
          "Yr_file", "File Year"
        ) %>%
        str_replace_all(
          "Prez", "Pres. Party"
        ) %>%
        str_replace_all(
          "Other", "Other Plaintiffs"
        )
      
      # plot model results with sjPlot
      all_models <- plot_models(
        log_wl_model, 
        log_wl_model_fed, log_wl_model_NGO, log_wl_model_BIZ,
        #grid = TRUE,
        show.values = TRUE,
        axis.lim = c(0.5,2),
        colors = "viridis",
        value.size = 2.5,
        spacing = 0.65,
        legend.title = "Model",
        vline.color = "grey",
        title = NULL,
        axis.labels = axis_labels_list,
        m.labels = c("All Plaintiffs","Federal Government Only", "ENGOs Only", "Firms Only")
        #m.labels = c("Federal Government Only", "ENGOs Only", "Firms Only")
      ) + theme_linedraw()
      
      # plot
      all_models
      
      #save
      ggsave(
        str_c("Logit_Odds_Ratios", save_name, ".png"),
        plot = last_plot(),
        width = 7,
        height = 14,
        path = "regressions/forest_plots"
      )
      
    } else if (party_or_admin == "ADMIN" & judges != TRUE) {
      # fit model with all plaintiff types
      log_wl_model <- glm(
        PLT_wl ~ #dplyr::lead(Prez, n = 0) +
          prez_name + 
          #yr_file +
          PLT_typ2 +
          REGION,
        data = reg_df,
        family = 'binomial'
      )
      
      # look at VIF scores (for manual inspection)
      vif(log_wl_model)
      
      # collect tidy results
      log_wl_model_tidy <- tidy_results(log_wl_model,"Full Model", judges = judges, judge_pv = judge_pv)
      
      # fit model with only federal plaintiffs
      log_wl_model_fed <- glm(
        PLT_wl ~ #dplyr::lead(Prez, n = 0) +
          prez_name + 
          #yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "FED"
          ),
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_fed_tidy <- tidy_results(log_wl_model_fed, "Federal Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # fit model with only ENGO plaintiffs
      log_wl_model_NGO <- glm(
        PLT_wl ~ #dplyr::lead(Prez, n = 0) +
          prez_name + 
          #yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "NGO"
          ),
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_NGO_tidy <- tidy_results(log_wl_model_NGO, "ENGO Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      
      # fit model with only BIZ plaintiffs
      log_wl_model_BIZ <- glm(
        PLT_wl ~ #dplyr::lead(Prez, n = 0) +
          prez_name + 
          #yr_file +
          #PLT_typ2 +
          REGION,
        data = reg_df %>%
          filter(
            PLT_typ == "BIZ"
          ),,
        family = 'binomial'
      )
      
      # collect tidy results
      log_wl_model_BIZ_tidy <- tidy_results(log_wl_model_BIZ, "Firm Plaintiffs", judges = judges, judge_pv = judge_pv)
      
      # build axis labels for plot figure based on model results
      # reverse order of list
      axis_labels_list <- rev(labels(log_wl_model$coefficients)[-1]) # exclude first item (intercept)
      
      # clean up axis labels list
      axis_labels_list <- axis_labels_list %>%
        str_remove_all(
          "REGION|PLT_typ2|prez_name"
        ) %>%
        str_to_title() %>%
        str_replace_all(
          "And", "and"
        ) %>%
        str_replace_all(
          "H.w.", "H.W."
        ) %>%
        str_replace_all(
          "Other", "Other Plaintiffs"
        )
      
      
      # plot model results with sjPlot
      all_models <- plot_models(
        log_wl_model, 
        log_wl_model_fed, log_wl_model_NGO, log_wl_model_BIZ,
        #grid = TRUE,
        show.values = TRUE,
        axis.lim = c(0.5,2),
        colors = "viridis",
        value.size = 2.5,
        spacing = 0.65,
        legend.title = "Model",
        vline.color = "grey",
        title = NULL,
        axis.labels = axis_labels_list,
        m.labels = c("All Plaintiffs","Federal Government Only", "ENGOs Only", "Firms Only")
        #m.labels = c("Federal Government Only", "ENGOs Only", "Firms Only")
      ) + theme_linedraw()
      
      # plot
      all_models
      
      #save
      ggsave(
        str_c("Logit_Odds_Ratios", save_name, ".png"),
        plot = last_plot(),
        width = 7,
        height = 14,
        path = "regressions/forest_plots"
      )
    } else {
      stop(cat('Cannot tell which model to run. Did you set party_or_admin = "PARTY" or "ADMIN" and judges = TRUE or FALSE?'))
    }
    
  # build and write out a tidy df of all model results ####
  
  # first join ENGO df to full model
  all_models_tidy <- left_join(
    log_wl_model_tidy,
    log_wl_model_NGO_tidy, by = "var_name_pretty"
  )
  
  # then join fed df
  all_models_tidy <- left_join(
    all_models_tidy,
    log_wl_model_fed_tidy, by = "var_name_pretty"
  )
  
  # then join BIZ df
  all_models_tidy <- left_join(
    all_models_tidy,
    log_wl_model_BIZ_tidy, by = "var_name_pretty"
  )
  
  # with models assembled, drop variable names with stderr
  all_models_tidy <- all_models_tidy %>%
    mutate(
     var_name_pretty = case_when(
       str_detect(var_name_pretty, " - stderr") ~ "",
       str_detect(var_name_pretty, " - est") ~ str_remove_all(var_name_pretty, " - est"),
       TRUE ~ var_name_pretty
       ) 
     ) %>%
    rename(
      "Variable" = "var_name_pretty"
    ) %>%
    # get rid of NAs; replace with blanks
    mutate(
      `ENGO Plaintiffs` = case_when(
        is.na(`ENGO Plaintiffs`) ~ "",
        TRUE ~ `ENGO Plaintiffs`
      ),
      `Federal Plaintiffs` = case_when(
        is.na(`Federal Plaintiffs`) ~ "",
        TRUE ~ `Federal Plaintiffs`
      ),
      `Firm Plaintiffs` = case_when(
        is.na(`Firm Plaintiffs`) ~ "",
        TRUE ~ `Firm Plaintiffs`
      )
    )
  
  ##write out tidy regression results as csv
  write_csv(
    all_models_tidy,
    file = str_c("regressions/table_results/logit_model_table", save_name, ".csv")
  )
  
# the end.
  
}