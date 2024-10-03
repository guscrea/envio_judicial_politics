# Function for running win-loss regressions, collecting and plotting results

# inputs
# judge_pv: variable specifying which indicator of judge partisanship/ideology
#           will be modeled. Can be "prez_party", "dime", "jcs_dw", "jcs_cf",
#           "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", "del_dime"
#           See Bonica and Sen (2013[?]) for details.


# for testing
# df = "resl"
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
# RESL = TRUE
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
    RESL = FALSE,
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
  
  # resl
  if(RESL == FALSE){
    save_name = save_name
    } else if (RESL != FALSE) {
    save_name = str_c(save_name,"_resl")
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
          # make age at term NA when it exceeds 100. (there are a couple of outliers that are obviously wrong)
          age_at_term = case_when(
            age_at_term > 100 ~ NA_real_,
            TRUE ~ age_at_term
          )
        )
      } else {
        .
      }
    } %>%
    ungroup()
  
  # build and plot correlation matrix ####
  
  # first, select variables of interest
  if(judges == TRUE & RESL == FALSE){
    cor_vars <- c(
      "PLT_wl",
      "yr_file",
      "Prez",
      "prez_name",
      "PLT_typ2",
      "REGION",
      "gender",
      "age_at_term",
      "green_gen",
      "party.affiliation.of.president",
      "imputed.dime.cfscore",
      "jcs.score.dw",
      "jcs.cfscore.cf",
      "pres.dw",
      "pres.dime.cfscore",
      "senscore.dw",
      "senscore.dime.cfscore",
      "state.delegation.dw",
      "state.delegation.dime.cfscore"
      )
  } else if(judges == TRUE & RESL == TRUE){
    cor_vars <- c(
      "PLT_wl",
      "yr_file",
      "Prez",
      "prez_name",
      "PLT_typ2",
      "REGION",
      "gender",
      "age_at_term",
      "green_gen",
      "party.affiliation.of.president",
      "imputed.dime.cfscore",
      "jcs.score.dw",
      "jcs.cfscore.cf",
      "pres.dw",
      "pres.dime.cfscore",
      "senscore.dw",
      "senscore.dime.cfscore",
      "state.delegation.dw",
      "state.delegation.dime.cfscore",
      "ooc_mc1")
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
    #filter(
    #  !is.na(REGION)
    #) %>%
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
      # make REGIONS columns NA if NA column (from pivot_wider, above) is 1
      `Northeast/Mid-Atlantic` = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ `Northeast/Mid-Atlantic`
      ),
      South = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ South
      ),
      Midwest = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ Midwest
      ),
      Pacific = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ Pacific
      ),
      `Western Interior` = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ `Western Interior`
      ),
      Plains = case_when(
        `NA` == 1 ~ NA_real_,
        TRUE ~ Plains
      ),
    ) %>%
    # drop NA category
    select(
      -`NA`
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
          #names_repair = "unique"
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
            names_from = green_gen,
            values_from = value,
            values_fill = 0
          ) %>%
          rename(
            .,
            "Pre-Green Gen" = "PG",
            "Green Gen" = "GG",
            "Post-Green Gen" = "AG",
          ) %>%
          mutate(
            .,
            # make generation columns NA if NA column (from pivot_wider, above)
            # is 1
            `Pre-Green Gen` = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ `Pre-Green Gen`
            ),
            `Green Gen` = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ `Green Gen`
            ),
            `Post-Green Gen` = case_when(
              `NA` == 1 ~ NA_real_,
              TRUE ~ `Post-Green Gen`
            ),
            # make new values column for newxt pivot wider 
            value = 1
          ) %>%
          # drop NA column
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
    } %>% {
      if(RESL == TRUE){
        mutate(
          .,
          value = 1
        ) %>%
          pivot_wider(
            .,
            names_from = ooc_mc1,
            values_from = value,
            values_fill = 0
          )
      } else {
          .
        }
      } %>%
    # drop ROW ID
    select(
      -rowID
      )
  
  cor_mat <- stats::cor(
    cor_df,
    use = "complete.obs"
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
  
  if(party_or_admin == "PARTY" & judges == TRUE & RESL == FALSE){
    
    # fit model with all plaintiff types
    if(judge_pv == "prez_party"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          gender +
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
          #age_at_term +
          green_gen +
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
        "Green_gengg", "Green Generation (1935-1950)"
      ) %>%
      str_replace_all(
        "Green_genpg", "Pre-Green Generation (Pre-1935)"
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
      #axis.lim = c(0.5,3),
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
    
    } else if (party_or_admin == "ADMIN" & judges == TRUE & RESL == FALSE) {
      
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
        #axis.lim = c(0.5,3),
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
    } else if(party_or_admin == "PARTY" & judges != TRUE & RESL == FALSE){
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
        #axis.lim = c(0.5,3),
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
      
    } else if (party_or_admin == "ADMIN" & judges != TRUE & RESL == FALSE) {
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
        #axis.lim = c(0.5,3),
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
    } else if (party_or_admin == "PARTY" & judges == TRUE & RESL == TRUE){
    
    # fit model with all plaintiff types
    if(judge_pv == "prez_party"){
      log_wl_model <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
          ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
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
    
    # fit model with only waste and pollution (WP) focus
    if(judge_pv == "prez_party"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          party.affiliation.of.president,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "dime"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          imputed.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          jcs.score.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          jcs.cfscore.cf,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          pres.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          pres.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          senscore.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          senscore.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          state.delegation.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model_WP <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          state.delegation.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Waste & Pollution"
          ),
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # collect tidy results
    log_wl_model_WP_tidy <- tidy_results(log_wl_model_WP, "Waste and Pollution Conflicts", judges = judges, judge_pv = judge_pv)
    
    # fit model with only conservation (CON) focus
    if(judge_pv == "prez_party"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          party.affiliation.of.president,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "dime"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          imputed.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_dw"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          jcs.score.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "jcs_cf"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          jcs.cfscore.cf,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dw"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          pres.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "prez_dime"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          pres.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dw"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          senscore.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "sen_dime"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          senscore.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dw"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          state.delegation.dw,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else if(judge_pv == "del_dime"){
      log_wl_model_CON <- glm(
        PLT_wl ~ dplyr::lead(Prez, n = 0) +
          #prez_name + 
          yr_file +
          PLT_typ2 +
          REGION +
          #ooc_mc1 +
          gender +
          #age_at_term +
          green_gen +
          state.delegation.dime.cfscore,
        data = reg_df %>%
          filter(
            ooc_mc1 == "Conservation"
          ),
        family = 'binomial'
      )
    } else {
      stop(cat('Cannot tell which inidcator of judicial ideology to model. Did you set judge_pv to an appropriate value? Options include "prez_party", "dime", "jcs_dw", "jcs_cf", "prez_dw", "prez_dime", "sen_dw", "sen_dime", "del_dw", and "del_dime".'))
    }
    
    # collect tidy results
    log_wl_model_CON_tidy <- tidy_results(log_wl_model_CON, "Conservation Conflicts", judges = judges, judge_pv = judge_pv)
    
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
        "Green_gengg", "Green Generation (1935-1950)"
      ) %>%
      str_replace_all(
        "Green_genpg", "Pre-Green Generation (Pre-1935)"
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
      ) %>%
      str_replace_all(
        "Ooc_mc1waste & Pollution", "Waste & Pollution"
      ) %>%
      str_replace_all(
        "Ooc_mc1other Topic", "Other Topic"
      ) %>%
      str_replace_all(
        "Ooc_mc1conservation", "Conservation"
      ) %>%
      str_replace_all(
        "Ooc_mc1energy & Mineral Resources", "Energy & Mineral Resources"
      )
    
    # remove Plains
    axis_labels_list <- axis_labels_list[axis_labels_list != "Plains"]
    
    # plot model results with sjPlot
    all_models <- plot_models(
      log_wl_model, 
      log_wl_model_fed, log_wl_model_NGO, log_wl_model_BIZ,
      log_wl_model_WP, log_wl_model_CON,
      #grid = TRUE,
      show.values = TRUE,
      #axis.lim = c(0.5,3),
      #rm.terms = c("REGIONPlains"), # we remove the plains region from the 
      # forest plots because, particularly for the ENGO model, the error bars 
      # are off the charts b/c there are so few observations!
      colors = "viridis",
      value.size = 2.5,
      spacing = 0.65,
      legend.title = "Model",
      vline.color = "grey",
      title = NULL,
      axis.labels = axis_labels_list,
      m.labels = c("All Plaintiffs","Federal Government Only", "ENGOs Only", "Firms Only", "Waste and Pollution", "Conservation")
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
    
  } else if (party_or_admin == "ADMIN" & judges == TRUE & RESL == FALSE) {
    
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
      #axis.lim = c(0.5,3),
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
  } else if(party_or_admin == "PARTY" & judges != TRUE & RESL == FALSE){
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
      #axis.lim = c(0.5,3),
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
    
  } else if (party_or_admin == "ADMIN" & judges != TRUE & RESL == FALSE) {
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
      #axis.lim = c(0.5,3),
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
      stop(cat('Cannot tell which model to run. Did you set party_or_admin = "PARTY" or "ADMIN", judges = TRUE or FALSE, and RESL as TRUE or FALSE?'))
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
  
  if(RESL == TRUE){
    # join WP and CON dfs as well
    all_models_tidy <- left_join(
      all_models_tidy,
      log_wl_model_CON_tidy, by = "var_name_pretty"
    )
    
    all_models_tidy <- left_join(
      all_models_tidy,
      log_wl_model_WP_tidy, by = "var_name_pretty"
    )
    
    # specify file name/path for resl output
    just_ideology_path = "regressions/ideology_OR_tables/ideology_OR_resl.csv"
  } else {
    # specify path for non-resl output
    just_ideology_path = "regressions/ideology_OR_tables/ideology_OR_fjc.csv"
  }
  
  # write out just ideology coeff results (as odds ratios)
  # list of judge ideology variable names
  ideology_vars <- c("Democratic|Republican|DIME Score|JCS Score|President Score|Senate Del|State Del")
  
  # write out assembled judge ideology variables 
  
  # first, assemble and transform variables of interest
  just_judge_ideology <- all_models_tidy %>%
    filter(
      str_detect(var_name_pretty,ideology_vars),
      !str_detect(var_name_pretty, "Appointing President's Party"),
      !str_detect(var_name_pretty, "- stderr")
    ) %>%
    rowwise() %>%
    mutate(
      var_name_pretty = str_remove(var_name_pretty, " - est"),
      # extract levels of significance from coeff.
      full_sig = str_extract_all(`Full Model`, "\\*{1,3}|†"),
      engo_sig = str_extract_all(`ENGO Plaintiffs`, "\\*{1,3}|†"),
      fed_sig = str_extract_all(`Federal Plaintiffs`, "\\*{1,3}|†"),
      biz_sig = str_extract_all(`Firm Plaintiffs`, "\\*{1,3}|†"),
      # now remove signifiers of significance
      `Full Model` = str_remove_all(`Full Model`, "\\*{1,3}|†"),
      `ENGO Plaintiffs` = str_remove_all(`ENGO Plaintiffs`, "\\*{1,3}|†"),
      `Federal Plaintiffs` = str_remove_all(`Federal Plaintiffs`, "\\*{1,3}|†"),
      `Firm Plaintiffs` = str_remove_all(`Firm Plaintiffs`, "\\*{1,3}|†"),
      # now make vars numeric
      `Full Model` = as.numeric(`Full Model`),
      `ENGO Plaintiffs` = as.numeric(`ENGO Plaintiffs`),
      `Federal Plaintiffs` = as.numeric(`Federal Plaintiffs`),
      `Firm Plaintiffs` = as.numeric(`Firm Plaintiffs`),
      # now exponentiate (for odds ratios)
      `Full Model` = round(exp(`Full Model`),4),
      `ENGO Plaintiffs` = round(exp(`ENGO Plaintiffs`),4),
      `Federal Plaintiffs` = round(exp(`Federal Plaintiffs`),4),
      `Firm Plaintiffs` = round(exp(`Firm Plaintiffs`),4),
      # get rid of character(0) values introduced above (turn them into NAs)
      full_sig = ifelse(
        length(full_sig) == 0, NA, full_sig
      ),
      engo_sig = ifelse(
        length(engo_sig) == 0, NA, engo_sig
      ),
      fed_sig = ifelse(
        length(fed_sig) == 0, NA, fed_sig
      ),
      biz_sig = ifelse(
        length(biz_sig) == 0, NA, biz_sig
      ),
      # replace NAs with ""
      full_sig = case_when(
        is.na(full_sig) ~ "",
        TRUE ~ full_sig
      ),
      engo_sig = case_when(
        is.na(engo_sig) ~ "",
        TRUE ~ engo_sig
      ),
      fed_sig = case_when(
        is.na(fed_sig) ~ "",
        TRUE ~ fed_sig
      ),
      biz_sig = case_when(
        is.na(biz_sig) ~ "",
        TRUE ~ biz_sig
      ),
      # now bring significance indicators back
      `Full Model` = str_c(`Full Model`,full_sig),
      `ENGO Plaintiffs` = str_c(`ENGO Plaintiffs`,engo_sig),
      `Federal Plaintiffs` = str_c(`Federal Plaintiffs`,fed_sig),
      `Firm Plaintiffs` = str_c(`Firm Plaintiffs`, biz_sig)
    ) %>%
    # drop significance columns
    select(
      -c(full_sig, engo_sig, fed_sig, biz_sig)
    ) %>%
    ungroup() %>%
    {
      if(RESL == TRUE){
        rowwise(.,) %>%
          mutate(
            .,
            # extract levels of significance from coeff.
            con_sig = str_extract_all(`Conservation Conflicts`, "\\*{1,3}|†"),
            wp_sig = str_extract_all(`Waste and Pollution Conflicts`, "\\*{1,3}|†"),
            # now remove signifiers of significance
            `Conservation Conflicts` = str_remove_all(`Conservation Conflicts`, "\\*{1,3}|†"),
            `Waste and Pollution Conflicts` = str_remove_all(`Waste and Pollution Conflicts`, "\\*{1,3}|†"),
            # now make vars numeric
            `Conservation Conflicts` = as.numeric(`Conservation Conflicts`),
            `Waste and Pollution Conflicts` = as.numeric(`Waste and Pollution Conflicts`),
            # now exponentiate (for odds ratios)
            `Conservation Conflicts` = round(exp(`Conservation Conflicts`),4),
            `Waste and Pollution Conflicts` = round(exp(`Waste and Pollution Conflicts`),4),
            # get rid of character(0) values introduced above (turn them into NAs)
            con_sig = ifelse(
              length(con_sig) == 0, NA, con_sig
            ),
            wp_sig = ifelse(
              length(wp_sig) == 0, NA, wp_sig
            ),
            # replace NAs with ""
            con_sig = case_when(
              is.na(con_sig) ~ "",
              TRUE ~ con_sig
            ),
            wp_sig = case_when(
              is.na(wp_sig) ~ "",
              TRUE ~ wp_sig
            ),
            # now bring significance indicators back
            `Conservation Conflicts` = str_c(`Conservation Conflicts`,con_sig),
            `Waste and Pollution Conflicts` = str_c(`Waste and Pollution Conflicts`,wp_sig)
          ) %>%
          # drop significance columns
          select(
            .,
            -c(con_sig, wp_sig)
          ) %>%
          ungroup(.,)
      } else {
        .
      }
    } %>%
    rename(
      "Indicator of Judicial Ideology" = "var_name_pretty"
    )
  
  # for first ideology variable in list (DIME score of ruling judge), create
  # new csv file. for subsequent variables, append to this file.
  if(judge_pv == "dime"){
    write_csv(
      just_judge_ideology,
      just_ideology_path
    )
  } else {
    write_csv(
      just_judge_ideology,
      just_ideology_path,
      append = TRUE
    )
  }
  
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
    ) %>%
    # get rid of NAs for conservation/ waste and pollution columns if present
    {
      if(RESL == TRUE){
        # get rid of NAs; replace with blanks
        mutate(
          .,
          `Conservation Conflicts` = case_when(
            is.na(`Conservation Conflicts`) ~ "",
            TRUE ~ `Conservation Conflicts`
          ),
          `Waste and Pollution Conflicts` = case_when(
            is.na(`Waste and Pollution Conflicts`) ~ "",
            TRUE ~ `Waste and Pollution Conflicts`
          )
        )
      } else {
        .
      }
      
    }
  
  ##write out tidy regression results as csv
  write_csv(
    all_models_tidy,
    file = str_c("regressions/table_results/logit_model_table", save_name, ".csv")
  )
  
# the end.
  
}