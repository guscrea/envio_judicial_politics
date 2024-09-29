# simple filter for restricting observations to particular ranges,etc.

# df = data frame to use as input
# jud = if TRUE, include only cases that have been settled or reached
#     judgement
# disp_drop = additional dispositions to DROP from data frame. Note that
#     of jud == TRUE, DISP  0, 1, 10, 11, and -8 have already been dropped.
#     See FJC codebooks for more info on disposition codes. NULL if all
#     DISPS (after jud) are to be retained.
# noBP_IMC = if TRUE, drop observations assocaited with BP oil spill in 
#     LA Eastern district (District 3L) and IMC Global, Inc. in South
#     Carolina (District 20)
# diag = if FALSE, drop intra-type suit observations (e.g., FED suing FED, 
#     BIZ suing BIZ, etc.)

simp_filt <- function(df, jud, disp_drop, noBP_IMC, diag){
  df <- get(df)
  df <- df %>%
    ungroup() %>%
    # filter by whether case is resolved (settled or judgement)
    {
      if(jud == TRUE)
        filter(., jud_or_set == 1)
      else
        .
      } %>%
    # filter out additional dispositions
    {
      if(is.vector(disp_drop) == T)
        filter(
          !(DISP %in% disp_drop)
          )
        else
          .
      } %>%
    # filter out (or not) BP and IMC global cases
    {
      if(noBP_IMC == TRUE)
        filter(
          .,
          !(DISTRICT == "3L" & (str_detect(PLT,"BP")==T | str_detect(DEF,"BP")==T)), # drop BP cases in LA
          !(DISTRICT == "20" & str_detect(DEF,"IMC")==T) # drop IMC cases in SC
          )
      else
        .
      } %>%
    # filter out intra-type suits (diag)
    {
      if(diag == FALSE)
        filter(
          .,
          PLT_typ != DEF_typ
        )
      else
        .
      } %>%
    # drop cases where the plaintiff type or defendant type is unknown
    # based on prior coding and cleaning, there should be no NAs!
    filter(
      .,
      !is.na(PLT_typ),
      !is.na(DEF_typ)
      )
}