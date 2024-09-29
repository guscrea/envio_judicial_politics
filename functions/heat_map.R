# heat map function ####
# function for counting plaintiff-type by defendant type frequencies for all
# cases combined; plot heat map of frequency combinations; allows selection of
# particular dispositions

# inputs
# df = dataframe used as input to create matrix. This input may be
#      the filtering function used to winnow down data to include
#      a particular subset of the data.
# yr_start = starting year used to build matrix (yr_file)
# yr_end = ending year to used to build matrix (yr_file)
# l_typs = vector of litigant types to be included in matrix
# title = title of matrix plot
# court_level = if "D", label with district-level axis labels. Else 
#       (e.g., "A"), label with appellate-level axis labels.

# for testing
# df = simp_filt(
#    df = "fjc_e",
#    jud = TRUE,
#    disp_drop = NULL,
#    noBP_IMC = TRUE,
#    diag = TRUE
#    )
# yr_start = 1988
# yr_end = 2022
# l_typs = l_typs
# title = "Plaintiff and Defendant Type Combinations - Federal District Courts"
# court_level = "D"

plot_PD_combo_heatmap <- function(df,
                                  yr_start,
                                  yr_end,
                                  l_typs,
                                  title,
                                  court_level){
  # build df for making matrix
  df <- df %>%
    ungroup() %>%
    # filter by years to include
    filter(
      yr_file >= yr_start,
      yr_file <= yr_end
      ) %>%
    mutate(
      # count all the cases in [possibly filtered] df
      tot_cases = n()
    ) %>%
    group_by(PLT_typ,DEF_typ) %>%
    mutate(
      PD_freq = n(),
      PD_pct = round(PD_freq/tot_cases*100,2)
    ) %>% select(
      PLT_typ, DEF_typ, PD_freq,PD_pct, tot_cases
    ) %>%
    filter(
      row_number() == 1
    ) %>%
    ungroup() %>%
    filter(
    # retain only specified plaintiff and defendant types
      PLT_typ %in% l_typs,
      DEF_typ %in% l_typs
    ) %>%
    # rename plaintiff types to easy-to-read formats
    mutate(
      PLT_typ = case_when(
        PLT_typ == "BIZ" ~ "Firm & Trade Assn",
        PLT_typ == "FED" ~ "Federal Gov't",
        PLT_typ == "IND" ~ "Individual",
        PLT_typ == "LOC" ~ "Local Gov't",
        PLT_typ == "NGO" ~ "Enviro. NGOs",
        PLT_typ == "STA" ~ "State Gov't",
        PLT_typ == "CIVIC" ~ "Civic Orgs",
        PLT_typ == "NGO_O" ~ "Other NGOs",
        PLT_typ == "OTHER" ~ "Other Orgs",
        PLT_typ == "TRIBE" ~ "Tribes",
        TRUE ~ PLT_typ
      ),
      DEF_typ = case_when(
        DEF_typ == "BIZ" ~ "Firm & Trade Assn",
        DEF_typ == "FED" ~ "Federal Gov't",
        DEF_typ == "IND" ~ "Individual",
        DEF_typ == "LOC" ~ "Local Gov't",
        DEF_typ == "NGO" ~ "Enviro. NGOs",
        DEF_typ == "STA" ~ "State Gov't",
        DEF_typ == "CIVIC" ~ "Civic Orgs",
        DEF_typ == "NGO_O" ~ "Other NGOs",
        DEF_typ == "OTHER" ~ "Other Orgs",
        DEF_typ == "TRIBE" ~ "Tribes",
        TRUE ~ DEF_typ
      )
    ) %>%
    # reorder factors for plot
    mutate(
      DEF_typ = factor(
        DEF_typ,
        levels = sort(unique(DEF_typ), decreasing = TRUE),
        labels = sort(unique(DEF_typ), decreasing = TRUE)
      )
    )
  
    # build plot
  df %>%
    ggplot() +
    geom_tile(
      aes(
        x = PLT_typ,
        y = DEF_typ,
        fill = log(PD_pct)
      )
    ) +
    geom_text(
      aes(
        x = PLT_typ,
        y = DEF_typ,
        label = PD_pct
      ),
      size.unit = "pt",
      size = 7
    ) + 
    scale_fill_viridis_c(
      option="mako",
      breaks = c(-4.605, -2.30, 0, 2.302, 3.688),
      labels = c(0.01, 0.1, 1, 10, 40)
      #limits = c(-4.60517,4.0943)
    ) +
    scale_x_discrete(position = "top") +
    {
      if(court_level == "D"){
        labs(
          title = title,
          subtitle = str_c("n = ", prettyNum(first(df$tot_cases), big.mark = ","), sep = ""),
          x = "Plaintiff Types",
          y = "Defendant Types",
          fill = "Frequency\n(% of all cases)"
        )
      } else{
        labs(
          title = title,
          subtitle = str_c("n = ", prettyNum(first(df$tot_cases), big.mark = ","), sep = ""),
          x = "Appellant Types",
          y = "Appellee Types",
          fill = "Frequency\n(% of all cases)"
        )
      }
    } +
    theme_linedraw() +
    theme(plot.title = element_text(size = 7),
          plot.subtitle = element_text(size = 7),
          text = element_text(size = 7)
          )
      
  }