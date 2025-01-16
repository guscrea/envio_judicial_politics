library(tidyverse)
library(tidytext)
library(SnowballC)
library(striprtf)
library(patchwork)

# read in list of ENGO-plaintiff non-conservation cases; build file name to match 
# naming convention used by LexisNexis in downloaded decision files
ENGO_non_con <- read_csv(
  "data/decisions_to_investigate/resl_non-conservation_ENGO_plt_decisions.csv"
  ) %>%
  mutate(
    file_name =
      str_c(
        str_replace_all(case_name,",","_"),
        "_ ",
        cite,
        ".RTF"
      ),
    file_name = str_replace_all(file_name,"'","_"),
    file_name = str_replace_all(file_name,"&","_"),
    file_name = str_replace_all(
      file_name,
      " ",
      "\\\ "
    )
  )

# get list of all unique file names
files <- unique(ENGO_non_con$file_name)
#files <- files[163]

# for testing, truncate list of file name to first 20
#files <- files[1:20]

# read in and process text for decisions ####

# call function to pre-process decision texts
source("functions/pre_process_decisions.R")

# loop through list of decision texts; pre_process text for each. This geneates a list of
# data frames, each dataframe with the pre-processed tidy text from each decision.
decisions <- mapply(
  decison_preprocess, # function for pre-processing text
  files, # list of file names, from above
  i = seq(1,length(files),1),
  folder = "ENGO_Non_Conservation_Decisions/",
  SIMPLIFY = FALSE
  )

# compile decision documents into single dataframe
ENGO_non_con_dec <- dplyr::bind_rows(decisions)

# join resl data to decisions text data
ENGO_non_con_dec_resl <- left_join(
  ENGO_non_con_dec,
  ENGO_non_con,
  by = "file_name"
  )

# get list of missing decisions
ENGO_non_con_dec_missing <- ENGO_non_con_dec_resl %>%
  filter(
    is.na(line)
  )

# write out missing decisions
write_csv(
  ENGO_non_con_dec_missing,
  "data/decisions_to_investigate/ENGO_PLT_non-conservation_missing.csv"
)


# preliminary analysis #####
ENGO_non_con_dec_resl_anal <- ENGO_non_con_dec_resl %>%
  #select(
    # word,
    # n_des,
    # file_name,
    # ID,
    # case_name,
    # url,
    # cite,
    # DOCKET,
    # court,
    # TERMDATE,
    # PLT,
    # PLT_typ,
    # PLT_typ_all,
    # DEF,
    # DEF_typ,
    # DEF_typ_all,
    # judge,
    # outcome,
    # agy,
    # statute,
    # species,
    # judge_clean,
    # nid,
    # jid,
    # president.name,
    # party.affiliation.of.president,
    # imputed.dime.cfscore,
    # jcs.score.dw,
    # pres.dw,
    # yr_file,
    # FILEDATE,
    # REGION,
    # age_at_term,
    # green_gen
  #) %>%
  mutate(
    dime_d = case_when(
      imputed.dime.cfscore <= 0 ~ "L",
      imputed.dime.cfscore > 0 ~ "C"
    )
  ) %>%
  group_by(
    dime_d
  ) %>%
  mutate(
    num_desc = length(unique(file_name))
  ) %>%
  filter(
    !is.na(word) &
    !is.na(dime_d)
  ) %>%
  group_by(
    dime_d, word
  ) %>%
  mutate(
    n_dime_d = n()
  ) %>%
  filter(
    row_number() == 1
  ) %>%
  group_by(
    word
  ) %>%
  # now pivot wider by dime_d for word counts, so the same word counts for L
  # and C judges are presented on the same row and can be manipulated easily
  # (ratios taken).
  pivot_wider(
    names_from = dime_d,
    values_from = c(n_dime_d, num_desc)
  ) %>%
  mutate(
    # first, fill in NA L and C values for words
    n_dime_d_C = first(max(n_dime_d_C, na.rm = T)),
    n_dime_d_L = first(max(n_dime_d_L, na.rm = T)),
    num_desc_C = first(max(num_desc_C, na.rm = T)),
    num_desc_L = first(max(num_desc_L, na.rm = T)),
  ) %>%
  select(
    word,
    n_dime_d_C,
    n_dime_d_L,
    num_desc_C,
    num_desc_L
  ) %>%
  # keep just one row per word
  filter(
    row_number() == 1
  ) %>%
  rename(
    "n_C" = "n_dime_d_C",
    "n_L" = "n_dime_d_L"
  ) %>%
  ungroup()

# replace all -Inf with NA
ENGO_non_con_dec_resl_anal[ENGO_non_con_dec_resl_anal == -Inf] <- NA

ENGO_non_con_dec_resl_anal <- ENGO_non_con_dec_resl_anal %>%
  mutate(
    L_norm = n_L/num_desc_L,
    C_norm = n_C/num_desc_C,
    L_to_C = L_norm/C_norm, #normalize values for the number of decisions filed by each type of judge
    C_to_L = C_norm/L_norm,
    L_label = str_c(round(L_norm,2), " to ", round(C_norm,2)),
    C_label = str_c(round(C_norm,2), " to ", round(L_norm,2))
  ) 


# capture uncommonly frequent words among L decisions
words_dis_prop_L <- ENGO_non_con_dec_resl_anal %>%
  filter(
    L_norm > 2 &
      L_to_C > 1.3
  ) %>%
  arrange(
    desc(L_to_C)
  ) %>%
  mutate(
    rank = row_number(),
    rank = factor(
      x = rank,
      labels = word
    ),
    ideology = "Decisions by Liberal-Leaning Judges"
  )

# capture uncommonly frequent words among C judge decisions
words_dis_prop_C <- ENGO_non_con_dec_resl_anal %>%
  # capture uncommonly frequent words in L or C
  filter(
    C_norm > 2 &
      C_to_L > 1.1
  ) %>%
  arrange(
    desc(C_to_L)
  ) %>%
  mutate(
    rank = row_number(),
    rank = factor(
      x = rank,
      labels = word
      ),
    ideology = "Decisions by Conservative-Leaning Judges"
  )

# plot common words disproportionately common in Liberal-leaning decisions
L_word_freq_plot <- words_dis_prop_L %>%
  ggplot(
    aes(
      y = reorder_within(rank, by = L_to_C, within = num_desc_L),
      x = L_to_C
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(
      label = L_label
    ),
    hjust = -.2
  ) + 
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = -Inf,
    yend = Inf,
    color = "red",
    #linetype = 11,
    alpha = .7
  ) +
  scale_y_reordered() + # remove facet name appending
  labs(
    x = "Ratio of word freq. in Liberal-to-Conservative Judge Opinions",
    y = "Word",
    #caption = str_c("Note: labeled valus are normalized word frequencies, adjusted by number of decisions by ideological category\n",
    #                "(",
    #                first(words_dis_prop_L$num_desc_L),
    #                " decisions by liberal-leaning judges and ",
    #                first(words_dis_prop_L$num_desc_C),
    #                " decisions by conservative-leaning judges.)"
    #)
  ) + 
  facet_wrap(
    vars(ideology)
  ) +
  coord_cartesian(xlim = c(0, 4.5)) +
  theme_linedraw()

# plot common words disproportionately common in conservative-leaning decisions
C_word_freq_plot <- words_dis_prop_C %>%
  ggplot(
    aes(
      y = reorder_within(rank, by = C_to_L, within = num_desc_C),
      x = C_to_L
    )
  ) +
  geom_bar(
    stat = "identity"
  ) +
  geom_text(
    aes(
      label = C_label
    ),
    hjust = -.2
  ) + 
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = -Inf,
    yend = Inf,
    color = "red",
    #linetype = 11,
    alpha = .7
  ) +
  scale_y_reordered() + # remove facet name appending
  labs(
    x = "Ratio of word freq. in Conservative-to-Liberal Judge Opinions",
    y = "Word"
  ) + 
  facet_wrap(
    vars(ideology)
  ) +
  coord_cartesian(xlim = c(0, 4.5)) +
  theme_linedraw()

L_word_freq_plot / C_word_freq_plot +
  plot_annotation(
    tag_levels = 'A',
    caption = str_c("Note: labeled valus are normalized word frequencies, adjusted by number of decisions by ideological category.\n",
                    "(",
                    first(words_dis_prop_L$num_desc_L),
                    " decisions by liberal-leaning judges and ",
                    first(words_dis_prop_L$num_desc_C),
                    " decisions by conservative-leaning judges.)"
    )
    ) & 
  theme(plot.tag = element_text(face = 'bold'))

ggsave(
  "disproportionately_common_words_in_L_and_C_decisions.png",
  units = "mm",
  width = 300,
  height =  400,
  path = "Figures"
)
  


