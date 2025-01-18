library(tidyverse)
library(tidytext)
library(striprtf)
library(patchwork)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd)
library(geometry)
library(stm)
library(igraph)

# read meta-data (RESL data) in and build file paths for decisions ####

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

# pre-process decisions ####

# call function to pre-process decision texts into tidy foramt
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
    is.na(text)
  )

# write out missing decisions
write_csv(
  ENGO_non_con_dec_missing,
  "data/decisions_to_investigate/ENGO_PLT_non-conservation_missing.csv"
)


# preliminary (word frequency count) analysis #####

# transform decision text into tidy format


# make text of decision tidy
ENGO_non_con_dec_resl_tdy <- ENGO_non_con_dec_resl %>%
  ungroup() %>%
  unnest_tokens(word, text)

# get rid of stop words
data(stop_words)
ENGO_non_con_dec_resl_tdy <- ENGO_non_con_dec_resl_tdy %>%
  anti_join(stop_words, by = join_by(word))

# stem words; remove any remaining punctuation
ENGO_non_con_dec_resl_tdy <- ENGO_non_con_dec_resl_tdy %>%
  mutate(
    word = wordStem(word),
    word = str_remove_all(word, "[[:punct:]]")
  )

# count words in each decision
ENGO_non_con_dec_resl_tdy <- ENGO_non_con_dec_resl_tdy %>%
  group_by(
    file_name, word
  ) %>%
  mutate(
    n_des = n()
  ) %>%
  ungroup()

ENGO_non_con_dec_resl_tdy_anal <- ENGO_non_con_dec_resl_tdy %>%
  select(
    word,
    n_des,
    file_name,
    ID,
    case_name,
    url,
    cite,
    DOCKET,
    court,
    TERMDATE,
    PLT,
    PLT_typ,
    PLT_typ_all,
    DEF,
    DEF_typ,
    DEF_typ_all,
    judge,
    outcome,
    agy,
    statute,
    species,
    judge_clean,
    nid,
    jid,
    president.name,
    party.affiliation.of.president,
    imputed.dime.cfscore,
    jcs.score.dw,
    pres.dw,
    yr_file,
    FILEDATE,
    REGION,
    age_at_term,
    green_gen
  ) %>%
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
ENGO_non_con_dec_resl_tdy_anal[ENGO_non_con_dec_resl_tdy_anal == -Inf] <- NA

ENGO_non_con_dec_resl_tdy_anal <- ENGO_non_con_dec_resl_tdy_anal %>%
  mutate(
    L_norm = n_L/num_desc_L,
    C_norm = n_C/num_desc_C,
    L_to_C = L_norm/C_norm, #normalize values for the number of decisions filed by each type of judge
    C_to_L = C_norm/L_norm,
    L_label = str_c(round(L_norm,2), " to ", round(C_norm,2)),
    C_label = str_c(round(C_norm,2), " to ", round(L_norm,2))
  ) 


# capture uncommonly frequent words among L decisions
words_dis_prop_L <- ENGO_non_con_dec_resl_tdy_anal %>%
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
words_dis_prop_C <- ENGO_non_con_dec_resl_tdy_anal %>%
  # capture uncommonly frequent words in L or C
  filter(
    C_norm > 2 &
      C_to_L > 1.1305
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
  ggplot2::annotate(
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
  ggplot2::annotate(
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
  



# implementing structural topic model (stm) ####

# first, pare down to text, minimal metadata: year of termination, judge
# ideology, document ID, substantive focus

ENGO_non_con_stm <- ENGO_non_con_dec_resl %>%
  select(
    ID, yr_file, ooc_mc1, imputed.dime.cfscore, text
  ) %>%
  # make simple liberal-conservative binary
  mutate(
    dime_d = case_when(
      imputed.dime.cfscore <= 0 ~ "L",
      imputed.dime.cfscore > 0 ~ "C"
      ),
    dime_d = as.factor(dime_d)
    ) %>%
  #remove decisions with no text (-31) and no ideology (-12) from 533 starting
  filter(
    !is.na(text), #
    !is.na(imputed.dime.cfscore)
  ) %>%
  #capture intro of decision (first 1,000 characters)
  mutate(
    text_intro = str_sub(text, 1, 1000)
  ) %>%
  ungroup()

# prepare data for stm model

# build document -term matrix with meta data
ENGO_non_con_stm_processed <- textProcessor(ENGO_non_con_stm$text, metadata = ENGO_non_con_stm)

# examine nubmer of words that would be removed at various thresholds for inclusion
plotRemoved(ENGO_non_con_stm_processed$documents, lower.thresh = seq(1, 200, by = 100))

# not obvious that any words need to be removed... (make lower.thresh = 0)

# drop uncommon words below threshold
ENGO_non_con_stm_processed_prep <- prepDocuments(
  ENGO_non_con_stm_processed$documents,
  ENGO_non_con_stm_processed$vocab,
  ENGO_non_con_stm_processed$meta,
  lower.thresh = 2)

# get a sense for how many topics
# here we iterate through models from 5 to 50 topics. This takes a long time!
storage <- searchK(ENGO_non_con_stm_processed_prep$documents,
                   ENGO_non_con_stm_processed_prep$vocab,
                   K = seq(5,50,1),
                   prevalence =~ s(imputed.dime.cfscore),
                   data =ENGO_non_con_stm_processed_prep$meta
                   )

# plot exclusivity v. semantic coherence
findK <- storage$results %>%
  mutate(
    semcoh = as.numeric(semcoh),
    exclus = as.numeric(exclus),
    K = as.numeric(K)
  )

findK %>%
  ggplot(
    aes(
      x = semcoh,
      y = exclus,
      color = K,
    )
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_text(
    aes(
      label = K
    )
  ) +
  scale_color_viridis_c() +
  labs(
    x = "Semantic Coherence",
    y = "Exclusivity",
    color = "K (num. topics)"
  ) +
  theme_linedraw()

ggsave(
  "excl_v_seman_coher_thresh_2_K_1_to_50_imputed_dime.png",
  units = "mm",
  width = 300,
  height =  200,
  path = "Figures"
)

# notes:

# FOR 1 covariate, dime_d:

# for prepDocuments lower.thresh = 0, K = 5 to 50, Execl v. Sem. Coher plot
# suggests 24 (higher coherence) or maybe 26 (higher exclusivity) topics. There
# are diminishing returns after 33-34 topics.

# for prepDocuments lower.thresh = 2, K = 5 to 50, Execl v. Sem. Coher plot
# suggests 16 (higher coherence) or maybe 17 (higher exclusivity) topics. There
# are diminishing returns after 22 topics.



# fit stm 
# note: particularly with K = 0 and init.type = "Spectral", this
# takes several minutes. This also yields 105 topics - WAY too many!
ENGO_non_con_stm_fit <- stm(documents = ENGO_non_con_stm_processed_prep$documents,
                            vocab = ENGO_non_con_stm_processed_prep$vocab,
                            K = 16, # with init.type = "Spectral", K = 0 estimates an "optimal" number of topics...
                            #prevalence =~ dime_d + s(yr_file),
                            prevalence =~ s(imputed.dime.cfscore),
                            max.em.its = 75,
                            data = ENGO_non_con_stm_processed_prep$meta,
                            init.type = "Spectral"
                            )

# explore stm results ####

# look at corpus-level topic prevalence
plot(ENGO_non_con_stm_fit, type = "summary", xlim = c(0, 0.3))

# look at five topics:
labelTopics(
  ENGO_non_con_stm_fit,
  c(7,13,15,5,16,11)
)

# as an example, look more closely at topic 13
thoughts14 <- findThoughts(ENGO_non_con_stm_fit,
                           texts = ENGO_non_con_stm$text_intro,
                           n = 5,
                           topics = 13)$docs[[1]]
plotQuote(thoughts14, width = 150, main = "Topic 13 - Lands Conflicts")


# examine topic-meta-data relation
ENGO_non_con_stm_processed_prep$meta$dime_d <- as.factor(ENGO_non_con_stm_processed_prep$meta$dime_d)

prep <- estimateEffect(1:16 ~ dime_d,
                       ENGO_non_con_stm_fit,
                       metadata = ENGO_non_con_stm_processed_prep$meta,
                       uncertainty = "Global")
# look at model resultss for each topic
lapply(
  seq(1,16,1),
  summary,
  object = prep
  )

# note:  in lower.thresh = 2, K = 16 model, topic 9 is
# the only one that appears to be significant with respect
# to dime_d. 

# look at topic 9:
labelTopics(
  ENGO_non_con_stm_fit,
  c(9)
)

# plot difference of moving from conservative to liberal decision for each topic
# Note again, only topic 9 is significant
plot(prep,
     covariate = "dime_d",
     #topics = c(5,11,6,16,9),
     #topics = c(8,12,7),
     model = ENGO_non_con_stm_fit,
     method = "difference",
     cov.value1 = "C", # check!
     cov.value2 = "L",
     xlab = "More Conservative ... More Liberal",
     main = "Effect of Conservative vs. Liberal",
     xlim = c(-0.1, 0.1),
     labeltype = "prob" 
     #labeltype = "custom", custom.labels = seq(1,16,1)
)

# network of topics (means of visualizing which topics appear together)
mod.out.corr <- topicCorr(ENGO_non_con_stm_fit)
plot(mod.out.corr)
