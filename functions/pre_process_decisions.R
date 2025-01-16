# function for reading in and cleaning text from judicial decisions
# downloaded from LexisNexis

# inputs
# folder = in nlp directory to look in (include trailing slash in name!)
# file_name = appropriately formatted file name for decison text, located in
# folder above.
# i = starting iteration number

# for testing
i = 163
file_name = files[i]
folder = "ENGO_Non_Conservation_Decisions/"


decison_preprocess <- function(folder,file_name,i){
  
  # note: code has to be able to handle file names that do not exist in the
  # directory of decision texts, in case those decisions are not present for some
  # reason. It's also important to track such decisions, to add them if they
  # were inappropriately missed for some reason, and to double check that the
  # script cannot open them just because of an error in, e.g., constructing the
  # file name.
  
  # construct file path
  file_path = str_c(
    "data/nlp/",
    folder,
    file_name
    )
  
  # print index number, i, for counting
  message(
    str_c("Pre-processing decision # ",i)
    )
  
  # check to see if file path exists; warn if not, continue if so.
  
   if(!file.exists(file_path)) { # if file path does not exist
     message("Decision text file could not be openend.")
     message(str_c("file name: ", file_name))
     message("Recording attempted file name and moving to next file.")
     
     # build empty data frame with same components as data
     df_elements <- list(
       line = NA,
       word = NA,
       n_des = NA,
       file_name = file_name
     )
     
     text_op <- as.data.frame(df_elements)
     
   } else { # if file path does exist, pre-process text!
     
     # get the full text
     text <- read_rtf(file_path)
     
     # the next section of code identifies the start of the actual written
     # opinion so that we can capture just that, and excluide all the leading
     # head notes that LexisNexis adds to the decision document.
  
    # find the first line that contains the word "Opinion" but do not include "Opinion "
    # (with a trailing space) or "Advisory Opinions."
    lines_op <- first(
      which(
        str_detect(text, "Opinion") &
          !str_detect(text, "Opinion ") &
          !str_detect(text, "Advisory Opinions")
        )
    )
    
    # find blank lines
    lines_blank <- which(!nzchar(text))
    
    # function for comparing line numbers. We compare the line numbers of the
    # lines where "Opinion" appears (and "Opinion " with a trailing space and
    # "Advisory Opinions" do not appear) to the line numbers that are blank; we
    # assume the opinion starts following the first line that contains "Opinion"
    # (and does not contain "Opinion " or "Advisory Opinion") and is also
    # followed by a blank line, i.e. where the line number of the "opinion" line
    # + 1 = the line number of the blank line.
    sames_lines <- function(l1,l2){
      if(l1+1 == l2){
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    # find first blank line that follows a line that contains the word "Opinion"
    index_start_op <- lapply(
      lines_blank,
      sames_lines,
      l1 = lines_op
    )
    # get get index of TRUE result from above
    index_start_op <- str_which(index_start_op,"TRUE")
    
    # get line number from lines_blank based on index_start_op
    op_start <- lines_blank[index_start_op]
    
    # drop text before op_start (maintain only opinion after start line)
    text_op <- text[op_start:length(text)]
    
    # now we process the text of just the opinion into a tidy format.
    
    # make text into df
    text_op <- tibble(
      line = 1:length(text_op),
      text = text_op
    )
    
    # make text of decision tidy
    text_op <- text_op %>%
      unnest_tokens(word, text)
    
    # get rid of stop words
    data(stop_words)
    text_op <- text_op %>%
      anti_join(stop_words, by = join_by(word))
    
    # stem words; remove any remaining punctuation
    text_op <- text_op %>%
      mutate(
        word = wordStem(word),
        word = str_remove_all(word, "[[:punct:]]")
      )
    
    # count words; add file name as identifier to link to RESL data
    text_op <- text_op %>%
      group_by(
        word
      ) %>%
      mutate(
        n_des = n()
      ) %>%
      ungroup() %>%
      mutate(
        file_name = file_name
      )
    
    #return tidy decision text df
    return(text_op)
   }
}