#############################################################################
######### Dataset: mentions of West, Western countries, Europe ###############
#############################################################################

library(tidyverse)
library(readr)
library(quanteda)
library(quanteda.dictionaries) 
library(stringi)
library(openxlsx)

# import manifestos data in CEE
M_CE <- read_csv("manifesos_CEE_full_050526.csv", locale = locale(encoding = "UTF-8")) 

# cleaning
M_CE$text_en <- gsub("Ø", "", M_CE$text_en) #delete Ø
M_CE$text_en <- gsub("Â-", "", M_CE$text_en) #delete Â-
M_CE$text_en <- gsub("ø", "", M_CE$text_en) #delete ø
M_CE$text_en <- gsub("�", "", M_CE$text_en) #delete �
M_CE$text_en <- gsub("", "", M_CE$text_en) #delete 

#function to prepare the text for better sentence splitting

preprocess_manifesto_text <- function(x) {
  x |>
    # basic whitespace cleanup
    str_replace_all("\u00A0", " ") |>
    str_replace_all("[\r\t]", " ") |>
    str_replace_all("\\s+", " ") |>
    
    # protect common abbreviations from sentence splitting
    # e.g. e. g. / i. e.
    str_replace_all("\\be\\.\\s*g\\.", "e.g.") |>
    str_replace_all("\\bi\\.\\s*e\\.", "i.e.") |>
    
    # fix missing spaces after punctuation, except semicolon
    # e.g. following:1.1 -> following: 1.1
    str_replace_all("([:,.!?])(?=\\S)", "\\1 ") |>
    
    # semicolon + capitalized item
    # e.g. "... Regions; Increased requirements..." -> "... Regions. Increased requirements..."
    str_replace_all(
      "[;；]\\s+(?=[A-ZÀ-ÖØ-Þ])",
      ". "
    ) |>
    
    # semicolon + numbered list marker
    # e.g. "; 5) digitises" -> ". 5) digitises"
    str_replace_all(
      "[;；]\\s+(?=\\d+\\)\\s)",
      ". "
    ) |>
    
    # semicolon + hyphen bullet
    # e.g. "; - It has..." -> ". It has..."
    str_replace_all(
      "[;；]\\s*[-–—]\\s*(?=[A-ZÀ-ÖØ-Þ])",
      ". "
    ) |>
    
    # period/colon/exclamation/question mark + hyphen bullet
    # e.g. ". - It has..." -> ". It has..."
    #      "Essential: - to decentralise..." -> "Essential: to decentralise..."
    str_replace_all(
      "([.:!?])\\s*[-–—]\\s*(?=[A-Za-zÀ-ÖØ-öø-ÿ])",
      "\\1 "
    ) |>
    
    # comma + hyphen bullet only when followed by clear list starts
    # e.g. ", - to legitimise" -> ". to legitimise"
    #      ", - Align" -> ". Align"
    str_replace_all(
      ",\\s*[-–—]\\s*(?=(to\\s+|[A-ZÀ-ÖØ-Þ]))",
      ". "
    ) |>
    
    # split before numbered list markers
    # e.g. "5) digitises" / "6) increases"
    str_replace_all(
      "\\s+(?=\\d+\\)\\s)",
      "\n"
    ) |>
    
    # split before numbered headings/list items
    # e.g. "1.1 Fiscal policy"
    str_replace_all(
      "\\s+(?=\\d+(?:\\.\\d+)*\\s+[A-ZÀ-ÖØ-Þ])",
      "\n"
    ) |>
    
    # split before lettered list markers
    # e.g. "a) reduction ... b) abolition"
    str_replace_all(
      "\\s+(?=[a-z]\\)\\s)",
      "\n"
    ) |>
    
    # normalize U. S. / U. S. A.
    str_replace_all("\\bU\\.\\s*S\\.\\s*A\\.\\b", "U.S.A.") |>
    str_replace_all("\\bU\\.\\s*S\\.\\b", "U.S.") |>
    
    # split before hyphen bullets only when they appear at start or after punctuation
    # this avoids splitting ordinary spaced hyphen phrases as much as possible
    str_replace_all(
      "(^|(?<=[.:!?])\\s*)[-–—]\\s*(?=[A-Za-zÀ-ÖØ-öø-ÿ])",
      "\n- "
    ) |>
    
    # clean line breaks
    str_replace_all("[ ]*\\n[ ]*", "\n") |>
    str_replace_all("\\n+", "\n") |>
    str_trim()
}


corp <- corpus(M_CE$text_en) #transform in corpus

docvars(corp, "manifesto_id") <- M_CE$manifesto_id #assign docvars variables
docvars(corp, "party") <- M_CE$party
docvars(corp, "date") <- M_CE$date
docvars(corp, "title") <- M_CE$title
docvars(corp, "id") <- M_CE$id

# make corpus
corp_doc <- corpus_group(corp, groups = manifesto_id)


# 2. Clean the grouped manifesto text
texts_clean <- preprocess_manifesto_text(as.character(corp_doc))

# 3. Rebuild corpus with same document names
corp_clean <- corpus(
  texts_clean,
  docnames = docnames(corp_doc)
)

# 4. Preserve docvars
docvars(corp_clean) <- docvars(corp_doc)

# 5. Sentence tokenize
tokens_sent <- tokens(corp_clean, what = "sentence")

dict <- dictionary(list(
  West = c("\\bwest\\b|\\bwestern\\b"),
  
  Europe = "\\beurop\\w*\\b",
  
  USA = c(
    "\\bUSA\\b|\\bU\\.S\\.A\\.\\b|\\bU\\.S\\.\\b",
    "\\bUnited\\s+States\\b",
    "\\bAmerican\\w*\\b",
    "\\bAmerica\\w*\\b",
    "\\bthe\\s+U\\.S\\.\\b",
    "\\bWashington\\w*\\b",
    "\\bnew\\s+york\\b"
  ),
  
  Canada = c(
    "\\bcanada\\b",
    "\\bcanadian\\w*\\b",
    "\\bottawa\\w*\\b",
    "\\btoronto\\w*\\b",
    "\\bmontreal\\w*\\b",
    "\\bvancouver\\w*\\b"
  ),
  
  Austria = c(
    "\\baustria\\w*\\b",
    "\\baustrian\\w*\\b",
    "\\bvienna\\w*\\b",
    "\\bwien\\b"
  ),
  
  Belgium = c(
    "\\bbelgi\\w*\\b",
    "\\bbelgian\\w*\\b"
  ),
  
  Brussels = "\\bbrussels\\w*\\b",
  
  Cyprus = c(
    "\\bcyprus\\w*\\b",
    "\\bcypriot\\w*\\b",
    "\\bnicosia\\w*\\b"
  ),
  
  Denmark = c(
    "\\bdenmark\\b",
    "\\bdanish\\w*\\b",
    "\\bcopenhagen\\w*\\b"
  ),
  
  Finland = c(
    "\\bfinland\\w*\\b",
    "\\bfinnish\\w*\\b",
    "\\bfinn\\w*\\b",
    "\\bhelsinki\\w*\\b"
  ),
  
  France = c(
    "\\bfrance\\w*\\b",
    "\\bfrench\\w*\\b",
    "\\bparis\\b"
  ),
  
  Germany = c(
    "\\bgermany\\w*\\b",
    "\\bgerman\\w*\\b",
    "\\bberlin\\w*\\b"
  ),
  
  Greece = c(
    "\\bgreece\\w*\\b",
    "\\bgreek\\w*\\b",
    "\\bathens\\w*\\b"
  ),
  
  Ireland = c(
    "\\bireland\\w*\\b",
    "\\birish\\w*\\b",
    "\\bdublin\\w*\\b"
  ),
  
  Italy = c(
    "\\bitaly\\w*\\b",
    "\\bitalian\\w*\\b",
    "\\bitalia\\w*\\b",
    "\\brome\\b"
  ),
  
  Luxembourg = c(
    "\\bluxembourg\\w*\\b",
    "\\bluxemburg\\w*\\b"
  ),
  
  Malta = c(
    "\\bmalta\\w*\\b",
    "\\bmaltese\\w*\\b",
    "\\bvalletta\\w*\\b"
  ),
  
  Netherlands = c(
    "\\bnetherlands\\w*\\b",
    "\\bholland\\w*\\b",
    "\\bdutch\\w*\\b",
    "\\bamsterdam\\w*\\b",
    "\\bthe\\s+hague\\b"
  ),
  
  Portugal = c(
    "\\bportugal\\w*\\b",
    "\\bportuguese\\w*\\b",
    "\\blisbon\\w*\\b",
    "\\blisabon\\w*\\b"
  ),
  
  Spain = c(
    "\\bspain\\w*\\b",
    "\\bspanish\\w*\\b",
    "\\bmadrid\\w*\\b"
  ),
  
  Sweden = c(
    "\\bsweden\\w*\\b",
    "\\bswedish\\w*\\b",
    "\\bstockholm\\w*\\b"
  ),
  
  UK = c(
    "\\bUK\\b|\\bU\\.K\\.\\b",
    "\\bunited\\s+kingdom\\b",
    "\\bgreat\\s+britain\\b",
    "\\bbritain\\w*\\b",
    "\\bbritish\\w*\\b",
    "\\bengl\\w*\\b",
    "\\bscotland\\w*\\b",
    "\\bscottish\\w*\\b",
    "\\bwales\\b",
    "\\bwelsh\\w*\\b",
    "\\bnorthern\\s+ireland\\b",
    "\\blondon\\w*\\b"
  ),
  
  Switzerland = c(
    "\\bswitzerland\\w*\\b",
    "\\bswiss\\w*\\b",
    "\\bbern\\w*\\b",
    "\\bberne\\w*\\b",
    "\\bzurich\\w*\\b",
    "\\bgeneva\\w*\\b"
  )
))
# datset: targeted sentences + pre and post
M_CE_west <-kwic(tokens_sent, pattern = dict, valuetype = 'regex', window = 1) 


#Long and short sentences

M_CE_west$lenght <- sapply(strsplit(M_CE_west$keyword, " "), length)

M_CE_west$lenght <- as.numeric(M_CE_west$lenght)

#M_CE_west1 <- M_CE_west %>% filter(lenght >= 5 & lenght <= 200) 

M_CE_west_length_5_1 <- as.data.frame(M_CE_west %>% filter(!(lenght >= 5)))

M_CE_west_length_200_1 <- as.data.frame(M_CE_west %>% filter(!(lenght <= 100)))

# Exclude the 'pattern'  columns

M_CE_west_length_200_2 <- M_CE_west_length_200_1[, !(names(M_CE_west_length_200_1) %in% c("pattern"))]

M_CE_west_length_200_2 <- unique(M_CE_west_length_200_2)

#Ask chatGPT to do sentence identification 

# Chat GPT 4 mini

library(httr)
library(chatgpt)

#my_API <- ""

#version gpt-4o-mini

#version gpt-4o-mini
hey_chatGPT_turbo <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-4o-mini",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}


for (i in 1:nrow(M_CE_west_length_200_2)) {
  print(i)
  question <- "Please analyze the following paragraph and correctly separate the sentences. 
  Do not include any additional text in your answer, separate each sentence correctly by punctuation and capitalization ONLY. If the text is a list separated by ;, each item is a sentence."
  text <- M_CE_west_length_200_2[i,5]
  concat <- paste(question, text)
  result <- hey_chatGPT_turbo(concat)
  while(length(result) == 0){
    result <- hey_chatGPT_turbo(concat)
    print(result)
  }
  print(result)
  M_CE_west_length_200_2$gpt_text[i] <- result
}

write_csv(M_CE_west_length_200_2, "sentences_long_060626.csv")

#re-run dictionary

M_CE_west_length_200_2 <- read_csv("sentences_long_060626.csv", locale = locale(encoding = "UTF-8")) 


corp1 <- corpus(M_CE_west_length_200_2$gpt_text) #transform in corpus

docvars(corp1, "docname") <- M_CE_west_length_200_2$docname #assign docvars variables

#add details


corp_doc1 <- corpus_group(corp1, groups = docname)

tokens1 <- tokens(corp_doc1,
                  what = "sentence")

M_CE_west_s <-kwic(tokens1, pattern = dict, valuetype = 'regex', window = 1)

M_CE_west_s$lenght <- sapply(strsplit(M_CE_west_s$keyword, " "), length) # check lenght

M_CE_west_s1 <- as.data.frame(M_CE_west_s %>% filter(lenght > 3 & lenght < 100))

#exclude sentences from the subset
M_CE_west_1 <- as.data.frame(M_CE_west %>% filter(lenght > 3 & lenght < 100))

M_CE_west_2 <- rbind(M_CE_west_s1, M_CE_west_1)

#exclude under 5 and over 100
M_CE_west_3 <- as.data.frame(M_CE_west_2 %>% filter(lenght > 5 & lenght < 100))

write_csv(M_CE_west_3, "manifesos_west_dictionary_based_070626.csv")

set.seed(1234)

# Draw a random sample of 1000 observations
sample_size <- 100
WE_sample <- M_CE_west_3[sample(1:nrow(M_CE_west_3), sample_size), ]

WE_sample <- WE_sample %>%
  mutate(row_id = row_number()) %>%
  mutate(
    actor = NA_character_
  )

write.xlsx(
  WE_sample,
  "M_CE_west_sample_dictionaryraw_07052026.xlsx",
  overwrite = TRUE
)

# Save dataset with unique entries

M_CE_west_4 <- as.data.frame(M_CE_west_3)

M_CE_west_5 <- M_CE_west_4[, names(M_CE_west_4) != "pattern"]

M_CE_west_5 <- unique(M_CE_west_5)

M_CE_west_5 <- M_CE_west_5 %>%
  as.data.frame() %>%
  mutate(
    text = stringr::str_squish(paste(pre, keyword, post))
  )

# save 
write_csv(M_CE_west_5, "manifesos_west_dictionary_based_unique_07052026.csv")


# Add number of sentences


no_sentences_1 <- as.data.frame(ntoken(tokens_sent))
no_sentences <- tibble::rownames_to_column(no_sentences_1, "manifesto_id")
names(no_sentences)<- c("manifesto_id", "total_sentences")

# Dataset with manifestos containing fewer than 50 sentences
short_manifestos <- no_sentences %>%
  filter(total_sentences < 50)

short_manifestos_text <- tibble(
  manifesto_id = docnames(tokens_sent),
  text = vapply(
    as.list(tokens_sent),
    function(x) paste(x, collapse = "\n"),
    character(1)
  )
) %>%
  semi_join(short_manifestos, by = "manifesto_id") %>%
  left_join(short_manifestos, by = "manifesto_id")

library(httr)

my_API <- ""

hey_chatGPT <- function(answer_my_question) {
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(
      Authorization = paste("Bearer", my_API)
    ),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-5.4-mini",
      reasoning_effort = "none",
      max_completion_tokens = 8000,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    ),
    timeout(120)
  )
  
  if (http_error(response)) {
    stop(
      "API error: ",
      status_code(response), " - ",
      content(response, as = "text", encoding = "UTF-8")
    )
  }
  
  result <- content(response)$choices[[1]]$message$content
  
  str_trim(result)
}

question <- paste(
  "You are correcting sentence segmentation in a political manifesto.",
  "The text may have missing or incorrect sentence boundaries.",
  "Identify and separate all sentences correctly.",
  "Preserve the original language, words, spelling.",
  "Do not rewrite, summarize, translate, or delete any content.",
  "Add sentence-ending punctuation only where necessary.",
  "Treat semicolon-separated list items as separate sentences.",
  "Return one complete sentence per line.",
  "Do not include numbering, explanations, or additional text."
)

short_manifestos_text$gpt_text <- NA_character_

for (i in seq_len(nrow(short_manifestos_text))) {
  
  print(i)
  
  text <- short_manifestos_text$text[i]
  
  if (is.na(text) || !nzchar(trimws(text))) next
  
  result <- tryCatch(
    hey_chatGPT(paste(question, "\n\nTEXT:\n", text)),
    error = function(e) {
      message(conditionMessage(e))
      NA_character_
    }
  )
  
  short_manifestos_text$gpt_text[i] <- result
  
  # Save progress
  if (i %% 5 == 0) {
    readr::write_csv(
      short_manifestos_text,
      "short_manifestos_checkpoint.csv"
    )
  }
}

# Keep successfully processed manifestos
short_corrected <- short_manifestos_text %>%
  filter(!is.na(gpt_text), nzchar(trimws(gpt_text)))

# Split corrected texts into individual sentences
corrected_sentences <- short_corrected %>%
  mutate(
    sentence = str_split(gpt_text, "\\r?\\n")
  ) %>%
  tidyr::unnest(sentence) %>%
  mutate(sentence = str_trim(sentence)) %>%
  filter(sentence != "") %>%
  group_by(manifesto_id) %>%
  mutate(sentence_id = row_number()) %>%
  ungroup()

no_sentences_corrected <- corrected_sentences %>%
  count(manifesto_id, name = "total_sentences_new")

sentence_comparison <- short_manifestos %>%
  left_join(no_sentences_corrected, by = "manifesto_id") %>%
  mutate(
    difference = total_sentences_new - total_sentences
  )


no_sentences_updated <- no_sentences %>%
  left_join(no_sentences_corrected, by = "manifesto_id") %>%
  mutate(
    total_sentences = coalesce(
      total_sentences_new,
      total_sentences
    )
  ) %>%
  select(-total_sentences_new)


write_csv(no_sentences_updated, "No_sentences_161026.csv")
