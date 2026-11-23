###########################################################
######### Full Dataset of Manifesots in CEE ###############
##########################################################

#load libraries
library(manifestoR)
library(tidyverse)
library(readr) 

# Create one dataset that contains all the manifestos 
# (The manifestos are in different dataset and are downloaded as a list)

mp_setapikey("manifesto_apikey.txt") # create and download your API key at https://manifesto-project.wzb.eu before
mp_load_cache(file = "manifesto_cache.RData") # to ensure replicability, make sure you have this file in your working directory

#Manifesto corpus version: 2025-1

# Load Data
election_programmes_ro <- mp_corpus_df_bilingual(countryname == "Romania") 
election_programmes_ro$country_code <- "ROU" #country code

election_programmes_bg <- mp_corpus_df_bilingual(countryname == "Bulgaria") 
election_programmes_bg$country_code <- "BGR" #country code

election_programmes_cz <- mp_corpus_df_bilingual(countryname == "Czech Republic")
election_programmes_cz$country_code <- "CZE" #country code

election_programmes_sk <- mp_corpus_df_bilingual(countryname == "Slovakia")
election_programmes_sk$country_code <- "SVK" #country code

election_programmes_pl <- mp_corpus_df_bilingual(countryname == "Poland")
election_programmes_pl$country_code <- "POL" #country code

election_programmes_hu <- mp_corpus_df_bilingual(countryname == "Hungary")
election_programmes_hu$country_code <- "HUN" #country code

election_programmes_sl <- mp_corpus_df_bilingual(countryname == "Slovenia")
election_programmes_sl$country_code <- "SVN" #country code

election_programmes_ee <- mp_corpus_df_bilingual(countryname == "Estonia")
election_programmes_ee$country_code <- "EST" #country code

election_programmes_lv <- mp_corpus_df_bilingual(countryname == "Latvia")
election_programmes_lv$country_code <- "LVA" #country code

election_programmes_lt <- mp_corpus_df_bilingual(countryname == "Lithuania")
election_programmes_lt$country_code <- "LTU" #country code

#mp_save_cache(file = "manifesto_cache.RData")

WE <- rbind(election_programmes_bg, election_programmes_cz,
            election_programmes_hu, election_programmes_pl, 
            election_programmes_ro, election_programmes_sk,
            election_programmes_sl, election_programmes_ee,
            election_programmes_lv, election_programmes_lt)


#save the dataset

write_csv(WE, "manifesos_CEE_full_050526.csv") # 265779 documents 

# Info of the manifestos

M_CE_info <- unique(WE[, c(6:9, 12)])

#Date
M_CE_info$year <- substr(M_CE_info$date, 1, 4)
M_CE_info$month <- substr(M_CE_info$date, 5, 6)

M_CE_info$year <- as.numeric(M_CE_info$year)
M_CE_info$month <- as.numeric(M_CE_info$month)


#save the party family data

info <- read_csv("MPDataset_MPDS2023a.csv")

info1 <- info %>%
  select(date, party, partyname, parfam)

M_CE_info_2 <- left_join(M_CE_info, info1, by = c("party", "date"))

write_csv(M_CE_info_2, "Manifestos_variables_120526.csv")


