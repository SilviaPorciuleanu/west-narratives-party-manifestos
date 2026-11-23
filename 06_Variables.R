library(tidyverse)
library(readr)

#ideological measure

M_CE <- read_csv("MPDataset_MPDS2026a.csv", locale = locale(encoding = "UTF-8")) 

# Categories from Werner & Habersack (2025)

# Cultural dimension
cultural_left <- paste0("per", c(501, 602, 604, 607, 706))
cultural_right <- paste0("per", c(601, 603, 605, 608))

# Economic dimension
economic_left <- paste0("per", c(
  403, 404, 405, 406, 409, 412, 413,
  415, 416, 504, 701
))

economic_right <- paste0("per", c(
  401, 402, 407, 410, 414, 505, 702
))

# Calculate ideological positions
party_ideology <- M_CE %>%
  group_by(party, date) %>%
  summarise(
    cultural_left = sum(rowSums(across(all_of(cultural_left)))),
    cultural_right = sum(rowSums(across(all_of(cultural_right)))),
    
    economic_left = sum(rowSums(across(all_of(economic_left)))),
    economic_right = sum(rowSums(across(all_of(economic_right)))),
    
    .groups = "drop"
  ) %>%
  mutate(
    gal_tan = if_else(
      cultural_left + cultural_right > 0,
      (cultural_right - cultural_left) /
        (cultural_right + cultural_left),
      NA_real_
    ),
    
    economic_lr = if_else(
      economic_left + economic_right > 0,
      (economic_right - economic_left) /
        (economic_right + economic_left),
      NA_real_
    )
  )

# add also cmp scores

cmp <- read_csv("1999-2024_CHES_dataset_meansV2.csv")

cmp1 <- cmp %>%
  select(year, country, party_id, cmp_id, electionyear, lrgen, lrecon, galtan, family)

# Info of the manifestos I use 

info <- read_csv("Manifestos_variables_120526.csv")

M_2 <- left_join(info, cmp1, by = c("party" = "cmp_id", "year" = "electionyear"))

# data with different measures of ideology

M_3 <- left_join(M_2, party_ideology)

# add the code of new sentences in the replication code

sentences <- read_csv("No_sentences_161026.csv")

M_4 <- left_join(M_3, sentences)

# EU years

M_4$year <- as.numeric(M_4$year)
M_4$EU_years <- ifelse(M_4$country_code %in% c("CZE", "EST", "HUN", "LVA", "LTU", "POL", "SVK", "SVN") & 
                         M_4$year > 2004, M_4$year - 2004,
                          ifelse(M_4$country_code %in% c("ROU", "BGR") & 
                                   M_4$year > 2007, M_4$year - 2007, 0))

#EU binary
M_4$EU_membership <- ifelse(M_4$country_code %in% c("CZE", "EST", "HUN", "LVA", "LTU", "POL", "SVK", "SVN") & 
                              M_4$year >= 2004, 1,
                               ifelse(M_4$country_code %in% c("ROU", "BGR") & 
                                        M_4$year >= 2007, 1, 0))

# NATO years since accession
M_4$NATO_years <- ifelse(
  M_4$country_code %in% c("CZE", "HUN", "POL") &
    M_4$year > 1999,
  M_4$year - 1999,
  ifelse(
    M_4$country_code %in% c("EST", "LVA", "LTU", "SVK", "SVN", "ROU", "BGR") &
      M_4$year > 2004,
    M_4$year - 2004,
    0
  )
)

# NATO binary membership
M_4$NATO_membership <- ifelse(
  M_4$country_code %in% c("CZE", "HUN", "POL") &
    M_4$year >= 1999,
  1,
  ifelse(
    M_4$country_code %in% c("EST", "LVA", "LTU", "SVK", "SVN", "ROU", "BGR") &
      M_4$year >= 2004,
    1,
    0
  )
)
#party info

# Aggregate the coded dataset

A_West <- read_csv("west_self_other_valence_gpt-6_1-sol_full_dataset_medium.csv")

A_West <- A_West %>%
  filter(referent_category != "EU, NATO and European structures")

# Keep only relevant variables
A_West_selected <- A_West %>%
  select(
    manifesto_id, keyword,
    self_other_api_new, valence_api_new,
    referent_category,
    year, month, date, country_code
  )

# Dataset containing duplicated keywords within each manifesto
A_West_duplicates <- A_West_selected %>%
  group_by(manifesto_id, keyword) %>%
  filter(n() > 1) %>%
  arrange(manifesto_id, keyword) %>%
  ungroup()

# Inspect duplicated rows
A_West %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))

duplicates_different <- A_West_duplicates %>%
  group_by(manifesto_id, keyword) %>%
  filter(
    n_distinct(self_other_api_new, na.rm = FALSE) > 1 |
      n_distinct(valence_api_new, na.rm = FALSE) > 1
  ) %>%
  arrange(manifesto_id, keyword) %>%
  ungroup()

# Number of unique duplicated keywords with conflicting annotations
duplicates_different %>%
  distinct(manifesto_id, keyword) %>%
  nrow()

A_West_clean <- A_West %>%
  anti_join(
    duplicates_different %>%
      distinct(manifesto_id, keyword),
    by = c("manifesto_id", "keyword")
  )

A_West_clean_1 <- A_West_clean %>%
  select(
    manifesto_id, keyword,
    self_other_api_new, valence_api_new,
    referent_category,
    year, month, date, country_code
  ) %>%
  distinct()

West_agg <- A_West_clean_1 %>%
  group_by(manifesto_id) %>%
  summarise(
    total_count = n_distinct(keyword, na.rm = TRUE),
    
    # Self/Other - unique keywords
    self = n_distinct(keyword[self_other_api_new == 1], na.rm = TRUE),
    other = n_distinct(keyword[self_other_api_new == -1], na.rm = TRUE),
    
    # Valence - unique keywords
    positive = n_distinct(keyword[valence_api_new == 1], na.rm = TRUE),
    negative = n_distinct(keyword[valence_api_new == -1], na.rm = TRUE),
    
    # Combined narratives - unique keywords
    self_positive = n_distinct(
      keyword[self_other_api_new == 1 & valence_api_new == 1],
      na.rm = TRUE
    ),
    
    self_negative = n_distinct(
      keyword[self_other_api_new == 1 & valence_api_new == -1],
      na.rm = TRUE
    ),
    
    other_positive = n_distinct(
      keyword[self_other_api_new == -1 & valence_api_new == 1],
      na.rm = TRUE
    ),
    
    other_negative = n_distinct(
      keyword[self_other_api_new == -1 & valence_api_new == -1],
      na.rm = TRUE
    ),
    
    # Referent categories - all observations
    #EU_NATO = sum(referent_category == "EU, NATO and European structures", na.rm = TRUE),
    West = sum(referent_category == "West", na.rm = TRUE),
    Western_countries = sum(referent_category == "Western countries", na.rm = TRUE),
    Western_signifier = sum(referent_category == "Western signifier", na.rm = TRUE),
    
    # Manifesto information
    year = first(year),
    month = first(month),
    date = first(date),
    country_code = first(country_code),
    
    .groups = "drop"
  )

West_by_category <- A_West_clean_1 %>%
  group_by(manifesto_id, referent_category) %>%
  summarise(
    # Total unique mentions
    category_total = n_distinct(keyword, na.rm = TRUE),
    
    # Self/Other
    self = n_distinct(keyword[self_other_api_new == 1], na.rm = TRUE),
    other = n_distinct(keyword[self_other_api_new == -1], na.rm = TRUE),
    
    # Valence
    positive = n_distinct(keyword[valence_api_new == 1], na.rm = TRUE),
    negative = n_distinct(keyword[valence_api_new == -1], na.rm = TRUE),
    
    # Combined narratives
    self_positive = n_distinct(
      keyword[self_other_api_new == 1 & valence_api_new == 1],
      na.rm = TRUE
    ),
    
    self_negative = n_distinct(
      keyword[self_other_api_new == 1 & valence_api_new == -1],
      na.rm = TRUE
    ),
    
    other_positive = n_distinct(
      keyword[self_other_api_new == -1 & valence_api_new == 1],
      na.rm = TRUE
    ),
    
    other_negative = n_distinct(
      keyword[self_other_api_new == -1 & valence_api_new == -1],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Self/Other percentages
    self_pct = self / category_total * 100,
    other_pct = other / category_total * 100,
    
    # Valence percentages
    positive_pct = positive / category_total * 100,
    negative_pct = negative / category_total * 100,
    
    # Combined narrative percentages
    self_positive_pct = self_positive / category_total * 100,
    self_negative_pct = self_negative / category_total * 100,
    other_positive_pct = other_positive / category_total * 100,
    other_negative_pct = other_negative / category_total * 100
  )

West_by_category <- West_by_category %>%
  mutate(
    referent_category = recode(
      referent_category,
      "West" = "West",
      "Western countries" = "Western_countries",
      "Western signifier" = "Western_signifier"
    )
  ) %>%
  select(
    manifesto_id, referent_category,
    category_total,
    
    # Original counts
    self, other, positive, negative,
    
    # Combined counts
    self_positive, self_negative,
    other_positive, other_negative,
    
    # Original percentages
    self_pct, other_pct, positive_pct, negative_pct,
    
    # Combined percentages
    self_positive_pct, self_negative_pct,
    other_positive_pct, other_negative_pct
  ) %>%
  pivot_wider(
    names_from = referent_category,
    values_from = c(
      category_total,
      self, other, positive, negative,
      self_positive, self_negative,
      other_positive, other_negative,
      self_pct, other_pct, positive_pct, negative_pct,
      self_positive_pct, self_negative_pct,
      other_positive_pct, other_negative_pct
    ),
    names_glue = "{.value}_{referent_category}"
  )

West_agg <- West_agg %>%
  left_join(West_by_category, by = "manifesto_id")

# Add manifesto-level information
West_agg_1 <- left_join(M_4, West_agg)

West_agg_2 <- West_agg_1 %>%
  mutate(
    # Self/Other percentages
    self_pct = self / total_count * 100,
    other_pct = other / total_count * 100,
    
    # Valence percentages
    positive_pct = positive / total_count * 100,
    negative_pct = negative / total_count * 100,
    
    # Combined narrative percentages
    self_positive_pct = self_positive / total_count * 100,
    self_negative_pct = self_negative / total_count * 100,
    other_positive_pct = other_positive / total_count * 100,
    other_negative_pct = other_negative / total_count * 100,
    
    # Referent category percentages
    West_pct = West / total_count * 100,
    Western_countries_pct = Western_countries / total_count * 100,
    Western_signifier_pct = Western_signifier / total_count * 100,
    
    # Salience: percentage of ALL manifesto sentences
    total_west_pct = total_count / total_sentences * 100,
    West_pct_total = West / total_sentences * 100,
    Western_countries_pct_total = Western_countries / total_sentences * 100,
    Western_signifier_pct_total = Western_signifier / total_sentences * 100
  )

West_agg_2 <- West_agg_2 %>%
  rename(year_cmp = year.y) %>%
  mutate(
    across(
      c(total_west_pct,
        West_pct_total,
        Western_countries_pct_total,
        Western_signifier_pct_total
      ),
      ~ replace_na(.x, 0)
    )
  )

West_agg_2 <- West_agg_2 %>%
  mutate(
    combined_total = rowSums(
      across(c(
        self_positive,
        self_negative,
        other_positive,
        other_negative
      )),
      na.rm = TRUE
    ),
    
    combined_coverage = if_else(
      total_count > 0,
      combined_total / total_count * 100,
      NA_real_
    )
  )

summary(West_agg_2$combined_coverage)

# Check for impossible totals
West_agg_2 %>%
  filter(combined_total > total_count) %>%
  select(
    manifesto_id, total_count, combined_total,
    self_positive, self_negative,
    other_positive, other_negative
  )

write_csv(West_agg_2, "Manifestos_West_FULL_171026.csv")

