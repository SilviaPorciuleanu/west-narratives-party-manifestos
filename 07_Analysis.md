Manifesto analysis
================

## Setup and salience over time

``` r
library(tidyverse)
library(lme4)
library(readr)
library(RColorBrewer)
library(ggrepel)
library(geomtextpath)
library(ggpubr)
library(tseries) 
library(lmtest)
library(plm)
library(car)
library(pcse)
library(fixest)
library(lfe)
library(sandwich)
library(lmtest)
library(ggstream)
library(patchwork)

Df_west <- read.csv("Manifestos_West_FULL_171026.csv")

Df_west <- Df_west %>%
  filter(total_sentences > 30)
```

``` r
salience_year <- Df_west %>%
  group_by(year) %>%
  summarise(
    Europe = mean(Western_signifier_pct_total, na.rm = TRUE),
    West = mean(West_pct_total, na.rm = TRUE),
    Western_countries = mean(Western_countries_pct_total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Europe, West, Western_countries),
    names_to = "Category",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Category = factor(
      Category,
      levels = c("Europe", "Western_countries", "West")
    )
  )


# Smooth each referent category separately
salience_smooth <- salience_year %>%
  group_by(Category) %>%
  arrange(year, .by_group = TRUE) %>%
  group_modify(~ {
    model <- loess(
      Percentage ~ year,
      data = .x,
      span = 0.50,
      degree = 1
    )
    
    tibble(
      year = seq(min(.x$year), max(.x$year), by = 1),
      Percentage = predict(
        model,
        newdata = data.frame(
          year = seq(min(.x$year), max(.x$year), by = 1)
        )
      )
    )
  }) %>%
  ungroup() %>%
  mutate(
    # Prevent negative predicted percentages
    Percentage = pmax(Percentage, 0)
  )

# Plot smoothed stacked areas
p_salience_stacked <- ggplot(
  salience_smooth,
  aes(x = year, y = Percentage, fill = Category)
) +
  geom_area(alpha = 0.9, color = "white", linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Europe" = "#0B3C5D",
      "Western_countries" = "#328CC1",
      "West" = "#BFD7EA"
    ),
    labels = c(
      "Europe",
      "Western countries",
      "West"
    )
  ) +
  coord_cartesian(ylim = c(0, 7)) +
  scale_y_continuous(breaks = seq(0, 7, by = 1)) +
  labs(
    x = "Year",
    y = "Mean percentage of manifesto sentences",
    fill = "",
    title = "Salience of Western References Over Time"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p_salience_stacked
```

![](10_Analysis_GitHub_files/figure-gfm/section-1-1.png)<!-- -->

``` r
ggsave(
  "salience_through_time.png",
  plot = p_salience_stacked,
  width = 7,
  height = 5.5,
  dpi = 300
)
```

## Country salience

``` r
# country salience

# Calculate mean salience by country and election year
country_election <- Df_west %>%
  group_by(country_code, year) %>%
  summarise(
    Europe = mean(Western_signifier_pct_total, na.rm = TRUE),
    West = mean(West_pct_total, na.rm = TRUE),
    Western_countries = mean(Western_countries_pct_total, na.rm = TRUE),
    n_manifestos = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Europe, West, Western_countries),
    names_to = "Category",
    values_to = "Percentage"
  ) %>%
  mutate(
    Category = factor(
      Category,
      levels = c("Europe", "Western_countries", "West"),
      labels = c("Europe", "Western countries", "West")
    )
  )

# Stacked bars by election year, separately for each country
p_country_elections <- ggplot(
  country_election,
  aes(
    x = factor(year),
    y = Percentage,
    fill = Category
  )
) +
  geom_col(width = 0.95, color = "white", linewidth = 0.2) +
  facet_wrap(~ country_code, ncol = 4, scales = "free_x") +
  scale_fill_manual(
    values = c(
      "Europe" = "#0B3C5D",
      "Western countries" = "#328CC1",
      "West" = "#BFD7EA"
    )
  ) +
  labs(
    x = "Election year",
    y = "Mean percentage of manifesto sentences",
    fill = "",
    title = "Salience of Western References Across Elections"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_country_elections
```

![](10_Analysis_GitHub_files/figure-gfm/section-2-1.png)<!-- -->

``` r
ggsave(
  "countries_through_time.png",
  plot = p_country_elections,
  width = 7,
  height = 7,
  dpi = 300
)
```

## Self and other references

``` r
### self other

Df_west <- Df_west %>%
  mutate(
    across(
      c(self_pct_West, other_pct_West,
        positive_pct_West, negative_pct_West),
      ~ if_else(coalesce(category_total_West, 0) > 0, .x, NA_real_)
    ),
    across(
      c(self_pct_Western_countries, other_pct_Western_countries,
        positive_pct_Western_countries, negative_pct_Western_countries),
      ~ if_else(coalesce(category_total_Western_countries, 0) > 0, .x, NA_real_)
    ),
    across(
      c(self_pct_Western_signifier, other_pct_Western_signifier,
        positive_pct_Western_signifier, negative_pct_Western_signifier),
      ~ if_else(coalesce(category_total_Western_signifier, 0) > 0, .x, NA_real_)
    )
  )


# Self / Other graph
self_other_plot <- Df_west %>%
  select(year, self_pct, other_pct) %>%
  pivot_longer(
    cols = c(self_pct, other_pct),
    names_to = "Category",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Category = factor(
      Category,
      levels = c("self_pct", "other_pct"),
      labels = c("Self", "Other")
    )
  ) %>%
  ggplot(aes(x = year, y = Percentage, colour = Category)) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Self" = "#1f78b4",
      "Other" = "#e31a1c"
    )
  ) +
  labs(
    colour = "",
    x = "Year",
    y = "Mean percentage of mentions",
    title = "Self and other references through time"
  ) +
  coord_cartesian(ylim = c(0, 85)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

self_other_plot
```

![](10_Analysis_GitHub_files/figure-gfm/section-3-1.png)<!-- -->

``` r
# Positive / Negative graph
valence_plot <- Df_west %>%
  select(year, positive_pct, negative_pct) %>%
  pivot_longer(
    cols = c(positive_pct, negative_pct),
    names_to = "Category",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Category = factor(
      Category,
      levels = c("positive_pct", "negative_pct"),
      labels = c("Positive", "Negative")
    )
  ) %>%
  ggplot(aes(x = year, y = Percentage, colour = Category)) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Positive" = "#33a02c",
      "Negative" = "#6a3d9a"
    )
  ) +
  labs(
    colour = "",
    x = "Year",
    y = "Mean percentage of mentions",
    title = "Positive and negative references through time"
  ) +
  coord_cartesian(ylim = c(0, 85)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

valence_plot
```

![](10_Analysis_GitHub_files/figure-gfm/section-3-2.png)<!-- -->

``` r
# Combine plots
combined_plot <- self_other_plot + valence_plot +
  plot_layout(ncol = 2)

# Display
combined_plot
```

![](10_Analysis_GitHub_files/figure-gfm/section-3-3.png)<!-- -->

``` r
# Save for Overleaf
ggsave(
  "self_other_valence.png",
  plot = combined_plot,
  width = 10,
  height = 4,
  dpi = 300
)
```

## References by category

``` r
# by category 

self_other_by_term <- Df_west %>%
  select(
    year,
    self_pct_West, other_pct_West,
    self_pct_Western_countries, other_pct_Western_countries,
    self_pct_Western_signifier, other_pct_Western_signifier
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = c("Narrative", "Referent"),
    names_pattern = "(self|other)_pct_(.*)",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Referent = dplyr::recode(
      Referent,
  "Western_signifier" = "Europe",
  "West" = "West",
  "Western_countries" = "Western countries"
),
    Referent = factor(
      Referent,
      levels = c("Europe", "West", "Western countries")
    ),
    Narrative = factor(
      Narrative,
      levels = c("self", "other"),
      labels = c("Self", "Other")
    )
  )

p_self_other <- ggplot(
  self_other_by_term,
  aes(x = year, y = Percentage, color = Narrative)
) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(~ Referent, nrow = 1) +
  scale_color_manual(
    values = c(
      "Self" = "#1f78b4",
      "Other" = "#e31a1c"
    )
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(
    x = "Year",
    y = "Percentage of mentions",
    color = "",
    title = "Self and Other Narratives by Referent"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text = element_text(face = "bold")
  )

p_self_other
```

![](10_Analysis_GitHub_files/figure-gfm/section-4-1.png)<!-- -->

``` r
valence_by_term <- Df_west %>%
  select(
    year,
    positive_pct_West, negative_pct_West,
    positive_pct_Western_countries, negative_pct_Western_countries,
    positive_pct_Western_signifier, negative_pct_Western_signifier
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = c("Valence", "Referent"),
    names_pattern = "(positive|negative)_pct_(.*)",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Referent = dplyr::recode(
  Referent,
  "Western_signifier" = "Europe",
  "West" = "West",
  "Western_countries" = "Western countries"
),
    Referent = factor(
      Referent,
      levels = c("Europe", "West", "Western countries")
    ),
    Valence = factor(
      Valence,
      levels = c("positive", "negative"),
      labels = c("Positive", "Negative")
    )
  )

p_valence <- ggplot(
  valence_by_term,
  aes(x = year, y = Percentage, color = Valence)
) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(~ Referent, nrow = 1) +
  scale_color_manual(
    values = c(
      "Positive" = "#33a02c",
      "Negative" = "#6a3d9a"
    )
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(
    x = "Year",
    y = "Percentage of mentions",
    color = "",
    title = "Positive and Negative Narratives by Referent"
  ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey95", color = "grey80"),
  strip.text = element_text(face = "bold"))

p_valence
```

![](10_Analysis_GitHub_files/figure-gfm/section-4-2.png)<!-- -->

``` r
combined_narratives <- p_self_other / p_valence

combined_narratives
```

![](10_Analysis_GitHub_files/figure-gfm/section-4-3.png)<!-- -->

``` r
ggsave(
  "narratives_by_referent.png",
  combined_narratives,
  width = 12,
  height = 7,
  dpi = 300
)
```

## Combined narratives

``` r
#self_positive 

# Colors for combined narratives
narrative_colors <- c(
  "Self + Positive"  = "#009E73",
  "Self + Negative"  = "#CC79A7",
  "Other + Positive" = "#0072B2",
  "Other + Negative" = "#D55E00"
)

combined_long <- Df_west %>%
  select(
    year,
    self_positive_pct,
    self_negative_pct,
    other_positive_pct,
    other_negative_pct
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = "Narrative",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    Narrative = factor(
      Narrative,
      levels = c(
        "self_positive_pct",
        "self_negative_pct",
        "other_positive_pct",
        "other_negative_pct"
      ),
      labels = c(
        "Self + Positive",
        "Self + Negative",
        "Other + Positive",
        "Other + Negative"
      )
    )
  ) %>%
  filter(is.finite(Percentage))

# Plot
p_combined <- ggplot(
  combined_long,
  aes(x = year, y = Percentage, color = Narrative)
) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.75,
    se = FALSE,
    linewidth = 1
  ) +
  scale_color_manual(values = narrative_colors) +
  coord_cartesian(ylim = c(0, 70)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(
    x = "Year",
    y = "Percentage of mentions",
    color = "",
    title = "Narratives Through Time"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

p_combined
```

![](10_Analysis_GitHub_files/figure-gfm/section-5-1.png)<!-- -->

``` r
combined_by_referent <- Df_west %>%
  select(
    year,
    matches(
      "^(self_positive|self_negative|other_positive|other_negative)_pct_(West|Western_countries|Western_signifier)$"
    )
  ) %>%
  pivot_longer(
    cols = -year,
    names_to = c("Narrative", "Referent"),
    names_pattern = "(self_positive|self_negative|other_positive|other_negative)_pct_(.*)",
    values_to = "Percentage"
  ) %>%
  mutate(
    year = as.numeric(as.character(year)),
    
    Referent = dplyr::recode(
  Referent,
  "Western_signifier" = "Europe",
  "West" = "West",
  "Western_countries" = "Western countries"
),
    
    Referent = factor(
      Referent,
      levels = c("Europe", "West", "Western countries")
    ),
    
    Narrative = factor(
      Narrative,
      levels = c(
        "self_positive",
        "self_negative",
        "other_positive",
        "other_negative"
      ),
      labels = c(
        "Self + Positive",
        "Self + Negative",
        "Other + Positive",
        "Other + Negative"
      )
    )
  ) %>%
  filter(is.finite(Percentage))

# Plot
p_combined_referent <- ggplot(
  combined_by_referent,
  aes(x = year, y = Percentage, color = Narrative)
) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    span = 0.9,
    se = FALSE,
    linewidth = 1
  ) +
  facet_wrap(~ Referent, nrow = 1) +
  scale_color_manual(values = narrative_colors) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 20)) +
  labs(
    x = "Year",
    y = "Percentage of mentions",
    color = "",
    title = "Narratives by term"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.background = element_rect(
      fill = "grey95",
      color = "grey80"
    ),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_combined_referent
```

![](10_Analysis_GitHub_files/figure-gfm/section-5-2.png)<!-- -->

``` r
ggsave(
  "combined_narratives.png",
  plot = p_combined,
  width = 6,
  height = 5,
  dpi = 300
)

ggsave(
  "combined_narratives_by_referent.png",
  plot = p_combined_referent,
  width = 12,
  height = 4.5,
  dpi = 300
)
```

## Salience regressions

``` r
# Regressions Salience

# Models 1–2: No fixed effects
m1 <- feols(
  total_west_pct ~ EU_years + gal_tan + economic_lr,
  data = Df_west,
  vcov = ~ party
)

m2 <- feols(
  total_west_pct ~ NATO_years + gal_tan + economic_lr,
  data = Df_west,
  vcov = ~ party
)

# Models 3–4: Country fixed effects
m3 <- feols(
  total_west_pct ~ EU_years + gal_tan + economic_lr | country_code,
  data = Df_west,
  vcov = ~ party
)

m4 <- feols(
  total_west_pct ~ NATO_years + gal_tan + economic_lr | country_code,
  data = Df_west,
  vcov = ~ party
)

# Models 5–6: Country and year fixed effects
m5 <- feols(
  total_west_pct ~ EU_years + gal_tan + economic_lr |
    country_code + year,
  data = Df_west,
  vcov = ~ party
)

m6 <- feols(
  total_west_pct ~ NATO_years + gal_tan + economic_lr |
    country_code + year,
  data = Df_west,
  vcov = ~ party
)

knitr::kable(etable(
  m3, m4, m5, m6,

  digits = 3,
  digits.stats = 3,

  title = "Salience of references to the West, Europe or Western countries",

  # Only four model columns
  headers = c(
    "EU", "NATO", "EU", "NATO"
  ),
  
  dict = c(
    EU_years = "Years since EU accession",
    NATO_years = "Years since NATO accession",
    gal_tan = "Cultural GAL--TAN",
    economic_lr = "Economic left--right",
    country_code = "Country",
    year = "Year"
  ),
  
  order = c(
    "Years since EU",
    "Years since NATO",
    "Cultural",
    "Economic"
  ),
  
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05
  ),
  
  fitstat = ~ n + r2 + wr2
), format = "pipe")
```

|  | m3 | m4 | m5 | m6 |
|:---|:---|:---|:---|:---|
|  | EU | NATO | EU | NATO |
| Dependent Var.: | total_west_pct | total_west_pct | total_west_pct | total_west_pct |
|  |  |  |  |  |
| Years since EU accession | -0.018 (0.023) |  | -0.560 (0.410) |  |
| Years since NATO accession |  | -0.008 (0.021) |  | -0.135 (0.166) |
| Cultural GAL–TAN | 0.005 (0.359) | -0.006 (0.360) | -0.131 (0.439) | -0.167 (0.438) |
| Economic left–right | 1.12 (0.673) | 1.13 (0.677) | 1.50\* (0.680) | 1.47\* (0.684) |
| Fixed-Effects: | ————– | ————– | ————– | ————– |
| Country | Yes | Yes | Yes | Yes |
| Year | No | No | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 |
| R2 | 0.101 | 0.100 | 0.216 | 0.208 |
| Within R2 | 0.031 | 0.030 | 0.058 | 0.048 |

``` r
# Compare all six models (EU and NATO)
knitr::kable(etable(
  m1, m2, m3, m4, m5, m6,
  digits = 3,
  dict = c(
    EU_years = "Years since EU accession",
    NATO_years = "Years since NATO accession",
    gal_tan = "Cultural GAL–TAN",
    economic_lr = "Economic left–right"
  )
), format = "pipe")
```

|  | m1 | m2 | m3 | m4 | m5 | m6 |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | total_west_pct | total_west_pct | total_west_pct | total_west_pct | total_west_pct | total_west_pct |
|  |  |  |  |  |  |  |
| Constant | 3.22\*\*\* (0.384) | 3.04\*\*\* (0.355) |  |  |  |  |
| Years since EU accession | -0.025 (0.023) |  | -0.018 (0.023) |  | -0.560 (0.410) |  |
| Cultural GAL–TAN | -0.353 (0.402) | -0.417 (0.408) | 0.005 (0.359) | -0.006 (0.360) | -0.131 (0.439) | -0.167 (0.438) |
| Economic left–right | 1.13 (0.726) | 1.22 (0.752) | 1.12. (0.673) | 1.13. (0.677) | 1.50\* (0.680) | 1.47\* (0.684) |
| Years since NATO accession |  | 0.006 (0.024) |  | -0.008 (0.021) |  | -0.135 (0.166) |
| Fixed-Effects: | ————— | ————— | ————– | ————– | ————– | ————– |
| country_code | No | No | Yes | Yes | Yes | Yes |
| year | No | No | No | No | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 | 326 | 326 |
| R2 | 0.04187 | 0.03899 | 0.10143 | 0.10032 | 0.21605 | 0.20820 |
| Within R2 | – | – | 0.03131 | 0.03011 | 0.05763 | 0.04820 |

## Salience by referent

``` r
# By term

# Dependent variables
outcomes <- c(
  "West_pct_total",
  "Western_signifier_pct_total",
  "Western_countries_pct_total"
)

# Function to estimate six models per outcome
run_models <- function(y) {
  
  list(
    # EU models
    EU_noFE = feols(
      as.formula(paste(y, "~ EU_years + gal_tan + economic_lr")),
      data = Df_west,
      vcov = ~ party
    ),
    
    EU_countryFE = feols(
      as.formula(paste(y, "~ EU_years + gal_tan + economic_lr | country_code")),
      data = Df_west,
      vcov = ~ party
    ),
    
    EU_country_yearFE = feols(
      as.formula(paste(y, "~ EU_years + gal_tan + economic_lr | country_code + year")),
      data = Df_west,
      vcov = ~ party
    ),
    
    # NATO models
    NATO_noFE = feols(
      as.formula(paste(y, "~ NATO_years + gal_tan + economic_lr")),
      data = Df_west,
      vcov = ~ party
    ),
    
    NATO_countryFE = feols(
      as.formula(paste(y, "~ NATO_years + gal_tan + economic_lr | country_code")),
      data = Df_west,
      vcov = ~ party
    ),
    
    NATO_country_yearFE = feols(
      as.formula(paste(y, "~ NATO_years + gal_tan + economic_lr | country_code + year")),
      data = Df_west,
      vcov = ~ party
    )
  )
}

# Run all models
models_by_outcome <- lapply(outcomes, run_models)

names(models_by_outcome) <- c(
  "West",
  "Europe",
  "Western_countries"
)

# West
knitr::kable(etable(
  models_by_outcome$West,
  digits = 3
), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_y.. | NATO_noFE | NATO_countryFE | NATO_country\_.. |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | West_pct_total | West_pct_total | West_pct_total | West_pct_total | West_pct_total | West_pct_total |
|  |  |  |  |  |  |  |
| Constant | 0.434\*\*\* (0.110) |  |  | 0.417\*\*\* (0.113) |  |  |
| EU_years | -0.011 (0.008) | -0.014 (0.009) | -0.149 (0.095) |  |  |  |
| gal_tan | 0.147 (0.100) | 0.174. (0.102) | 0.190. (0.108) | 0.136 (0.100) | 0.176. (0.102) | 0.180. (0.107) |
| economic_lr | 0.218 (0.206) | 0.270 (0.192) | 0.278 (0.203) | 0.224 (0.207) | 0.263 (0.193) | 0.264 (0.199) |
| NATO_years |  |  |  | -0.007 (0.008) | -0.015. (0.009) | -0.287\* (0.130) |
| Fixed-Effects: | —————- | ————– | ————– | —————- | ————— | ————— |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 | 326 | 326 |
| R2 | 0.03884 | 0.13523 | 0.22390 | 0.03424 | 0.13945 | 0.23832 |
| Within R2 | – | 0.05613 | 0.05182 | – | 0.06073 | 0.06943 |

``` r
# Europe
knitr::kable(etable(
  models_by_outcome$Europe,
  digits = 3
), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | Western_signifier_pct_total | Western_signifier_pct_total | Western_signifier_pct_total | Western_signifier_pct_total | Western_signifier_pct_total | Western_signifier_pct_total |
|  |  |  |  |  |  |  |
| Constant | 2.09\*\*\* (0.195) |  |  | 2.01\*\*\* (0.200) |  |  |
| EU_years | -0.033. (0.019) | -0.025 (0.017) | -0.448 (0.357) |  |  |  |
| gal_tan | -0.241 (0.185) | -0.095 (0.220) | -0.164 (0.220) | -0.281 (0.188) | -0.107 (0.219) | -0.192 (0.228) |
| economic_lr | 0.235 (0.279) | 0.206 (0.269) | 0.537. (0.278) | 0.268 (0.287) | 0.222 (0.272) | 0.522. (0.284) |
| NATO_years |  |  |  | -0.016 (0.016) | -0.014 (0.015) | 0.138 (0.193) |
| Fixed-Effects: | ————————— | ————————— | ————————— | ————————— | ————————— | ————————— |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 | 326 | 326 |
| R2 | 0.02160 | 0.06596 | 0.21466 | 0.01465 | 0.06282 | 0.20563 |
| Within R2 | – | 0.00874 | 0.02516 | – | 0.00541 | 0.01396 |

``` r
# Western countries
knitr::kable(etable(
  models_by_outcome$Western_countries,
  digits = 3
), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | Western_countries_pct_total | Western_countries_pct_total | Western_countries_pct_total | Western_countries_pct_total | Western_countries_pct_total | Western_countries_pct_total |
|  |  |  |  |  |  |  |
| Constant | 0.699\*\* (0.227) |  |  | 0.608\*\* (0.192) |  |  |
| EU_years | 0.020\* (0.008) | 0.020\* (0.008) | 0.038 (0.099) |  |  |  |
| gal_tan | -0.260 (0.252) | -0.074 (0.213) | -0.157 (0.281) | -0.272 (0.254) | -0.075 (0.213) | -0.155 (0.281) |
| economic_lr | 0.682 (0.477) | 0.642 (0.436) | 0.681 (0.447) | 0.732 (0.492) | 0.648 (0.438) | 0.683 (0.448) |
| NATO_years |  |  |  | 0.029\* (0.011) | 0.020\* (0.008) | 0.015 (0.072) |
| Fixed-Effects: | ————————— | ————————— | ————————— | ————————— | ————————— | ————————— |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 | 326 | 326 |
| R2 | 0.06153 | 0.18454 | 0.25230 | 0.07821 | 0.18630 | 0.25214 |
| Within R2 | – | 0.05282 | 0.05542 | – | 0.05487 | 0.05521 |

``` r
# Export the three salience regression tables

table_titles <- c(
  West = "Salience of references to the West",
  Europe = "Salience of references to Europe",
  Western_countries = "Salience of references to Western countries"
)

for (outcome in names(models_by_outcome)) {
  
  models <- models_by_outcome[[outcome]]
  
  # Format regression table
  latex <- knitr::kable(etable(
    models$EU_countryFE,
    models$NATO_countryFE,
    models$EU_country_yearFE,
    models$NATO_country_yearFE,

    depvar = FALSE,
    digits = 3,
    digits.stats = 3,
    
    title = unname(table_titles[[outcome]]),
    
    headers = c("EU", "NATO", "EU", "NATO"),
    
    dict = c(
      EU_years = "Years since EU accession",
      NATO_years = "Years since NATO accession",
      gal_tan = "Cultural GAL--TAN",
      economic_lr = "Economic left--right",
      country_code = "Country",
      year = "Year"
    ),
    
    order = c(
      "Years since EU",
      "Years since NATO",
      "Cultural",
      "Economic"
    ),
    
    signif.code = c(
      "***" = 0.001,
      "**" = 0.01,
      "*" = 0.05
    ),
    
    fitstat = ~ n + r2 + wr2
  ), format = "pipe")
  
  # Display the table
  cat("\n", latex, "\n\n", sep = "\n")
}
```

|  | models$EU_co.. |models$NATO\_.. | models$EU_co.. |models$NATO_c.. |  |  |
|:---|:---|:---|:---|:---|
|  | EU | NATO | EU | NATO |
|  |  |  |  |  |
| Years since EU accession | -0.014 (0.009) |  | -0.149 (0.095) |  |
| Years since NATO accession |  | -0.015 (0.009) |  | -0.287\* (0.130) |
| Cultural GAL–TAN | 0.174 (0.102) | 0.176 (0.102) | 0.190 (0.108) | 0.180 (0.107) |
| Economic left–right | 0.270 (0.192) | 0.263 (0.193) | 0.278 (0.203) | 0.264 (0.199) |
| Fixed-Effects: | ————– | ————– | ————– | ————— |
| Country | Yes | Yes | Yes | Yes |
| Year | No | No | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 |
| R2 | 0.135 | 0.139 | 0.224 | 0.238 |
| Within R2 | 0.056 | 0.061 | 0.052 | 0.069 |

|  | models$EU_co.. |models$NATO\_.. | models$EU_co.. |models$NATO\_.. |  |  |
|:---|:---|:---|:---|:---|
|  | EU | NATO | EU | NATO |
|  |  |  |  |  |
| Years since EU accession | -0.025 (0.017) |  | -0.448 (0.357) |  |
| Years since NATO accession |  | -0.014 (0.015) |  | 0.138 (0.193) |
| Cultural GAL–TAN | -0.095 (0.220) | -0.107 (0.219) | -0.164 (0.220) | -0.192 (0.228) |
| Economic left–right | 0.206 (0.269) | 0.222 (0.272) | 0.537 (0.278) | 0.522 (0.284) |
| Fixed-Effects: | ————– | ————– | ————– | ————– |
| Country | Yes | Yes | Yes | Yes |
| Year | No | No | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 |
| R2 | 0.066 | 0.063 | 0.215 | 0.206 |
| Within R2 | 0.009 | 0.005 | 0.025 | 0.014 |

|  | models$EU_co.. |models$NATO\_.. | models$EU_co.. |models$NATO\_.. |  |  |
|:---|:---|:---|:---|:---|
|  | EU | NATO | EU | NATO |
|  |  |  |  |  |
| Years since EU accession | 0.020\* (0.008) |  | 0.038 (0.099) |  |
| Years since NATO accession |  | 0.020\* (0.008) |  | 0.015 (0.072) |
| Cultural GAL–TAN | -0.074 (0.213) | -0.075 (0.213) | -0.157 (0.281) | -0.155 (0.281) |
| Economic left–right | 0.642 (0.436) | 0.648 (0.438) | 0.681 (0.447) | 0.683 (0.448) |
| Fixed-Effects: | ————– | ————– | ————– | ————– |
| Country | Yes | Yes | Yes | Yes |
| Year | No | No | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 326 | 326 | 326 | 326 |
| R2 | 0.185 | 0.186 | 0.252 | 0.252 |
| Within R2 | 0.053 | 0.055 | 0.055 | 0.055 |

## Narrative regressions

``` r
# Narratives
outcomes <- c(
  # Salience
  "West_pct_total",
  "Western_signifier_pct_total",
  "Western_countries_pct_total",
  
  # Combined narratives - overall
  "self_positive_pct",
  "self_negative_pct",
  "other_positive_pct",
  "other_negative_pct",
  
  # Combined narratives - West
  "self_positive_pct_West",
  "self_negative_pct_West",
  "other_positive_pct_West",
  "other_negative_pct_West",
  
  # Combined narratives - Europe
  "self_positive_pct_Western_signifier",
  "self_negative_pct_Western_signifier",
  "other_positive_pct_Western_signifier",
  "other_negative_pct_Western_signifier",
  
  # Combined narratives - Western countries
  "self_positive_pct_Western_countries",
  "self_negative_pct_Western_countries",
  "other_positive_pct_Western_countries",
  "other_negative_pct_Western_countries"
)

run_models <- function(y) {
  
  list(
    EU_noFE = feols(
      as.formula(paste(y, "~ EU_years + gal_tan + economic_lr")),
      data = Df_west,
      vcov = ~ party
    ),
    
    EU_countryFE = feols(
      as.formula(paste(
        y, "~ EU_years + gal_tan + economic_lr | country_code"
      )),
      data = Df_west,
      vcov = ~ party
    ),
    
    EU_country_yearFE = feols(
      as.formula(paste(
        y, "~ EU_years + gal_tan + economic_lr | country_code + year"
      )),
      data = Df_west,
      vcov = ~ party
    ),
    
    NATO_noFE = feols(
      as.formula(paste(y, "~ NATO_years + gal_tan + economic_lr")),
      data = Df_west,
      vcov = ~ party
    ),
    
    NATO_countryFE = feols(
      as.formula(paste(
        y, "~ NATO_years + gal_tan + economic_lr | country_code"
      )),
      data = Df_west,
      vcov = ~ party
    ),
    
    NATO_country_yearFE = feols(
      as.formula(paste(
        y, "~ NATO_years + gal_tan + economic_lr | country_code + year"
      )),
      data = Df_west,
      vcov = ~ party
    )
  )
}

# Estimate all models
models_by_outcome <- lapply(outcomes, run_models)
names(models_by_outcome) <- outcomes

knitr::kable(do.call(etable, models_by_outcome[["self_positive_pct"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_ye.. |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | self_positive_pct | self_positive_pct | self_positive_pct | self_positive_pct | self_positive_pct | self_positive_pct |
|  |  |  |  |  |  |  |
| Constant | 26.29\*\*\* (2.639) |  |  | 26.70\*\*\* (2.962) |  |  |
| EU_years | 0.7147\* (0.2834) | 0.6250\* (0.2999) | 0.0359 (2.840) |  |  |  |
| gal_tan | 0.8493 (3.344) | -4.279 (3.274) | -3.379 (3.279) | 1.219 (3.397) | -4.346 (3.268) | -3.377 (3.277) |
| economic_lr | 4.812 (4.704) | 3.588 (4.656) | 3.036 (4.753) | 4.781 (4.734) | 3.816 (4.685) | 3.029 (4.723) |
| NATO_years |  |  |  | 0.5203\* (0.2608) | 0.6341\* (0.2798) | -0.4696 (2.720) |
| Fixed-Effects: | —————– | —————– | —————– | —————– | —————– | —————– |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.02307 | 0.14702 | 0.23309 | 0.01738 | 0.15023 | 0.23312 |
| Within R2 | – | 0.02116 | 0.00519 | – | 0.02484 | 0.00524 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["self_negative_pct"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_ye.. |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | self_negative_pct | self_negative_pct | self_negative_pct | self_negative_pct | self_negative_pct | self_negative_pct |
|  |  |  |  |  |  |  |
| Constant | 0.5273 (0.3894) |  |  | 0.3917 (0.4265) |  |  |
| EU_years | 0.1777\* (0.0707) | 0.1843\* (0.0727) | 0.2089 (0.3281) |  |  |  |
| gal_tan | 0.4508 (0.4517) | 0.9519 (0.6925) | 1.101 (0.8306) | 0.4863 (0.4518) | 0.9481 (0.6875) | 1.107 (0.8307) |
| economic_lr | 2.133 (1.541) | 1.751 (1.431) | 2.167 (1.547) | 2.242 (1.579) | 1.790 (1.439) | 2.176 (1.543) |
| NATO_years |  |  |  | 0.1624\* (0.0713) | 0.1740\* (0.0688) | -0.1731 (0.2711) |
| Fixed-Effects: | —————– | —————– | —————– | —————– | —————– | —————– |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.04361 | 0.07594 | 0.13945 | 0.04685 | 0.07765 | 0.13936 |
| Within R2 | – | 0.04586 | 0.02708 | – | 0.04763 | 0.02698 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["other_positive_pct"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yea.. |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | other_positive_pct | other_positive_pct | other_positive_pct | other_positive_pct | other_positive_pct | other_positive_pct |
|  |  |  |  |  |  |  |
| Constant | 49.26\*\*\* (2.802) |  |  | 48.75\*\*\* (2.965) |  |  |
| EU_years | -1.733\*\*\* (0.3168) | -1.506\*\*\* (0.3102) | 3.945 (3.802) |  |  |  |
| gal_tan | -5.955 (3.662) | -3.015 (3.750) | -4.792 (3.714) | -6.739. (3.693) | -3.030 (3.739) | -4.681 (3.718) |
| economic_lr | -7.122. (3.954) | -5.298 (4.147) | -4.084 (4.211) | -7.281. (3.977) | -5.538 (4.165) | -3.740 (4.239) |
| NATO_years |  |  |  | -1.328\*\*\* (0.2674) | -1.384\*\*\* (0.2885) | 5.948\* (2.541) |
| Fixed-Effects: | —————— | —————— | —————— | —————— | —————— | —————— |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.13235 | 0.21607 | 0.28292 | 0.11002 | 0.21674 | 0.28551 |
| Within R2 | – | 0.09392 | 0.01428 | – | 0.09469 | 0.01783 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["other_negative_pct"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yea.. |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | other_negative_pct | other_negative_pct | other_negative_pct | other_negative_pct | other_negative_pct | other_negative_pct |
|  |  |  |  |  |  |  |
| Constant | 3.337\* (1.436) |  |  | 3.177\* (1.550) |  |  |
| EU_years | 0.1448 (0.1951) | 0.1140 (0.2071) | 0.4522 (1.047) |  |  |  |
| gal_tan | 5.146\*\* (1.927) | 7.104\*\*\* (2.051) | 8.857\*\*\* (2.271) | 5.163\*\* (1.916) | 7.219\*\*\* (2.069) | 8.873\*\*\* (2.288) |
| economic_lr | -3.426 (2.194) | -3.844. (2.024) | -5.366\* (2.503) | -3.312 (2.201) | -4.026. (2.076) | -5.394\* (2.487) |
| NATO_years |  |  |  | 0.1393 (0.1658) | 0.0118 (0.2125) | -2.820\* (1.401) |
| Fixed-Effects: | —————— | —————— | —————— | —————— | —————— | —————— |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.04192 | 0.12619 | 0.24770 | 0.04256 | 0.12470 | 0.25200 |
| Within R2 | – | 0.06177 | 0.09025 | – | 0.06017 | 0.09545 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["self_positive_pct_West"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | self_positive_pct_West | self_positive_pct_West | self_positive_pct_West | self_positive_pct_West | self_positive_pct_West | self_positive_pct_West |
|  |  |  |  |  |  |  |
| Constant | 11.58\*\* (3.589) |  |  | 12.82\*\* (3.940) |  |  |
| EU_years | 1.282\*\* (0.4446) | 1.229\*\* (0.4207) | -6.438 (5.802) |  |  |  |
| gal_tan | 11.64\* (4.954) | 5.773 (4.644) | 3.568 (5.980) | 12.27\* (5.192) | 5.792 (4.673) | 3.170 (5.966) |
| economic_lr | 12.22\* (6.101) | 7.315 (5.289) | 9.753 (6.710) | 11.75. (6.178) | 7.392 (5.293) | 9.136 (6.605) |
| NATO_years |  |  |  | 0.8285\* (0.3650) | 1.144\*\* (0.3650) | 1.735 (4.753) |
| Fixed-Effects: | ———————- | ———————- | ———————- | ———————- | ———————- | ———————- |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 | 155 | 155 |
| R2 | 0.09522 | 0.24468 | 0.30781 | 0.07679 | 0.24630 | 0.30399 |
| Within R2 | – | 0.06087 | 0.02024 | – | 0.06288 | 0.01485 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["self_negative_pct_West"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | self_negative_pct_West | self_negative_pct_West | self_negative_pct_West | self_negative_pct_West | self_negative_pct_West | self_negative_pct_West |
|  |  |  |  |  |  |  |
| Constant | 0.0605 (0.0692) |  |  | 0.1549 (0.1494) |  |  |
| EU_years | 0.0018 (0.0064) | 0.0059 (0.0116) | 0.6131 (0.6751) |  |  |  |
| gal_tan | 0.4516 (0.4706) | 0.5740 (0.5935) | 0.6376 (0.6473) | 0.4649 (0.4809) | 0.5747 (0.5939) | 0.6825 (0.6934) |
| economic_lr | -0.3024 (0.3793) | -0.2203 (0.3433) | -0.7323 (0.8883) | -0.3531 (0.4272) | -0.2225 (0.3447) | -0.6746 (0.8341) |
| NATO_years |  |  |  | -0.0112 (0.0143) | 0.0045 (0.0099) | 0.1274 (0.1415) |
| Fixed-Effects: | ———————- | ———————- | ———————- | ———————- | ———————- | ———————- |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 | 155 | 155 |
| R2 | 0.00890 | 0.04132 | 0.16566 | 0.00958 | 0.04129 | 0.16070 |
| Within R2 | – | 0.01128 | 0.02664 | – | 0.01125 | 0.02085 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["other_positive_pct_West"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | other_positive_pct_West | other_positive_pct_West | other_positive_pct_West | other_positive_pct_West | other_positive_pct_West | other_positive_pct_West |
|  |  |  |  |  |  |  |
| Constant | 54.47\*\*\* (6.260) |  |  | 54.56\*\*\* (6.338) |  |  |
| EU_years | -0.7664 (0.6598) | -0.5336 (0.6544) | 14.09 (10.70) |  |  |  |
| gal_tan | -5.832 (6.921) | -1.455 (7.211) | -3.668 (7.301) | -6.098 (6.972) | -1.468 (7.193) | -2.675 (7.283) |
| economic_lr | 1.594 (9.965) | 5.772 (9.192) | 10.86 (9.065) | 1.431 (9.952) | 5.760 (9.209) | 12.19 (9.141) |
| NATO_years |  |  |  | -0.6067 (0.5140) | -0.4888 (0.5956) | 1.387 (4.124) |
| Fixed-Effects: | ———————– | ———————– | ———————– | ———————– | ———————– | ———————– |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 | 155 | 155 |
| R2 | 0.01683 | 0.13068 | 0.33214 | 0.01589 | 0.13072 | 0.32057 |
| Within R2 | – | 0.00933 | 0.03115 | – | 0.00937 | 0.01436 |

``` r
knitr::kable(do.call(etable, models_by_outcome[["other_negative_pct_West"]]), format = "pipe")
```

|  | EU_noFE | EU_countryFE | EU_country_yearFE | NATO_noFE | NATO_countryFE | NATO_country_yearFE |
|:---|:---|:---|:---|:---|:---|:---|
| Dependent Var.: | other_negative_pct_West | other_negative_pct_West | other_negative_pct_West | other_negative_pct_West | other_negative_pct_West | other_negative_pct_West |
|  |  |  |  |  |  |  |
| Constant | 8.370. (4.586) |  |  | 6.713 (4.568) |  |  |
| EU_years | -0.1303 (0.4314) | -0.3060 (0.4602) | 0.8647 (2.671) |  |  |  |
| gal_tan | 1.161 (5.989) | 3.485 (5.734) | 5.735 (5.910) | 0.8924 (5.863) | 3.578 (5.743) | 5.784 (5.893) |
| economic_lr | -11.83 (8.030) | -11.37 (7.322) | -17.47\*\* (6.346) | -10.96 (8.101) | -11.85 (7.276) | -17.39\*\* (6.250) |
| NATO_years |  |  |  | 0.1206 (0.3548) | -0.4576 (0.4579) | -0.3875 (2.732) |
| Fixed-Effects: | ———————– | ———————– | ———————– | ———————– | ———————– | ———————– |
| country_code | No | Yes | Yes | No | Yes | Yes |
| year | No | No | Yes | No | No | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 | 155 | 155 |
| R2 | 0.02618 | 0.14626 | 0.39089 | 0.02633 | 0.15176 | 0.39082 |
| Within R2 | – | 0.03079 | 0.07949 | – | 0.03704 | 0.07938 |

``` r
latex_overall <- knitr::kable(etable(
  # Self + Positive
  models_by_outcome[["self_positive_pct"]]$EU_countryFE,
  models_by_outcome[["self_positive_pct"]]$NATO_countryFE,
  
  # Self + Negative
  models_by_outcome[["self_negative_pct"]]$EU_countryFE,
  models_by_outcome[["self_negative_pct"]]$NATO_countryFE,
  
  # Other + Positive
  models_by_outcome[["other_positive_pct"]]$EU_countryFE,
  models_by_outcome[["other_positive_pct"]]$NATO_countryFE,
  
  # Other + Negative
  models_by_outcome[["other_negative_pct"]]$EU_countryFE,
  models_by_outcome[["other_negative_pct"]]$NATO_countryFE,

  depvar = FALSE,
  digits = 3,
  digits.stats = 3,
  
  title = "Self and Other Narratives of the West",

  headers = list(
    "Narrative" = c(
      "Self + Positive" = 2,
      "Self + Negative" = 2,
      "Other + Positive" = 2,
      "Other + Negative" = 2
    ),
    "Integration" = rep(c("EU", "NATO"), 4)
  ),
  
  dict = c(
    EU_years = "Years since EU accession",
    NATO_years = "Years since NATO accession",
    gal_tan = "Cultural GAL--TAN",
    economic_lr = "Economic left--right",
    country_code = "Country"
  ),
  
  order = c(
    "Years since EU",
    "Years since NATO",
    "Cultural",
    "Economic"
  ),
  
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05
  ),
  
  fitstat = ~ n + r2 + wr2
), format = "pipe")

# Display the table
cat("\n", latex_overall, "\n\n", sep = "\n")
```

|  | models_by_ou.. | models_by_ou.. | models_by_ou.. | models_by_ou.. | models_by_outc.. | models_by_outc.. | models_by_ou.. | models_by_ou.. |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Narrative | Self + Positive | Self + Positive | Self + Negative | Self + Negative | Other + Positive | Other + Positive | Other + Negative | Other + Negative |
| Integration | EU | NATO | EU | NATO | EU | NATO | EU | NATO |
|  |  |  |  |  |  |  |  |  |
| Years since EU accession | 0.625\* (0.300) |  | 0.184\* (0.073) |  | -1.51\*\*\* (0.310) |  | 0.114 (0.207) |  |
| Years since NATO accession |  | 0.634\* (0.280) |  | 0.174\* (0.069) |  | -1.38\*\*\* (0.288) |  | 0.012 (0.213) |
| Cultural GAL–TAN | -4.28 (3.27) | -4.35 (3.27) | 0.952 (0.693) | 0.948 (0.688) | -3.02 (3.75) | -3.03 (3.74) | 7.10\*\*\* (2.05) | 7.22\*\*\* (2.07) |
| Economic left–right | 3.59 (4.66) | 3.82 (4.69) | 1.75 (1.43) | 1.79 (1.44) | -5.30 (4.15) | -5.54 (4.17) | -3.84 (2.02) | -4.03 (2.08) |
| Fixed-Effects: | ————– | ————– | ————– | ————– | —————- | —————- | ————– | ————– |
| Country | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.147 | 0.150 | 0.076 | 0.078 | 0.216 | 0.217 | 0.126 | 0.125 |
| Within R2 | 0.021 | 0.025 | 0.046 | 0.048 | 0.094 | 0.095 | 0.062 | 0.060 |

``` r
# Year FE 
latex_narratives_yearFE <- knitr::kable(etable(
  # Self + Positive
  models_by_outcome[["self_positive_pct"]]$EU_country_yearFE,
  models_by_outcome[["self_positive_pct"]]$NATO_country_yearFE,
  
  # Self + Negative
  models_by_outcome[["self_negative_pct"]]$EU_country_yearFE,
  models_by_outcome[["self_negative_pct"]]$NATO_country_yearFE,
  
  # Other + Positive
  models_by_outcome[["other_positive_pct"]]$EU_country_yearFE,
  models_by_outcome[["other_positive_pct"]]$NATO_country_yearFE,
  
  # Other + Negative
  models_by_outcome[["other_negative_pct"]]$EU_country_yearFE,
  models_by_outcome[["other_negative_pct"]]$NATO_country_yearFE,

  depvar = FALSE,
  digits = 3,
  digits.stats = 3,
  
  title = "Self and Other Narratives of the West: Country and Year Fixed Effects",

  headers = list(
    "Narrative" = c(
      "Self + Positive" = 2,
      "Self + Negative" = 2,
      "Other + Positive" = 2,
      "Other + Negative" = 2
    ),
    "Integration" = rep(c("EU", "NATO"), 4)
  ),
  
  dict = c(
    EU_years = "Years since EU accession",
    NATO_years = "Years since NATO accession",
    gal_tan = "Cultural GAL--TAN",
    economic_lr = "Economic left--right",
    country_code = "Country",
    year = "Year"
  ),
  
  order = c(
    "Years since EU",
    "Years since NATO",
    "Cultural",
    "Economic"
  ),
  
  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05
  ),
  
  fitstat = ~ n + r2 + wr2
), format = "pipe")

# Display the table
cat("\n", latex_narratives_yearFE, "\n\n", sep = "\n")
```

|  | models_by\_.. | models_by_o.. | models_by_o.. | models_by_ou.. | models_by\_.. | models_by\_.. | models_by_ou.. | models_by_ou.. |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Narrative | Self + Positive | Self + Positive | Self + Negative | Self + Negative | Other + Positive | Other + Positive | Other + Negative | Other + Negative |
| Integration | EU | NATO | EU | NATO | EU | NATO | EU | NATO |
|  |  |  |  |  |  |  |  |  |
| Years since EU accession | 0.036 (2.84) |  | 0.209 (0.328) |  | 3.95 (3.80) |  | 0.452 (1.05) |  |
| Years since NATO accession |  | -0.470 (2.72) |  | -0.173 (0.271) |  | 5.95\* (2.54) |  | -2.82\* (1.40) |
| Cultural GAL–TAN | -3.38 (3.28) | -3.38 (3.28) | 1.10 (0.831) | 1.11 (0.831) | -4.79 (3.71) | -4.68 (3.72) | 8.86\*\*\* (2.27) | 8.87\*\*\* (2.29) |
| Economic left–right | 3.04 (4.75) | 3.03 (4.72) | 2.17 (1.55) | 2.18 (1.54) | -4.08 (4.21) | -3.74 (4.24) | -5.37\* (2.50) | -5.39\* (2.49) |
| Fixed-Effects: | ———— | ————- | ————- | ————– | ———— | ———— | ————– | ————– |
| Country | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| Year | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 | 301 | 301 | 301 | 301 |
| R2 | 0.233 | 0.233 | 0.139 | 0.139 | 0.283 | 0.286 | 0.248 | 0.252 |
| Within R2 | 0.005 | 0.005 | 0.027 | 0.027 | 0.014 | 0.018 | 0.090 | 0.096 |

## Regressions by referent

``` r
# Referent categories and their corresponding variable suffixes
referents <- c(
  Europe = "Western_signifier",
  West = "West",
  Western_countries = "Western_countries"
)

narratives <- c(
  "self_positive",
  "self_negative",
  "other_positive",
  "other_negative"
)


referents <- c(
  Europe = "Western_signifier",
  West = "West",
  Western_countries = "Western_countries"
)

for (ref in names(referents)) {
  
  suffix <- referents[[ref]]
  
  # Retrieve already estimated models
  sp <- models_by_outcome[[paste0("self_positive_pct_", suffix)]]
  sn <- models_by_outcome[[paste0("self_negative_pct_", suffix)]]
  op <- models_by_outcome[[paste0("other_positive_pct_", suffix)]]
  on <- models_by_outcome[[paste0("other_negative_pct_", suffix)]]
  
  latex <- knitr::kable(etable(
    sp$EU_countryFE,
    sp$NATO_countryFE,
    
    sn$EU_countryFE,
    sn$NATO_countryFE,
    
    op$EU_countryFE,
    op$NATO_countryFE,
    
    on$EU_countryFE,
    on$NATO_countryFE,

    depvar = FALSE,
    digits = 3,
    digits.stats = 3,
    
    title = paste0(
      "Self and Other Narratives of ",
      gsub("_", " ", ref)
    ),
    
    headers = list(
      "Narrative" = c(
        "Self + Positive" = 2,
        "Self + Negative" = 2,
        "Other + Positive" = 2,
        "Other + Negative" = 2
      ),
      "Integration" = rep(c("EU", "NATO"), 4)
    ),
    
    dict = c(
      EU_years = "Years since EU accession",
      NATO_years = "Years since NATO accession",
      gal_tan = "Cultural GAL--TAN",
      economic_lr = "Economic left--right",
      country_code = "Country"
    ),
    
    order = c(
      "Years since EU",
      "Years since NATO",
      "Cultural",
      "Economic"
    ),
    
    signif.code = c(
      "***" = 0.001,
      "**" = 0.01,
      "*" = 0.05
    ),
    
    fitstat = ~ n + r2 + wr2
  ), format = "pipe")
  
  cat("\n", latex, "\n\n", sep = "\n")
}
```

|  | sp$EU_countryFE |sp$NATO_count.. | sn$EU_countryFE |sn$NATO_count.. | op$EU_countryFE  |op$NATO_countr.. | on$EU_count..    |on$NATO_cou.. |  |  |  |  |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Narrative | Self + Positive | Self + Positive | Self + Negative | Self + Negative | Other + Positive | Other + Positive | Other + Negative | Other + Negative |
| Integration | EU | NATO | EU | NATO | EU | NATO | EU | NATO |
|  |  |  |  |  |  |  |  |  |
| Years since EU accession | 0.976\*\* (0.319) |  | 0.239\*\* (0.081) |  | -2.10\*\*\* (0.349) |  | 0.146 (0.120) |  |
| Years since NATO accession |  | 0.966\*\* (0.289) |  | 0.224\*\* (0.076) |  | -1.97\*\*\* (0.323) |  | 0.115 (0.116) |
| Cultural GAL–TAN | -1.47 (3.35) | -1.62 (3.36) | 1.51 (0.887) | 1.48 (0.886) | -7.32 (4.22) | -7.13 (4.21) | 5.17\*\* (1.59) | 5.19\*\* (1.60) |
| Economic left–right | 3.13 (4.84) | 3.57 (4.88) | 2.25 (1.83) | 2.32 (1.85) | -9.10 (4.71) | -9.79\* (4.74) | -1.97 (1.87) | -1.97 (1.90) |
| Fixed-Effects: | ————— | ————— | ————— | ————— | —————- | —————- | ————- | ————- |
| Country | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 285 | 285 | 285 | 285 | 285 | 285 | 285 | 285 |
| R2 | 0.133 | 0.137 | 0.088 | 0.089 | 0.276 | 0.280 | 0.123 | 0.121 |
| Within R2 | 0.037 | 0.042 | 0.056 | 0.057 | 0.169 | 0.173 | 0.055 | 0.054 |

|  | sp$EU_countr..  |sp$NATO_coun.. | sn$EU_countr..  |sn$NATO_coun.. | op$EU_countr..   |op$NATO_coun.. | on$EU_countr..   |on$NATO_coun.. |  |  |  |  |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Narrative | Self + Positive | Self + Positive | Self + Negative | Self + Negative | Other + Positive | Other + Positive | Other + Negative | Other + Negative |
| Integration | EU | NATO | EU | NATO | EU | NATO | EU | NATO |
|  |  |  |  |  |  |  |  |  |
| Years since EU accession | 1.23\*\* (0.421) |  | 0.006 (0.012) |  | -0.534 (0.654) |  | -0.306 (0.460) |  |
| Years since NATO accession |  | 1.14\*\* (0.365) |  | 0.005 (0.010) |  | -0.489 (0.596) |  | -0.458 (0.458) |
| Cultural GAL–TAN | 5.77 (4.64) | 5.79 (4.67) | 0.574 (0.593) | 0.575 (0.594) | -1.45 (7.21) | -1.47 (7.19) | 3.48 (5.73) | 3.58 (5.74) |
| Economic left–right | 7.31 (5.29) | 7.39 (5.29) | -0.220 (0.343) | -0.223 (0.345) | 5.77 (9.19) | 5.76 (9.21) | -11.4 (7.32) | -11.8 (7.28) |
| Fixed-Effects: | ————– | ————– | ————– | ————– | ————– | ————– | ————– | ————– |
| Country | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 | 155 | 155 | 155 | 155 |
| R2 | 0.245 | 0.246 | 0.041 | 0.041 | 0.131 | 0.131 | 0.146 | 0.152 |
| Within R2 | 0.061 | 0.063 | 0.011 | 0.011 | 0.009 | 0.009 | 0.031 | 0.037 |

|  | sp$EU_countr..  |sp$NATO_coun.. | sn$EU_countr..  |sn$NATO_coun.. | op$EU_count..    |op$NATO_cou.. | on$EU_count..    |on$NATO_cou.. |  |  |  |  |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| Narrative | Self + Positive | Self + Positive | Self + Negative | Self + Negative | Other + Positive | Other + Positive | Other + Negative | Other + Negative |
| Integration | EU | NATO | EU | NATO | EU | NATO | EU | NATO |
|  |  |  |  |  |  |  |  |  |
| Years since EU accession | -0.450 (0.538) |  | -0.012 (0.024) |  | 0.351 (0.571) |  | 0.204 (0.291) |  |
| Years since NATO accession |  | -0.376 (0.512) |  | -0.008 (0.022) |  | 0.255 (0.536) |  | 0.182 (0.265) |
| Cultural GAL–TAN | -8.78 (5.01) | -8.83 (4.98) | -0.360 (0.318) | -0.366 (0.320) | 8.59 (5.54) | 8.71 (5.58) | 10.2\* (3.90) | 10.2\* (3.91) |
| Economic left–right | -1.75 (7.71) | -1.71 (7.77) | 0.051 (0.519) | 0.060 (0.520) | 6.33 (6.99) | 6.17 (7.02) | -4.50 (4.01) | -4.48 (4.05) |
| Fixed-Effects: | ————– | ————– | ————– | ————– | ————- | ————- | ————- | ————- |
| Country | Yes | Yes | Yes | Yes | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party | by: party | by: party | by: party | by: party |
| Observations | 202 | 202 | 202 | 202 | 202 | 202 | 202 | 202 |
| R2 | 0.202 | 0.201 | 0.077 | 0.076 | 0.115 | 0.114 | 0.116 | 0.116 |
| Within R2 | 0.020 | 0.019 | 0.011 | 0.011 | 0.020 | 0.019 | 0.061 | 0.061 |

## Coefficient plots

``` r
library(broom)

referents <- c(
  Europe = "Western_signifier",
  West = "West",
  "Western countries" = "Western_countries"
)

narratives <- c(
  "Self + Positive" = "self_positive",
  "Self + Negative" = "self_negative",
  "Other + Positive" = "other_positive",
  "Other + Negative" = "other_negative"
)


# Extract existing country-FE coefficients
coef_integration <- list()

for (ref in names(referents)) {
  for (narr in names(narratives)) {
    
    y <- paste0(
      narratives[[narr]], "_pct_", referents[[ref]]
    )
    
    for (integration in c("EU", "NATO")) {
      
      model <- models_by_outcome[[y]][[
        paste0(integration, "_countryFE")
      ]]
      
      coef_integration[[length(coef_integration) + 1]] <-
        tidy(model, conf.int = TRUE) %>%
        filter(term %in% c("EU_years", "NATO_years")) %>%
        mutate(
          referent = ref,
          narrative = narr,
          integration = integration
        )
    }
  }
}

coef_integration <- bind_rows(coef_integration) %>%
  mutate(
    referent = factor(
      referent,
      levels = c("Western countries", "Europe", "West")
    ),
    narrative = factor(
      narrative,
      levels = names(narratives)
    )
  )

p_integration <- ggplot(
  coef_integration,
  aes(x = estimate, y = referent)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.15,
    color = "#328CC1"
  ) +
  geom_point(size = 2.5, color = "#0B3C5D") +
  facet_grid(narrative ~ integration, scales = "free_x") +
  labs(
    x = "Percentage of mentions",
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_integration
```

![](10_Analysis_GitHub_files/figure-gfm/section-10-1.png)<!-- -->

``` r
ggsave(
  "integration_coefficients.png",
  p_integration,
  width = 8,
  height = 8
)

models_ideology <- list()

for (ref in names(referents)) {
  for (narr in names(narratives)) {
    
    y <- paste0(
      narratives[[narr]], "_pct_", referents[[ref]]
    )
    
    models_ideology[[y]] <- feols(
      as.formula(
        paste(
          y,
          "~ gal_tan + economic_lr | country_code + year"
        )
      ),
      data = Df_west,
      vcov = ~ party
    )
  }
}

coef_ideology <- list()

for (ref in names(referents)) {
  for (narr in names(narratives)) {
    
    y <- paste0(
      narratives[[narr]], "_pct_", referents[[ref]]
    )
    
    coef_ideology[[length(coef_ideology) + 1]] <-
      tidy(models_ideology[[y]], conf.int = TRUE) %>%
      filter(term %in% c("gal_tan", "economic_lr")) %>%
      mutate(
        referent = ref,
        narrative = narr
      )
  }
}

coef_ideology <- bind_rows(coef_ideology) %>%
  mutate(
    referent = factor(
      referent,
      levels = c("Western countries", "Europe", "West")
    ),
    narrative = factor(
      narrative,
      levels = names(narratives)
    ),
    ideology = dplyr::recode(
      term,
      gal_tan = "Cultural GAL–TAN",
      economic_lr = "Economic left–right"
    )
  )

p_ideology <- ggplot(
  coef_ideology,
  aes(x = estimate, y = referent)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    orientation = "y",
    width = 0.15,
    color = "#328CC1"
  ) +
  geom_point(size = 2.5, color = "#0B3C5D") +
  facet_grid(narrative ~ ideology, scales = "free_x") +
  labs(
    x = "Percentage of mentions",
    y = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    strip.background = element_rect(fill = "grey95"),
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

p_ideology
```

![](10_Analysis_GitHub_files/figure-gfm/section-10-2.png)<!-- -->

``` r
ggsave(
  "ideology_coefficients.png",
  p_ideology,
  width = 8,
  height = 8
)
```

## Ideology regression tables

``` r
# tables for appendix of the graphs on ideology

referents <- c(
  West = "West",
  Europe = "Western_signifier",
  Western_countries = "Western_countries"
)

table_titles <- c(
  West = "Party Ideology and Narratives of the West",
  Europe = "Party Ideology and Narratives of Europe",
  Western_countries = "Party Ideology and Narratives of Western Countries"
)

for (ref in names(referents)) {
  
  suffix <- referents[[ref]]
  
  sp <- models_ideology[[paste0("self_positive_pct_", suffix)]]
  sn <- models_ideology[[paste0("self_negative_pct_", suffix)]]
  op <- models_ideology[[paste0("other_positive_pct_", suffix)]]
  on <- models_ideology[[paste0("other_negative_pct_", suffix)]]
  
  latex <- knitr::kable(etable(
    sp, sn, op, on,

    depvar = FALSE,
    digits = 3,
    digits.stats = 3,
    
    title = unname(table_titles[[ref]]),
    label = paste0("tab:ideology_", tolower(ref)),
    
    headers = c(
      "Self + Positive",
      "Self + Negative",
      "Other + Positive",
      "Other + Negative"
    ),
    
    dict = c(
      gal_tan = "Cultural GAL--TAN",
      economic_lr = "Economic left--right",
      country_code = "Country FE",
      year = "Year FE"
    ),

    signif.code = c(
      "***" = 0.001,
      "**" = 0.01,
      "*" = 0.05
    ),
    
    fitstat = ~ n + r2 + wr2,
    
    notes = c(
      "Standard errors clustered by party in parentheses.",
      "*** $p<0.001$, ** $p<0.01$, * $p<0.05$."
    )
  ), format = "pipe")
  
  # Display the table
  cat("\n", latex, "\n\n", sep = "\n")
}
```

|  | sp | sn | op | on |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN | 3.13 (5.94) | 0.679 (0.688) | -2.71 (7.24) | 5.79 (5.84) |
| Economic left–right | 9.14 (6.54) | -0.674 (0.831) | 12.2 (9.14) | -17.4\*\* (6.23) |
| Fixed-Effects: | ———– | ————– | ———— | ————– |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 155 | 155 | 155 | 155 |
| R2 | 0.303 | 0.160 | 0.320 | 0.391 |
| Within R2 | 0.014 | 0.020 | 0.014 | 0.079 |

|  | sp | sn | op | on |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN | -1.55 (3.29) | 1.96 (1.19) | -7.91 (4.17) | 5.65\*\* (1.78) |
| Economic left–right | 4.04 (5.24) | 2.84 (2.03) | -9.98\* (4.79) | -1.99 (2.06) |
| Fixed-Effects: | ———— | ———– | ————- | ————- |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 285 | 285 | 285 | 285 |
| R2 | 0.214 | 0.148 | 0.353 | 0.173 |
| Within R2 | 0.003 | 0.038 | 0.037 | 0.049 |

|  | sp | sn | op | on |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN | -8.73 (5.59) | -0.492 (0.395) | 8.15 (5.64) | 11.8\* (4.61) |
| Economic left–right | -1.89 (9.37) | 0.300 (0.586) | 4.19 (8.25) | -6.19 (4.98) |
| Fixed-Effects: | ———— | ————– | ———– | ———— |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 202 | 202 | 202 | 202 |
| R2 | 0.306 | 0.142 | 0.246 | 0.181 |
| Within R2 | 0.014 | 0.017 | 0.014 | 0.067 |

## Overall ideology regressions

``` r
# Aggregated narratives across all Western referents
overall_narratives <- c(
  "self_positive_pct",
  "self_negative_pct",
  "other_positive_pct",
  "other_negative_pct"
)

models_ideology_overall <- lapply(overall_narratives, function(y) {
  feols(
    as.formula(
      paste(y, "~ gal_tan + economic_lr | country_code + year")
    ),
    data = Df_west,
    vcov = ~ party
  )
})

names(models_ideology_overall) <- overall_narratives

latex_overall <- knitr::kable(etable(
  models_ideology_overall[["self_positive_pct"]],
  models_ideology_overall[["self_negative_pct"]],
  models_ideology_overall[["other_positive_pct"]],
  models_ideology_overall[["other_negative_pct"]],

  depvar = FALSE,
  digits = 3,
  digits.stats = 3,
  
  title = "Party Ideology and Narratives of Western Actors",

  headers = c(
    "Self + Positive",
    "Self + Negative",
    "Other + Positive",
    "Other + Negative"
  ),
  
  dict = c(
    gal_tan = "Cultural GAL--TAN",
    economic_lr = "Economic left--right",
    country_code = "Country FE",
    year = "Year FE"
  ),

  signif.code = c(
    "***" = 0.001,
    "**" = 0.01,
    "*" = 0.05
  ),
  
  fitstat = ~ n + r2 + wr2
), format = "pipe")

cat("\n", latex_overall, "\n\n", sep = "\n")
```

|  | models_ide.. | models_ide.. | models_ide.. | models_ideol.. |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN | -3.38 (3.27) | 1.11 (0.829) | -4.68 (3.67) | 8.87\*\*\* (2.26) |
| Economic left–right | 3.04 (4.72) | 2.18 (1.54) | -3.85 (4.22) | -5.34\* (2.48) |
| Fixed-Effects: | ———— | ———— | ———— | ————– |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 301 | 301 | 301 | 301 |
| R2 | 0.233 | 0.139 | 0.280 | 0.248 |
| Within R2 | 0.005 | 0.027 | 0.010 | 0.090 |

## Alternative ideology measures

``` r
# With Parlgov

# Outcomes grouped by referent
ideology_outcomes <- list(
  Overall = c(
    "self_positive_pct",
    "self_negative_pct",
    "other_positive_pct",
    "other_negative_pct"
  ),
  
  West = c(
    "self_positive_pct_West",
    "self_negative_pct_West",
    "other_positive_pct_West",
    "other_negative_pct_West"
  ),
  
  Europe = c(
    "self_positive_pct_Western_signifier",
    "self_negative_pct_Western_signifier",
    "other_positive_pct_Western_signifier",
    "other_negative_pct_Western_signifier"
  ),
  
  Western_countries = c(
    "self_positive_pct_Western_countries",
    "self_negative_pct_Western_countries",
    "other_positive_pct_Western_countries",
    "other_negative_pct_Western_countries"
  )
)

# Estimate models using ParlGov ideology
models_ideology_parlgov <- lapply(
  ideology_outcomes,
  function(outcomes) {
    models <- lapply(outcomes, function(y) {
      feols(
        as.formula(
          paste(y, "~ galtan + lrecon | country_code + year")
        ),
        data = Df_west,
        vcov = ~ party
      )
    })
    
    names(models) <- c(
      "Self_Positive",
      "Self_Negative",
      "Other_Positive",
      "Other_Negative"
    )
    
    models
  }
)

table_titles <- c(
  Overall = "Party Ideology and Narratives of Western Actors",
  West = "Party Ideology and Narratives of the West",
  Europe = "Party Ideology and Narratives of Europe",
  Western_countries = "Party Ideology and Narratives of Western Countries"
)

for (ref in names(models_ideology_parlgov)) {
  
  models <- models_ideology_parlgov[[ref]]
  
  latex <- knitr::kable(etable(
    models$Self_Positive,
    models$Self_Negative,
    models$Other_Positive,
    models$Other_Negative,

    depvar = FALSE,
    digits = 3,
    digits.stats = 3,
    
    title = unname(table_titles[[ref]]),
    label = paste0("tab:ideology_parlgov_", tolower(ref)),
    
    headers = c(
      "Self + Positive",
      "Self + Negative",
      "Other + Positive",
      "Other + Negative"
    ),
    
    dict = c(
      galtan = "Cultural GAL--TAN (ParlGov)",
      lrecon = "Economic left--right (ParlGov)",
      country_code = "Country FE",
      year = "Year FE"
    ),

    signif.code = c(
      "***" = 0.001,
      "**" = 0.01,
      "*" = 0.05
    ),
    
    fitstat = ~ n + r2 + wr2,
    
    notes = c(
      "Standard errors clustered by party in parentheses.",
      "*** $p<0.001$, ** $p<0.01$, * $p<0.05$."
    )
  ), format = "pipe")
  
  cat("\n", latex, "\n\n", sep = "\n")
}
```

|  | models$Self..   |models$Self.. | models$Other_..  |models$Other.. |  |  |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN (ParlGov) | 0.427 (0.855) | 0.333 (0.213) | -2.17\*\* (0.756) | 1.14\* (0.469) |
| Economic left–right (ParlGov) | 1.71 (1.00) | 0.386 (0.302) | -1.63 (0.863) | -0.150 (0.560) |
| Fixed-Effects: | ————- | ————- | ————— | ————– |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 187 | 187 | 187 | 187 |
| R2 | 0.248 | 0.214 | 0.321 | 0.228 |
| Within R2 | 0.022 | 0.034 | 0.060 | 0.067 |

|  | models$Sel..    |models$Self\_.. | models$Oth..     |models$Oth.. |  |  |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN (ParlGov) | 0.483 (1.15) | 0.088 (0.115) | -2.53 (1.64) | 1.24 (1.46) |
| Economic left–right (ParlGov) | 3.13 (1.58) | -0.221 (0.238) | 0.397 (2.76) | -3.28 (1.80) |
| Fixed-Effects: | ———— | ————– | ———— | ———— |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 92 | 92 | 92 | 92 |
| R2 | 0.253 | 0.208 | 0.280 | 0.442 |
| Within R2 | 0.037 | 0.024 | 0.029 | 0.144 |

|  | models$Self..   |models$Self.. | models$Other_P.. |models$Other.. |  |  |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN (ParlGov) | 0.696 (0.823) | 0.494 (0.253) | -2.97\*\*\* (0.851) | 1.07\* (0.516) |
| Economic left–right (ParlGov) | 1.21 (0.884) | 0.379 (0.313) | -2.19\* (0.910) | -0.013 (0.532) |
| Fixed-Effects: | ————- | ————- | —————- | ————– |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 180 | 180 | 180 | 180 |
| R2 | 0.282 | 0.217 | 0.370 | 0.194 |
| Within R2 | 0.014 | 0.038 | 0.095 | 0.049 |

|  | models$Self..   |models$Self_N.. | models$Oth..     |models$Oth.. |  |  |
|:---|:---|:---|:---|:---|
|  | Self + Positive | Self + Negative | Other + Positive | Other + Negative |
|  |  |  |  |  |
| Cultural GAL–TAN (ParlGov) | -0.578 (1.56) | -0.148\* (0.065) | 1.15 (1.22) | 1.88 (0.964) |
| Economic left–right (ParlGov) | 1.46 (2.45) | 0.059 (0.092) | 0.110 (1.83) | 0.523 (1.46) |
| Fixed-Effects: | ————- | ————— | ———— | ———— |
| Country FE | Yes | Yes | Yes | Yes |
| Year FE | Yes | Yes | Yes | Yes |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ | \_\_\_\_\_\_\_\_\_\_\_\_ |
| S.E.: Clustered | by: party | by: party | by: party | by: party |
| Observations | 132 | 132 | 132 | 132 |
| R2 | 0.323 | 0.194 | 0.308 | 0.200 |
| Within R2 | 0.008 | 0.043 | 0.007 | 0.050 |
