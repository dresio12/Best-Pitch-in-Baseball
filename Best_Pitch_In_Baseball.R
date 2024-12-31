#The Best Pitch in Baseball

#load libraries
library(tidyverse)
library(baseballr)

#All Pitch Run Values from Statcast
#
#imported from my GitHub, 2019-2024, only qualified pitchers (minimum 10 PA)
stats <- read.csv("https://raw.githubusercontent.com/dresio12/Best-Pitch-in-Baseball/main/pitch_arsenal_stats.csv")

#change col1 to name
colnames(stats)[1] <- 'name'

#determine active players
stats <- stats %>%
  group_by(name) %>%
  mutate(active_in_2024 = ifelse(any(year == 2024), "Yes", "No")) %>%
  ungroup()


# Calculate the number of distinct years a player used each pitch
stats <- stats %>%
  group_by(name, pitch_type) %>%
  mutate(num_yrs_pitch = n_distinct(year)) %>%
  ungroup()

# Calculate average stats per pitch across all seasons while retaining `num_yrs_pitch`
stats_avg <- stats %>%
  group_by(name, pitch_type, pitch_name, active_in_2024, num_yrs_pitch) %>%
  summarize(
    rvp100_avg = mean(run_value_per_100),
    rv_avg = mean(run_value),
    pitches_avg = mean(pitches),
    pitch_usage_avg = mean(pitch_usage),
    ba_avg = mean(ba),
    slg_avg = mean(slg),
    woba_avg = mean(woba),
    whiff_avg = mean(whiff_percent),
    k_avg = mean(k_percent),
    putaway_avg = mean(put_away, na.rm = TRUE),
    xba_avg = mean(est_ba),
    xslg_avg = mean(est_slg),
    xwoba_avg = mean(est_woba),
    hhp_avg = mean(hard_hit_percent)
  ) %>%
  filter(pitches_avg >= 200) %>%
  ungroup()

# Convert pitch_usage to 0-1 scale
stats_avg <- stats_avg %>%
  mutate(pitch_usage_avg = pitch_usage_avg / 100)


# Add a bonus based on the number of years the pitch was used
stats_avg <- stats_avg %>%
  mutate(
    yrs_comp = case_when(
      num_yrs_pitch == 6 ~ 0.2,
      num_yrs_pitch == 5 ~ 0.15,
      num_yrs_pitch == 4 ~ 0.1,
      num_yrs_pitch == 3 ~ 0.05,
      TRUE ~ 0 # No bonus for fewer than 3 years of pitch usage
    )
  )

#creating a single "score" for each pitch
#
#testing two methods using PCA to weight the variables
#
# 1) averaging the seasons and getting a single score
#and
# 2) computing the score for each season and averaging the scores
#
#



#Averaged stats, single score approach

#Lorenzen's sinker has a NA in putaway%, it is not going to be a contender
#for best pitch so I am removing it
# Filter out rows with NA in putaway%
stats_avg <- stats_avg[!is.na(stats_avg$putaway_avg), ]

# Add a bonus based on the number of years the pitch was used
stats_avg <- stats_avg %>%
  mutate(
    yrs_comp = case_when(
      num_yrs_pitch == 6 ~ 0.2,
      num_yrs_pitch == 5 ~ 0.15,
      num_yrs_pitch == 4 ~ 0.1,
      num_yrs_pitch == 3 ~ 0.05,
      TRUE ~ 0 # No bonus for fewer than 3 years of pitch usage
    )
  )

# Normalize averages using z-scores within each pitch type
stats_avg <- stats_avg %>%
  group_by(pitch_type) %>%
  mutate(
    z_rvp100 = (rvp100_avg - mean(rvp100_avg, na.rm = TRUE)) / sd(rvp100_avg, na.rm = TRUE),
    z_rv = (rv_avg - mean(rv_avg, na.rm = TRUE)) / sd(rv_avg, na.rm = TRUE),
    z_ba = (ba_avg - mean(ba_avg, na.rm = TRUE)) / sd(ba_avg, na.rm = TRUE),
    z_slg = (slg_avg - mean(slg_avg, na.rm = TRUE)) / sd(slg_avg, na.rm = TRUE),
    z_woba = (woba_avg - mean(woba_avg, na.rm = TRUE)) / sd(woba_avg, na.rm = TRUE),
    z_whiff = (whiff_avg - mean(whiff_avg, na.rm = TRUE)) / sd(whiff_avg, na.rm = TRUE),
    z_k = (k_avg - mean(k_avg, na.rm = TRUE)) / sd(k_avg, na.rm = TRUE),
    z_putaway = (putaway_avg - mean(putaway_avg, na.rm = TRUE)) / sd(putaway_avg, na.rm = TRUE),
    z_xba = (xba_avg - mean(xba_avg, na.rm = TRUE)) / sd(xba_avg, na.rm = TRUE),
    z_xslg = (xslg_avg - mean(xslg_avg, na.rm = TRUE)) / sd(xslg_avg, na.rm = TRUE),
    z_xwoba = (xwoba_avg - mean(xwoba_avg, na.rm = TRUE)) / sd(xwoba_avg, na.rm = TRUE),
    z_hhp = (hhp_avg - mean(hhp_avg, na.rm = TRUE)) / sd(hhp_avg, na.rm = TRUE)
  ) %>%
  ungroup()

# Flip the signs for columns where lower numbers are better
lower_is_better <- c("z_ba", "z_slg", "z_woba", "z_xba", "z_xslg", "z_xwoba", "z_hhp")
stats_avg <- stats_avg %>%
  mutate(across(all_of(lower_is_better), ~ . * -1))

# Perform PCA to objectively weight variables
stats_numeric <- stats_avg %>%
  select(starts_with("z_"))

stats_numeric_clean <- stats_numeric %>%
  filter(across(everything(), ~ !is.na(.) & is.finite(.)))


pca <- prcomp(stats_numeric_clean, center = TRUE, scale. = TRUE)

# Extract weights from the first principal component
weights <- abs(pca$rotation[, 1])  # Absolute values of PC1 loadings
weights <- weights / sum(weights)  # Normalize to sum to 1

# Calculate the composite score with PCA weights and yrs_comp bonus
stats_avg <- stats_avg %>%
  mutate(
    composite_score = as.matrix(stats_numeric) %*% weights 
  )

stats_avg <- stats_avg %>%
  mutate(
    normalized_yrs_pitched = (num_yrs_pitch - min(num_yrs_pitch)) / (max(num_yrs_pitch) - min(num_yrs_pitch))
  )

stats_avg <- stats_avg %>%
  mutate(
    composite_score = as.matrix(stats_numeric) %*% weights + (0.1 * normalized_yrs_pitched)
  )

# Sort the data by the composite score
stats_avg <- stats_avg %>%
  arrange(-composite_score)

#
#
#
#Individual seasons, score averaged at the end

#convert pitch_usage to 0-1 scales
stats <- stats %>%
  mutate(pitch_usage = pitch_usage / 100)

stats <- stats[!is.na(stats$put_away), ]

# Add a bonus based on the number of years the pitch was used
stats <- stats %>%
  mutate(
    yrs_comp = case_when(
      num_yrs_pitch == 6 ~ 0.2,
      num_yrs_pitch == 5 ~ 0.15,
      num_yrs_pitch == 4 ~ 0.1,
      num_yrs_pitch == 3 ~ 0.05,
      TRUE ~ 0 # No bonus for fewer than 3 years of pitch usage
    )
  )


#normalize seasons using z scores
stats$z_rvp100 <- (stats$run_value_per_100 - mean(stats$run_value_per_100)) / sd(stats$run_value_per_100)
stats$z_rv <- (stats$run_value - mean(stats$run_value)) / sd(stats$run_value)
stats$z_ba <- (stats$ba - mean(stats$ba)) / sd(stats$ba)
stats$z_slg <- (stats$slg - mean(stats$slg)) / sd(stats$slg)
stats$z_woba <- (stats$woba - mean(stats$woba)) / sd(stats$woba)
stats$z_whiff <- (stats$whiff_percent - mean(stats$whiff_percent)) / sd(stats$whiff_percent)
stats$z_k <- (stats$k_percent - mean(stats$k_percent)) / sd(stats$k_percent)
stats$z_putaway <- (stats$put_away - mean(stats$put_away)) / sd(stats$put_away)
stats$z_xba <- (stats$est_ba - mean(stats$est_ba)) / sd(stats$est_ba)
stats$z_xslg <- (stats$est_slg - mean(stats$est_slg)) / sd(stats$est_slg)
stats$z_xwoba <- (stats$est_woba - mean(stats$est_woba)) / sd(stats$est_woba)
stats$z_hhp <- (stats$hard_hit_percent - mean(stats$hard_hit_percent)) / sd(stats$hard_hit_percent)

#account for columns where lower numbers are better
lower_is_better <- c("z_ba", "z_slg", "z_woba", "z_xba", "z_xslg", "z_xwoba", "z_hhp")

# Flip the signs for these columns
stats <- stats %>%
  mutate(across(all_of(lower_is_better), ~ . * -1)) |>
  filter(pitches >= 200)


#perform PCA to objectively weight variables
stats_numeric_szn <- stats %>%
  select(starts_with("z_"))

pca_szn <- prcomp(stats_numeric_szn, center = TRUE, scale. = TRUE)
summary(pca_szn)

#weight interpretation
weights_szn <- abs(pca_szn$rotation[, 1])  # Weights are absolute values of the first PC loadings
weights_szn <- weights_szn / sum(weights_szn)  # Normalize to sum to 1

stats$composite_score <- as.matrix(stats_numeric_szn) %*% weights_szn


stats_szn <- stats |>
  group_by(name, player_id, pitch_type, active_in_2024) |>
  summarize(mean_cs = mean(composite_score), 
            num_yrs_pitch = max(num_yrs_pitch, na.rm = TRUE)) |>
  ungroup() 

stats_szn <- stats_szn %>%
  mutate(
    yrs_comp = case_when(
      num_yrs_pitch == 6 ~ 0.2,
      num_yrs_pitch == 5 ~ 0.15,
      num_yrs_pitch == 4 ~ 0.1,
      num_yrs_pitch == 3 ~ 0.05,
      TRUE ~ 0 # No bonus for fewer than 3 years of pitch usage
    ),
    mean_cs = mean_cs + yrs_comp  # Add the bonus to the final mean score
  )

stats_szn <- stats_szn %>%
  filter(active_in_2024 == "Yes") %>%
  arrange(-mean_cs)



# Select numeric columns for entropy calculation
# Determine the rescaling constant
min_z <- min(as.matrix(stats %>% select(starts_with("z_"))))
rescaling_constant <- abs(min_z) + 1e-10

# Rescale z-scores
rescaled_stats <- stats %>%
  select(starts_with("z_")) %>%
  mutate(across(everything(), ~ . + rescaling_constant))

# Convert to numeric matrix
numeric_matrix <- as.matrix(rescaled_stats)

# Normalize rows to get proportional matrix
row_sums <- rowSums(numeric_matrix)
row_sums[row_sums == 0] <- 1e-10  # Avoid division by zero

proportional_matrix <- numeric_matrix / row_sums

# Replace zero or negative values with a small positive constant
proportional_matrix[proportional_matrix <= 0] <- 1e-10

# Number of observations
n <- nrow(proportional_matrix)

# Normalization constant
k <- 1 / log(n)

# Compute entropy
entropy <- -k * colSums(proportional_matrix * log(proportional_matrix))

# Complementary entropy and weights
complementary_entropy <- 1 - entropy
weights <- complementary_entropy / sum(complementary_entropy)

# Apply weights to the original numeric matrix
weighted_matrix <- numeric_matrix %*% diag(weights)

# Calculate composite score
stats$composite_score_entropy <- rowSums(weighted_matrix)

entropy_szn <- stats %>%
  group_by(name, pitch_type) %>%
  summarize(
    weighted_avg_score = sum(composite_score_entropy * pitches) / sum(pitches),
    total_pitches = sum(pitches),  # Optional, for reference
    .groups = "drop"
  )



#my own weighting
personal_weights <- stats %>%
  # Calculate the number of distinct years a player appears in the dataset
  group_by(name) %>%
  mutate(num_yrs_player = n_distinct(year)) %>%
  ungroup() %>%
  # Calculate the number of distinct years a specific pitch was used by a player
  group_by(name, pitch_type) %>%
  mutate(num_yrs_pitch = n_distinct(year)) %>%
  ungroup() %>%
  # Add the composite score with an additional bonus based on num_yrs_pitch
  mutate(
    yrs_comp = ifelse(
      num_yrs_pitch == 6, 0.2,
      ifelse(
        num_yrs_pitch == 5, 0.15,
        ifelse(
          num_yrs_pitch == 4, 0.1,
          ifelse(
            num_yrs_pitch == 3, 0.05,
            ifelse(
              num_yrs_pitch == 2, 0.025,
            0
            )# No bonus for fewer than 2 years of pitch usage
          )
        )
      )
    ),
    # Add the base score and apply the bonus
    score = (z_rvp100 * (2/12)) + 
      (z_rv * (1/24)) + 
      (z_ba * (2/12)) + 
      (z_slg * (2/12)) + 
      (z_woba * (2/12)) + 
      (z_whiff * (1/24)) + 
      (z_k * (1/24)) + 
      (z_putaway * (1/24)) + 
      (z_xba * (1/24)) + 
      (z_xslg * (1/24)) + 
      (z_xwoba * (1/24)) + 
      (z_hhp * (1/24)) +
      (.001 * pitches) + 
      yrs_comp
  )


my_score <- personal_weights |>
  group_by(name, player_id, pitch_type, active_in_2024) |>
  summarize(mean_cs = mean(score)) |>
  ungroup() |>
  arrange(-mean_cs) |>
  filter(active_in_2024 == 'Yes')



#calculating the score by using a percent change based on league average
#two approaches, 
#scoring within pitch type, then adjusting later
#and
#scoring across all pitch types

#comparing all pitches to each other
lg_avg <- stats |>
  select(1:24) |>
  group_by(year) |>
  summarise(
    lg_rvp100 = mean(run_value_per_100),
    lg_rv = mean(run_value),
    lg_ba = mean(ba),
    lg_slg = mean(slg),
    lg_woba = mean(woba),
    lg_whiff = mean(whiff_percent),
    lg_k = mean(k_percent),
    lg_putaway = mean(put_away),
    lg_xba = mean(est_ba),
    lg_xslg = mean(est_slg),
    lg_xwoba = mean(est_woba),
    lg_hhp = mean(hard_hit_percent)
  )


#comparing pitches within pitch type
lg_avg_pitch <- stats |>
  select(1:24) |>
  group_by(pitch_type, year) |>
  summarise(
    lg_rvp100 = mean(run_value_per_100),
    lg_rv = mean(run_value),
    lg_ba = mean(ba),
    lg_slg = mean(slg),
    lg_woba = mean(woba),
    lg_whiff = mean(whiff_percent),
    lg_k = mean(k_percent),
    lg_putaway = mean(put_away),
    lg_xba = mean(est_ba),
    lg_xslg = mean(est_slg),
    lg_xwoba = mean(est_woba),
    lg_hhp = mean(hard_hit_percent)
  )

my_score2prep <- stats |>
  select(1:24) |>
  left_join(lg_avg)


my_score2prep <- my_score2prep |>
  mutate(score = 100,
         pct_rvp100 = (run_value_per_100/lg_rvp100) * 100,
         pct_rv = (run_value/lg_rv) * 100,
         pct_ba = (ba/lg_ba) * 100,
         pct_slg = (slg/lg_slg) * 100,
         pct_woba = (woba/lg_woba) * 100,
         pct_whiff = (whiff_percent/lg_whiff) * 100,
         pct_k = (k_percent/lg_k) * 100,
         pct_putaway = (put_away/lg_putaway) * 100,
         pct_xba = (est_ba/lg_xba) * 100,
         pct_xslg = (est_slg/lg_xslg) * 100,
         pct_xwoba = (est_woba/lg_xwoba) * 100,
         pct_hhp = (hard_hit_percent/lg_hhp) * 100
  )

my_score2prep <- my_score2prep |>
  mutate(
    score = score +
      (5 * (pct_rvp100 - 1)) +
      (10 * (pct_rv - 1)) +
      (4 * (1 - pct_ba)) +  # Flip sign
      (2 * (1 - pct_slg)) + # Flip sign
      (4 * (1 - pct_woba)) + # Flip sign
      (8 * (pct_whiff - 1)) +
      (10 * (pct_k - 1)) +
      (5 * (pct_putaway - 1)) +
      (4 * (1 - pct_xba)) + # Flip sign
      (2 * (1 - pct_xslg)) + # Flip sign
      (4 * (1 - pct_xwoba)) + # Flip sign
      (7 * (1 - pct_hhp)) # Flip sign
  )


my_score2 <- my_score2prep |>
  group_by(name, pitch_type, num_yrs_pitch, active_in_2024) |>
  summarise(comp_score = mean(score)) |>
  ungroup()

my_score2 <- my_score2 |>
  mutate(
    comp_score = case_when(
      num_yrs_pitch == 1 ~ comp_score * 0.75,  # 20% penalty for 1 year
      num_yrs_pitch == 2 ~ comp_score * 0.85,  # 10% penalty for 2 years
      TRUE ~ comp_score                    # No penalty for 3+ years
    )
  )

my_score2 <- my_score2 |>
  arrange(desc(comp_score)) |>  # Sort in descending order of composite score
  filter(active_in_2024 == 'Yes')

my_score3 <- stats |>
  group_by(name, pitch_type, active_in_2024) |>
  summarize(mean_RV = mean(run_value))


my_score4 <- stats |>
  # Rank run values by season (across all pitches, not by pitch type)
  group_by(year) |>
  mutate(run_value_rank = rank(-run_value, ties.method = "min")) |> # Rank in descending order (highest run value gets rank 1)
  ungroup() |>
  
  # Calculate mean pitch ranking for each pitcher and pitch type
  group_by(name, pitch_type) |>
  summarise(
    mean_run_value_rank = mean(run_value_rank, na.rm = TRUE), # Mean rank for each pitcher's pitch
    .groups = "drop" # Ensure we return an ungrouped data frame
  ) |>
  arrange(mean_run_value_rank)


my_score5 <- stats |>
  # Rank pitches by run_value_per_100 within each season
  group_by(year) |>
  mutate(run_value_per_100_rank = rank(-run_value_per_100, ties.method = "average")) |>
  ungroup() |>
  
  # Calculate mean rank for each pitcher's pitch type
  group_by(name, pitch_type) |>
  summarise(mean_run_value_per_100_rank = mean(run_value_per_100_rank, na.rm = TRUE)) |>
  ungroup() |>  
  arrange(mean_run_value_per_100_rank)


