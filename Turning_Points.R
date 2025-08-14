
# Load per-chapter sentiment series, keep real chapters, order within book,
# and compute first differences (Δ sentiment) per book.
sentiment_record <- readRDS("data/processed/sentiment_record.rds") %>%
  group_by(book) %>%
  filter(chapter > 0) %>%                     
  arrange(chapter, .by_group = TRUE) %>%
  mutate(
    delta_sentiment = sentiment_score - lag(sentiment_score)
  ) %>%
  ungroup()

# min–max rescale to [0,1]; safe for constant/empty ranges (returns zeros).
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (is.infinite(rng[1]) || rng[1] == rng[2]) return(rep(0, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# Within each book, derive comparable feature scales:
# - abs Δ sentiment (magnitude of change),
# - total topic shift = L1 sum of per-topic deltas across delta_topic_* columns,
# - rescale sentiment/entity/topic features to [0,1] for downstream fusion.
sentiment_record <- sentiment_record %>%
  group_by(book) %>%
  mutate(
    abs_delta_sentiment = abs(delta_sentiment),
    total_delta_topic   = rowSums(across(starts_with("delta_topic_"), ~abs(.x)), na.rm = TRUE),
    scaled_sentiment    = scale01(abs_delta_sentiment),
    scaled_entities     = scale01(new_entities),
    scaled_topics       = scale01(total_delta_topic)
  ) %>%
  ungroup()

# Assemble plotting frame: keep book/chapter index, sentiment score,
# all topic mixture columns (topic_*), and entity counts for visualization.
plot_data <- sentiment_record %>%
  select(book, chapter, sentiment_score, starts_with("topic_"), new_entities)

# Perform min-max scaling within each book
plot_data_scaled <- plot_data %>%
  group_by(book) %>%
  mutate(
    sentiment_score_scaled = (sentiment_score - min(sentiment_score, na.rm = TRUE)) / (max(sentiment_score, na.rm = TRUE) - min(sentiment_score, na.rm = TRUE)),
    new_entities_scaled = (new_entities - min(new_entities, na.rm = TRUE)) / (max(new_entities, na.rm = TRUE) - min(new_entities, na.rm = TRUE)),
    across(starts_with("topic_"), ~ (.x - min(.x, na.rm = TRUE)) / (max(.x, na.rm = TRUE) - min(.x, na.rm = TRUE)), .names = "{.col}_scaled")
  ) %>%
  ungroup()

# Arranged into tidy format
plot_data_long <- plot_data_scaled %>%
  select(book, chapter, sentiment_score_scaled, new_entities_scaled, ends_with("_scaled")) %>%
  pivot_longer(
    cols = -c(book, chapter),
    names_to = "measure",
    values_to = "value"
  )


######################################################
# Classify series for styling:
# - mark sentiment & entity series as "main" (thicker/opaque);
# - map measure names to legend-friendly color labels; keep topics as-is.
plot_data_long <- plot_data_long %>%
  mutate(
    line_type = ifelse(measure %in% c("sentiment_score_scaled", "new_entities_scaled"), "main", "secondary"),
    color_group = case_when(
      measure == "sentiment_score_scaled" ~ "Sentiment",
      measure == "new_entities_scaled" ~ "New_Entities",
      TRUE ~ measure  
    )
  )

# Palette: muted tones for topics; strong accents for main series.
# Keys MUST match values in `color_group` (topics = "topic_i_scaled", mains = "Sentiment"/"New_Entities").
theme_colors <- c(
  "topic_1_scaled" = "#a6bddb",  
  "topic_2_scaled" = "#bcbddc",  
  "topic_3_scaled" = "#c7e9c0",  
  "topic_4_scaled" = "#fdbb84",  
  "topic_5_scaled" = "#d9d9d9",  
  "topic_6_scaled" = "#d0d1e6",
  "topic_7_scaled" = "#cbc9e2",
  "topic_8_scaled" = "#c6dbef",
  "topic_9_scaled" = "#fdd0a2",
  "topic_10_scaled" = "#dadaeb",
  "topic_11_scaled" = "#d9f0a3",
  "topic_12_scaled" = "#f2f0f7",
  "Sentiment" = "firebrick",     
  "New_Entities" = "steelblue"   
)

# Faceted multi-series trajectories over chapters:
# - emphasize main series via linewidth/alpha; topics remain subtle;
# - manual color/legend styling; hide linewidth/alpha legends to avoid clutter.
ggplot(plot_data_long, aes(x = chapter, y = value, group = measure)) +
  geom_line(
    aes(color = color_group, linewidth = line_type, alpha = line_type),
    lineend = "round", linejoin = "round"
  ) +
  facet_wrap(~ book, scales = "free_x") +
  scale_color_manual(values = theme_colors) +
  scale_linewidth_manual(values = c(main = 1.2, secondary = 0.6)) +
  scale_alpha_manual(values = c(main = 1,   secondary = 0.8)) +
  labs(
    title  = "Highlighting Sentiment, New Entities, and Topics (Soft Transitions)",
    x = "Chapter", y = "Scaled Value (0-1)", color = "Measure"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    strip.text       = element_text(size = 13, face = "bold"),  
    legend.position  = "bottom",
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11),
    legend.key.width = unit(18, "pt"),
    legend.key.height= unit(12, "pt")
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 2, byrow = TRUE,
      override.aes = list(linewidth = 2, alpha = 1)  
    ),
    linewidth = "none",  
    alpha     = "none"   
  )


#######################################################################

# Define the jump score calculation formula
objective_function_new <- function(a, b, c) {
  
  temp <- sentiment_record %>%
    mutate(
      Jump_Score = a * scaled_sentiment + b * scaled_entities + c * scaled_topics
    )
  
  # In each book, select the top 20% chapters with the highest Jump_Score
  temp <- temp %>%
    group_by(book) %>%
    arrange(desc(Jump_Score)) %>%
    mutate(
      rank = row_number(),
      total_chapters = n()
    ) %>%
    filter(rank <= ceiling(0.20 * total_chapters)) %>% 
    ungroup()
  
  # If the screening is too small (for example, all chapters are less than 10), a mandatory penalty
  if(nrow(temp) < 10) {
    return(list(Score = -9999))
  }

  mean_sentiment <- mean(temp$scaled_sentiment, na.rm = TRUE)
  mean_entity <- mean(temp$scaled_entities, na.rm = TRUE)
  mean_topic <- mean(temp$scaled_topics, na.rm = TRUE)
  
  # Comprehensive score (emphasis on emotion and entity)
  final_score <- (3 * mean_sentiment) + (2 * mean_entity) + (1 * mean_topic)
  
  return(list(Score = final_score))
}

# Run Bayesian Optimization
set.seed(123)

# Bayesian optimization of three weights (a, b, c) for the objective:
# - Search space: each weight ∈ [0.5, 3.0]
# - 10 random initial evaluations, then 25 BO iterations
# - Acquisition: UCB with kappa = 2.576 (more exploratory)
res_new <- BayesianOptimization(
  FUN = objective_function_new,
  bounds = list(
    a = c(0.5, 3.0),
    b = c(0.5, 3.0),
    c = c(0.5, 3.0)
  ),
  init_points = 10,
  n_iter = 25,
  acq = "ucb",
  kappa = 2.576,
  verbose = TRUE
)

print(res_new$Best_Par)

# Composite jump score using BO-optimized weights (Best_Par from `res_new`):
jump_record <- sentiment_record %>%
  mutate(
    Jump_Score = (2.81 * scaled_sentiment) + (1.91 * scaled_entities) + (1.04 * scaled_topics)
  )

# Ensure per-book chronological order for plotting/inspection.
jump_record <- jump_record %>%
  group_by(book) %>%
  arrange(chapter) %>%
  ungroup()

# Plot BO-weighted Jump_Score trajectories across chapters for each book.
ggplot(jump_record, aes(x = chapter, y = Jump_Score)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ book, scales = "free_x") +
  labs(
    title = "Jump_Score per Chapter per Book",
    x = "Chapter",
    y = "Jump_Score"
  ) +
  theme_minimal(base_size = 13) +                    
  theme(strip.text = element_text(size = 13, face = "bold"))  



jump_record_processed <- jump_record

# Process the last chapter of Boy_Scouts_in_Southern_Waters first. Set Jump_Score to 0
jump_record_processed <- jump_record_processed %>%
  group_by(book) %>%
  mutate(
    max_chapter = max(chapter, na.rm = TRUE), 
    Jump_Score = ifelse(book == "Boy_Scouts_in_Southern_Waters" & chapter == max_chapter, 0, Jump_Score)
  ) %>%
  ungroup() %>%
  select(-max_chapter)

# Per-book thresholding of Jump_Score:
# compute μ and σ within each book, set threshold = μ + 1·σ, and flag chapters above it as turning points.
jump_record_processed <- jump_record_processed %>%
  group_by(book) %>%
  mutate(
    mean_score = mean(Jump_Score, na.rm = TRUE),
    sd_score = sd(Jump_Score, na.rm = TRUE),
    
    # Define identification criteria: greater than mean + 1.0 × standard deviation
    threshold = mean_score + 1.0 * sd_score,
    
    # Is it a turning point chapter
    is_turning_point = Jump_Score > threshold
  ) %>%
  ungroup()

# View recognition results
turning_points <- jump_record_processed %>%
  filter(is_turning_point == TRUE) %>%
  select(book, chapter, Jump_Score)

print(turning_points)

#####################################################

sentiment_record_processed <- sentiment_record

# Process the theme changes first 
topic_long <- sentiment_record_processed %>%
  select(book, chapter, starts_with("delta_topic_")) %>%
  pivot_longer(
    cols = starts_with("delta_topic_"),
    names_to = "topic",
    values_to = "delta_value"
  ) %>%
  mutate(
    topic = gsub("delta_topic_", "", topic) 
  )

# Calculate the thematic change criteria for each book, marking dramatic changes
topic_changes <- topic_long %>%
  group_by(book, topic) %>%
  mutate(
    topic_mean = mean(delta_value, na.rm = TRUE),
    topic_sd = sd(delta_value, na.rm = TRUE),
    
    is_increase = delta_value > (topic_mean + topic_sd),
    is_decrease = delta_value < (topic_mean - topic_sd)
  ) %>%
  group_by(book, chapter) %>%
  summarise(
    increased_topics = list(topic[is_increase == TRUE]),
    decreased_topics = list(topic[is_decrease == TRUE]),
    .groups = "drop"
  )

# Merge back to master data
sentiment_record_processed <- sentiment_record_processed %>%
  left_join(topic_changes, by = c("book", "chapter")) %>%
  mutate(
    increased_topics = ifelse(is.na(increased_topics), list(character()), increased_topics),
    decreased_topics = ifelse(is.na(decreased_topics), list(character()), decreased_topics)
  )

# Calculate the criteria for sentiment change and entity change
sentiment_record_processed <- sentiment_record_processed %>%
  group_by(book) %>%
  mutate(
    mean_delta_sentiment = mean(delta_sentiment, na.rm = TRUE),
    sd_delta_sentiment = sd(delta_sentiment, na.rm = TRUE),
    mean_new_entities = mean(new_entities, na.rm = TRUE),
    sd_new_entities = sd(new_entities, na.rm = TRUE),
    is_large_sentiment_change = abs(delta_sentiment - mean_delta_sentiment) > sd_delta_sentiment,
    is_many_new_entities = (new_entities - mean_new_entities) > sd_new_entities,
    is_theme_shift = (lengths(increased_topics) > 0 | lengths(decreased_topics) > 0)
  ) %>%
  ungroup()

# Filter out the required chapters
selected_turning_points <- sentiment_record_processed %>%
  filter(
    is_large_sentiment_change == TRUE |
      is_many_new_entities == TRUE |
      is_theme_shift == TRUE
  ) %>%
  select(
    book, chapter,
    delta_sentiment,
    new_entities,
    increased_topics,
    decreased_topics
  )

print(selected_turning_points)

#########################################################
# Alias final selection for downstream use.
turning_points_classified <- selected_turning_points

# Per-book summary stats for sentiment change and entity introductions
# (means/SDs used for diagnostics, scaling checks, or report tables).
sentiment_entity_stats <- sentiment_record %>%
  group_by(book) %>%
  summarise(
    mean_delta_sentiment = mean(delta_sentiment, na.rm = TRUE),
    sd_delta_sentiment = sd(delta_sentiment, na.rm = TRUE),
    mean_new_entities = mean(new_entities, na.rm = TRUE),
    sd_new_entities = sd(new_entities, na.rm = TRUE),
    .groups = "drop"
  )

# Join the standard
turning_points_classified <- turning_points_classified %>%
  left_join(sentiment_entity_stats, by = "book")

# Classification
turning_points_classified <- turning_points_classified %>%
  mutate(
    # Emotional mutation classification
    emotion_change = case_when(
      delta_sentiment > (mean_delta_sentiment + sd_delta_sentiment) ~ "Positive Shift",
      delta_sentiment < (mean_delta_sentiment - sd_delta_sentiment) ~ "Negative Shift",
      TRUE ~ "No Significant Shift"
    ),
    
    # Classification of new entity mutations
    entity_change = case_when(
      new_entities > (mean_new_entities + sd_new_entities) ~ "Many New Entities",
      TRUE ~ "No Significant Entities"
    ),
    
    # Theme mutation classification）
    theme_change = case_when(
      lengths(increased_topics) == 1 & (lengths(decreased_topics) == 1 | lengths(decreased_topics) > 1) ~ "Theme Shift",
      TRUE ~ "No Theme Shift"
    ),
    
    # If there is no topic mutation, clear the topic column
    increased_topics = ifelse(theme_change == "No Theme Shift", NA, increased_topics),
    decreased_topics = ifelse(theme_change == "No Theme Shift", NA, decreased_topics)
  )

# Divide into three tibbles
emotion_turning_points <- turning_points_classified %>%
  filter(emotion_change != "No Significant Shift") %>%
  select(book, chapter, delta_sentiment, emotion_change)

entity_turning_points <- turning_points_classified %>%
  filter(entity_change == "Many New Entities") %>%
  select(book, chapter, new_entities, entity_change)

theme_turning_points <- turning_points_classified %>%
  filter(theme_change == "Theme Shift") %>%
  select(book, chapter, increased_topics, decreased_topics, theme_change)

print(emotion_turning_points)
print(entity_turning_points)
print(theme_turning_points)

###############################################

# Select top-half chapters per book by absolute sentiment change (largest |Δsentiment|).
# Deterministic tie-breaking via ties.method = "first".
emotion_turning_points_half <- emotion_turning_points %>%
  group_by(book) %>%
  mutate(
    abs_sentiment_change = abs(delta_sentiment),        
    row_rank = rank(-abs_sentiment_change, ties.method = "first"),  
    total_rows = n()
  ) %>%
  filter(row_rank <= ceiling(total_rows / 2)) %>%       
  select(book, chapter, delta_sentiment, emotion_change) %>%
  ungroup()

# Select top-half chapters per book by number of new entities (largest counts).
entity_turning_points_half <- entity_turning_points %>%
  group_by(book) %>%
  mutate(
    row_rank = rank(-new_entities, ties.method = "first"),  
    total_rows = n()
  ) %>%
  filter(row_rank <= ceiling(total_rows / 2)) %>%       
  select(book, chapter, new_entities, entity_change) %>%
  ungroup()


print(emotion_turning_points_half)
print(entity_turning_points_half)

############################################################################


# Extract (book, chapter) from three tables
covered_chapters <- bind_rows(
  emotion_turning_points_half %>% select(book, chapter),
  entity_turning_points_half %>% select(book, chapter),
  theme_turning_points %>% select(book, chapter)
) %>%
  distinct()  

# Check if turning_points is completely covered
turning_points_check <- turning_points %>%
  select(book, chapter) %>%
  left_join(covered_chapters %>% mutate(covered = TRUE), by = c("book", "chapter")) %>%
  mutate(
    covered = ifelse(is.na(covered), FALSE, covered)
  )

# Are there any chapters that are not covered
not_covered <- turning_points_check %>%
  filter(covered == FALSE)

if (nrow(not_covered) == 0) {
  cat("all chapters are covered!\n")
} else {
  cat("There are some chapters not covered, as follows：\n")
  print(not_covered)
}

###########################################################################################

# Remove chapters that are flagged as "not covered" (drop by (book, chapter) keys).
turning_points_cleaned <- turning_points %>%
  anti_join(not_covered %>% select(book, chapter), by = c("book","chapter"))

# Attach emotion labels to remaining turning points:
# - join per (book, chapter) from the top-half emotion set,
# - label by sign of delta_sentiment,
# - default missing labels to "No Significant Shift".
turning_points_cleaned <- turning_points_cleaned %>%
  left_join(
    emotion_turning_points_half %>%
      mutate(emotion_type = case_when(
        delta_sentiment > 0 ~ "Positive Shift",
        delta_sentiment < 0 ~ "Negative Shift",
        TRUE ~ "No Significant Shift"
      )) %>%
      select(book, chapter, emotion_type),
    by = c("book", "chapter")
  ) %>%
  mutate(
    emotion_type = ifelse(is.na(emotion_type), "No Significant Shift", emotion_type)
  )

# Mark if there are many new entities
turning_points_cleaned <- turning_points_cleaned %>%
  left_join(
    entity_turning_points_half %>%
      mutate(entity_type = "Many New Entities") %>%
      select(book, chapter, entity_type),
    by = c("book", "chapter")
  ) %>%
  mutate(
    entity_type = ifelse(is.na(entity_type), "No Significant Entities", entity_type)
  )

# Mark if there is a theme change and add detailed information based on theme_turning_points
turning_points_cleaned <- turning_points_cleaned %>%
  left_join(
    theme_turning_points %>%
      mutate(theme_type = "Theme Shift") %>%
      select(book, chapter, increased_topics, decreased_topics, theme_type),
    by = c("book", "chapter")
  ) %>%
  mutate(
    theme_type = ifelse(is.na(theme_type), "No Theme Shift", theme_type)
  )

print(turning_points_cleaned)

##################################################################

# Add turning point types to turning_points_cleaned
turning_points_cleaned <- turning_points_cleaned %>%
  mutate(
    turning_point_type = case_when(
      emotion_type == "Positive Shift" & entity_type == "Many New Entities" & theme_type == "Theme Shift" ~ 1,
      emotion_type == "Positive Shift" & entity_type == "Many New Entities" & theme_type == "No Theme Shift" ~ 2,
      emotion_type == "Positive Shift" & entity_type == "No Significant Entities" & theme_type == "Theme Shift" ~ 3,
      emotion_type == "Positive Shift" & entity_type == "No Significant Entities" & theme_type == "No Theme Shift" ~ 4,
      
      emotion_type == "Negative Shift" & entity_type == "Many New Entities" & theme_type == "Theme Shift" ~ 5,
      emotion_type == "Negative Shift" & entity_type == "Many New Entities" & theme_type == "No Theme Shift" ~ 6,
      emotion_type == "Negative Shift" & entity_type == "No Significant Entities" & theme_type == "Theme Shift" ~ 7,
      emotion_type == "Negative Shift" & entity_type == "No Significant Entities" & theme_type == "No Theme Shift" ~ 8,
      
      emotion_type == "No Significant Shift" & entity_type == "Many New Entities" & theme_type == "Theme Shift" ~ 9,
      emotion_type == "No Significant Shift" & entity_type == "Many New Entities" & theme_type == "No Theme Shift" ~ 10,
      emotion_type == "No Significant Shift" & entity_type == "No Significant Entities" & theme_type == "Theme Shift" ~ 11,
      emotion_type == "No Significant Shift" & entity_type == "No Significant Entities" & theme_type == "No Theme Shift" ~ 12,
      
      TRUE ~ NA_integer_  
    )
  )

# Create a type explanation tibble
turning_point_type_explanation <- tibble(
  turning_point_type = 1:12,
  emotion = c(rep("Positive Shift", 4), rep("Negative Shift", 4), rep("No Significant Shift", 4)),
  entity = rep(c(rep("Many New Entities", 2), rep("No Significant Entities", 2)), 3),
  theme = rep(c("Theme Shift", "No Theme Shift"), 6)
)

print(turning_points_cleaned)
print(turning_point_type_explanation)
############################################################
# Simple summary of the number of each type
turning_type_count <- turning_points_cleaned %>%
  group_by(turning_point_type) %>%
  summarise(count = n()) %>%
  ungroup()

# Bar chart: raw distribution of types
ggplot(turning_type_count, aes(x = factor(turning_point_type), y = count)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of turning point types",
    x = "Turning point type (1-12)",
    y = "Number of chapters"
  ) +
  theme_minimal()

# Sort types by frequency for a ranked bar chart
turning_type_count_sorted <- turning_type_count %>%
  arrange(desc(count)) %>%
  mutate(turning_point_type = factor(turning_point_type, levels = turning_point_type))


ggplot(turning_type_count_sorted, aes(x = turning_point_type, y = count)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of turning point types (sorted by number)",
    x = "Turning point types (sorted)",
    y = "Number of chapters"
  ) +
  theme_minimal()

# Persist cleaned data and type explanations
saveRDS(turning_points_cleaned, file = "data/processed/turning_points_cleaned.rds")

saveRDS(turning_point_type_explanation, file = "data/processed/turning_point_type_explanation.rds")

# Join jump scores with labeled turning points (by book + chapter)
full_turning_points <- jump_record %>%
  left_join(turning_points_cleaned, by = c("book", "chapter"))

#Build plotting fields:
  #    Jump_Score: choose the post-join score column ('.y' here)
  #    Plot_Score: signed height for plotting bumps/dips
  #    Point_Label: prefixed label (+ / - / 0) with the type number
  #    Point_Type: legend-friendly category (High/Low/Neutral)
full_turning_points <- full_turning_points %>%
  mutate(
    Jump_Score = Jump_Score.y,  
    Plot_Score = case_when(
      is.na(turning_point_type) ~ 0,
      turning_point_type %in% 1:4  ~ Jump_Score,
      turning_point_type %in% 5:8  ~ -Jump_Score,
      turning_point_type %in% 9:12 ~ 0
    ),
    Point_Label = case_when(
      turning_point_type %in% 1:4  ~ paste0("+", turning_point_type),
      turning_point_type %in% 5:8  ~ paste0("-", turning_point_type),
      turning_point_type %in% 9:12 ~ paste0("0", turning_point_type),
      TRUE ~ NA_character_
    ),
    Point_Type = case_when(
      turning_point_type %in% 1:4  ~ "High",
      turning_point_type %in% 5:8  ~ "Low",
      turning_point_type %in% 9:12 ~ "Neutral",
      TRUE ~ NA_character_
    )
  )


# Faceted narrative arc plot:
  #    Grey baseline curve shows chapter-wise Jump_Score trajectory
  #    Colored points/text highlight detected turning points
  #    Facet by book; free x-scale lets each book have its own chapter range

ggplot(full_turning_points, aes(x = chapter, y = Plot_Score)) +
  geom_line(color = "grey60", linewidth = 1) +
  
  geom_point(
    data = full_turning_points %>% filter(!is.na(turning_point_type)),
    aes(color = Point_Type),
    size = 3
  ) +
  geom_text(
    data = full_turning_points %>% filter(!is.na(turning_point_type)),
    aes(label = Point_Label, color = Point_Type),
    vjust = -1.2,
    size = 3,
    show.legend = FALSE         
  ) +
  
  facet_wrap(~ book, scales = "free_x") +
  scale_y_continuous(
    name = "Narrative Intensity (Jump Score)",
    breaks = pretty_breaks(n = 5)
  ) +
  scale_color_manual(
    values = c(High = "firebrick", Low = "steelblue", Neutral = "darkgreen"),
    name = "Turning Point Category"
  ) +
  labs(
    title = "Narrative Arc: Chapter-wise Emotional Turning Points",
    x = "Chapter"
  ) +
  theme_minimal(base_size = 13) +                   
  theme(
    strip.text       = element_text(size = 13, face = "bold"),  
    legend.position  = "bottom",
    legend.title     = element_text(size = 12, face = "bold"),
    legend.text      = element_text(size = 11),
    legend.key.width = unit(18, "pt"),
    legend.key.height= unit(12, "pt")
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 1,
      override.aes = list(size = 4)
    )
  )