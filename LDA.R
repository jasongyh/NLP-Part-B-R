
# Create a chapter ID
book_tokens_id <- book_tokens %>%
  mutate(doc_id = paste(book, chapter, sep = "_"))

# Generate DTM: Each document is a chapter 
chapter_dtm <- book_tokens_id %>%
  count(doc_id, word) %>%
  cast_dtm(document = doc_id, term = word, value = n)

result_k <- FindTopicsNumber(
  chapter_dtm,
  topics = seq(2, 12, by = 2),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 1L
)

# Visualize selection results
FindTopicsNumber_plot(result_k)

# Fit a 12-topic LDA on the chapter-level DTM (one doc = one chapter); seed for reproducibility.
lda_model <- LDA(chapter_dtm, k = 12, control = list(seed = 123))

# Tidy the per-document topic mixture (γ): one row per (document, topic) with probability `gamma`.
chapter_topics <- tidy(lda_model, matrix = "gamma")  

# Split the document id "book_chapter" into separate columns; coerce chapter to integer.
chapter_topics <- chapter_topics %>%
  extract(document, into = c("book", "chapter"),
          regex = "^(.*)_(\\d+)$", convert = TRUE)

# Quick spot-check: topic mixture for Chapter 1 of "20000_Leagues_Under_the_Sea".
chapter_topics %>%
  filter(book == "20000_Leagues_Under_the_Sea", chapter == 1)

# Visualize topic drift across chapters for one book: lines show γ per topic over chapter index.
chapter_topics %>%
  filter(book == "Around_the_World_in_80_Days") %>%
  ggplot(aes(x = chapter, y = gamma, color = factor(topic), group = topic)) +
  geom_line() +
  labs(title = "Chapter topic distribution", y = "Topic Probability", x = "chapter", color = "topic") +
  theme_minimal()

# Count rows by topic for another book (diagnostic).
chapter_topics %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea") %>%
  count(topic)

# Plot per-book topic trajectories across chapters:
# lines show each topic’s γ (probability) over chapter index; facets split by book (free x-scale).
# Legend moved to bottom (2 rows) and styling tweaked for readability.
chapter_topics %>%
  ggplot(aes(x = chapter, y = gamma, color = factor(topic), group = topic)) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  facet_wrap(~ book, scales = "free_x") +
  labs(
    title = "Chapter distribution of the 8 themes in each novel",
    x = "chapter",
    y = "Topic Probability",
    color = "Topic Number"
  ) +
  theme_minimal(base_size = 13) +  
  theme(
    strip.text = element_text(size = 13, face = "bold"),  
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text  = element_text(size = 11),
    legend.key.width  = unit(18, "pt"),
    legend.key.height = unit(12, "pt")
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow = 2, byrow = TRUE,                 
      override.aes = list(linewidth = 2, alpha = 1)  
    )
  )
###################################################################
topic_terms <- tidy(lda_model, matrix = "beta")

# The top 10 most important words for each topic
top_terms <- topic_terms %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Visualization: The most important words for each topic
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() +
  labs(title = "Top 10 Terms in Each Topic", x = "Beta (Term Probability)", y = NULL)

topic_labels <- tibble(
  topic = 1:12,
  topic_label = c(
    "Boys' Adventure & Island Life",        # 1
    "Pirates & Maritime Conflict",          # 2
    "Island Survival & Natives",            # 3
    "Global Voyage & Adventure",            # 4
    "Underwater Exploration & Nautilus",    # 5
    "Engineering & Collective Work A",      # 6
    "Engineering & Collective Work B",      # 7
    "Gutenberg Metadata Noise",             # 8
    "Sea Survival & the Captain",           # 9
    "Lost World & Dinosaur Expedition",     #10
    "Shipwreck & Character Relations",      #11
    "African Exploration & Tribal Conflict" #12
  )
)

# Join the labels to chapter_topics
chapter_topics_labeled <- chapter_topics %>%
  left_join(topic_labels, by = "topic")
##############################################################################

# Reshape per-chapter topic mixtures to wide format:
# one row per (book, chapter) with columns topic_1..topic_k holding γ probabilities.
topic_probs <- chapter_topics %>%
  pivot_wider(
    names_from = topic,
    values_from = gamma,
    names_prefix = "topic_"
  )

# Calculate the change in each topic (chapter difference)
topic_deltas <- topic_probs %>%
  arrange(book, chapter) %>%
  group_by(book) %>%
  mutate(across(starts_with("topic_"), ~ .x - lag(.x), .names = "delta_{.col}")) %>%
  ungroup()

# Merge topic + delta_topic back into sentiment_record
sentiment_record <- sentiment_record %>%
  left_join(topic_deltas, by = c("book", "chapter"))

