
# Bridge R → Python spaCy via reticulate/spacyr:
# - select an existing Conda env named "spacy" (Windows conda.exe path given),
# - install spaCy (+ models) into that env if needed,
# - then initialize the Python backend for NLP calls from R.

library(reticulate)
library(spacyr)
my_conda <- "D:/User/Scripts/conda.exe"
use_condaenv("spacy", conda = my_conda, required = TRUE)
spacy_install()
spacy_initialize()

# Collapse lines into one string per (book, chapter) for NER input.
ner_input <- end_trimmed_books %>%
  filter(text != "") %>%
  group_by(book, chapter) %>%
  summarise(full_text = str_c(text, collapse = " "), .groups = "drop")

# Run spaCy NER chapter-by-chapter, attach (book, chapter), and row-bind results.
ner_result <- map_dfr(
  1:nrow(ner_input),
  function(i) {
    temp <- spacy_extract_entity(ner_input$full_text[i])
    if (nrow(temp) == 0) return(NULL)
    temp$book <- ner_input$book[i]
    temp$chapter <- ner_input$chapter[i]
    return(temp)
  }
)

#################################################################

# Bar chart: total mentions by entity TYPE across the whole corpus (horizontal for readability).
ner_result %>%
  count(ent_type, sort = TRUE) %>%
  ggplot(aes(x = reorder(ent_type, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Entity Type", y = "Count", title = "Entity Counts by Type")

# Per-book, per-chapter time series: count entities by TYPE and plot chapter-wise trajectories.
ner_result %>%
  count(book, chapter, ent_type) %>%
  ggplot(aes(x = chapter, y = n, color = ent_type)) +
  geom_line() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Chapter-wise Entity Count by Type", y = "Count", x = "Chapter")

# "Top 30" entity TYPES by frequency (not individual surface forms).
ner_result %>%
  count(ent_type, sort = TRUE) %>%
  slice_head(n = 30) %>%
  ggplot(aes(y = reorder(ent_type, n), x = n)) +
  geom_col(fill = "purple") +
  labs(title = "Top 30 Entities", x = "Count", y = "Entity")

# Top surface forms within each TYPE: take top 15 per ent_type (filtering singletons), faceted bars.
ner_result %>%
  count(ent_type, text, sort = TRUE) %>%
  group_by(ent_type) %>%
  slice_max(n, n = 15) %>%
  ungroup() %>%
  filter(n > 1) %>%
  ggplot(aes(x = reorder(text, n), y = n, fill = ent_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ ent_type, scales = "free") +
  labs(x = "Entity", y = "Count", title = "Top Entities by Type")

# Per-chapter diversity: number of DISTINCT entity TYPES seen in each chapter.
ner_chapter_summary <- ner_result %>%
  group_by(book, chapter) %>%
  summarise(entity_count = n_distinct(ent_type))

ggplot(ner_chapter_summary, aes(x = chapter, y = entity_count)) +
  geom_line() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "Entity Count per Chapter", x = "Chapter", y = "Entity Count")

#####################################################
valid_entity_types <- c("PERSON", "GPE", "LOC")

filtered_ner <- ner_result %>%
  filter(ent_type %in% valid_entity_types)

# Record the entity set of each chapter
chapter_entities <- filtered_ner %>%
  distinct(book, chapter, entity = text) %>%
  arrange(book, chapter)

# Count the number of new entities per chapter of each book (by "kind")

chapter_entity_sets <- chapter_entities %>%
  group_by(book, chapter) %>%
  summarise(entities = list(unique(entity)), .groups = "drop")

# Accumulate and count new entities 
entity_jump_by_chapter <- chapter_entity_sets %>%
  group_by(book) %>%
  arrange(chapter) %>%
  mutate(
    accumulated = accumulate(entities, union),
    previous_entities = lag(accumulated),
    new_entities = map2_int(entities, previous_entities, ~ length(setdiff(.x, .y)))
  ) %>%
  select(book, chapter, new_entities) %>%
  mutate(new_entities = replace_na(new_entities, 0))

# Merge back into sentiment_record
sentiment_record <- sentiment_record %>%
  left_join(entity_jump_by_chapter, by = c("book", "chapter")) %>%
  mutate(new_entities = replace_na(new_entities, 0))

# Drop placeholder columns created earlier; keep only sentiment-based fields for now.
sentiment_record <- sentiment_record %>%
  select(-topic_shift, -entity_shift)

saveRDS(sentiment_record, file = "data/processed/sentiment_record.rds")
