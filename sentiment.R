
# Load multiple sentiment lexicons for later comparison/scoring:
# AFINN via tidytext
# SentiWordNet and SenticNet hash lexicons (from lexicon package datasets)
install.packages("lexicon")
library(lexicon)
library(textdata)


# Extract unigram vocabularies from three sentiment lexicons for coverage/joins.
afinn <- get_sentiments("afinn")
head(afinn)

data(hash_sentiment_sentiword)
data(hash_sentiment_senticnet)
head(hash_sentiment_sentiword)
head(hash_sentiment_senticnet)

afinn_words <- unique(afinn$word)

sentiword_words <- hash_sentiment_sentiword %>%
  filter(!str_detect(x, " ")) %>%   
  pull(x) %>%
  unique()

senticnet_words <- hash_sentiment_senticnet %>%
  filter(!str_detect(x, " ")) %>%
  pull(x) %>%
  unique()

# Token-level lexicon coverage: share of all tokens found in each lexicon.
token_words <- book_tokens$word

# Fraction of tokens present in a given lexicon.
compute_coverage <- function(token_words, lexicon_words) {
  mean(token_words %in% lexicon_words)
}

# Compare coverage across dictionaries and print.
coverage_results <- tibble(
  dictionary = c("AFINN", "SentiWordNet", "SenticNet"),
  coverage = c(
    compute_coverage(token_words, afinn_words),
    compute_coverage(token_words, sentiword_words),
    compute_coverage(token_words, senticnet_words)
  )
)

print(coverage_results)





##################################################################

#################################################################
# Build a unigram SenticNet lexicon (word → polarity value) for token joins.
senticnet <- hash_sentiment_senticnet %>%
  filter(!str_detect(x, " ")) %>%
  rename(word = x, value = y)

# Compute chapter-level sentiment as a token-frequency–weighted sum per (book, chapter).
senticnet_sentiment <- book_tokens %>%
  inner_join(senticnet, by = "word") %>%
  group_by(book, chapter) %>%
  summarise(sentiment_score = sum(value), .groups = "drop")

# Plot sentiment trajectories by chapter for each book.
ggplot(senticnet_sentiment, aes(x = chapter, y = sentiment_score)) +
  geom_line() +
  facet_wrap(~ book, scales = "free_x") +
  labs(title = "", y = "Sentiment score", x = "chapter") +
  theme_bw() +
  theme(strip.text = element_text(size = 13, face = "bold"))

# polarity + word_counts 
sentiment_words <- book_tokens %>%
  inner_join(senticnet, by = "word") %>%
  mutate(polarity = if_else(value > 0, "positive", "negative"))

word_counts <- sentiment_words %>%
  count(book, polarity, word, sort = TRUE)
#################################################################################
# Draw positive words for Treasure Island
word_counts %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea", polarity == "positive") %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Greens")))

# Draw negative words for Treasure Island
word_counts %>%
  filter(book == "20000_Leagues_ Under_ the_ Sea", polarity == "negative") %>%
  with(wordcloud(word, n, max.words = 100, colors = brewer.pal(8, "Reds")))


book_list <- unique(word_counts$book)

# Draw a word cloud for each book (one positive and one negative)
for (book_name in book_list) {
  cat("book name：", book_name, "\n")

  word_counts %>%
    filter(book == book_name, polarity == "positive") %>%
    with(wordcloud(word, n, max.words = 100, 
                   colors = brewer.pal(8, "Greens") 
                   ))
  title(paste("Positive words in", book_name))
  
 
  readline(prompt = "Press Enter to view the negative word cloud:")
  
  # Negative word cloud
  word_counts %>%
    filter(book == book_name, polarity == "negative") %>%
    with(wordcloud(word, n, max.words = 100, 
                   colors = brewer.pal(8, "Reds")
                   ))
  title(paste("Positive words in", book_name))
  
  readline(prompt = "Press Enter to view the negative word cloud:")
}


# Per-book chapter sequence features:
# sort chapters, then compute first-order sentiment change (delta) within each book;
# create placeholders for topic/entity shifts to be filled later; first chapter’s delta is NA.
sentiment_record <- senticnet_sentiment  %>%
  arrange(book, chapter) %>%
  group_by(book) %>%
  mutate(
    delta_sentiment = sentiment_score - lag(sentiment_score),
    topic_shift = NA_real_,
    entity_shift = NA_real_
  ) %>%
  ungroup()


####################################################


