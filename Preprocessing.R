# Load necessary libraries
library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(syuzhet)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(purrr)
library(ldatuning)
library(topicmodels)
library(purrr)
library(forcats)
library(rBayesianOptimization)
library(grid) 

# Read the dataset into the script
file_paths <- list.files("data/raw_data", pattern = "\\.txt$", full.names = TRUE)

book_titles <- tools::file_path_sans_ext(basename(file_paths))

# Build a single data frame of raw lines from each book (one row per line).
raw_books <- map2_df(file_paths, book_titles, function(path, title) {
  # Read file as UTF-8; suppress warning about missing trailing newline.
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  tibble(
    book = title,
    text = lines
  )
})

# Defining regular and alternate regular
chapter_regex <- regex(
  "^\\s*(chapter\\s+(\\d+|[ivxlc]{1,6})[.]?(\\s+[^\\n]{1,80})?\\s*)$|^\\s*(\\d+|[ivxlc]{1,6})\\s*$",
  ignore_case = TRUE
)

# Exact "contents" line; used to suppress the nearby table-of-contents region.
contents_regex <- regex("^\\s*contents\\s*$", ignore_case = TRUE)

# General chapter identification function
process_one_book <- function(df, chapter_regex) {
  df <- df %>% mutate(linenumber = row_number())
  
  # Identify "directory" lines and only look for them in the first 100 lines
  content_line <- which(df$linenumber <= 100 & str_detect(df$text, contents_regex))[1]
  
  # Defines whether it is a chapter
  df <- df %>%
    mutate(
      is_chapter = str_detect(text, chapter_regex),
      is_content_zone = if (!is.na(content_line)) linenumber > content_line & linenumber <= content_line + 100 else FALSE,
      is_year_like = str_detect(text, "^\\s*\\d{4}\\s*$"),
      is_chapter = if_else(is_content_zone | is_year_like, FALSE, is_chapter),
      chapter = cumsum(is_chapter)
    ) %>%
    select(-is_chapter, -is_content_zone)
  
  return(df)
}

# Apply chapter detection per book and recombine:
# For each `book`, run `process_one_book()` to add per-book `linenumber` and `chapter` IDs.
original_books <- raw_books %>%
  group_by(book) %>%
  group_modify(~ process_one_book(.x, chapter_regex)) %>%
  ungroup()

# Summarize detected chapter counts per book (unique headers only) and print all rows.
original_books %>%
  filter(chapter > 0) %>%
  distinct(book, chapter) %>%
  count(book, name = "n_chapters") %>%
  arrange(desc(n_chapters)) %>%
  print(n = Inf)

# List the first occurrence of each chapter header per book (line number + header text), then print all.
original_books %>%
  filter(chapter > 0) %>%
  group_by(book, chapter) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(book, linenumber, text, chapter) %>%
  print(n = Inf)

# Trim trailing Gutenberg license/metadata per book:
# For each book, find the first "*** END OF THE PROJECT GUTENBERG EBOOK" marker (case-insensitive)
# and drop all lines at/after it; if absent, keep the book unchanged.
end_trimmed_books <- original_books %>%
  group_by(book) %>%
  group_modify(~ {
    end_line <- which(str_detect(.x$text, fixed("*** END OF THE PROJECT GUTENBERG EBOOK", ignore_case = TRUE)))[1]
    if (!is.na(end_line)) {
      .x <- .x %>% filter(linenumber < end_line)
    }
    return(.x)
  }) %>%
  ungroup()

# Drop the diagnostic helper column; keep only fields needed downstream.
end_trimmed_books <- end_trimmed_books %>%
  select(-any_of("is_year_like"))

# Output the detection log to check whether each book is successfully truncated
trim_check <- original_books %>%
  group_by(book) %>%
  summarise(
    end_line = which(str_detect(text, fixed("*** END OF THE PROJECT GUTENBERG EBOOK", ignore_case = TRUE)))[1],
    total_lines_before = n()
  ) %>%
  left_join(
    end_trimmed_books %>%
      group_by(book) %>%
      summarise(total_lines_after = n()),
    by = "book"
  ) %>%
  mutate(
    was_trimmed = !is.na(end_line),
    lines_removed = total_lines_before - total_lines_after
  ) %>%
  select(book, was_trimmed, end_line, lines_removed) %>%
  arrange(desc(lines_removed))

print(trim_check, n = Inf)

#######################################################################
saveRDS(end_trimmed_books, file = "data/processed/final_books.rds")

# Remove non-content lines: empty strings, whitespace-only, or lines made only of dashes/asterisks.
cleaned_books <- end_trimmed_books %>%
  filter(text != "", !str_detect(text, "^\\s*$"), !str_detect(text, "^[-–—\\*\\s]*$"))

# Normalize line text for stable matching/tokenization: collapse whitespace, standardize curly quotes to ASCII,
cleaned_books <- cleaned_books %>%
  mutate(
    text = str_squish(text),                          
    text = str_replace_all(text, "[‘’]", "'"),       
    text = str_replace_all(text, '[“”]', '"'),        
    text = str_to_lower(text)                        
  )

# Tokenize cleaned lines into lowercase words, remove stop words, and keep alphabetic tokens only.
book_tokens <- cleaned_books %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word") %>%
  filter(str_detect(word, "[a-z]")) 
##############################################################################
saveRDS(book_tokens, file = "data/processed/book_tokens.rds")

# For each book, compute token frequencies and print the top 5 words
book_tokens %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = 5) %>%
  ungroup() %>%
  print(n = Inf)

top_n <- 5

# Calculate word frequency by book group and extract the top N
top_words_per_book <- book_tokens %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  slice_max(n, n = top_n) %>%
  ungroup()

# Generate a word frequency graph for each book and save it as a list
plots <- top_words_per_book %>%
  group_split(book) %>%
  map(~ {
    book_title <- unique(.x$book)
    
    ggplot(.x, aes(x = n, y = fct_reorder(word, n))) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste("Top", top_n, "Words in", book_title),
        x = "Frequency", y = NULL
      ) +
      theme_minimal(base_size = 12)
  })

plots[[10]] 


# Unify factor levels 
books_levels <- book_tokens %>% distinct(book) %>% arrange(book) %>% pull(book)

# Define a fixed color map
book_palette <- setNames(brewer.pal(length(books_levels), "Paired"), books_levels)

# Word frequency visualization: descending order within each book
freq_plot_data <- top_words_per_book %>%
  mutate(
    book = factor(book, levels = books_levels),
    word = reorder_within(word, n, book)
  )

# Faceted horizontal bar charts of each book’s top words:
# - orders labels within facets, hides legend (color = book), applies custom palette,
# - tweaks spacing/typography for readability, then prints the plot.
p_freq <- ggplot(freq_plot_data, aes(x = n, y = word, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free_y", ncol = 2) +
  scale_y_reordered() +
  scale_fill_manual(values = book_palette) +
  labs(title = paste("Top", top_n, "Words per Book"),
       x = "Frequency", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 12, lineheight = 0.95), 
    strip.text  = element_text(size = 12, face = "bold"),
    plot.title  = element_text(size = 14, face = "bold"),
    panel.spacing.y = unit(8, "pt"),         
    plot.margin = margin(6, 10, 6, 10)
  )

p_freq



# Compute per-book TF–IDF: term counts per (book, word) → tidytext::bind_tf_idf → rank by distinctiveness.
tfidf_by_book <- book_tokens %>%
  count(book, word, sort = TRUE) %>%             
  bind_tf_idf(word, book, n) %>%                
  arrange(desc(tf_idf))                          

# For each book, take the top 5 most distinctive words by TF–IDF.
top_tfidf_words <- tfidf_by_book %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 5, with_ties = FALSE) %>%
  ungroup()

# Visualize each book’s top TF–IDF terms as horizontal bar charts:
# - reorder labels within facets by TF–IDF,
# - hide legend (color encodes book but facets already separate),
# - use free scales per facet for readability, with light typographic tweaks.
ggplot(top_tfidf_words, aes(tf_idf, fct_reorder(word, tf_idf), fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, scales = "free", ncol = 2) +
  labs(
    title = "Top 5 TF-IDF Words per Book",
    x = "TF-IDF Score", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 12, lineheight = 0.95))


