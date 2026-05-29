# AIM: Topic analysis to vizualice patterns in current practices. We asked to participants 4 questions:
# 1) Challenges & barriers: What are the main challenges or barriers you face when working with different types of data format? Technical issues? Time constraints? Access or permissions?Lack of training or tools?
# 2) IT support infraestructure: How available and responsive is IT support at your institution when you need help managing different types of data format?
# Are the tools and infrastructure adequate for your needs?
# 3) Learned lessons: What key lessons have you learned from working with different types of data format?
# What worked well? What didn’t work well? Are there any tips, lessons, or useful tricks you would like to share?
# 4) Skills $ Capabilities: Do you have the knowledge and skills to work with different types of data format independently?
# Or do you rely on others (e.g., students, postdocs)? Or on external support (e.g., contractors, collaborators)?

# 0. Call packages and functions ----
library(tidyverse)       # To manipulate data and plotting
library(readxl)          # For reading data in Excel format
library(tidytext)
library(topicmodels)
library(RColorBrewer)
library(quanteda)       # A library for quantitative text analysis
library(wordcloud)
library(patchwork)       # To combine ggplots

# 1. Bring the data -----
practice_data <- read_excel("input/EIW_FeedbackWorkshops_Miro_Content.xls", sheet = 3)
str(practice_data) 

# 2. Topic model ----
results <- practice_data |>
  split(list(practice_data$board_name, practice_data$quadrant)) |>
  map(~ {
    
    # Create tokens
    toks <- .x |>
      corpus(docid_field = "doc_id", text_field = "practice") |>
      # Remove punctuation and numbers
      tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
      # Remove common words in English
      tokens_select(stopwords("en"), selection = "remove") |>
      # Simplify the words
      tokens_wordstem()
    
    # Apply custom filter
    toks <- tokens_select(
      toks,
      c(
        "a", "are","and", "anymore", "also", "analysis", "always", "australia",
        "be", "can",
        "do","due","data",
        "format", "files", "file",
        "ecosystem", "e.g.", "e.g", "etc",
        "indicator", 
        "language",
        "platform", "programming",
        "rely", 
        "support",  "siever",
        "tools", "tool", 
        "us",
        "yes"
        ),
      selection = "remove"
    )
    
    # Create bigrams (or unique words in this case)
    toks_bigram <- tokens_ngrams(toks, n = 1)
    
    # Create dfm
    dfm_mat <- dfm(toks_bigram) |>
      dfm_trim(min_termfreq = 1)
    
    # Skip empty or too small groups (with only 1 comment)
    if (ndoc(dfm_mat) < 2 || nfeat(dfm_mat) < 4) return(NULL)
    
    # Run LDA
    lda <- dfm_mat |>
      convert(to = "topicmodels") |>
      LDA(k = 3, control = list(seed = 42))
    
    # Extract the top 10 terms
    terms_df <- as.data.frame(terms(lda, 10))
    
    # Add metadata
    terms_df$board_name <- unique(.x$board_name)
    terms_df$quadrant <- unique(.x$quadrant)
    
    return(terms_df)
  })

# Combine results
topic_results <- bind_rows(results, .id = "group_id")

head(topic_results)


# 3. Plot it ------
ecosystem_palette <- c(
  "#0B3C5D",  # dark blue
  "#9FC5E8",  # light blue
  "#6AA84F",  # green
  "#F1C232",  # yellow
  "#E69138"   # orange
)


# Create the basic data frame
topic_long <- topic_results |>
  pivot_longer(
    cols = starts_with("Topic"),
    names_to = "topic",
    values_to = "term"
  ) |>
  # ranking within topic
  group_by(board_name, quadrant, topic) |>
  mutate(rank = row_number()) |>
  ungroup()
  
# Word cloud by topic and quadrant
topic_long |>
  group_split(board_name, quadrant) |>
  walk(~ {
    df <- .x
    wordcloud(
      words = df$term,
      freq = rev(seq_along(df$term)),  # rank-based weight
      max.words = 50,
      colors = RColorBrewer::brewer.pal(8, "Dark2")
    )
    title(paste(unique(df$board_name), "-", unique(df$quadrant)))
  })

# Term frequency plot
plot_freqterms <- topic_long |>
  group_by(quadrant) |>
  count(term) |>
  filter(n >= 2) |>
  ggplot(aes(x = reorder_within(term, -n, quadrant),
             y = -n,
             fill = quadrant)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ quadrant, scales = "free") +
  coord_flip() +
  labs(
    y = "Number of times the term is mentioned",
    x = "Term",
    title = "Most frequently mentioned terms across learning quadrants",
    subtitle = "Terms appearing at least twice in participant responses, grouped by familiarity level (from comfort zone to beyond limits)"
  ) +
  scale_x_reordered() +
  scale_fill_manual(values = ecosystem_palette) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 10, hjust = 0)
  )


ggsave( "output/plot_freqterms.png", plot = plot_freqterms, width = 10, height = 10)
