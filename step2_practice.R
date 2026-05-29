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
library(tm)
library(wordcloud)
library(topicmodels)
library(RColorBrewer)
library(quanteda)       # A library for quantitative text analysis


# 1. Bring the data -----
practice_data <- read_excel("input/EIW_FeedbackWorkshops_Miro_Content.xls", sheet = 3)
str(practice_data) 


# 2. Text cleaning ----
## 2.1  General cleaning ----
practice_data <- practice_data |>
  mutate(practice = tolower(practice)) |>
  group_by(board_name, quadrant) |>
  mutate(
    doc_id = paste0(
      board_name, "_",
      quadrant, "_",
      str_pad(row_number(), width = 2, pad = "0")
    )
  ) |>
  ungroup()


## 2.2 Tokenization ----
# Tokenization (lexical analysis) is the process of splitting a text into tokens
# (i.e. convert the text into smaller, more specific text features, such as words or word combinations)
# There are many ways to tokenize text (by sentence, by word, or by line). 
# For our data, we tokenized by words.
# Notice that we remove punctuation and numbers along the way.
# First check if there are duplicate presentations by checking for pres_id

# Tokens for the practices text
practice_toks <- practice_data |>
  # Create a corpus in which each line is a participant comments. 
  # Keep the quadrant and board as document identifier
  corpus(docid_field = "doc_id", text_field = "practice")  |> 
  tokens(remove_punct = TRUE, remove_numbers = TRUE) |>
  # Remove common stop words en English
  quanteda::tokens_select(quanteda::stopwords('en'), selection = 'remove') |>
  # Simplify words
  quanteda::tokens_wordstem(language = quanteda::quanteda_options("language_stemmer")) 

#Let's see the first 10 tokens in the 2nd participant's comment
head(practice_toks[[20]], 10) 


# Still there are few stop words. 
# Let's create a customized list of filter words,
# simplify words to avoid confusion with similar and plural terms, and
# create n-gram (bigram. (tokens in sequence)
practice_toks <- practice_toks |>
  # List of filter words. I'm including conservation and social as these are basic to this topic
  quanteda::tokens_select(c(
    "a", "are","and", "anymore",
    "format","also","platform","always","analysis",
    "be", "can",
    "do","due","data",
    "etc", "tools", "rely", "files", "indicator", "ecosystem",
    "programming", "languages", "us"), 
                          selection = 'remove')

#Let's see the first 50 tokens in the 4th abstract
head(practice_toks[[20]], 50) 

# Now create the bigram
practice_bigram <-  practice_toks |>
  # Create bigram
  quanteda::tokens_ngrams(n = 2) 

# Show the bi-gram for the 20th participant's comment
practice_bigram[[20]] 


# Create DTM ------
# Create a document term matrix (DTM). A DTM is a matrix in which rows are documents, columns are terms, and cells indicate how often 
# each term occurred in each document.
dfm_practice_bigram <- quanteda::dfm(practice_bigram) # Using bigrams
# Document-feature matrix of: 109 documents, 860 features (99.05% sparse) and 2 docvars.

# Visualize 
# Here we use the version with just stop words, but also we can make it using the bigrams with the concept unification
feat_practice <- names(topfeatures(dfm_practice_bigram, 50))

fcm_practice <- dfm_practice_bigram |>
  fcm() |> #Create the fcm
  fcm_select(feat_practice) #Select the top 50

# Make size proportional to freq.
size <- (colSums(dfm_select(fcm_practice, feat_practice))) 

# Now plot it!
quanteda.textplots::textplot_network(fcm_practice, 
                                     vertex_size = size/max(size) * 3,
                                     vertex_color = "#4D4D4D",
                                     # Use only the bigram mentioned at least twice
                                     min_freq = 1, 
                                     vertex_labelsize = 3)



# LDA model -----
# Latent Dirichlet allocation is one of the most common algorithms for topic modeling
lda_practice <- dfm_practice_bigram |>
  # Keep only words occurring >= 4 times 
  dfm_trim(min_termfreq = 1) |> 
  convert(to = "topicmodels") |>
  # Set the number of topics to 4 and control parameter to avoid changes in the models at every run
  LDA(k = 5, control = list(seed = 42)) 

# Show top 10 words pertaining to each topic
terms(lda_practice, 10) 

## 4.2 Beta probability -----
# Beta probability is the per-topic-per-word probability.
# For each combination, the model computes the probability of that term being generated from that topic
topic_detected <- lda_abstract |>
  # Per-topic-per-word probabilities, called  β(“beta”)
  tidy(matrix = "beta") |> 
  group_by(topic) |>
  # Find the top 10 terms within each topic
  slice_max(beta, n = 10) |> 
  ungroup() |>
  arrange(topic, -beta) |>
  mutate(term = reorder_within(term, beta, topic)) |>
  # Define a label for the topics based on the word with the highest frequency 
  mutate(topic_label = case_when(
    topic == 1 ~ "Communities",
    topic == 2 ~ "Ecosystem services",
    topic == 3 ~ "Behaviour change",
    topic == 4 ~ "Climate change"
  ))

topic_detected |>
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_label, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(y = "Beta probability",
       x = "") +
  scale_fill_manual(values = c("#5E5752FF",  "#E8CFABFF",  "#F5BC5CFF", "#E2853AFF", "black")) +
  theme_linedraw() +
  theme(
    strip.background = element_rect(fill = "black", color = NA),
    strip.text = element_text(color = "white"),
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(colour = "white")
  )

ggsave("output/topic_detected.png", width = 8, height = 5)























# 2. Word clould ------
custom_stopwords <- c(
  "a", "are","and", "anymore",
  "format","also","platform","always","analysis",
  "be", "can",
  "do","due","data",
  "etc", "tools", "rely", "files", "indicator", "ecosystem",
  "programming", "languages")
  

word_freq <- practice |> 
  # Change capital for lower case
  mutate(practice = tolower(practice)) |> 
  # Create tokens
  unnest_tokens(word, practice) |> 
  # removes common English stop words from your dataset.
  anti_join(stop_words) |> 
  # Remove custom stop words
  filter(!word %in% custom_stopwords) |>
  # keeps only words that contain at least one lowercase letter (a–z). That is remove spcial characteres and numbers
  filter(str_detect(word, "[a-z]")) |>
  # Count the tokens
  count(board_name, word, sort = TRUE)

head(word_freq)

par(mfrow = c(3, 2))  # layout

topics <- unique(word_freq$board_name)

for (t in topics) {
  temp <- word_freq |> filter(board_name == t)
  
  wordcloud(
    words = temp$word,
    freq = temp$n,
    max.words = 100,
    colors = brewer.pal(8, "Dark2"),
    scale = c(3, 0.8),
    random.order = FALSE
  )
  title(t)
}



word_freq2 <- practice |> 
  mutate(practice = tolower(practice)) |> 
  unnest_tokens(word, practice) |> 
  anti_join(stop_words) |> 
  filter(str_detect(word, "[a-z]")) |>
  count(board_name, quadrant, word, sort = TRUE)

# Example: one topic
word_freq2 |> 
  filter(board_name == "data_collection", quadrant == "challenges") |> 
  with(wordcloud(word, n, max.words = 80, colors = "red"))

# 3. Topic modelling ------


dtm_df <- practice |> 
  mutate(doc_id = row_number()) |> 
  unnest_tokens(word, practice) |> 
  anti_join(stop_words) |> 
  count(doc_id, word) |> 
  cast_dtm(doc_id, word, n)

lda_model <- LDA(dtm_df, k = 4, control = list(seed = 123))

# Extract topics
topics <- tidy(lda_model, matrix = "beta")

top_terms <- topics |> 
  group_by(topic) |> 
  slice_max(beta, n = 10) |> 
  ungroup()

ggplot(top_terms, aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Topic model by board name 
results <- practice |>
  split(practice$board_name) |> 
  map(~ {
    dtm <- .x |> 
      mutate(doc_id = row_number()) |> 
      unnest_tokens(word, practice) |> 
      anti_join(stop_words) |> 
      count(doc_id, word) |> 
      cast_dtm(doc_id, word, n)
    
    lda <- LDA(dtm, k = 3, control = list(seed = 123))
    
    tidy(lda, matrix = "beta") |> 
      group_by(topic) |> 
      slice_max(beta, n = 8) |> 
      mutate(boad_name = unique(.x$boad_name))
  })

topic_results <- bind_rows(results)


ggplot(topic_results,
       aes(reorder_within(term, beta, topic), beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_grid( ~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()



