library(tidyverse)
library(feather)
library(text2vec)
library(tokenizers)
library(stringr)
library(slam)

###### EDA ######

# loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

share_of_data <- 0.01
set.seed(1211)
data <- read_feather("data/ph_ads_payment_indicator.feather")
subdf <- data %>% mutate(rnd = runif(dim(data)[1])) %>% filter(rnd < share_of_data)
rm(data)
save.image()

# data cleaning
prep_fun <- function(x) {
  x <- tolower(x) # lower case
  x <- str_replace_all(x, '[:digit:]', ' ') # removing numbers
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ')  # removing one and two letter words
  x <- str_replace_all(x, '\\s+', ' ') # removing white space
}
tok_fun <- function(x){tokenize_words(x, stopwords = stopwords())} # removing stopwords

it_train <- itoken(subdf$description, # data to clean
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = subdf$id, 
                  progressbar = TRUE)
vocab <- create_vocabulary(it_train)
pruned_vocab <- prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)
vectorizer <- vocab_vectorizer(pruned_vocab)

# creating DTM from cleaned data
t1 <- Sys.time()
dtm_train <- create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))


# some statistics
summary(col_sums(dtm_train))
sort(col_sums(dtm_train), decreasing = TRUE)[1:100]
dim(dtm_train)
dtm_train <- dtm_train[row_sums(dtm_train) > 0,]

# getting tfidf
tfidf <- dtm_train %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  as.tbl() %>%
  mutate(sums = rowSums(.)) %>% 
  mutate_all(funs(. / sums)) %>% 
  select(-sums) %>% sapply(mean) * 
  log2(dim(dtm_train)[1] / col_sums(dtm_train))
summary(tfidf)

# visualization
library(wordcloud)
freq <- data.frame(freqterms = sort(colSums(as.matrix(dtm_train)), decreasing = TRUE))
wordcloud(rownames(freq), freq[, 1], max.words=50, colors = brewer.pal(3, "Dark2"))

# getting tcm, necessary for GloVe
tcm <- create_tcm(it_train, vectorizer)
print(dim(tcm))

# semantic similarity setup
glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = pruned_vocab, x_max = 10)
vectors_main <- glove$fit_transform(tcm, n_iter = 20)
word_vectors_context <- glove$components
word_vectors <- vectors_main + t(word_vectors_context)
d <- dist2(word_vectors, method = "cosine")  # smaller values means closer
print(dim(d))

# finding the similar words
find_close_words <- function(w, d, n) {
  words <- rownames(d)
  i <- which(words == w)
  if (length(i) > 0) {
    res <- sort(d[i,])
    print(as.matrix(res[2:(n + 1)]))
  } 
  else {
    print("Word not in corpus.")
  }
}


find_close_words("flat", d, 10)
find_close_words("manila", d, 10)
find_close_words("bar", d, 10)
find_close_words("car", d, 10)
find_close_words("iphone", d, 10)

find_close_words("sunny", d, 10)
find_close_words("south", d, 10)
find_close_words("north", d, 10)
