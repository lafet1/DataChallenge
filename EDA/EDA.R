library(tidyverse)
library(feather)
library(text2vec)
library(tokenizers)
library(stringr)
library(slam)

share_of_data <- 0.01


set.seed(1211)
data <- read_feather("C:\\Users\\jan\\Documents\\Škola\\UvA\\Výuka\\Machine Learning for Econometrics\\Project\\data\\ph_ads_payment_indicator.feather")
subdf <- data %>% mutate(rnd = runif(dim(data)[1])) %>% filter(rnd<share_of_data)
rm(data)
save.image()

prep_fun = function(x) {
  x <- tolower(x)
  x <- str_replace_all(x, '[:digit:]', ' ')
  x <- str_replace_all(x, '\\b\\w{1,2}\\b',' ')
  x <- str_replace_all(x, '\\s+', ' ')
}
tok_fun <-function(x){tokenize_words(x, stopwords = stopwords())}

it_train = itoken(subdf$description, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = subdf$id, 
                  progressbar = TRUE)
vocab = create_vocabulary(it_train)
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(pruned_vocab)

t1 = Sys.time()
dtm_train = create_dtm(it_train, vectorizer)
print(difftime(Sys.time(), t1, units = 'sec'))



summary(col_sums(dtm_train))
sort(col_sums(dtm_train), decreasing=TRUE)[1:100]
dim(dtm_train)
dtm_train <- dtm_train[row_sums(dtm_train)>0,]

tfidf <- dtm_train %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  as.tbl() %>%
  mutate(sums = rowSums(.)) %>% 
  mutate_all(funs(./sums)) %>% 
  select(-sums) %>% sapply(mean) * 
  log2(dim(dtm_train)[1] / col_sums(dtm_train))
summary(tfidf)


library(wordcloud)
freq = data.frame(freqterms=sort(colSums(as.matrix(dtm_train)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(3, "Dark2"))

# # define tfidf model
# tfidf = TfIdf$new()
# # fit model to train data and transform train data with fitted model
# dtm_train_tfidf = fit_transform(dtm_train, tfidf)
# glimpse(dtm_train_tfidf)

tcm <- create_tcm(it_train, vectorizer)
print(dim(tcm))

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = pruned_vocab, x_max = 10)
vectors_main <- glove$fit_transform(tcm, n_iter = 20)
word_vectors_context <- glove$components
word_vectors = vectors_main + t(word_vectors_context)
d = dist2(word_vectors, method="cosine")  #Smaller values means closer
print(dim(d))


find_close_words = function(w,d,n) {
  words = rownames(d)
  i = which(words==w)
  if (length(i) > 0) {
    res = sort(d[i,])
    print(as.matrix(res[2:(n+1)]))
  } 
  else {
    print("Word not in corpus.")
  }
}


find_close_words("flat",d,10)
find_close_words("manila",d,10)
find_close_words("bar",d,10)
find_close_words("car",d,10)
find_close_words("iphone",d,10)

find_close_words("sunny",d,10)
find_close_words("south",d,10)
find_close_words("north",d,10)
