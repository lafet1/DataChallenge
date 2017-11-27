library(text2vec)
library(feather)

def <- read_feather("file.feather")
abc <- read_feather("doc_topic.feather")


lda_model = LDA$new(n_topics = 50, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = lda_model$fit_transform(x = dtm, n_iter = 500, 
                          convergence_tol = 0.001, n_check_convergence = 25)




doc_topic_distr <- doc_topic_distr %>% as.data.frame()
doc_topic_distr <- doc_topic_distr %>% rownames_to_column(var = "id")


subdf <- subdf %>% inner_join(doc_topic_distr, by = "id")
write_feather(subdf, "file.feather")




lda_model_title <- LDA$new(n_topics = 25, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr_title <- lda_model_title$fit_transform(x = dtm_title, 
                                                n_iter = 250, 
                                                convergence_tol = 0.001, 
                                                n_check_convergence = 25)


doc_topic_distr_title <- doc_topic_distr_title %>% 
  as.data.frame()
doc_topic_distr_title <- doc_topic_distr_title %>% rownames_to_column(var = "id")


def <- def %>% inner_join(doc_topic_distr_title, by = "id")
write_feather(def, "file.feather")
write_feather(doc_topic_distr_title, "doc_topic.feather")
doc_topic_distr_title <- abc %>% as.tbl()
colnames(doc_topic_distr_title) <- c("id", paste("titleV", 1:25, sep = "_"))

def$title <- NULL
def$description <- NULL

write_feather(def, "file_notext.feather")
