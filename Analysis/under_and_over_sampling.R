library(mlr)


set.seed(921021)

rm(list = setdiff(ls(), "d"))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rnd <- runif(n = dim(d)[1])
d <- d[rnd < 0.1, ]

factors <- c("was_promoted", 
             "visible_in_profile", "is_liquid", "excl_descr", 
             "has_capslock_descr", "excl_title", "has_capslock_title", 
             "no_price", "no_descr", "bad_cl_text", "good_cl_text",           
             "good_cl_categs",          
             "eight_to_sixteen",        
             "seventeen_to_twentyfour", 
             "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9",
             "is_weekend")


d_factors <- d_train %>% mutate_if(colnames(.) %in% factors, as.factor)



task = makeClassifTask(data = d_factors, target = "was_promoted")
task.over = oversample(task, rate = 5.85)
task.under = undersample(task.over, rate = 1/7.6)

table(getTaskTargets(task))
table(getTaskTargets(task.over))
table(getTaskTargets(task.under))
sum(table(getTaskTargets(task.under))) / nrow(d_factors)


d_undersampled <- getTaskData(task.under)

my_fun <- function(x){as.numeric(as.vector(x))}
d_undersampled <- d_undersampled %>% mutate_if(colnames(.) %in% factors, my_fun)



