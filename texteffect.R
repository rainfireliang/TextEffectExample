############################## Pre-Process ####################################
library(stringr)
library(dplyr)
setwd("D:/Dropbox/Projects/Digital Marketing on Twitter/HGX/HotelReviews")
reviews <- read.csv("D:/Dropbox/Projects/Digital Marketing on Twitter/HGX/HotelReviews/reviews.csv", stringsAsFactors=FALSE)

#
cln = colnames(reviews)
ix = str_detect(cln,"NPS")
reviews = reviews[,!ix]
reviews$reviewerID = c(1:nrow(reviews)) #5325 reviewers

demographics = reviews[,c("reviewerID","Q3319","Q3320_1","Q3321","Q3321_6_TEXT","Q3322","Q3323")]
colnames(demographics) = c("reviewerID","gender","age","enth","other","house_size","income")

ratings = reviews[,1:4110]
txt_ratings = data.frame()
for (i in 1:822){
  x=ratings[,c((1+5*(i-1)):((1+5*(i-1))+4))]
  x$UID=c(1:nrow(x))
  colnames(x) = c("trust","useful","rate","select","recommend","UID")
  x=x[!is.na(x$rate),]
  x$txtID = i
  txt_ratings = bind_rows(txt_ratings,x)
  print(i)
}
save(txt_ratings,file='txt_ratings.Rdata')

#
reviews_10_Finished <- read_excel("reviews_10_Finished.xlsx")
reviews_20_Finished <- read_excel("reviews_20_Finished.xlsx")
colnames(reviews_20_Finished) <- colnames(reviews_10_Finished)
reviews_30_Finished <- read_excel("reviews_30_Finished.xlsx")
colnames(reviews_30_Finished) <- c("X__1","ID","reviewIDs","reviewRatings","reviewTxts","hotel_links","cluster")
reviews_40_Finished <- read_excel("reviews_40_Finished.xlsx")
colnames(reviews_40_Finished) <- colnames(reviews_10_Finished)
reviews_50_Finished <- read_excel("reviews_50_Finished.xlsx")
colnames(reviews_50_Finished) <- colnames(reviews_10_Finished)

reviews_txt = bind_rows(reviews_10_Finished,reviews_20_Finished,reviews_30_Finished,reviews_40_Finished,reviews_50_Finished)
reviews_txt$X__1 = NULL
colnames(reviews_txt)[1] = "txtID"

data = merge(txt_ratings,reviews_txt,by="txtID")
data = merge(data,demographics,by.x="UID",by.y='reviewerID')
save(data,file='data.Rdata')

write.csv(data,file='data.csv',row.names = F,quote = T)

####################################################################################
library(quanteda)
load("data.Rdata")

data$reviewRatings = as.numeric(data$reviewRatings)/10
res = data %>% group_by(reviewRatings,useful) %>% summarise(x=mean(rate,na.rm=T))
res = res[!is.na(res$useful),]

library(ggplot2)
ggplot() +
  geom_line(data = res, aes(x = reviewRatings, y = x, color = factor(trust)), size = 1)


col <- tokens(unique(data$reviewTxts)) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_select('^[a-zA-Z]', valuetype = 'regex', padding = TRUE) %>% 
  textstat_collocations(min_count = 2) %>%
  arrange(desc(count))
head(col,50)

##################

comp_tok <- tokens_compound(tokens_tolower(tokens(data$reviewTxts[!is.na(data$useful)],
                                                  remove_numbers = TRUE,  remove_punct = TRUE)), phrase(c(col$collocation[1:50],"las vegas")))
#head(kwic(comp_toks3, c('United_States', 'New_York')))

#cp = corpus(data$reviewTxts)
#docvars(cp,"select") = data$select
review_dfm <- dfm(comp_tok,
                remove = c(stopwords("english"),"just","via","continental","inn","us","also","onto"),
                stem = F,
                remove_punct = TRUE, 
                remove_numbers = TRUE)
review_dfm <- dfm_remove(review_dfm,valuetype="regex",min_nchar=3)
review_dfm <- dfm_trim(review_dfm, min_docfreq = round(0.005*nrow(data)),max_docfreq = round(0.995*nrow(data)))

Y = data$useful[!is.na(data$useful)]
X = convert(review_dfm,to="matrix")

################
#install.packages('texteffect')
library('texteffect')
set.seed(123)
train.ind <- get_training_set(X, Y, training.prop = 0.8)
# Fit an sIBP on the training data
sibp.fit <- sibp(X, Y, K = 10, alpha = 2, sigmasq.n = 0.8, train.ind = train.ind)
sibp_top_words(sibp.fit, colnames(X),num.words = 20)
save(sibp.fit,file='sibp.fit1.Rdata')

# Pre-process the test set
X.test <- t(apply(X, 1, function(x) (x - sibp.fit$meanX)/sibp.fit$sdX))
# Infer the latent treatments in the test set
infer_Z(sibp.fit, X.test)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 4, alphas = c(2,4), sigmasq.ns = c(0.8, 1),
                                 iters = 1, train.ind = train.ind)
## Not run:
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)
# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 10, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 10, verbose = TRUE)
# Select the most interest treatments to investigate
sibp.fit <- sibp.search[["4"]][["0.8"]][[1]]
# Estimate the AMCE using the test set
amce<-sibp_amce(sibp.fit, X, Y)
# Plot 95% confidence intervals for the AMCE of each treatment
sibp_amce_plot(amce)


#######################################
comp_tok <- tokens_compound(tokens_tolower(tokens(data$reviewTxts[!is.na(data$trust)],remove_numbers = TRUE,  remove_punct = TRUE)),
                            phrase(c(col$collocation[1:50],"las vegas")))
review_dfm <- dfm(comp_tok,
                  remove = c(stopwords("english"),"just","via","continental","inn","us","also","onto"),
                  stem = F,
                  remove_punct = TRUE, 
                  remove_numbers = TRUE)
review_dfm <- dfm_remove(review_dfm,valuetype="regex",min_nchar=3)
review_dfm <- dfm_trim(review_dfm, min_docfreq = round(0.005*nrow(data)),max_docfreq = round(0.995*nrow(data)))
Y = data$trust[!is.na(data$trust)]
X = convert(review_dfm,to="matrix")
train.ind <- get_training_set(X, Y, training.prop = 0.8)
sibp.fit2 <- sibp(X, Y, K = 15, alpha = 2, sigmasq.n = 0.8, train.ind = train.ind) #6 ok #4 better #5 very good #8 better # 20/15 too many
sibp_top_words(sibp.fit2, colnames(X),num.words = 20)
save(sibp.fit2,file='sibp.fit2.Rdata')

amce2<-sibp_amce(sibp.fit2, X, Y)
sibp_amce_plot(amce2)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 5, alphas = c(2,4), sigmasq.ns = c(0.8, 1),
                                 iters = 1, train.ind = train.ind)
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)
# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["2"]][["0.8"]][[1]], colnames(X), 20, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["0.8"]][[1]], colnames(X), 20, verbose = TRUE) # OK

##############################################################
load("data.Rdata")
txtIDs = unique(data$txtID)
txts = data$reviewTxts[!duplicated(data$txtID)]

options(java.parameters = "-Xmx4096m")

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_101')
library(rJava)
library(coreNLP,quietly = T)
initCoreNLP()

txts_lemma = c()
for (i in 1:length(txts)){
  output = annotateString(txts[i])
  lst = getToken(output) #lemmatization+POS+NER 
  nsen = paste(lst$lemma,collapse = " ")
  txts_lemma = c(txts_lemma,nsen)
  print(i)
}

texts = data.frame(txtIDs,txts_lemma,stringsAsFactors = F)
data = merge(data,texts,by.x='txtID',by.y='txtIDs')
save(data,file='data.Rdata')

##
load("data.Rdata") #data with lemmas

data$reviewRatings = as.numeric(data$reviewRatings)/10
data$trust = data$trust-1
data$rate[data$rate>10]=10
res = data %>% group_by(reviewRatings,trust) %>% summarise(x=mean(rate,na.rm=T))
res = res[!is.na(res$trust),]

library(ggplot2)
ggplot() +
  geom_line(data = res, aes(x = reviewRatings, y = x, color = factor(trust)), size = 1)
write.csv(res,file='res.csv',row.names = F)

#
library(lme4)
lmd = lmer(rate~reviewRatings*trust+(1|UID)+(1|txtID),data)
summary(lmd)
library(MuMIn)
r.squaredGLMM(lmd)
library(sjstats)
icc(lmd)

k=data[!duplicated(data$UID),]

library(quanteda)
col <- tokens(unique(data$txts_lemma)) %>% 
  tokens_remove(stopwords("english")) %>% 
  tokens_select('^[a-zA-Z]', valuetype = 'regex', padding = TRUE) %>% 
  textstat_collocations(min_count = 2) %>%
  arrange(desc(count))
head(col,50)

comp_tok <- tokens_compound(tokens_tolower(tokens(data$txts_lemma[!is.na(data$trust)],remove_numbers = TRUE,  remove_punct = TRUE)),
                            phrase(c(col$collocation[1:50],"las vegas")))
review_dfm <- dfm(comp_tok,
                  remove = c(stopwords("english"),"just","via","continental","inn","us","also","onto","else","still","lrb","rrb"),
                  stem = F,
                  remove_punct = TRUE, 
                  remove_numbers = TRUE)
review_dfm <- dfm_remove(review_dfm,valuetype="regex",min_nchar=3)
review_dfm <- dfm_trim(review_dfm, min_docfreq = round(0.005*nrow(data)),max_docfreq = round(0.99*nrow(data)))

Y = data$trust[!is.na(data$trust)]
X = convert(review_dfm,to="matrix")

library(texteffect)
set.seed(12345)
train.ind <- get_training_set(X, Y, training.prop = 0.7)
sibp.fit <- sibp(X, Y, K = 5, alpha = 4, sigmasq.n = 1, train.ind = train.ind) #6 ok #4 better #5 very good #8 better # 20/15 too many
wds = sibp_top_words(sibp.fit, colnames(X),num.words = 20,verbose = T)
#save(sibp.fit,file='sibp.fit.Rdata')

amce<-sibp_amce(sibp.fit, X, Y)
sibp_amce_plot(amce) # 8(4) 10(4) 6(2) 5(2) 9(4)
write.csv(amce,file='amce.csv',row.names = F,quote = F)

# Search sIBP for several parameter configurations; fit each to the training set
sibp.search <- sibp_param_search(X, Y, K = 5, alphas = c(2,3,4), sigmasq.ns = c(0.8, 0.9,1),
                                 iters = 1, train.ind = train.ind)
# Get metric for evaluating most promising parameter configurations
sibp_rank_runs(sibp.search, X, 10)

# Qualitatively look at the top candidates
sibp_top_words(sibp.search[["2"]][["1"]][[1]], colnames(X), 20, verbose = TRUE)
sibp_top_words(sibp.search[["4"]][["1"]][[1]], colnames(X), 20, verbose = TRUE) # OK

amce<-sibp_amce(sibp.search[["4"]][["1"]][[1]], X, Y)
sibp_amce_plot(amce)

k=data$txts_lemma[grepl("lyft",data$txts_lemma)]
p=data$reviewTxts[grepl("lyft",data$txts_lemma)]

#
f.fit = sibp.search[["2"]][["1"]][[1]]
topicA = f.fit$nu
save(f.fit,file='f.fit.Rdata') # 5 treatments, 2, 1
