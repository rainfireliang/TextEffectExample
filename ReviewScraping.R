# url="https://www.tripadvisor.com/HotelsNear-g34438-qMIA-Miami_Florida.html"

library(readxl)
citylink <- read_excel("D:/Dropbox/citylink.xlsx")
citylink <- citylink[!is.na(citylink$URL),]

library(RCurl)
library(XML)

# Get Hotels
# GetHotels = function(url){
#   doc=htmlParse(getURL(url))
#   hotel_names=xpathSApply(doc,"//div[@id='ACCOM_OVERVIEW']//div[@class='listing_title']/a",xmlValue)
#   hotel_links=xpathSApply(doc,"//div[@id='ACCOM_OVERVIEW']//div[@class='listing_title']/a/@href")
#   hotel_links=paste0("https://en.tripadvisor.com.hk",hotel_links)
#   hotels = data.frame(cbind(hotel_names,hotel_links),stringsAsFactors = F)
#   return(hotels)
# }

GetAllHotels = function(url){
  doc=htmlParse(getURL(url))
  hotel_names=xpathSApply(doc,"//a[@data-clicksource='HotelName']",xmlValue)
  hotel_links=xpathSApply(doc,"//a[@data-clicksource='HotelName']/@href")
  hotel_links=paste0("https://www.tripadvisor.com",hotel_links)
  hotels = data.frame(cbind(hotel_names,hotel_links),stringsAsFactors = F)
  
  nextp=xpathSApply(doc,"//div[@class='unified pagination standard_pagination']/a/@href")
  nextp=paste0('https://www.tripadvisor.com',nextp)
  
  ls=as.numeric(xpathSApply(doc,"//div[@class='unified pagination standard_pagination']/@data-numpages"))
  k=1
  
  while(k<=min(10,ls)){
    doc=htmlParse(getURL(url))
    hotel_names=xpathSApply(doc,"//a[@data-clicksource='HotelName']",xmlValue)
    hotel_links=xpathSApply(doc,"//a[@data-clicksource='HotelName']/@href")
    hotel_links=paste0("https://www.tripadvisor.com",hotel_links)
    hotels =rbind(hotels, data.frame(cbind(hotel_names,hotel_links),stringsAsFactors = F))
    
    nextp=xpathSApply(doc,"//div[@class='unified pagination standard_pagination']/a[last()]/@href")
    nextp=paste0('https://www.tripadvisor.com',nextp)
    k=k+1
  }
  
  return(hotels[!duplicated(hotels),])
  
}
# x=GetAllHotels(url)

hotel_urls = data.frame()
k=0
for (url in citylink$URL){
  trsd = GetAllHotels(url)
  trsd$citylink=url
  hotel_urls = rbind(hotel_urls,trsd)
  k=k+1
  print(k)
}
save(hotel_urls,file='hotel_urls.Rdata')

# library(RSelenium)
# Sys.which("java")
# rD <- rsDriver(verbose = FALSE,port=4444L,browser="firefox")
# remDr <- rD$client
# remDr$navigate(url)
# 
# ps=remDr$getPageSource()[[1]]
# hts=GetHotels(ps)
# 
# nextb <- remDr$findElement(using = "class", "query")
# nextb$clickElement()
# 
# # close client/server
# remDr$close()
# rD$server$stop()



# Get Reviews
# url=hotel_links[1]
GetReviews_SinglePage = function(url){
  p=htmlParse(getURL(url,followlocation=TRUE))
  reviewIDs=xpathSApply(p,"//div[@class='review-container']//div[@class='reviewSelector']/@data-reviewid")
  reviewRatings=xpathSApply(p,"//div[@class='review-container']//div[@class='wrap']/div[@class='rating reviewItemInline']/span[1]/@class")
  reviewRatings=sapply(reviewRatings,function(x)substr(x,25,nchar(x)))
  reviewTxts=xpathSApply(p,"//div[@class='review-container']//div[@class='wrap']/div[@class='prw_rup prw_reviews_text_summary_hsx']",xmlValue)
  idx=grep('...More$',reviewTxts)
  for (i in idx){
    pg=htmlParse(getURL(paste0('https://www.tripadvisor.com/OverlayWidgetAjax?Mode=EXPANDED_HOTEL_REVIEWS&metaReferer=Hotel_Review&reviews=',
                               reviewIDs[i])))
    reviewTxts[i]=xpathApply(pg,"//p[@class='partial_entry']",xmlValue)[[1]]
  }
  
  df=cbind(reviewIDs,reviewRatings,reviewTxts)
  return(df)
}
GetReview = function(url){
  p=htmlParse(getURL(url))
  ls=as.numeric(xpathApply(p,"//span[@class='pageNum last taLnk ']/@data-page-number")[[1]])
  allRvs=GetReviews_SinglePage(url)
  if (ls>1){
    for (i in 2:min(20,ls)){
      st = paste0("-Reviews-or",5*(i-1),"-")
      nxt = gsub("-Reviews-",st,url)
      allRvs=rbind(allRvs,GetReviews_SinglePage(nxt))
    }
  }
  allRvs=data.frame(allRvs,stringsAsFactors = F)
  allRvs$hotel_links=url
  return(allRvs)
}

reviews = data.frame()
for (i in 828:nrow(hotel_urls)){
  reviews = rbind(reviews,GetReview(hotel_urls$hotel_links[i]))
  print(i)
}
save(reviews,file='reviews.Rdata')

###
library(quanteda)
load("reviews.Rdata")
x=unlist(reviews$reviewTxts) 
IDs=unlist(reviews$reviewIDs)
ratings=as.numeric(unlist(reviews$reviewRatings))

names(x)=IDs
rcorpus <- corpus(x)
save(rcorpus,file='rcorpus.Rdata')

summary(rcorpus) #79,265
rdfm <- dfm(rcorpus, tolower=TRUE, stem=TRUE, remove_punct = TRUE,remove_numbers=TRUE,
             remove_twitter=FALSE,remove=c(stopwords("english"), "t.co", "https", "rt", "amp", "http", "t.c", "can", "u"),
            ngrams=1, verbose=TRUE)
save(rdfm,file='rdfm.Rdata')

twdfm <- dfm_trim(rdfm, min_docfreq=30, verbose=TRUE)

# tokenize(tweets$text[1])
# tokens_wordstem(tokenize(tweets$text[1]))
# char_wordstem(c("win", "winning", "wins", "won", "winner"))

head(textstat_keyness(rdfm,target="",measure="chi2"), n=20)

rew <- tfidf(twdfm)
topfeatures(rew)


# K-Means Cluster Analysis
rt=50

mydata = rew[ratings==rt,]
fit <- kmeans(mydata, 5) # 5 cluster solution
# get cluster means 
k=fit$centers
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)
mydata$dis.index = 0
euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

for (i in 1:5){
  dp=mydata[mydata$fit.cluster==i,1:5099]
  dis=sapply(1:nrow(dp),function(x) euc.dist(c(unlist(dp[x,])),k[i,]))
  mydata$dis.index[mydata$fit.cluster==i]=dis
  print(i)
}
sel=data.frame(rk=which(ratings==rt),dis=mydata$dis.index,cluster=mydata$fit.cluster)
library(dplyr)
ssel=sel %>% 
  arrange(dis) %>% 
  group_by(cluster) %>% slice(1:40)
reviews_10 = reviews[ssel$rk,]
reviews_10$cluster = ssel$cluster

library(xlsx) # Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_101\\jre')
write.xlsx(reviews_10, "reviews_50.xlsx")

###################################
###################################
# Determine number of clusters
mydata=rew[ratings==10,]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 15) # 5 cluster solution
# get cluster means 
k=aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)


simils <- textstat_simil(rew, method="cosine")
# most similar documents
df <- data.frame(
  docname = rownames(simils),
  simil = as.numeric(simils),
  stringsAsFactors=F
)
tail(df[order(simils),])

# compute distances
distances <- textstat_dist(rew[ratings==10,])
as.matrix(distances)[1:5, 1:5]
# clustering
cluster <- hclust(distances)
plot(cluster)

