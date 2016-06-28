
## Build a dfm (document feature matrix)
## By default: remove punctuation, whitespace, numbers, punctuation, converting to lowercase
## Remove twitter characters like # and @
## Remove vulgary (seawords) and “stopwords” (common words) that usually have no analytic value

## for parallel processing
suppressMessages(library(parallel)) 
suppressMessages(library(foreach))
suppressMessages(library(doParallel))

## Initialize parallel processing
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)



## Load th swearwords
load("data/corpus.RData")

swear.file<- file("data/swear_words", "rt")
swear.words <- suppressWarnings(readLines(swear.file))
close(swear.file)


## ngrams = 1
if (require(quanteda))
        dfm.unigram <- dfm(corpus, removeTwitter=TRUE, ngrams=1, ignoredFeatures = c(swear.words, stopwords("english")))
        
  
## Create frequency table
num.unigram <- length(features(dfm.unigram))
unigram.freq <- topfeatures(dfm.unigram, num.unigram)
unigram.freq <- as.data.frame(unigram.freq)
names(unigram.freq)[1] <- "freq"
unigram.freq["w1"] <- rownames(unigram.freq)
unigram.freq[,c("w2","w3", "w4","ngram")] <- NA ## Add additional empty columns
unigram.freq[,"ngram"] <- 1 ## Add val 1 for ngram column
if (require(data.table))
  unigram.freq.dt <- as.data.table(unigram.freq) ## Convert to data.table
  unigram.freq.dt <- subset(unigram.freq.dt, freq > 1) ## Removing “singletons” (terms occuring once) by filter ngrams with frequency > 2

save(unigram.freq.dt, file= "data/stop/freq_unigram_dt.RData")

rm(dfm.unigram)
rm(unigram.freq)


## ngrams = 2
dfm.bigram <- dfm(corpus, removeTwitter=TRUE, ngrams=2, ignoredFeatures = c(swear.words, stopwords("english")))


## Create frequency table
num.bigram <- length(features(dfm.bigram))
bigram.freq <- topfeatures(dfm.bigram, num.bigram)
bigram.freq <- as.data.frame(bigram.freq)
names(bigram.freq)[1] <- "freq"
bigram.freq["w1"] <- rownames(bigram.freq)
bigram.freq[,c("w2","w3", "w4")] <- NA ## Add additional empty columns
bigram.freq[,"ngram"] <- 2 ## Add val 2 for ngram column
bigram.freq.dt <- as.data.table(bigram.freq) ## Convert to data.table
bigram.freq.dt <- subset(bigram.freq.dt, freq > 1) ## Removing “singletons” (terms occuring once)
bigram.freq.dt[, c("w1", "w2") := tstrsplit(bigram.freq.dt[[2]], "_", fixed=TRUE)][]

save(bigram.freq.dt, file= "data/stop/freq_bigram_dt.RData")

rm(dfm.bigram)
rm(bigram.freq)



## ngrams = 3
dfm.trigram <- dfm(corpus, removeTwitter=TRUE, ngrams=3, ignoredFeatures = c(swear.words, stopwords("english")))

## Create frequency table
num.trigram <- length(features(dfm.trigram))
trigram.freq <- topfeatures(dfm.trigram, num.trigram)
trigram.freq <- as.data.frame(trigram.freq)
names(trigram.freq)[1] <- "freq"
trigram.freq["w1"] <- rownames(trigram.freq)
trigram.freq[,c("w2","w3", "w4")] <- NA ## Add additional empty columns
trigram.freq[,"ngram"] <- 3 ## Add val 3 for ngram column
trigram.freq.dt <- as.data.table(trigram.freq) ## Convert to data.table
trigram.freq.dt <- subset(trigram.freq.dt, freq > 1) ## Removing “singletons” (terms occuring once)
trigram.freq.dt[, c("w1", "w2", "w3") := tstrsplit(trigram.freq.dt[[2]], "_", fixed=TRUE)][]

save(trigram.freq.dt, file= "data/stop/freq_trigram_dt.RData")

rm(dfm.trigram)
rm(trigram.freq)



# ngrams = 4
dfm.quadgram <- dfm(corpus, removeTwitter=TRUE, ngrams=4, ignoredFeatures = c(swear.words, stopwords("english")))

## Create frequency table
num.quadgram <- length(features(dfm.quadgram))
quadgram.freq <- topfeatures(dfm.quadgram, num.quadgram)
quadgram.freq <- as.data.frame(quadgram.freq)
names(quadgram.freq)[1] <- "freq"
quadgram.freq["w1"] <- rownames(quadgram.freq)
quadgram.freq[,c("w2","w3", "w4")] <- NA ## Add additional empty columns
quadgram.freq[,"ngram"] <- 4 ## Add val 4 for ngram column
quadgram.freq.dt <- as.data.table(quadgram.freq) ## Convert to data.table
quadgram.freq.dt <- subset(quadgram.freq.dt, freq > 1) ## Removing “singletons” (terms occuring once)
quadgram.freq.dt[, c("w1", "w2", "w3", "w4") := tstrsplit(quadgram.freq.dt[[2]], "_", fixed=TRUE)][]

save(quadgram.freq.dt, file= "data/stop/freq_quadgram_dt.RData")

rm(dfm.quadgram)
rm(quadgram.freq)


## Remove swearwords and corpus
rm(swear.words)
rm(corpus)


## Merge all ngrams into one
l <- list(unigram.freq.dt, bigram.freq.dt, trigram.freq.dt, quadgram.freq.dt)
freq.ngrams.dt <- rbindlist(l, use.names=TRUE)
freq.ngrams.dt[, ..I := .I]
setkey(freq.ngrams.dt,..I)

save(freq.ngrams.dt, file= "data/stop/freq_ngrams_dt.RData")


## Stop parallel processing
stopCluster(cluster)
rm(cluster)

rm(unigram.freq.dt)
rm(bigram.freq.dt)
rm(trigram.freq.dt)
rm(quadgram.freq.dt)
rm(freq.ngrams.dt)
rm(l)
