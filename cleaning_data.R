


suppressMessages(library(quanteda))

## Load all data
#en.text <- textfile("data/final/en_US/*.txt", cache=FALSE)

## Load sampe data
load("data/sample_data.RData")
## Build one sample
en.text <- c(sample_blogs, sample_news, sample_twitter)


## Build corpus
corpus <- corpus(en.text)

save(corpus, file= "data/corpus.RData")

## Remove to have more disk space
rm(sample_blogs)
rm(sample_news)
rm(sample_twitter)
rm(en.text)
