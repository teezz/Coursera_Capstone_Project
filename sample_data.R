## Load data
load("data/blogs.RData")
load("data/news.RData")
load("data/twitter.RData")

## Sample data (10% blogs & news, 7.5% twitter)
sample_blogs   <- blogs[sample(1:length(blogs),0.1*length(blogs))]
sample_news    <- news[sample(1:length(news),0.1*length(news))]
sample_twitter <- twitter[sample(1:length(twitter),0.075*length(twitter))]

## Save samples
save(sample_blogs, sample_news, sample_twitter, file= "data/sample_data.RData")

## Remove to have more disk space
rm(sample_blogs)
rm(sample_news)
rm(sample_twitter)
rm(blogs)
rm(news)
rm(twitter)

