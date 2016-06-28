

library(stringi) ## Character string processing

filez <- list.files(path=data_dir, pattern="^en_.*.txt", recursive=T, full.names=T)

## Generic metrics function

## Blogs
con1 <- file(filez[1])
blogs <- readLines(con1, skipNul = TRUE, encoding="UTF-8")
close(con1)
rm(con1)

## News
con2 <- file(filez[2])
news <- readLines(filez[2], skipNul = TRUE, encoding="UTF-8")
close(con2)
rm(con2)

## Twitter
con3 <- file(filez[3])
twitter <- readLines(filez[3], skipNul = TRUE, encoding="UTF-8")
close(con3)
rm(con3)

## Drop non UTF-8 characters
twitter <- iconv(twitter, from = "latin1", to = "UTF-8", sub="")
twitter <- stri_replace_all_regex(twitter, "\u2019|`","'")
twitter <- stri_replace_all_regex(twitter, "\u201c|\u201d|u201f|``",'"')

## Save the data to an .RData files
save(blogs, file="data/blogs.RData")
save(news, file="data/news.RData")
save(twitter, file="data/twitter.RData")

rm(blogs)
rm(news)
rm(twitter)
