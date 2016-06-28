

library(knitr)

## Generic metrics function
getMetrics <- function(file_path, file_lines) {
  fiSize <- round(file.info(file_path)$size/1024^2, 2) ## in MB
  x <- stri_stats_general(file_lines) 
  nLines <- x[[1]]
  nChars <- x[[3]]
  charsNWhite <- x[[4]]
  words <- stri_count_words(file_lines) ## count the number of text boundaries in a string.
  nWords <- prettyNum(sum(words), big.mark = ",") ## sum all words up  ##prettyNum(sum(words), big.mark = ",")
  vars <- c(fiSize, nLines, nChars, charsNWhite, nWords)
  vars <- unlist(lapply(vars, function(i) {prettyNum(i, big.mark = ",")}))
  return(vars)
}

blog_metrics <- getMetrics(filez[1], blogs)
news_metrics <- getMetrics(filez[2], news)
twitter_metrics <- getMetrics(filez[3], twitter)

## Build the metrics table
metrics <- as.data.frame(rbind(blog_metrics, news_metrics, twitter_metrics))

