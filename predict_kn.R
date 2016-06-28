## This function uses the Kneser-Ney smoothing for predicting the next word

predictKN <- function(dtx, in_string) {
        if (require(quanteda))
                #tokens <- removeFeatures(tokenize(in_string, removePunct = TRUE), stopwords("english"))
                #tokens <- tokens[[1]]
                tokens <- tokenize(toLower(in_string), removePunct = TRUE, simplify = TRUE) ## to lower and remove punctation
                from <- length(tokens)-2
                n <- tokens[from:length(tokens)] ## Get the last 3 words
        
        ## Test in n-grams
        if(length(n) >= 3) {
                result4 <- dtx[ngram == 4 & w1== n[1] & w2 == n[2] & w3 == n[3] ]
                result4words <- result4[,w4]
                result4p <- result4[, P_KN]
                result4ngram <- result4[, ngram]
                
                result3 <- dtx[ngram == 3 & w2== n[2] & w3 == n[3] ]
                result3words <- result3[,w4]
                result3p <- result3[, P_KN]
                result3ngram <- result3[, ngram]
                
                result2 <- dtx[ngram == 2 & w3== n[3] ]
                result2words <- result2[,w4]
                result2p <- result2[, P_KN]
                result2ngram <- result2[, ngram]
        }
        
        w <- as.data.table(c(result4words, result3words, result2words))
        p <- as.data.table(c(result4p, result3p, result2p))
        ng <- as.data.table(c(result4ngram, result3ngram, result2ngram))
        results <- cbind(w, p, ng)
        names(results)[1] <- "next_word"
        names(results)[2] <- "PKN"
        results <- results[order(-PKN)]#[1:20] ## order results by porbability
        results <- results[results[, .I[which.max(PKN)], by=next_word]$V1] ## get only the maximum value in the group 'next word'
        results <- results#[1:17]
        
        return(results)
        
        if (require(ggplot2))
                qplot(as.factor(results$next_word), results$PKN)
}