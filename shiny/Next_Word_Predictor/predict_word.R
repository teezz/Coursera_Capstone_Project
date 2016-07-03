## This function uses the Kneser-Ney smoothing for predicting the next word

predictNextWord <- function(dtx, in_string) {
        if (require(quanteda) & require(data.table))
                #tokens <- removeFeatures(tokenize(in_string, removePunct = TRUE), stopwords("english"))
                tokens <- tokenize(toLower(in_string), removePunct = TRUE, simplify = TRUE) ## to lower and remove punctation
        if (length(tokens) > 1) {
                from <- length(tokens)-2
                nt <- tokens[from:length(tokens)] ## Get the last 3 words        
        } else {
                nt <- tokens
        }
        
        
        last <- length(nt)
        first <- 1
        if (first < last-1) {
                mid <- last-1        
        } else mid <- first
        
        
        ## Test in 1-grams
        if(length(nt) == 1) {
                result2 <- dtx[ngram == 2 & w3== nt[last] ]
                result2words <- result2[,w4]
                result2p <- result2[, P]
                result2ngram <- result2[, ngram]
                
                w_vector <- result2words
                p_vector <- result2p
                ng_vector <- result2ngram
        }
        
        ## Test in > 2-grams
        if(length(nt) >= 2) {
                result4 <- dtx[ngram == 4 & w1== nt[first] & w2 == nt[mid] & w3 == nt[last] ]
                result4words <- result4[,w4]
                result4p <- result4[, P]
                result4ngram <- result4[, ngram]
                
                result3 <- dtx[ngram == 3 & w2== nt[mid] & w3 == nt[last] ]
                result3words <- result3[,w4]
                result3p <- result3[, P]
                result3ngram <- result3[, ngram]
                
                result2 <- dtx[ngram == 2 & w3== nt[last] ]
                result2words <- result2[,w4]
                result2p <- result2[, P]
                result2ngram <- result2[, ngram]
                
                w_vector <- c(result4words, result3words, result2words)
                p_vector <- c(result4p, result3p, result2p)
                ng_vector <- c(result4ngram, result3ngram, result2ngram)
        }
        
        w <- as.data.table(w_vector)
        p <- as.data.table(p_vector)
        ng <- as.data.table(ng_vector)
        results <- cbind(w, p, ng)
        
        names(results)[1] <- "next_word"
        names(results)[2] <- "P"
        names(results)[3] <- "ngram"
        
        results <- results[order(-P)] ## order results by porbability
        results <- results[results[, .I[which.max(P)], by=next_word]$V1] ## get only the maximum value in the group 'next word'
        
        results <- results[1:3] ## Get the best 3
        return(results)
        
        # if (require(ggplot2))
        #         qplot(as.factor(results$next_word), results$PKN)
        
        rm(dtx); rm(results); rm(results2); rm(results3); rm(results4) 
}


predictWord <- function(unigram_levels, in_string) {
        if (require(quanteda))
                tokens <- tokenize(toLower(in_string), removePunct = FALSE, simplify = TRUE)
        
        token <- tail(tokens, n=1) ## Get last element of a vector or list
        
        reg_exp <- paste("^", token, "..*", sep="")
        
        results <- unigram_levels[grepl(reg_exp, unigram_levels$w4)]
        results <- results[1:3] ## Get the best 3
        
        return(results)
        
        rm(results)
}
