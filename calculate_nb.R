# Calculate Naive Bayes
#
#
#


calculateNB <- function(dtx) {
        ## Priors: Propabilities of classes. Classes here are the number of different next words in 2-grams = frequency of w2 in 2-grams
        ## P(c) = NC / N
        ## P(c) = PC: Probability of a class (= last words)
        ## NC: number of different ngrams which ends with next word
        ## N2,3: total number of 2-grams or 3-grams
        ## For 2-grams
        N2 <- dim(dtx[dtx$ngram==2])[1] ## here 626933 different 2-grams
        dtxx <- dtx[dtx$ngram==2, NC := .N , by = w4]
        dtxx <- dtx[dtx$ngram==2, PC := NC/N2 ]
        
        ## For 3-grams
        N3 <- dim(dtxx[dtx$ngram==3])[1] ## here 676147 different 3-grams
        dtx <- dtx[dtx$ngram==3, NC := .N , by = w4]
        dtx <- dtx[dtx$ngram==3, PC := NC/N3 ]
        
        ## Conditional probabilities of words
        ## PWC = P(w | c) = (count(w|c)+1) / (count(c) + |V|): Conditional probabilities of words (w) given a class (c)
        ## X1: frequency of 1st word in 2- and 3-grams per class (here = w4)
        ## X2: frequency of 2nd word in 3-grams
        ## CC = count(c): total number of words per class
        ## V = |V|: total vocabulary size (unique words) in document
        V <- dim(dtx[ngram == 1])[1]
        
        ## Get only 2- and 3-grams
        dtx <- dtx[ngram==2 | ngram==3]
        
        ## Calculate Conditional probabilities of words
        dtx <- dtx[ngram==3, X1 := sum(freq) + 1, by = c("w2", "w4")]
        dtx <- dtx[, X2 := sum(freq) + 1, by = c("w3", "w4")]
        dtx <- dtx[ngram==2, X1 := 0]
        
        dtx <- dtx[ngram==2, CC := freq, by = w4]
        dtx <- dtx[ngram==3, CC := sum(freq)*2, by = w4]
        
        dtx <- dtx[, PC_X1 := X1/ (CC + V)]
        dtx <- dtx[, PC_X2 := X2/ (CC + V)]
        
        dtx <- dtx[ngram==2, P := PC*PC_X2 ]
        dtx <- dtx[ngram==3, P := PC*PC_X1*PC_X2 ]
        
        dtx <- dtx[, P_Log := log(P) ]
        dt <- dtx ## rename to get uniqu table name
        
        save(dt, file = "data/naiveBayes.RData")
        
        rm(dtx)
        rm(dt)
}