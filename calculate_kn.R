## Function creates Kneser-Ney smoothing values

calculateKN <- function(dtx) {
        ## Y = N_c / (N_c + 2N_(c+1)), where N_c is the count of ngrams with count==c (for Kneser-Ney)
        ## Note: This is a simplified calculation of Y using only the two lowest kept freqs.
        c1 <- min(dtx$freq) ## The lowest available Freq
        c2 <- min(subset(dtx, freq > c1)$freq) ## The second lowest
        
        ## D: Calculate Discounting parameter different for freq==1, freq==2 and freq>=3
        Y <- nrow(dtx[freq == c1]) / (nrow(dtx[freq == c1]) + 2 * nrow(dtx[freq == c2]))
        
        ## D = Discounting parameter different for freq==1, freq==2 and freq>=3
        ##     Ref Goodman and Chen (1999)
        dtx[, D := 0]
        dtx[freq == 1]$D <- 1 - 2 * Y * (nrow(dtx[freq == 2]) / nrow(dtx[freq == 1]))
        dtx[freq == 2]$D <- 2 - 3 * Y * (nrow(dtx[freq == 3]) / nrow(dtx[freq == 2]))
        dtx[freq > 2]$D  <- 3 - 4 * Y * (nrow(dtx[freq == 4]) / nrow(dtx[freq == 3]))
        
        
        ## Nom = First nominator in P_KN formula ( max{c(w_i-1, w_i)-D, 0} )
        dtx <- dtx[, Nom := pmax(freq-D, 0)]
        
        
        ## Continuation count Denominator is the count of the preceding word(s). It differs based on the order of n-grams
        ## CCN = continuation count for lower order models (1-3-grams)
        ## CCN = actual count for the highest order model (4-gram)
        dtx <- dtx[dtx$ngram==1, CCN := sum(freq)]
        dtx <- dtx[dtx$ngram==2, CCN := sum(freq), by = w3]
        dtx <- dtx[dtx$ngram==3, CCN := sum(freq), by = list(w3, w2)]
        dtx <- dtx[dtx$ngram==4, CCN := freq]
        
        
        ## The λ*Pcont element is the probability reserved for unknown words.
        ## The lambda λ normalizing constant is used to distribute the probability mass we have discounted with the D’s.
        ## For every given ngram (e.g. YZ): lambda(Y?) = d / c(Y?) * cont(Y?)
        ## NWF = number of word types that follows w_i-1 in the training data
        dtx <- dtx[dtx$ngram==1, NWF := .N , by = w4]
        dtx <- dtx[dtx$ngram==2, NWF := .N , by = w3]  # Using data.tables '.N' variable for getting the number of rows per case
        dtx <- dtx[dtx$ngram==3, NWF := .N , by = list(w3, w2)]
        dtx <- dtx[dtx$ngram==4, NWF := .N , by = list(w3, w2, w1)]
        
        ## L  = Lambda, normalizing constant, the probability mass we've discounted
        dtx[, L := (D / freq) * NWF]
        
        
        ## NBP = Number of different ngrams which ends with next word
        ## This is different for every ngram.
        dtx <- dtx[dtx$ngram==2, NBM := .N , by = w4]
        dtx <- dtx[dtx$ngram==3, NBM := .N , by = w4]
        dtx <- dtx[dtx$ngram==4, NBM := .N , by = w4]
        
        
        ## PC = P_continuation
        ## NNG = total number of ngrams per ngram (2,3,4)
        dtx <- dtx[, NNG := .N, by = ngram]
        
        dtx[, PC := NBM / nrow(dtx)] # Count of this novel continuation div. by number of unique grams
        
        
        ## Prob_KN - Estimated Kneser-Ney probability
        #dtx[, P_KN := (Nom/CCN) + L * PC]
        dtx[, P := (Nom/CCN) + ((D/CCN) * NWF) * PC]
        
        
        ## And P as log
        dtx[, P_Log := log(P)] 
        
        dt <- dtx
        rm(dtx)
        #save(dtx, file= "data/dt.kneser_ney.RData")
        save(dt, file= "data/dt.kneser_ney_full.RData")
        
        ## Remove obsolete columns
        dt[, c("D", "Nom", "CCN", "NWF", "L", "NBM", "PC") :=NULL]
        save(dt, file= "data/dt.kneser_ney.RData")
        rm(dt)
}