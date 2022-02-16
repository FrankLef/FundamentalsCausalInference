simdr <- function(n = 3000, ss = 100, probH = 0.05) {
  # ss is the number of confounders
  # i.e. the number of columns of H
  H <- matrix(0, n, ss)
  # Let all componentsof H be independent Bernoulli variables with p=0.05
  probH <- rep(0.05, n)
  for (i in 1:ss) {
    H[, i] <- rbinom(n = 3000, size = 1, prob = probH)
  }
  # NOT in original script
  preH <- apply(H, 1, sum)  # preparation for sumH
  sumH <- preH * 20 / ss
  
  
  
  list("preH" = preH,
       "sumH" = sumH)
}
