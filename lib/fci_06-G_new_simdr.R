new_simdr <- function(n = 3000, ss = 100, probH = 0.05) {
  
  # matrix of independent Bernoulli vector with prob = 0.05
  # "The columns of H were independent indicator variables each
  #  with probability 0.05"
  H <- cbind(replicate(n = ss, rbinom(n = n, size = 1, prob = probH)))
  
  # let the treatment depend on a function of H
  # "We simulated T  as indicator variables with probabilities that varied as
     # a linear function  of H such that approximately 600 individuals had T=1", 
  # i.e. 20% of n=3000
  preH <- apply(H, MARGIN = 1, FUN = sum)  # preparation for sumH
  sumH <- preH * 20 / ss
  probT <- 0.13 * sumH + 0.05 * rnorm(n = n, mean = 1, sd = 0.1)
  # make sure P(T=1) is between 0 and 1, i.e. positivity assumption
  stopifnot(all(probT > 0), all(probT < 1))
  `T` <- rbinom(n = n, size = 1, prob = probT)
  
  
  # generate the outcome depend on T and H
  # "We simulated Y as a function T ans sumH such hat approximatey 35 
  # individuals had Y = 1"
  probY <- 0.01 * `T` + 0.01 * sumH
  # make sure P(Y=1) is between 0 and 1, i.e. positivity assumption
  stopifnot(all(probY >= 0), all(probY < 1))
  Y <- rbinom(n = n, size = 1, prob = probY)
  
  # put results in a list
  out <- list("preH" = preH,
              "sumH" = c("min" = min(sumH), "mean" = mean(sumH), 
                         "max" = max(sumH)),
              "probT" = c("min" = min(probT), "mean" = mean(probT), 
                          "max" = max(probT)),
              "T" = c("sum" = sum(`T`)),
              "probY" = c("min" = min(probY), "mean" = mean(probY), 
                          "max" = max(probY)),
              "Y" = c("sum" = sum(Y))
              )
  
  # Estimate the expected potential outcomes
  est <- simdr_est(Y, `T`, H)
  
  append(out, est)
}

simdr_est <- function(Y, `T`, H) {
  
  # fit the exposure model
  e <- fitted(lm(`T` ~ H))
  
  # refit the exposure model using an incorrect logistic model
  e2 <- predict(glm(`T` ~ H, family = "binomial"), type = "response")
  
  # compute the weights
  w0 <- (1 - `T`) / (1 - e)
  w1 <- `T` / e
  w02 <- (1 - `T`) / (1 - e2)
  w12 <- T / e2
  
  # fit an overspecified (saturated) outcome model
  mod.out <- lm(Y ~ `T` * H)
  
  # Estimate the expected potential outcomes using the various methods
  dat <- data.frame("Y" = Y, "T" = `T`)
  dat0 <- dat
  dat0$`T` <- 0
  dat1 <- dat
  dat1$`T` <- 1
  
  # the predicted data
  preds0 <- predict(mod.out, newdata = dat0)
  preds1 <- predict(mod.out, newdata = dat1)
  
  # calculate the estimates
  EY0out <- mean(preds0)
  EY1out <- mean(preds1)
  EY0exp <- weighted.mean(Y, w = w0)
  EY1exp <- weighted.mean(Y, w = w1)
  EY0exp2 <- weighted.mean(Y, w = w02)
  EY1exp2 <- weighted.mean(Y, w = w12)
  EY0dr <- mean(w0 * Y + preds0 * (`T` - e) / (1 - e))
  EY1dr <- mean(w1 * Y - preds1 * (`T` - e) / e)
  EYT0 <- mean(Y * (1 - `T`))
  EYT1 <- mean(Y * `T`)
  
  list("mod.out" = coef(summary(mod.out)),
       "EY0out" = EY0out,
       "EY1out" = EY1out,
       "EY0exp" = EY0exp,
       "EY1exp" = EY1exp,
       "EY0exp2" = EY0exp2,
       "EY1exp2" = EY1exp2,
       "EY0dr" = EY0dr,
       "EY1dr" = EY1dr,
       "EYT0" = EYT0,
       "EYT1" = EYT1)
}
