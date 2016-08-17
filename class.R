#flip a fair coin:

rbinom(n = 100, 
        size = 1, 
        prob = 0.5) %>% sum()

#100 trials:
replicate(n = 100, 
          rbinom(n = 100, 
                 size = 1, 
                 prob = 0.5) %>% sum()) %>% hist(col = "blue", breaks = 10)


#1000 trials:
replicate(n = 1000, 
          rbinom(n = 100, 
                 size = 1, 
                 prob = 0.5) %>% sum()) %>% hist(col = "blue", breaks = 20, xlim = c(30, 70))

#10000 trials:
replicate(n = 10000, 
          rbinom(n = 100, 
                 size = 1, 
                 prob = 0.5) %>% sum()) %>% hist(col = "blue", breaks = 20, xlim = c(30, 70))

replicate(n = 10000, 
          rbinom(n = 100, 
                 size = 1, 
                 prob = 0.5) %>% sum()) %>% sd()

#probability to roll 60 or larger:
abline(v = 60)


#what if the coin is not fair?


rbinom(n = 100, 
       size = 1, 
       prob = 0.95) %>% sum()

#change n here: 10, 20, 100:
replicate(n = 1000, 
          rbinom(n = 10, 
                 size = 1, 
                 prob = 0.9) %>% sum()) %>% hist(col = "blue", breaks = 10)


#It turns out it doesn't matter what the original distribution is, the sample mean distribution will 
#end up being normally distributed:

you roll a 4 sided dice with different probabilities:

sample(x = 1:4, size = 100, prob = c(0.6, 0.1, 0.1, 0.2), replace = TRUE) %>% hist()


replicate(100, 
          sample(x = 1:4, size = 10, prob = c(0.6, 0.1, 0.1, 0.2), replace = TRUE) %>% mean()) %>% hist()


sample(x = c(1, 2, 9), size = 25, replace = TRUE) %>% hist()

replicate(100000, 
sample(x = c(1, 2, 9), size = 10, replace = TRUE) %>% sum()) %>% hist(breaks = 50, col = "blue")


replicate(100000, 
          sample(x = c(1, 2, 9), size = 50, replace = TRUE) %>% sum()) %>% hist(breaks = 100, col = "blue")




