library(dplyr)
source("Optd.R")
source("PW.R")

container <- data.frame(a = NA, b = NA, c = NA, d = NA, 
                PA = NA, PB = NA, PC = NA, PD = NA)

n = 10000
I05 <- seq(0, 0.5, length = n) # [0, ... , 0.5]
I01 <- seq(0, 1, length = n)   # [0, ... , 1]

for(ai in I05){
  print(ai)
  
  for(bj in I01){
    if(bj == ai) next
    
    for(ck in I01){
      if(ck == ai | ck == bj) next
      
      d_opt <- Optd(a = ai, b = bj, c = ck)
      len <- length(d_opt)
      if(len == 1){
        PA <- PW(w = ai, bj, ck, d_opt)
        PB <- PW(w = bj, ai, ck, d_opt)
        PC <- PW(w = ck, ai, bj, d_opt)
        PD <- PW(w = d_opt, ai, bj, ck)
        
        result_ijk <- data.frame(a = ai, b = bj, c = ck, d = d_opt,
                                 PA = PA, PB = PB, PC = PC, PD = PD)
        container <- rbind.data.frame(container, result_ijk)
      } else if(len == 2 | len == 3 | len == 4){
        for(index in 1:len){
          PA <- PW(w = ai, bj, ck, d_opt[index])
          PB <- PW(w = bj, ai, ck, d_opt[index])
          PC <- PW(w = ck, ai, bj, d_opt[index])
          PD <- PW(w = d_opt[index], ai, bj, ck)
          
          tie_ijk <- data.frame(a = ai, b = bj, c = ck, d = d_opt[index],
                                   PA = PA, PB = PB, PC = PC, PD = PD)
          container <- rbind.data.frame(container, tie_ijk)
        }
      } else {
        stop("There should be 1, 2, 3, or 4 optimal choices for d; why isn't there?")
        }

    }
  }
}

container2 <- container %>%
  group_by(a, b) %>%
  filter(PC == max(PC)) %>%
  arrange(a, b, c, d)

container3 <- container2 %>%
  group_by(a, b) %>%
  mutate(PB = mean(PB))

container4 <- container3 %>%
  group_by(a) %>%
  filter(PB == max(PB))

container5 <- container4 %>%
  group_by(a) %>%
  mutate(PA = mean(PA))

containerA <- container5 %>%
  select(a, PA) %>%
  unique()

ggplot(data = containerA) + 
  geom_line(aes(x = a, y = PA))
