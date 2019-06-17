# Example 2
# Subject Time X1   Event
# 2       0    1.6  0
# 2       1    2.3  0
# 2       2    2.1  0
# 2      10    3.1  0
# 2      11    4.7  0
# 2      12    4.1  1
#
# Anderson-Gill format:
#   Subject  Time1  Time2  X1  Event
# 2.1      0      1      1.6     0
# 2.2      1      2      2.3     0
# 2.3      2      10     2.1     0
# 2.4      10     11     3.1     0
# 2.5      11     12     4.7     1

test_dat <- data.frame(subject = c(1,1,1,1,2,2,2,2, 2, 2),
                   time    = c(5,6,8,9,0,1,2,10,11,12),
                   X1 = c(3,4,5,6, 1.6, 2.3, 2.1, 3.1, 4.7, 4.1),
                   event = c(0,0,0,0, 0,0,0,0,0,1))


ag.test <- function(dataset, id){
  id <- enquo(id)
  dataset %>% arrange(!! id)
}

test_dat %>% arrange(subject)

ag.test(test_dat, time)

#target_quo = parse_quosure(target_column)
# df <- read.csv('file.csv', stringsAsFactors=FALSE)
# df <- df[, c(1,4,10)]
# names(df) <-  c('place','state','mean_age')
# df1 <- df %>% group_by(state) %>% arrange(!!target_quo)
