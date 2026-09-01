library(haven)

data_2016 <- read_sav("~/Downloads/UKDA-8320-spss/spss/spss19/sdd_archive.sav")
data_2021 <- read_csv("~/Downloads/UKDA-9029-csv/csv/sdd_pupil_data_archive_2021.csv")

library(dplyr)

clean_data_2016 <- c("age1115", "sex", "ethnicgpr", "algivpar", "alwhypre", "alevr", "fas_bands", "region", "dlifgood1")
clean_data2016 <- sdd_archive[ , clean_data_2016]
head(clean_data2016)

clean_data2016 <- mutate(clean_data2016, datayr = 2016)

# recoding variables
# alevr
clean_data2016 <- clean_data2016[clean_data2016$alevr != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$alevr != -8, ]
clean_data2016$alevr <- clean_data2016$alevr - 1
# 1 = has drunk, 0 = has not drunk

# sex
clean_data2016 <- clean_data2016[clean_data2016$sex != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$sex != -8, ]
# 1 = boy, 2 = girl

# age
clean_data2016 <- clean_data2016[clean_data2016$age1115 != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$age1115 != -8, ]
# ages given by 11-15

# ethnic group
clean_data2016 <- clean_data2016[clean_data2016$ethnicgpr != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$ethnicgpr != -8, ]
# ethnicities given by 5 groupings: 
  # 1 = white, 2 = mixed, 3 = asian, 4 = black, 5 = other

# parental influence (algivpar = parents give alcohol)
clean_data2016 <- clean_data2016[clean_data2016$algivpar != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$algivpar != -8, ]
clean_data2016 <- clean_data2016[clean_data2016$algivpar != -1, ]
# 1 = yes, 2 = no

# peer influence (alwhypre = people my age drink because of pressure from friends)
clean_data2016 <- clean_data2016[clean_data2016$alwhypre != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$alwhypre != -8, ]
# 1 = agree, 2 = disagree

# region = no need to clean

# family affluence score (fasband)
clean_data2016 <- clean_data2016[clean_data2016$fas_bands != -9, ]

# mental health (dlifgood1)
clean_data2016 <- clean_data2016[clean_data2016$dlifgood1 != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$dlifgood1 != -8, ]

#renaming the columns to match 2021

clean_data2016 <- clean_data2016 %>%
  rename(dlifsat = dlifgood1)
clean_data2016 <- clean_data2016 %>%
  rename(ethnicgp5 = ethnicgpr)
clean_data2016 <- clean_data2016 %>%
  rename(fasbands = fas_bands)