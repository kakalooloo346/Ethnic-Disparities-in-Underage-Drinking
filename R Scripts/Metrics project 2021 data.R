
#creating a new dataset with only the vars we care abt - 
realfinal_2021 <- sdd_pupil_data_archive_2021[, c("datayr", "sex", "age1115", "region", "ethnicgp5", "algivpar", "alwhypre", "fasbands", "dlifsat", "alevr")]
#removed -9 and -8 from alevr - 
realfinal_2021 <- realfinal_2021[realfinal_2021$alevr != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$alevr != -8, ]

#changed 2 to 1 and 1 to 0 in alevr - 
realfinal_2021$alevr <- realfinal_2021$alevr - 1

#changing gender - removing the -9 
realfinal_2021 <- realfinal_2021[realfinal_2021$sex != -9, ]

#removing -9 and -8 in age1115
realfinal_2021 <- realfinal_2021[realfinal_2021$age1115 != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$age1115 != -8, ]

#changing ethinicgp5 - removing -9 and -8
realfinal_2021 <- realfinal_2021[realfinal_2021$ethnicgp5 != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$ethnicgp5 != -8, ]

#changing algivpar - removing -9, -8 and -1
realfinal_2021 <- realfinal_2021[realfinal_2021$algivpar != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$algivpar != -8, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$algivpar != -1, ]

#changing alwhypre to remove -9 and -8
realfinal_2021 <- realfinal_2021[realfinal_2021$alwhypre != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$alwhypre != -8, ]

#changing fasbands to remove -9 and -8
realfinal_2021 <- realfinal_2021[realfinal_2021$fasbands != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$fasbands != -8, ]

#changing dlifsat to remove -9 and -8
realfinal_2021 <- realfinal_2021[realfinal_2021$dlifsat != -9, ]
realfinal_2021 <- realfinal_2021[realfinal_2021$dlifsat != -8, ]