#cleaning data

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

#cleaning 2021 data - 
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

#pooling the data
pooled_data <- rbind(realfinal_2021, clean_data2016)
# swapping 0 and 1 in alevr - 
pooled_data$alevr <- 1 - pooled_data$alevr
# minusing 1 and swapping the values in aligpar and alwhypre
realfinal_2021$alevr <- realfinal_2021$alevr - 1
pooled_data$algivpar <- pooled_data$algivpar - 1

# running regression and tests on new pooled data

# convert variables into dummies
pooled_data$sex <- as.factor(pooled_data$sex)
pooled_data$region <- as.factor(pooled_data$region)
pooled_data$ethnicgp5 <- as.factor(pooled_data$ethnicgp5)
pooled_data$algivpar <- as.factor(pooled_data$algivpar)
pooled_data$alwhypre <- as.factor(pooled_data$alwhypre)
pooled_data$fasbands <- as.factor(pooled_data$fasbands)
pooled_data$dlifsat <- as.factor(pooled_data$dlifsat)
pooled_data$datayr <- as.factor(pooled_data$datayr)

# chi squared test (must run before logit)
chi_table <- table(pooled_data$ethnicgp5, pooled_data$alevr)
print(chi_table)
chisq.test(chi_table)

# run logit model
logit_model <- glm(alevr ~ sex + age1115 + region + ethnicgp5 + algivpar + 
                    alwhypre + fasbands + dlifsat + datayr, 
                  data = pooled_data, 
                  family = binomial(link = "logit"))
summary(logit_model)

# z-test (no need for extra code, we can just interpret coefficients)

# wald test 
# ethnicity wald
library(lmtest)
logit_restricted <- glm(alevr ~ sex + age1115 + region + algivpar + 
                          alwhypre + fasbands + dlifsat + datayr, 
                        data = pooled_data, 
                        family = binomial(link = "logit"))
waldtest(logit_restricted, logit_model, test = "Chisq")

# region wald
logit_restricted2 <- glm(alevr ~ sex + age1115 + ethnicgp5 + algivpar + 
                          alwhypre + fasbands + dlifsat + datayr, 
                        data = pooled_data, 
                        family = binomial(link = "logit"))
waldtest(logit_restricted2, logit_model, test = "Chisq")

# dlifsat wald
logit_restricted3 <- glm(alevr ~ sex + age1115 + region + algivpar + 
                          alwhypre + fasbands + ethnicgp5 + datayr, 
                        data = pooled_data, 
                        family = binomial(link = "logit"))
waldtest(logit_restricted3, logit_model, test = "Chisq")

# fasbands wald
logit_restricted4 <- glm(alevr ~ sex + age1115 + region + algivpar + 
                          alwhypre + ethnicgp5 + dlifsat + datayr, 
                        data = pooled_data, 
                        family = binomial(link = "logit"))
waldtest(logit_restricted4, logit_model, test = "Chisq")

# link test
yhat <- predict(logit_model, type = "link")
yhat_sq <- yhat^2
link_test_model <- glm(pooled_data$alevr ~ yhat + yhat_sq, 
                       family = binomial(link = "logit"))
summary(link_test_model)
# passed with flying colours (pr=0.992 for yhat^2)

# average marginal effects
library(margins)
logit_margins <- margins(logit_model)
summary(logit_margins)

# split-sample AME
data_white <- filter(pooled_data, ethnicgp5 == "1")
logit_white <- glm(alevr ~ sex + age1115 + region + algivpar + alwhypre + 
                     fasbands + dlifsat + datayr, 
                   data = data_white, family = binomial(link = "logit"))
summary(margins(logit_white))
data_mixed <- filter(pooled_data, ethnicgp5 == "2")
logit_mixed <- glm(alevr ~ sex + age1115 + region + algivpar + alwhypre + 
                     fasbands + dlifsat + datayr, 
                   data = data_mixed, family = binomial(link = "logit"))
summary(margins(logit_mixed))
data_asian <- filter(pooled_data, ethnicgp5 == "3")
logit_asian <- glm(alevr ~ sex + age1115 + region + algivpar + alwhypre + 
                     fasbands + dlifsat + datayr, 
                   data = data_asian, family = binomial(link = "logit"))
summary(margins(logit_asian))
data_black <- filter(pooled_data, ethnicgp5 == "4")
logit_black <- glm(alevr ~ sex + age1115 + region + algivpar + alwhypre + 
                     fasbands + dlifsat + datayr, 
                   data = data_black, family = binomial(link = "logit"))
summary(margins(logit_black))
data_other <- filter(pooled_data, ethnicgp5 == "5")
logit_other <- glm(alevr ~ sex + age1115 + region + algivpar + alwhypre + 
                     fasbands + dlifsat + datayr, 
                   data = data_other, family = binomial(link = "logit"))
summary(margins(logit_other))

# structural break
library(sandwich)
structural_model <- glm(alevr ~ age1115 + sex + region + algivpar + alwhypre + 
                          fasbands + dlifsat + 
                          ethnicgp5 * datayr, # <--- The Structural Break interaction
                        data = pooled_data, 
                        family = binomial(link = "logit"))
# added interaction term

coeftest(structural_model, vcov = vcovHC(structural_model, type = "HC1"))
# checking heteroscedasticity

# run logit chow test/ structural break test
waldtest(logit_model, structural_model, vcov = vcovHC(structural_model, type = "HC1"), test = "Chisq")
# interaction terms do not have any significnce, so there is no structural break in the effect of ethnicity on alcohol consumption

#Graphs
# Figure 1
#Figure 2
library(dplyr)
library(ggplot2)

age_eth_avg <- expand.grid(
  age1115 = 11:15,
  ethnicgp5 = levels(pooled_data$ethnicgp5)
)

results <- lapply(1:nrow(age_eth_avg), function(i) {
  
  temp <- pooled_data
  
  temp$age1115 <- age_eth_avg$age1115[i]
  temp$ethnicgp5 <- factor(age_eth_avg$ethnicgp5[i],
                           levels = levels(pooled_data$ethnicgp5))
  
  data.frame(
    age1115 = age_eth_avg$age1115[i],
    ethnicgp5 = age_eth_avg$ethnicgp5[i],
    predicted_prob = mean(predict(logit_model, newdata = temp, type = "response"))
  )
})

results <- bind_rows(results)

results <- results %>%
  filter(ethnicgp5 != "1")

ggplot(results, aes(x = age1115, y = predicted_prob, color = ethnicgp5)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Alcohol Consumption vs Age by Ethnicity",
    subtitle = "Average predicted probabilities (other variables vary)",
    x = "Age",
    y = "Predicted Probability of Drinking",
    color = "Ethnicity"
  ) +
  scale_color_manual(
    values = c("2" = "#F8766D", "3" = "#7CAE00", "4" = "#00BFC4", "5" = "#C77CFF"),
    labels = c("2" = "Mixed", "3" = "Asian", "4" = "Black", "5" = "Other")
  ) +
  ylim(0, 1) +
  theme_minimal()

#Figure 3

#Structural Break graph (not included in project)
library(dplyr)
library(ggplot2)

get_avg_preds <- function(data, label) {
  res <- lapply(11:15, function(a) {
    temp <- data
    temp$age1115 <- a
    
    data.frame(
      age1115 = a,
      predicted_prob = mean(predict(logit_model, newdata = temp, type = "response")),
      group = label
    )
  })
  bind_rows(res)
}

# 1. Pooled (all data)
pooled_preds <- get_avg_preds(pooled_data, "All")

# 2. 2016 only
preds_2016 <- get_avg_preds(
  subset(pooled_data, datayr == "2016"),
  "2016"
)

# 3. 2021 only
preds_2021 <- get_avg_preds(
  subset(pooled_data, datayr == "2021"),
  "2021"
)

# Combine
plot_data <- bind_rows(pooled_preds, preds_2016, preds_2021)
ggplot(plot_data, aes(x = age1115, y = predicted_prob, linetype = group)) +
  geom_line(size = 1) +
  labs(
    title = "Alcohol Consumption vs Age by Year",
    subtitle = "Average predicted probabilities",
    x = "Age",
    y = "Predicted Probability of Drinking",
    linetype = "Group"
  ) +
  scale_linetype_manual(
    values = c("All" = "solid", "2016" = "dotted", "2021" = "dotdash")
  ) +
  ylim(0, 1) +
  theme_minimal()