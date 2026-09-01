#pooling the data
pooled_data <- rbind(realfinal_2021, clean_data2016)
# swapping 0 and 1 in alevr - 
pooled_data$alevr <- 1 - pooled_data$alevr
# minusing 1 and swapping the values in aligpar and alwhypre
realfinal_2021$alevr <- realfinal_2021$alevr - 1
pooled_data$algivpar <- pooled_data$algivpar - 1

#convert variables into dummies
pooled_data$sex <- as.factor(pooled_data$sex)
pooled_data$region <- as.factor(pooled_data$region)
pooled_data$ethnicgp5 <- as.factor(pooled_data$ethnicgp5)
pooled_data$algivpar <- as.factor(pooled_data$algivpar)
pooled_data$alwhypre <- as.factor(pooled_data$alwhypre)
pooled_data$fasbands <- as.factor(pooled_data$fasbands)
pooled_data$dlifsat <- as.factor(pooled_data$dlifsat)
pooled_data$datayr <- as.factor(pooled_data$datayr)

#run logit model
logit_model <- glm(alevr ~ sex + age1115 + region + ethnicgp5 + algivpar + 
                     alwhypre + fasbands + dlifsat + datayr, 
                   data = pooled_data, 
                   family = binomial(link = "logit"))
summary(logit_model)

#fun funsies - I plotted out age against probability to drink for all ethnicity, with white as base
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

# Structural break plot -
library(dplyr)
library(ggplot2)
 
 # Function to compute average predicted probs for a dataset
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