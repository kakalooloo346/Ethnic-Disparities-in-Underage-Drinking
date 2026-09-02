library(haven)
library(tidyverse)
library(lmtest)
library(sandwich)
library(margins)
library(readr)
library(ggplot2)
library(ggeffects)

# Load dataset (update to match your local directory)
data_2016 <- read_sav("~Downloads/jkbhvgcfd/UKDA-8320-spss/spss/spss19/sdd_archive.sav")
data_2021 <- read_csv("~Downloads/jkbhvgcfd/UKDA-9029-csv/csv/sdd_pupil_data_archive_2021.csv")


clean_data_2016 <- c("age1115", "sex", "ethnicgpr", "algivpar", "alwhypre", "alevr", "fas_bands", "region", "dlifgood1")
clean_data2016 <- data_2016[ , clean_data_2016]
head(clean_data2016)

clean_data2016 <- mutate(clean_data2016, datayr = 2016)

# recoding variables 2016
# alevr
clean_data2016 <- clean_data2016[clean_data2016$alevr != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$alevr != -8, ]
clean_data2016$alevr <- clean_data2016$alevr - 1
clean_data2016$alevr <- 1 - clean_data2016$alevr
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
clean_data2016$algivpar <- clean_data2016$algivpar - 1
clean_data2016$algivpar <- 1 - clean_data2016$algivpar
# 1 = yes, 0 = no

# peer influence (alwhypre = people my age drink because of pressure from friends)
clean_data2016 <- clean_data2016[clean_data2016$alwhypre != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$alwhypre != -8, ]
clean_data2016$alwhypre <- clean_data2016$alwhypre - 1
clean_data2016$alwhypre <- 1 - clean_data2016$alwhypre
# 1 = agree, 0 = disagree

# region = no need to clean

# family affluence score (fasband)
clean_data2016 <- clean_data2016[clean_data2016$fas_bands != -9, ]

# mental health (dlifgood1)
clean_data2016 <- clean_data2016[clean_data2016$dlifgood1 != -9, ]
clean_data2016 <- clean_data2016[clean_data2016$dlifgood1 != -8, ]
clean_data2016 <- clean_data2016[clean_data2016$dlifgood1 != 0, ]

# cleaning 2021 data

clean_data_2021 <- c("age1115", "sex", "ethnicgp5", "algivpar", "alwhypre", "alevr", "fasbands", "region", "dlifsat", "datayr")
clean_data2021 <- data_2021[ , clean_data_2021]
head(clean_data2021)

# recoding variables 2021
# alevr
clean_data2021 <- clean_data2021[clean_data2021$alevr != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$alevr != -8, ]
clean_data2021$alevr <- clean_data2021$alevr - 1
clean_data2021$alevr <- 1 - clean_data2021$alevr
# 1 = has drunk, 0 = has not drunk

# sex
clean_data2021 <- clean_data2021[clean_data2021$sex != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$sex != -8, ]
# 1 = boy, 2 = girl

# age
clean_data2021 <- clean_data2021[clean_data2021$age1115 != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$age1115 != -8, ]
# ages given by 11-15

# ethnic group
clean_data2021 <- clean_data2021[clean_data2021$ethnicgp5 != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$ethnicgp5 != -8, ]
# ethnicities given by 5 groupings: 
# 1 = white, 2 = mixed, 3 = asian, 4 = black, 5 = other

# parental influence (algivpar = parents give alcohol)
clean_data2021 <- clean_data2021[clean_data2021$algivpar != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$algivpar != -8, ]
clean_data2021 <- clean_data2021[clean_data2021$algivpar != -1, ]
clean_data2021$algivpar <- clean_data2021$algivpar - 1
clean_data2021$algivpar <- 1 - clean_data2021$algivpar
# 1 = yes, 0 = no

# peer influence (alwhypre = people my age drink because of pressure from friends)
clean_data2021 <- clean_data2021[clean_data2021$alwhypre != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$alwhypre != -8, ]
clean_data2021$alwhypre <- clean_data2021$alwhypre - 1
clean_data2021$alwhypre <- 1 - clean_data2021$alwhypre
# 1 = agree, 0 = disagree

# region = no need to clean

# family affluence score (fasband)
clean_data2021 <- clean_data2021[clean_data2021$fasbands != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$fasbands != -8, ]

# mental health (dlifgood1)
clean_data2021 <- clean_data2021[clean_data2021$dlifsat != -9, ]
clean_data2021 <- clean_data2021[clean_data2021$dlifsat != -8, ]

# recoding 2016 columns to make sure columns are same
clean_data2016$ethnicgp5 <- clean_data2016$ethnicgpr
clean_data2016$dlifsat <- clean_data2016$dlifgood1
clean_data2016$fasbands <- clean_data2016$fas_bands

cols_to_keep <- c("age1115", "sex", "ethnicgp5", "algivpar", "alwhypre", "alevr", "fasbands", "region", "dlifsat", "datayr")
final_clean_data_2016 <- clean_data2016[ , cols_to_keep]
head(final_clean_data_2016)

# pool data
pooled_data <- rbind(final_clean_data_2016, clean_data2021)

save(pooled_data, file = "pooled_data.Rdata")
load("pooled_data.RData")

## running regression and tests on new pooled data

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

# robust standard errors test for the logit model
robust_main_model <- coeftest(logit_model, vcov = vcovHC(logit_model, type = "HC1"))
print(robust_main_model)

# wald test 
# ethnicity wald
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
structural_model <- glm(alevr ~ age1115 + sex + region + algivpar + alwhypre + 
                          fasbands + dlifsat + 
                          ethnicgp5 * datayr, # <--- The Structural Break interaction
                        data = pooled_data, 
                        family = binomial(link = "logit"))
# added interaction term

coeftest(structural_model, vcov = vcovHC(structural_model, type = "HC1"))
# checking heteroscedasticity (robust standard errors)

# run logit chow test/ structural break test
waldtest(logit_model, structural_model, vcov = vcovHC(structural_model, type = "HC1"), test = "Chisq")
# interaction terms do not have any significance, so there is no structural break in the effect of ethnicity on alcohol consumption

## graphs

# Figure 2: Alcohol Consumption vs Age by Ethnicity
new_data <- expand.grid(
  age1115 = seq(11, 15, by = 1),
  ethnicgp5 = factor(c("2", "3", "4", "5")),
  # Match the exact names and base levels from your logit model
  sex = factor("1", levels = levels(pooled_data$sex)),
  region = factor("1", levels = levels(pooled_data$region)),
  algivpar = factor("0", levels = levels(pooled_data$algivpar)),
  alwhypre = factor("0", levels = levels(pooled_data$alwhypre)),
  fasbands = factor("1", levels = levels(pooled_data$fasbands)),
  dlifsat = factor("1", levels = levels(pooled_data$dlifsat)),
  datayr = factor("2016", levels = levels(pooled_data$datayr))
)
results <- results %>%
  rename(age1115 = x, predicted_prob = predicted, ethnicgp5 = group)
ggplot(results, aes(x = age1115, y = predicted_prob * 100, color = ethnicgp5)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Alcohol Consumption vs Age by Ethnicity",
    subtitle = "Average predicted probabilities (other variables vary)",
    x = "Age",
    y = "Predicted Probability of Drinking (%)",
    color = "Ethnic Group"
  ) +
  scale_color_manual(
    values = c("2" = "#F8766D", "3" = "#7CAE00", "4" = "#00BFC4", "5" = "#C77CFF"),
    labels = c("2" = "Mixed", "3" = "Asian", "4" = "Black", "5" = "Other")
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  theme_classic() + # Starts with a clean slate and L-shaped axes
  theme(
    # Format Titles
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(color = "gray40", size = 12, margin = margin(b = 15)),
    # Format Axes Text and Titles
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    axis.text = element_text(face = "bold", size = 11, color = "black"),
    axis.line = element_line(color = "black"),
    # Custom Gridlines (Horizontal dashed only, just like the bar chart)
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # Format Legend
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11)
  )

# Figure 1: Raw Adolescent Drinking Rates by Ethnicity
eth_summary <- pooled_data %>%
  filter(!is.na(ethnicgp5) & !is.na(alevr)) %>%
  group_by(ethnicgp5) %>%
  summarise(Percent_Drank = mean(alevr) * 100) %>%
  mutate(Ethnicity_Name = recode_factor(as.factor(ethnicgp5),
                                        `1` = "White",
                                        `2` = "Mixed",
                                        `3` = "Asian",
                                        `4` = "Black",
                                        `5` = "Other"))
ggplot(eth_summary, aes(x = Ethnicity_Name, y = Percent_Drank, fill = Ethnicity_Name)) +
  geom_col(show.legend = FALSE, alpha = 0.85, width = 0.6) + 
  geom_text(aes(label = sprintf("%.1f%%", Percent_Drank)), 
            vjust = -0.8, size = 4.5, fontface = "bold", color = "#333333") +
  scale_y_continuous(limits = c(0, max(eth_summary$Percent_Drank) * 1.15), 
                     expand = expansion(mult = c(0, 0))) + 
  labs(title = "Raw Adolescent Drinking Rates by Ethnicity",
       subtitle = "Percentage of 11-15 year olds who report having drank alcohol (Pooled Sample)",
       x = "Ethnic Group",
       y = "Percentage of Drinkers (%)") +
  theme_classic() + 
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 15)),
    axis.text.x = element_text(size = 12, color = "black", face = "bold"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(margin = margin(t = 10), face = "bold"),
    axis.title.y = element_text(margin = margin(r = 10), face = "bold"),
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Figure 3: Societal Effects on Alcohol Consumption by Ethnicity
effects_data <- data.frame(
  Ethnicity = c("Asian", "Black", "Mixed", "Other", "White"),
  Parental_Effect = c(0.33, 0.35, 0.41, 0.51, 0.45),
  Peer_Effect = c(-0.02, 0.06, -0.02, -0.08, -0.05)
)
plot_data <- effects_data %>%
  pivot_longer(
    cols = c(Parental_Effect, Peer_Effect),
    names_to = "Effect_Type",
    values_to = "Change"
  ) %>%
  mutate(Effect_Type = recode(Effect_Type, 
                              "Parental_Effect" = "Parental Effect", 
                              "Peer_Effect" = "Peer Effect"))
  # plot
ggplot(plot_data, aes(x = Ethnicity, y = Change, fill = Effect_Type)) +
  # format
  geom_col(width = 0.6) + 
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  # text layering
  geom_text(data = filter(plot_data, Change <= -0.04 | Change >= 0),
            aes(label = Change), 
            position = position_stack(vjust = 0.5), 
            color = "black", size = 4.5) +
  geom_text(data = filter(plot_data, Change > -0.04 & Change < 0),
            aes(label = Change, y = Change - 0.015), # Nudges text down by 0.015
            color = "black", size = 4.5) +
  # colours
  scale_fill_manual(values = c("Parental Effect" = "#ED8A5B", "Peer Effect" = "#66A8CC")) +
  # labels
  labs(
    title = "Societal Effects on Alcohol Consumption by Ethnicity",
    x = "Ethnicity",
    y = "Change in Probability",
    fill = NULL 
  ) +
  # formatting
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0, margin = margin(b = 15)),
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    axis.text = element_text(face = "bold", size = 11, color = "black"),
    axis.line = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "gray80", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 11),
    legend.position = "right"
  )

## R to LaTeX
library(stargazer)

# regression results comparing baseline and structural model
stargazer(logit_model, structural_model, type = "latex", title = "Regression Results", align = TRUE, no.space = TRUE)

# Main AME
margin_results <- summary(logit_margins)
ame_table <- data.frame(
  Variable = margin_results$factor,
  AME = round(margin_results$AME, 3), # Rounds to 3 decimal places
  Std_Error = round(margin_results$SE, 3),
  P_Value = round(margin_results$p, 3)
)
ame_table$Sig <- ifelse(ame_table$P_Value < 0.01, "***", 
                        ifelse(ame_table$P_Value < 0.05, "**", 
                               ifelse(ame_table$P_Value < 0.1, "*", "")))
stargazer(ame_table, 
          type = "latex", 
          summary = FALSE,       # CRITICAL: Tells stargazer NOT to calculate means/mins/maxes
          rownames = FALSE,      # Keeps the left side clean
          title = "Overall Average Marginal Effects (Logit Model)", 
          align = TRUE, 
          no.space = TRUE)

# AME by ethnicity (split sample)
# Save margins summaries to objects
m_white <- margins(logit_white)
m_mixed <- margins(logit_mixed)
m_asian <- margins(logit_asian)
m_black <- margins(logit_black)
m_other <- margins(logit_other)

# Create helper functions to extract AMEs and SEs with their EXACT variable names
# This prevents the alphabetization bug from ruining your table
get_ame <- function(m) {
  s <- summary(m)
  res <- s$AME
  names(res) <- s$factor
  return(res)
}

get_se <- function(m) {
  s <- summary(m)
  res <- s$SE
  names(res) <- s$factor
  return(res)
}
# Generate the LaTeX Code
stargazer(logit_white, logit_mixed, logit_asian, logit_black, logit_other,
          type = "latex",
          title = "Average Marginal Effects of Adolescent Alcohol Initiation by Ethnicity",
          column.labels = c("White", "Mixed", "Asian", "Black", "Other"),
          dep.var.labels = "Probability of Ever Consuming Alcohol",
          
          # This is where the magic happens: overriding the log-odds with AMEs
          coef = list(get_ame(m_white), get_ame(m_mixed), get_ame(m_asian), get_ame(m_black), get_ame(m_other)),
          se = list(get_se(m_white), get_se(m_mixed), get_se(m_asian), get_se(m_black), get_se(m_other)),
          
          # Hide the boring controls AND the Constant (AMEs do not have intercepts)
          omit = c("region", "sex" , "datayr", "Constant"), 
          
          # Add clean lines indicating controls are present
          add.lines = list(c("Region Controls", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Demographic Controls", "Yes", "Yes", "Yes", "Yes", "Yes"),
                           c("Survey Year Control", "Yes", "Yes", "Yes", "Yes", "Yes")),
          font.size = "small",
          no.space = TRUE,
          digits = 3)

# Summary Statistics
summary_data <- pooled_data %>%
  select(alevr, ethnicgp5, algivpar, alwhypre, fasbands, dlifsat, age1115, sex, datayr)
summary_data[] <- lapply(summary_data, function(x) as.numeric(as.character(x)))
summary_data <- as.data.frame(summary_data)
stargazer(summary_data, 
          type = "latex", 
          summary = TRUE, 
          title = "Summary Statistics (Pooled Sample: 2016 and 2021)",
          font.size = "small",
          digits = 2, 
          covariate.labels = c("Ever Consumed Alcohol (alevr)", 
                               "Ethnicity (ethnicgp5)", 
                               "Parental Provision (algivpar)", 
                               "Peer Influence (alwhypre)", 
                               "Family Affluence Score (fasbands)", 
                               "Life Satisfaction (dlifsat)", 
                               "Age (age1115)", 
                               "Sex (sex)", 
                               "Data Year (datayr)"))

# Cross-tabulation
# 1. Alcohol by Ethnicity
table_eth <- addmargins(table(pooled_data$alevr, pooled_data$ethnicgp5))
df_eth <- as.data.frame.matrix(table_eth)
rownames(df_eth) <- c("Did Not Drink", "Drank Alcohol", "Total")
colnames(df_eth) <- c("White", "Mixed", "Asian", "Black", "Other", "Total")

stargazer(df_eth, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Alcohol Consumption by Ethnic Group")
# 2. Alcohol by Data Year
table_year <- addmargins(table(pooled_data$alevr, pooled_data$datayr))
df_year <- as.data.frame.matrix(table_year)
rownames(df_year) <- c("Did Not Drink", "Drank Alcohol", "Total")
colnames(df_year) <- c("2016", "2021", "Total")

stargazer(df_year, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Alcohol Consumption Pre- and Post-Pandemic")
# 3. Alcohol by Life Satisfaction
table_sat <- addmargins(table(pooled_data$alevr, pooled_data$dlifsat))
df_sat <- as.data.frame.matrix(table_sat)
rownames(df_sat) <- c("Did Not Drink", "Drank Alcohol", "Total")
colnames(df_sat) <- c("Low (1)", "Medium (2)", "High (3)", "Very High (4)", "Total")

stargazer(df_sat, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Alcohol Consumption by Life Satisfaction Level")
# 4. Parental Provision by Wealth
table_wealth <- addmargins(table(pooled_data$algivpar, pooled_data$fasbands))
df_wealth <- as.data.frame.matrix(table_wealth)
rownames(df_wealth) <- c("Parents Do Not Provide", "Parents Provide Alcohol", "Total")
colnames(df_wealth) <- c("Low Wealth", "Medium Wealth", "High Wealth", "Total")

stargazer(df_wealth, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Parental Provision of Alcohol by Household Wealth")
# 5. The Direct Behavioral Link: Alcohol Consumption by Parental Provision
table_par <- addmargins(table(pooled_data$alevr, pooled_data$algivpar))
df_par <- as.data.frame.matrix(table_par)
rownames(df_par) <- c("Did Not Drink", "Drank Alcohol", "Total")
colnames(df_par) <- c("Parents Do Not Provide (0)", "Parents Provide (1)", "Total")

stargazer(df_par, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Alcohol Consumption by Parental Provision")
# 6. Alcohol by Age
table_age <- addmargins(table(pooled_data$alevr, pooled_data$age1115))
df_age <- as.data.frame.matrix(table_age)
rownames(df_age) <- c("Did Not Drink", "Drank Alcohol", "Total")
colnames(df_age) <- c("Age 11", "Age 12", "Age 13", "Age 14", "Age 15", "Total")

stargazer(df_age, type = "latex", summary = FALSE, rownames = TRUE, 
          title = "Cross-Tabulation: Alcohol Consumption by Age")