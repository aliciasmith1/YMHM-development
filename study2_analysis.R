## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----Load data----------------------------------------------------------------
library(tidyverse)
library(psych)
library(irr)
library(DescTools)
library(ggplot2)
library(ggpubr)
library(MASS)
library(ordinal)


# Load dataset
df <- read.csv("study2_dataset.csv")

df$YAMHS_factor4_T1_rev <- -df$YAMHS_factor4_T1
df$YAMHS_factor4_T2_rev <- -df$YAMHS_factor4_T2


df$PGI.S_T1 <- ordered(df$PGI.S_T1, levels = 1:5)
df$PGI.S_T2 <- ordered(df$PGI.S_T2, levels = 1:5)

## ----Descriptive stats - sample-----------------------------------------------

df %>%
  dplyr::summarise(
    N = n(),
    Age_Mean = mean(age_T1, na.rm = TRUE),
    Age_SD = sd(age_T1, na.rm = TRUE),
    Female_count = sum(Gender_T1 == 2, na.rm = TRUE),
    Male_count = sum(Gender_T1 == 1, na.rm = TRUE),
    Else_count = sum(Gender_T1 == 3, na.rm = TRUE),
    Female_Percent = mean(Gender_T1 == 2, na.rm = TRUE) * 100,
    Male_Percent = mean(Gender_T1 == 1, na.rm = TRUE) * 100,
    Else_Percent = mean(Gender_T1 == 3, na.rm = TRUE) * 100
  )


## ----Descriptive stats - questionnaires---------------------------------------

# Descriptive stats for each scale at T1 and T2
questionnaire_totals <- c("K10_total_T1", "K10_total_T2", "PGI.S_T1", "PGI.S_T2")

df %>%
   dplyr::select(all_of(questionnaire_totals)) %>%
   summarise_all(list(mean = ~mean(., na.rm = TRUE),
                      sd = ~sd(., na.rm = TRUE),
                      min = ~min(., na.rm = TRUE),
                      max = ~max(., na.rm = TRUE)))



## ----Check distribution-------------------------------------------------------

hist(df$YAMHS_factor1_T1)
hist(df$YAMHS_factor2_T1)
hist(df$YAMHS_factor3_T1)
hist(df$YAMHS_factor4_T1)
hist(df$YAMHS_factor1_T2)
hist(df$YAMHS_factor2_T2)
hist(df$YAMHS_factor3_T2)
hist(df$YAMHS_factor4_T2)
hist(df$K10_total_T1)
hist(df$K10_total_T2)



## ----Test-retest reliability--------------------------------------------------
# ICC for factor scores
icc_factor1 <- icc(df[, c("YAMHS_factor1_T1", "YAMHS_factor1_T2")], model = "twoway", type = "consistency")
print(icc_factor1)

icc_factor2 <- icc(df[, c("YAMHS_factor2_T1", "YAMHS_factor2_T2")], model = "twoway", type = "consistency")
print(icc_factor2)

icc_factor3<- icc(df[, c("YAMHS_factor3_T1", "YAMHS_factor3_T2")], model = "twoway", type = "consistency")
print(icc_factor3)

icc_factor4 <- icc(df[, c("YAMHS_factor4_T1_rev", "YAMHS_factor4_T2_rev")], model = "twoway", type = "consistency")
print(icc_factor4)

# ICC for K10
icc_k10 <- icc(df[, c("K10_total_T1", "K10_total_T2")], model = "twoway", type = "consistency")
print(icc_k10)



## ----Bland-Altman plot--------------------------------------------------------

# Factor 1
df <- df %>%
  mutate(mean_factor = (YAMHS_factor1_T1 + YAMHS_factor1_T2) / 2,
         diff_factor = YAMHS_factor1_T1 - YAMHS_factor1_T2)

BA_1 <- ggplot(df, aes(x = mean_factor, y = diff_factor)) +
  geom_point() +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE), color = "blue") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) + 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) - 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  labs(title = "Uncontrollable Thinking Patterns Scores", x = "Mean of T1 and T2", y = "Difference (T1 - T2)")

# Factor 2
df <- df %>%
  mutate(mean_factor = (YAMHS_factor2_T1 + YAMHS_factor2_T2) / 2,
         diff_factor = YAMHS_factor2_T1 - YAMHS_factor2_T2)

BA_2 <- ggplot(df, aes(x = mean_factor, y = diff_factor)) +
  geom_point() +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE), color = "blue") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) + 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) - 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  labs(title = "Re-experiencing Difficult Events Scores", x = "Mean of T1 and T2", y = "Difference (T1 - T2)")

# Factor 3
df <- df %>%
  mutate(mean_factor = (YAMHS_factor3_T1 + YAMHS_factor3_T2) / 2,
         diff_factor = YAMHS_factor3_T1 - YAMHS_factor3_T2)

BA_3 <- ggplot(df, aes(x = mean_factor, y = diff_factor)) +
  geom_point() +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE), color = "blue") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) + 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) - 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  labs(title = "Rigid High Standards Scores", x = "Mean of T1 and T2", y = "Difference (T1 - T2)")

# Factor 4
df <- df %>%
  mutate(mean_factor = (YAMHS_factor4_T1 + YAMHS_factor4_T2) / 2,
         diff_factor = YAMHS_factor4_T1 - YAMHS_factor4_T2)

BA_4 <- ggplot(df, aes(x = mean_factor, y = diff_factor)) +
  geom_point() +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE), color = "blue") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) + 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = mean(df$diff_factor, na.rm = TRUE) - 1.96 * sd(df$diff_factor, na.rm = TRUE), linetype = "dashed", color = "red") +
  labs(title = "Emotional Agency Scores", x = "Mean of T1 and T2", y = "Difference (T1 - T2)")

ggarrange(BA_1, BA_2, BA_3, BA_4, ncol = 4)


## ----Internal consistency-----------------------------------------------------
# Chronbach's alpha T1
f1 <- df[ , c("YAMHS_19_T1", "YAMHS_20_T1","YAMHS_21_T1","YAMHS_22_T1","YAMHS_23_T1", "YAMHS_25_T1")]
alpha_f1_T1 <- psych::alpha(f1, check.keys=TRUE)
print(alpha_f1_T1$total$raw_alpha)

f2 <- df[ , c("YAMHS_1_T1", "YAMHS_2_T1","YAMHS_3_T1","YAMHS_4_T1","YAMHS_5_T1","YAMHS_6_T1","YAMHS_7_T1","YAMHS_8_T1")]
alpha_f2_T1 <- psych::alpha(f2, check.keys=TRUE)
print(alpha_f2_T1$total$raw_alpha)

f3 <- df[ , c("YAMHS_9_T1","YAMHS_10_T1","YAMHS_11_T1","YAMHS_12_T1","YAMHS_13_T1","YAMHS_14_T1","YAMHS_15_T1","YAMHS_16_T1","YAMHS_17_T1","YAMHS_18_T1", "YAMHS_34_T1")]
alpha_f3_T1 <- psych::alpha(f3, check.keys=TRUE)
print(alpha_f3_T1$total$raw_alpha)

f4 <- df[ , c("YAMHS_17_T1", "YAMHS_24_T1", "YAMHS_25_T1", "YAMHS_26_T1", "YAMHS_27_T1", "YAMHS_28_T1","YAMHS_29_T1","YAMHS_30_T1","YAMHS_31_T1","YAMHS_32_T1","YAMHS_33_T1")]
alpha_f4_T1 <- psych::alpha(f4, check.keys=TRUE)
print(alpha_f4_T1$total$raw_alpha)

# Chronbach's alpha T2
f1 <- df[ , c("YAMHS_19_T2", "YAMHS_20_T2","YAMHS_21_T2","YAMHS_22_T2","YAMHS_23_T2", "YAMHS_25_T2")]
alpha_f1_T2 <- psych::alpha(f1, check.keys=TRUE)
print(alpha_f1_T2$total$raw_alpha)

f2 <- df[ , c("YAMHS_1_T2", "YAMHS_2_T2","YAMHS_3_T2","YAMHS_4_T2","YAMHS_5_T2","YAMHS_6_T2","YAMHS_7_T2","YAMHS_8_T2")]
alpha_f2_T2 <- psych::alpha(f2, check.keys=TRUE)
print(alpha_f2_T2$total$raw_alpha)

f3 <- df[ , c("YAMHS_9_T2","YAMHS_10_T2","YAMHS_11_T2","YAMHS_12_T2","YAMHS_13_T2","YAMHS_14_T2","YAMHS_15_T2","YAMHS_16_T2","YAMHS_17_T2","YAMHS_18_T2", "YAMHS_34_T2")]
alpha_f3_T2 <- psych::alpha(f3, check.keys=TRUE)
print(alpha_f3_T2$total$raw_alpha)

f4 <- df[ , c("YAMHS_17_T2", "YAMHS_24_T2", "YAMHS_25_T2", "YAMHS_26_T2", "YAMHS_27_T2", "YAMHS_28_T2","YAMHS_29_T2","YAMHS_30_T2","YAMHS_31_T2","YAMHS_32_T2","YAMHS_33_T2")]
alpha_f4_T2 <- psych::alpha(f4, check.keys=TRUE)
print(alpha_f4_T2$total$raw_alpha)



## ----Construct validity - T1--------------------------------------------------
# Pearson's correlation between Factor 1 and K10 at T1
cor_test <- cor.test(df$YAMHS_factor1_T1, df$K10_total_T1, use = "complete.obs")
print(cor_test)

# Plot
f1_T1 <- ggscatter(df, x = "YAMHS_factor1_T1", y = "K10_total_T1",
          add = "reg.line", conf.int = TRUE,
          color = "#88a0dc",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#88a0dc", fill = "#88a0dc"),
          xlab = "Uncontrollable Thinking Patterns (T1)", ylab = "K10 (T1)")

# Pearson's correlation between Factor 2 and K10 at T1
cor_test <- cor.test(df$YAMHS_factor2_T1, df$K10_total_T1, use = "complete.obs")
print(cor_test)

# Plot
f2_T1 <- ggscatter(df, x = "YAMHS_factor2_T1", y = "K10_total_T1",
          add = "reg.line", conf.int = TRUE,
          color = "#ab3329",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#ab3329", fill = "#ab3329"),
          xlab = "Re-experiencing Difficult Events (T1)", ylab = "K10 (T1)")

# Pearson's correlation between Factor 3 and K10 at T1
cor_test <- cor.test(df$YAMHS_factor3_T1, df$K10_total_T1, use = "complete.obs")
print(cor_test)

# Plot
f3_T1 <- ggscatter(df, x = "YAMHS_factor3_T1", y = "K10_total_T1",
          add = "reg.line", conf.int = TRUE,
          color = "#7c4b73",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#7c4b73", fill = "#7c4b73"),
          xlab = "Rigid High Standards (T1)", ylab = "K10 (T1)")

# Pearson's correlation between Factor 4 and K10 at T1
cor_test <- cor.test(df$YAMHS_factor4_T1_rev, df$K10_total_T1, use = "complete.obs")
print(cor_test)

# Plot
f4_T1 <- ggscatter(df, x = "YAMHS_factor4_T1_rev", y = "K10_total_T1",
          add = "reg.line", conf.int = TRUE,
          color = "#e78429",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#e78429", fill = "#e78429"),
          xlab = "Emotional Agency (T1)", ylab = "K10 (T1)")

ggarrange(f1_T1, f2_T1, f3_T1, f4_T1, ncol = 4, nrow = 1)

## ----Construct validity - T2--------------------------------------------------
# Pearson's correlation between Factor 1 and K10 at T2
cor_test <- cor.test(df$YAMHS_factor1_T2, df$K10_total_T2, use = "complete.obs")
print(cor_test)

# Plot
f1_T2 <- ggscatter(df, x = "YAMHS_factor1_T2", y = "K10_total_T2",
          add = "reg.line", conf.int = TRUE,
          color = "#88a0dc",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#88a0dc", fill = "#88a0dc"),
          xlab = "Uncontrollable Thinking Patterns (T2)", ylab = "K10 (T2)")

# Pearson's correlation between Factor 2 and K10 at T2
cor_test <- cor.test(df$YAMHS_factor2_T2, df$K10_total_T2, use = "complete.obs")
print(cor_test)

# Plot
f2_T2 <- ggscatter(df, x = "YAMHS_factor2_T2", y = "K10_total_T2",
          add = "reg.line", conf.int = TRUE,
          color = "#ab3329",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#ab3329", fill = "#ab3329"),
          xlab = "Re-experiencing Difficult Events (T2)", ylab = "K10 (T2)")

# Pearson's correlation between Factor 3 and K10 at T2
cor_test <- cor.test(df$YAMHS_factor3_T2, df$K10_total_T2, use = "complete.obs")
print(cor_test)

# Plot
f3_T2 <- ggscatter(df, x = "YAMHS_factor3_T2", y = "K10_total_T2",
          add = "reg.line", conf.int = TRUE,
          color = "#7c4b73",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#7c4b73", fill = "#7c4b73"),
          xlab = "Rigid High Standards (T2)", ylab = "K10 (T2)")

# Pearson's correlation between Factor 4 and K10 at T2
cor_test <- cor.test(df$YAMHS_factor4_T2, df$K10_total_T2, use = "complete.obs")
print(cor_test)

# Plot
f4_T2 <- ggscatter(df, x = "YAMHS_factor4_T2_rev", y = "K10_total_T2",
          add = "reg.line", conf.int = TRUE,
          color = "#e78429",
          cor.coef = TRUE, cor.method = "pearson",
          add.params = list(color = "#e78429", fill = "#e78429"),
          xlab = "Emotional Agency (T2)", ylab = "K10 (T2)")

ggarrange(f1_T2, f2_T2, f3_T2, f4_T2, ncol = 4, nrow = 1)

ggarrange(f1_T1, f2_T1, f3_T1, f4_T1, f1_T2, f2_T2, f3_T2, f4_T2, ncol = 4, nrow = 2,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H"))

## ----Predictive validity------------------------------------------------------

# Factor 1 T1
model1 <- clm(PGI.S_T1 ~ YAMHS_factor1_T1, data = df, Hess = TRUE)
summary(model1)

# Factor 2 T1
model2 <- clm(PGI.S_T1 ~ YAMHS_factor2_T1, data = df, Hess = TRUE)
summary(model2)

# Factor 3 T1
model3 <- clm(PGI.S_T1 ~ YAMHS_factor3_T1, data = df, Hess = TRUE)
summary(model3)

# Factor 4 T1
model4 <- clm(PGI.S_T1 ~ YAMHS_factor4_T1, data = df, Hess = TRUE)
summary(model4)


# Factor 1 T2
model1 <- clm(PGI.S_T2 ~ YAMHS_factor1_T2, data = df, Hess = TRUE)
summary(model1)

# Factor 2 T2
model2 <- clm(PGI.S_T2 ~ YAMHS_factor2_T2, data = df, Hess = TRUE)
summary(model2)

# Factor 3 T2
model3 <- clm(PGI.S_T2 ~ YAMHS_factor3_T2, data = df, Hess = TRUE)
summary(model3)

# Factor 4 T2
model4 <- clm(PGI.S_T2 ~ YAMHS_factor4_T2, data = df, Hess = TRUE)
summary(model4)

