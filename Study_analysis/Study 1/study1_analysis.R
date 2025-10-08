library(ggplot2) 
library(lme4) 
library(caret)
library(plyr) 
library(psych)
library(GPArotation)
library(reshape)
library(polycor)
library(nFactors)
library(doBy)
library(ggthemes)
library(ggpubr)
library(RcmdrMisc)
library(readr)
library(performance)
library(reactable)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
  #### EFA ####
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

dataset1 <- read.csv("dataset1.csv")

# Drop demographics for EFA
colnames(dataset1)
primary_data <- dataset1 %>%
  subset(select = -c(P1.Demo_Gender, P1.Demo_Age_Years))

# Create dataset without total scores
full_data <-  primary_data %>%
  subset(select = -c(198:212))

-----------------------------------------------------------------------------
  # Check factorability
-----------------------------------------------------------------------------

# Remove IDs to run FA
f_data <- full_data[1:196]

# Correlation of variables
cor_matrix <- cor(f_data)
findCorrelation(cor_matrix, cutoff = 0.8)
setwd("/Users/alicismith/Library/CloudStorage/OneDrive-SharedLibraries-TheUniversityofMelbourne/Caitlin Hitchcock - Melbourne Mechanisms of Mental Health/Projects/Million Minds/Study 1/Data/results")
write.csv(cor_matrix, "cor_matrix.csv")

# The Kaiser-Meyer-Olkin (KMO) - used to measure sampling adequacy
KMO(f_data)
round(KMO(f_data)$MSA, 2 ) 


# Bartlett’s Test of Sphericity
cortest.bartlett(f_data, n=327)



-----------------------------------------------------------------------------
  # Determine number of factors
-----------------------------------------------------------------------------
  
# Get eigenvalues
ev <- eigen(cor(f_data)) 
ev$values

# Scree plot
# Factor extraction stops at the elbow 
scree(f_data, pc=FALSE)


# Alternative plot - bar
fafitfree <- fa(f_data,nfactors = ncol(f_data), rotate = "none")
n_factors <- length(fafitfree$e.values)

scree_num <- data.frame(
  Factor_n = as.numeric(1:n_factors),
  Eigenvalue = fafitfree$e.values
)

ggplot(scree_num, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_col() +
  xlab("Factors") +
  ylab("Eigenvalue") +
  labs( title = "Scree Plot") +
  scale_x_continuous(breaks=c(0, 50, 100, 150, 200))


# Subset data to plot first 20 factors (zoom)
zoom <- scree_num[1:20,]

ggplot(zoom, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_col() +
  xlab("Factors") +
  ylab("Eigenvalue") +
  labs( title = "Scree Plot") +
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20))


# Parallel analysis
parallel <- fa.parallel(f_data) # 15 factors

# Cattell-Nelson-Gorsuch CNG
nCng(cor_matrix, cor = TRUE, model = "factors", details = TRUE) # 3 factors


-----------------------------------------------------------------------------
  # Determine rotation of factors
-----------------------------------------------------------------------------
  
  
# If enough correlations are >0.3, use oblique rotation. If not, change to orthogonal
# For oblique rotation use promax, 
  
Nfacts <- 4
fit <- factanal(f_data, Nfacts, rotation = "oblimin")
print(fit, digits=2, cutoff=0.25, sort=TRUE) # stick with oblique


-----------------------------------------------------------------------------
  # Run factor analysis
-----------------------------------------------------------------------------
  
Nfacs <- 4  
fit <- factanal(f_data, Nfacs, rotation="oblimin")
print(fit, digits=2, cutoff=0.25, sort=TRUE)
loads <- fit$loadings
print(loads)

# Save loadings
dim(fit$loadings)
FactorLoadings <- round(fit$loadings[ 1:196,], 4)
write.csv(FactorLoadings, "facloads.csv")

# loadings table
fa_loadings <- fa_loadings %>%
  subset(select = -c(X)) %>%
  abs()

mean_load <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Factor1", "Factor2", "Factor3", "Factor4")
colnames(mean_load) <- x

columns <- list("Factor1", "Factor2", "Factor3", "Factor4")

for (i in columns) {
  mean_load[1, i] <- mean(fa_loadings[1:4, i])
  mean_load[2, i] <- mean(fa_loadings[5:11, i])
  mean_load[3, i] <- mean(fa_loadings[12:14, i])
  mean_load[4, i] <- mean(fa_loadings[15:29, i])
  mean_load[5, i] <- mean(fa_loadings[30:36, i])
  mean_load[6, i] <- mean(fa_loadings[37:57, i])
  mean_load[7, i] <- mean(fa_loadings[58:62, i])
  mean_load[8, i] <- mean(fa_loadings[63:69, i])
  mean_load[9, i] <- mean(fa_loadings[70:89, i])
  mean_load[10, i] <- mean(fa_loadings[90:96, i])
  mean_load[11, i] <- mean(fa_loadings[97:125, i])
  mean_load[12, i] <- mean(fa_loadings[126:137, i])
  mean_load[13, i] <- mean(fa_loadings[138:161, i])
  mean_load[14, i] <- mean(fa_loadings[162:183, i])
  mean_load[15, i] <- mean(fa_loadings[184:196, i])
}


numbers <- mean_load

mean_load$Questionnaire <- row.names(mean_load)
mean_load$Questionnaire[1] <- "Stress"
mean_load$Questionnaire[2] <- "Wellbeing"
mean_load$Questionnaire[3] <- "Social Anxiety"
mean_load$Questionnaire[4] <- "Experiential Avoidance"
mean_load$Questionnaire[5] <- "Sleep"
mean_load$Questionnaire[6] <- "Depression & Anxiety"
mean_load$Questionnaire[7] <- "Mania"
mean_load$Questionnaire[8] <- "Psychosis"
mean_load$Questionnaire[9] <- "PTSD"
mean_load$Questionnaire[10] <- "Disordered Eating"
mean_load$Questionnaire[11] <- "Perfectionism"
mean_load$Questionnaire[12] <- "Intolerance of Uncertainty"
mean_load$Questionnaire[13] <- "Emotion Regulation"
mean_load$Questionnaire[14] <- "Rumination"
mean_load$Questionnaire[15] <- "Behavioural Inhibition"

mean_load$Questionnaire <- row.names(mean_load)
mean_load$Questionnaire[1] <- "Perceived Stress Scale"
mean_load$Questionnaire[2] <- "Short Warwick-Edinburgh Wellbeing Scale"
mean_load$Questionnaire[3] <- "Social Phobia Inventory"
mean_load$Questionnaire[4] <- "Brief Experiential Avoidance Questionnaire"
mean_load$Questionnaire[5] <- "Insomnia Severity Index"
mean_load$Questionnaire[6] <- "Depression, Anxiety, Stress Scales"
mean_load$Questionnaire[7] <- "Altman Self-Rating Mania Scale"
mean_load$Questionnaire[8] <- "Adolescent Psychotic Symptom Screening"
mean_load$Questionnaire[9] <- "PTSD Checklist for DSM-5"
mean_load$Questionnaire[10] <- "Eating Disorders Examination Questionnaire"
mean_load$Questionnaire[11] <- "Frost Multidimensional Perfectionism Scale"
mean_load$Questionnaire[12] <- "Intolerance of Uncertainty Scale"
mean_load$Questionnaire[13] <- "Regulation of Emotional Symptoms Survey"
mean_load$Questionnaire[14] <- "Repetitive Negative Thoughts Questionnaire"
mean_load$Questionnaire[15] <- "Behavioural Inhibition and Activation Systems Scale"


mean_load <- mean_load |>
  dplyr::rename("Factor 1" = "Factor1",
                "Factor 2" = "Factor2",
                "Factor 3" = "Factor3",
                "Factor 4" = "Factor4")

mean_load <- mean_load[, c(5, 1, 2, 3, 4)]

is.num <- sapply(mean_load, is.numeric)
mean_load[is.num] <- lapply(mean_load[is.num], round, 2)

# Define the BuYlRd palette as a color function
BuYlRd <- colorRamp(c("#eaf3ff", "#89a6bb", "#5E6D9A"))  # Light to dark blue

# Get numeric values for normalization
numbers <- mean_load |>
  dplyr::select(where(is.numeric)) |>
  unlist() |>
  as.numeric()

# Min and max for scaling
min_val <- min(numbers, na.rm = TRUE)
max_val <- max(numbers, na.rm = TRUE)

# Color mapping function: rescale and convert to hex
get_color <- function(value) {
  if (is.na(value)) return(NA)
  norm_val <- scales::rescale(value, to = c(0, 1), from = c(min_val, max_val))
  rgb(BuYlRd(norm_val), maxColorValue = 255)
}

# Reactable table
table <- reactable(
  mean_load,
  defaultColDef = colDef(
    style = function(value, index, name) {
      if (!is.numeric(value) || is.na(value)) return(NULL)
      color <- get_color(value)
      style <- list(background = color)
      if (value >= 0.25) style$fontWeight <- "bold"
      style
    },
    format = colFormat(digits = 2),
    minWidth = 50,
    align = "center"
  ),
  columns = list(
    Subscale = colDef(style = list(borderRight = "1px solid rgba(0, 0, 0, 0.1)"))
  ),
  borderless = TRUE,
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem"),
  defaultPageSize = 20
)

# Legend
legend <- htmltools::div(
  style = "margin-top: 10px; display: flex; align-items: center; gap: 10px;",
  div("Low", style = "font-size: 0.8rem;"),
  div(style = "width: 100px; height: 10px; background: linear-gradient(to right, #eaf3ff, #89a6bb, #454b87); border: 1px solid #ccc;"),
  div("High", style = "font-size: 0.8rem;")
)

# Display table + legend
htmltools::browsable(
  htmltools::tagList(table, legend)
)



-----------------------------------------------------------------------------
  # Cronbach's alpha for full items
-----------------------------------------------------------------------------
  
# Items with loading >0.3 for each factor  
  
f1 <- full_data[ , c(	"P2.RNTQ_10", "P2.RNTQ_11", "P2.RNTQ_13", "P2.RNTQ_9" ,"P2.RNTQ_6", "P2.RNTQ_20", "P2.RNTQ_2", "P2.RNTQ_4",
                       "P2.RNTQ_21", "P2.RNTQ_14", "P2.RNTQ_19", "P2.RNTQ_22", "P2.RNTQ_5", "P2.RNTQ_8", "P2.RNTQ_12", "BEAQ_10", "P2.RESS24_2_9",
                        "P2.RESS24_1_1", "P2.RESS24_2_2", "P2.RESS24_1_8", "P2.RNTQ_1", "P2.IUS_S_1_10", "P2.IUS_S_1_7", "BEAQ_11", "P2.RNTQ_15",
                        "P2.IUS_S_1_3", "P2.RNTQ_17", "P2.RNTQ_3", "P2.RNTQ_16", "P2.IUS_S_1_1", "P2.IUS_S_1_12", "P2.IUS_S_1_6", "P2.RNTQ_18", "P1.MiniSpin_1_2",
                        "P1.MiniSpin_1_3", "P2.RNTQ_7", "P1.DASS21_1_9", "P2.IUS_S_1_2", "P2.IUS_S_1_8", "BEAQ_14", "BEAQ_5", "P2.IUS_S_1_5", "P2.RESS24_1_4",
                        "BEAQ_12", "BEAQ_8", "P1.DASS21_1_5", "P1.DASS21_1_8", "BEAQ_9", "P2.IUS_S_1_9", "P1.MiniSpin_1_1", "P2.IUS_S_1_4", "P1.DASS21_2_1", "BEAQ_7",
                        "P2.PSS_2", "BEAQ_2","BEAQ_4", "BEAQ_6")]
                     
f2 <- full_data[ , c("P1.PCL5_P4_5","P1.PCL5_P4_3","P1.PCL5_P4_2","P1.PCL5_P4_1","P1.PCL5_P4_4","P1.PCL5_P4_6","P1.PCL5_P5_1",
                     "P1.PCL5_P4_7","P1.PCL5_P4_10","P1.PCL5_P5_6","P1.PCL5_P5_5","P1.PCL5_P5_8","P1.DASS21_2_5","APSS_5",
                     "P1.PCL5_P5_7","P1.PCL5_P5_4","P1.PCL5_P5_10","P1.PCL5_P5_2","P1.DASS21_2_11","P1.PCL5_P5_3","P1.DASS21_1_1",
                     "APSS_4","P1.PCL5_P4_8","P1.DASS21_1_4","P1.PCL5_P4_9","P1.DASS21_2_10","P1.DASS21_1_7","P1.DASS21_2_7",
                     "P1.ISI_3","P1.DASS21_1_3","P1.ISI_1","P1.DASS21_2_6","P1.PCL5_P5_9","P1.DASS21_2_3","P1.ISI_2","P1.DASS21_2_2","P2.PSS_10",
                     "P1.DASS21_2_9","APSS_7","P1.DASS21_1_10","P1.ISI_6","P1.DASS21_2_8","P1.ISI_5","P2.SWEWS_1_5","P1.DASS21_2_1","P2.PSS_2","APSS_1","APSS_3",
                     "P1.DASS21_2_4","P2.FMPS29_1_3","P2.SWEWS_1_3","P1.ISI_7","P1.DASS21_1_6","APSS_6","P1.DASS21_1_8")]


f3 <- full_data[ , c("P2.FMPS29_2_8", "P2.FMPS29_2_3","P2.FMPS29_2_5","P2.FMPS29_2_4","P2.FMPS29_2_10","P2.FMPS29_2_2","P2.FMPS29_2_9",
                     "P2.FMPS29_1_10","P2.FMPS29_1_4","P2.FMPS29_3_4","P2.FMPS29_3_8","P2.FMPS29_2_6","P2.FMPS29_1_9","P2.FMPS29_3_9",
                     "P2.FMPS29_1_7","P2.FMPS29_2_7","P2.FMPS29_2_1","P2.FMPS29_1_1","P2.FMPS29_1_8","P2.FMPS29_1_3","P2.FMPS29_3_1",
                     "P2.FMPS29_3_7","P2.FMPS29_3_2","P2.FMPS29_3_3","P2.FMPS29_3_5","P2.FMPS29_1_2","P2.FMPS29_3_6","P2.RNTQ_3")]


f4 <- full_data[ , c("P2.RESS24_2_8","P2.RESS24_1_11","P2.RESS24_1_5","P2.FMPS29_3_3","P2.RESS24_2_11","P2.RESS24_2_7",
                     "P2.RESS24_1_12","P2.FMPS29_1_5","P2.RESS24_2_3","P2.FMPS29_1_6","ASRM_2","P2.RESS24_1_2",
                     "P2.RESS24_2_12","P2.FMPS29_3_5","P2.RESS24_1_3","P2.FMPS29_1_2","P2.RESS24_2_4","P2.FMPS29_3_2",
                     "P2.RESS24_2_5","P2.RESS24_1_6","ASRM_1","BAS_8","BAS_7","BAS_3","BAS_10","BAS_5",
                     "P2.SWEWS_1_1","BAS_11","BAS_1","BAS_12","BAS_2","BAS_4","P2.SWEWS_1_4","BEAQ_4","BAS_13","BAS_6","P2.SWEWS_1_2",
                     "BEAQ_9")]




alpha(f1, check.keys=TRUE) # raw alpha = 0.97
alpha(f2, check.keys=TRUE) # raw alpha = 0.96
alpha(f3, check.keys=TRUE) # raw alpha = 0.94
alpha(f4, check.keys=TRUE) # raw alpha = 0.88



-----------------------------------------------------------------------------
  # Cronbach's alpha for reduced set of items
-----------------------------------------------------------------------------

# Items with loading >0.3 for each factor  

f1_red <- full_data[ , c(	"P2.RNTQ_10", "P2.RNTQ_13", "P2.RNTQ_9" , "P2.RNTQ_20", "P2.RNTQ_21","BEAQ_4")]

f2_red <- full_data[ , c("P1.PCL5_P4_5","P1.PCL5_P4_3","P1.PCL5_P4_2", "P1.PCL5_P4_7","P1.PCL5_P4_10","P1.PCL5_P5_6","P1.PCL5_P5_5","P1.PCL5_P5_10")]

f3_red <- full_data[ , c("P2.FMPS29_2_8", "P2.FMPS29_2_3","P2.FMPS29_2_4","P2.FMPS29_2_10","P2.FMPS29_2_2","P2.FMPS29_2_9",
                     "P2.FMPS29_1_4","P2.FMPS29_3_8","P2.FMPS29_2_1","P2.FMPS29_3_3", "EDE7_2")]


f4_red <- full_data[ , c("P2.RESS24_2_8","P2.RESS24_1_5","P2.RESS24_2_11","P2.RESS24_2_7","P2.RESS24_2_4",
                     "P2.RESS24_2_5","BAS_7","BAS_5","P2.SWEWS_1_1","BEAQ_4")]




alpha(f1_red, check.keys=TRUE) # raw alpha = 0.89
alpha(f2_red, check.keys=TRUE) # raw alpha = 0.87
alpha(f3_red, check.keys=TRUE) # raw alpha = 0.86
alpha(f4_red, check.keys=TRUE) # raw alpha = 0.67



-----------------------------------------------------------------------------
  # Save factor scores
-----------------------------------------------------------------------------
  
FactorScores_fit <- factor.scores(f_data, fit)
FactorScores <- round(FactorScores_fit$scores[ 1:327,], 4)
write.csv(FactorScores, "facscores.csv")

EFA_data <- cbind(full_data, FactorScores)
write.csv(EFA_data, "EFA_data.csv")

-----------------------------------------------------------------------------
  # Save data for item reduction
-----------------------------------------------------------------------------
# duplicate items with lowest loadings removed

full_data_reduced <- full_data %>%
    subset(select = -c(P2.FMPS29_1_10, P2.FMPS29_2_5, P2.FMPS29_2_6, P2.FMPS29_3_4, P2.FMPS29_3_9, P2.RESS24_1_11, P2.RESS24_2_3, P2.FMPS29_1_9, P2.RESS24_1_2))
write.csv(full_data_reduced, "full_data_reduced.csv")

-----------------------------------------------------------------------------
  # bind with outcome data
-----------------------------------------------------------------------------

outcome = read_csv("MRFF_k10_pgii_gad7_phq9.csv") 
EFA_data = read_csv("EFA_data.csv")

outcome_wide <- spread(outcome, "survey", "survey_score", fill = NA)
k10_week0 <- subset(outcome_wide, weeks == 0)

fscores_k10 <- merge(EFA_data,k10_week0,by="record_id") 
mean(fscores_k10$k10)
sd(fscores_k10$k10)

fscores_outcome <- merge(EFA_data,outcome_wide,by="record_id") # REP data droppped because no outcome data

pgis = read_csv("MRFF_pgis.csv") 
fscores_pgis <- merge(EFA_data,pgis,by="record_id")



#### Replication analysis - Check consistency of factors ####
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
  # Factor analysis - non clinical REP dataset
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
  
nonclin_dataset2 <- read_csv("nonclin_dataset2.csv") 

colnames(nonclin_dataset2)
nonclin_data <- nonclin_data[2:197]

-----------------------------------------------------------------------------
  # Check factorability
-----------------------------------------------------------------------------
 
# Correlation of variables
cor_matrix <- cor(nonclin_data)


# The Kaiser-Meyer-Olkin (KMO) - used to measure sampling adequacy
KMO(nonclin_data)
round(KMO(nonclin_data)$MSA, 2 ) # KMO = 0.83


-----------------------------------------------------------------------------
  # Determine number of factors
-----------------------------------------------------------------------------
  
# Scree plot
scree(nonclin_data, pc=FALSE) # 3 or 4 factor solution



-----------------------------------------------------------------------------
  # Run factor analysis
-----------------------------------------------------------------------------

  
Nfacs <- 4  
fit_nonclin <- factanal(nonclin_data, Nfacs, rotation="oblimin")

# Save loadings
FactorLoadings_nonclin <- round(fit_nonclin$loadings[ 1:196,], 4)
write.csv(FactorLoadings_nonclin, "facloads_nonclinical.csv")


# loadings table
fa_loadings <- read.csv("facloads_nonclinical.csv")

fa_loadings <- fa_loadings %>%
  subset(select = -c(X)) %>%
  abs()

mean_load <- data.frame(matrix(ncol = 4, nrow = 0))
x <- c("Factor1", "Factor2", "Factor3", "Factor4")
colnames(mean_load) <- x

columns <- list("Factor1", "Factor2", "Factor3", "Factor4")

for (i in columns) {
  mean_load[1, i] <- mean(fa_loadings[1:4, i])
  mean_load[2, i] <- mean(fa_loadings[5:11, i])
  mean_load[3, i] <- mean(fa_loadings[12:14, i])
  mean_load[4, i] <- mean(fa_loadings[15:29, i])
  mean_load[5, i] <- mean(fa_loadings[30:36, i])
  mean_load[6, i] <- mean(fa_loadings[37:57, i])
  mean_load[7, i] <- mean(fa_loadings[58:62, i])
  mean_load[8, i] <- mean(fa_loadings[63:69, i])
  mean_load[9, i] <- mean(fa_loadings[70:89, i])
  mean_load[10, i] <- mean(fa_loadings[90:96, i])
  mean_load[11, i] <- mean(fa_loadings[97:125, i])
  mean_load[12, i] <- mean(fa_loadings[126:137, i])
  mean_load[13, i] <- mean(fa_loadings[138:161, i])
  mean_load[14, i] <- mean(fa_loadings[162:183, i])
  mean_load[15, i] <- mean(fa_loadings[184:196, i])
}


numbers <- mean_load

mean_load$Subscale <- row.names(mean_load)
mean_load$Subscale[1] <- "Stress"
mean_load$Subscale[2] <- "Wellbeing"
mean_load$Subscale[3] <- "Social Anxiety"
mean_load$Subscale[4] <- "Experiential Avoidance"
mean_load$Subscale[5] <- "Sleep"
mean_load$Subscale[6] <- "Depression & Anxiety"
mean_load$Subscale[7] <- "Mania"
mean_load$Subscale[8] <- "Psychosis"
mean_load$Subscale[9] <- "PTSD"
mean_load$Subscale[10] <- "Disordered Eating"
mean_load$Subscale[11] <- "Perfectionism"
mean_load$Subscale[12] <- "Intolerance of Uncertainty"
mean_load$Subscale[13] <- "Emotion Regulation"
mean_load$Subscale[14] <- "Rumination"
mean_load$Subscale[15] <- "Behavioural Inhibition"

mean_load$Questionnaire <- row.names(mean_load)
mean_load$Questionnaire[1] <- "Perceived Stress Scale"
mean_load$Questionnaire[2] <- "Short Warwick-Edinburgh Wellbeing Scale"
mean_load$Questionnaire[3] <- "Social Phobia Inventory"
mean_load$Questionnaire[4] <- "Brief Experiential Avoidance Questionnaire"
mean_load$Questionnaire[5] <- "Insomnia Severity Index"
mean_load$Questionnaire[6] <- "Depression, Anxiety, Stress Scales"
mean_load$Questionnaire[7] <- "Altman Self-Rating Mania Scale"
mean_load$Questionnaire[8] <- "Adolescent Psychotic Symptom Screening"
mean_load$Questionnaire[9] <- "PTSD Checklist for DSM-5"
mean_load$Questionnaire[10] <- "Eating Disorders Examination Questionnaire"
mean_load$Questionnaire[11] <- "Frost Multidimensional Perfectionism Scale"
mean_load$Questionnaire[12] <- "Intolerance of Uncertainty Scale"
mean_load$Questionnaire[13] <- "Regulation of Emotional Symptoms Survey"
mean_load$Questionnaire[14] <- "Repetitive Negative Thoughts Questionnaire"
mean_load$Questionnaire[15] <- "Behavioural Inhibition and Activation Systems Scale"


mean_load <- mean_load |>
  dplyr::rename("Factor 1" = "Factor1",
                "Factor 2" = "Factor2",
                "Factor 3" = "Factor3",
                "Factor 4" = "Factor4")

mean_load <- mean_load[, c(5, 1, 2, 3, 4)]

is.num <- sapply(mean_load, is.numeric)
mean_load[is.num] <- lapply(mean_load[is.num], round, 2)

# Define the BuYlRd palette as a color function
BuYlRd <- colorRamp(c("#eaf3ff", "#89a6bb", "#5E6D9A"))  # Light to dark blue

# Get numeric values for normalization
numbers <- mean_load |>
  dplyr::select(where(is.numeric)) |>
  unlist() |>
  as.numeric()

# Min and max for scaling
min_val <- min(numbers, na.rm = TRUE)
max_val <- max(numbers, na.rm = TRUE)

# Color mapping function: rescale and convert to hex
get_color <- function(value) {
  if (is.na(value)) return(NA)
  norm_val <- scales::rescale(value, to = c(0, 1), from = c(min_val, max_val))
  rgb(BuYlRd(norm_val), maxColorValue = 255)
}

# Reactable table
table <- reactable(
  mean_load,
  defaultColDef = colDef(
    style = function(value, index, name) {
      if (!is.numeric(value) || is.na(value)) return(NULL)
      color <- get_color(value)
      style <- list(background = color)
      if (value >= 0.25) style$fontWeight <- "bold"
      style
    },
    format = colFormat(digits = 2),
    minWidth = 50,
    align = "center"
  ),
  columns = list(
    Subscale = colDef(style = list(borderRight = "1px solid rgba(0, 0, 0, 0.1)"))
  ),
  borderless = TRUE,
  style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem"),
  defaultPageSize = 20
)

# Legend
legend <- htmltools::div(
  style = "margin-top: 10px; display: flex; align-items: center; gap: 10px;",
  div("Low", style = "font-size: 0.8rem;"),
  div(style = "width: 100px; height: 10px; background: linear-gradient(to right, #eaf3ff, #89a6bb, #454b87); border: 1px solid #ccc;"),
  div("High", style = "font-size: 0.8rem;")
)

# Display table + legend
htmltools::browsable(
  htmltools::tagList(table, legend)
)
