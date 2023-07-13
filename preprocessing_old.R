########################################################################################################################
# LOADING LIBRARIES
########################################################################################################################
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(pwr)
library(qgraph)
library(psych)
library(car)
library(lmtest)
library(nlme)
library(BayesFactor)

########################################################################################################################
# IMPORT & RE-STRUCTURE DATA
########################################################################################################################
data_full <- read.csv("PsySys_June+13,+2023_06.59/PsySys_June 13, 2023_06.59.csv")

# Delete unnecessary columns)
data <- select(data_full, -c("Status", "IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
                                 "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel",
                                 "UserLanguage", "Consent", "Exercise.1", "Exercise.2_0_GROUP", "Exercise.2_1_GROUP",
                                 "Exercise.2_0_38_RANK", "Exercise.2_0_39_RANK", "Exercise.2_0_40_RANK",
                                 "Exercise.2_0_41_RANK", "Exercise.2_0_42_RANK", "Exercise.2_0_43_RANK",
                                 "Exercise.2_0_44_RANK", "Exercise.2_0_45_RANK", "Exercise.2_0_46_RANK",
                                 "Exercise.2_0_47_RANK", "Exercise.2_0_48_RANK", "Exercise.2_0_49_RANK",
                                 "Exercise.2_1_38_RANK", "Exercise.2_1_39_RANK", "Exercise.2_1_40_RANK",
                                 "Exercise.2_1_41_RANK", "Exercise.2_1_42_RANK", "Exercise.2_1_43_RANK",
                                 "Exercise.2_1_44_RANK", "Exercise.2_1_45_RANK", "Exercise.2_1_46_RANK",
                                 "Exercise.2_1_47_RANK", "Exercise.2_1_48_RANK", "Exercise.2_1_49_RANK",
                                 "Exercise.3...part.2.", "Exercise.4_0_GROUP", "Exercise.4_0_2_RANK",
                                 "Exercise.4_0_3_RANK", "Exercise.4_0_4_RANK", "Exercise.4_0_5_RANK",
                                 "Exercise.4_0_6_RANK", "Exercise.4_0_7_RANK", "Exercise.4_0_8_RANK",
                                 "Exercise.4_0_9_RANK", "Exercise.4_0_10_RANK", "Exercise.4_0_11_RANK"))

# Re-code PHQ-9 to calculate score (not at all=0, several days=1, more than half the days=2, and nearly every day=3)
data <- data.frame(lapply(data, function(x) ifelse(x == "Not at all", 0, x)))
data <- data.frame(lapply(data, function(x) ifelse(x == "Several days", 1, x)))
data <- data.frame(lapply(data, function(x) ifelse(x == "More than half the days", 2, x)))
data <- data.frame(lapply(data, function(x) ifelse(x == "Nearly every day", 3, x)))

# Convert columns 13 to 21 to numeric
data[, 13:21] <- lapply(data[, 13:21], as.numeric)

# Calculate the PHQ-9 score (34 subjects had a PHQ-9 score > 9 - moderate depression) - we will not exclude
data$PHQ.9_score <- rowSums(data[, 13:21], na.rm = TRUE)
data$PHQ.9_category <- ifelse(data$PHQ.9_score >= 0 & data$PHQ.9_score <= 4, "minimal",
                              ifelse(data$PHQ.9_score >= 5 & data$PHQ.9_score <= 9, "mild",
                                     ifelse(data$PHQ.9_score >= 10 & data$PHQ.9_score <= 14, "moderate",
                                            ifelse(data$PHQ.9_score >= 15 & data$PHQ.9_score <= 19, "moderately severe",
                                                   ifelse(data$PHQ.9_score >= 20 & data$PHQ.9_score <= 27, "severe", NA)))))

########################################################################################################################
# DESCRIPTIVES: DROPOUT
########################################################################################################################
# Split data into completed and dropout
data_completed <- subset(data, Finished == "True")
data_dropout <- subset(data, Finished == "False")

# Append one subject that dropped out only before Acceptability questions to data_completed
data_completed <- rbind(data_completed, data_dropout[87,])
data_dropout <- data_dropout[-87,]

# Analyze dropout data
dropout_01 <- subset(data_dropout, as.numeric(Progress) <= 4) # at beginning (consent)
dropout_02 <- subset(data_dropout, as.numeric(Progress) > 4 & as.numeric(Progress) <= 27) # after descriptives
dropout_03 <- subset(data_dropout, as.numeric(Progress) > 27 & as.numeric(Progress) <= 35) # after PHQ-9
dropout_04 <- subset(data_dropout, as.numeric(Progress) > 35 & as.numeric(Progress) <= 49) # after IPQ-R
dropout_05 <- subset(data_dropout, as.numeric(Progress) > 49 & as.numeric(Progress) <= 54) # after Exercise 1
dropout_06 <- subset(data_dropout, as.numeric(Progress) > 54 & as.numeric(Progress) <= 62) # after Exercise 2
dropout_07 <- subset(data_dropout, as.numeric(Progress) > 62 & as.numeric(Progress) <= 84) # after Exercise 3
dropout_08 <- subset(data_dropout, as.numeric(Progress) > 84) # after Exercise 4

# Visualize dropout rates
dropouts <- c(nrow(dropout_01), nrow(dropout_02), nrow(dropout_03), nrow(dropout_04), sum(nrow(dropout_05),
                                                                                          nrow(dropout_06),
                                                                                          nrow(dropout_07),
                                                                                          nrow(dropout_08)))
# Make string vector with dropout stages for visualization
dropout_stages <- c("Beginning", "Descriptives", "PHQ-9", "IPQ-R", "PsySys")

# Calculate the proportions
total <- sum(dropouts)
proportions <- dropouts / total

# Create a pie chart with labels showing proportions
pie(dropouts, labels = paste0(dropout_stages, "\n", scales::percent(proportions)),
    border = "white", col = c("#66C2A5", "#FC8D62", "#A6D854", "#E78AC3", "#8DA0CB"))


########################################################################################################################
# DESCRIPTIVES: DURATION
########################################################################################################################
# Delete PHQ-9 columns (only keep score column)
data_completed <- data_completed[, -c(13:21)]

# Convert duration into minutes
data_completed$Duration..in.seconds. <- round(as.numeric(data_completed$Duration..in.seconds.) / 60, 2)
colnames(data_completed)[colnames(data_completed) == "Duration..in.seconds."] <- "Duration"

# Define different data sets based on duration
data_completed <- data_completed # N=74
data_cap_bottom_15 <- subset(data_completed, data_completed$Duration > 15) # N=61
data_cap_bottom_30 <- subset(data_completed, data_completed$Duration > 30) # N=50
data_cap_both <- subset(data_cap_bottom_15, data_cap_bottom_15$Duration < 90) # N=52
data_cap_both_conservative <- subset(data_cap_bottom_30, data_cap_bottom_30$Duration < 90) #41

# Calculate mean duration
mean_duration <- mean(data_completed$Duration) # 169.5235
mean_duration_cap_bottom_15 <- mean(data_cap_bottom_15$Duration) # 203.6439
mean_duration_cap_bottom_30 <- mean(data_cap_bottom_30$Duration) # 243.3028
mean_duration_cap_both <- mean(data_cap_both$Duration) # 39.86942
mean_duration_cap_both_conservative <- mean(data_cap_both_conservative$Duration) # 44.29439

# SPECIFY DATA_COMPLETED (for rest of analysis)
# data_completed <- data_completed # t-test replicates, wilcoxon replicates
# data_completed <- data_cap_bottom_15 # t-test replicates, wilcoxon replicates
# data_completed <- data_cap_bottom_30 # t-test significant
# data_completed <- data_cap_both # t-test replicates, wilcoxon replicates
data_completed <- data_cap_both_conservative # main analysis

# PLOTTING
ggplot(data_completed, aes(x = "", y = Duration)) +
  geom_boxplot(fill = "skyblue2") +
  labs(x = "", y = "Duration (minutes)") +
  theme_minimal()

########################################################################################################################
# DESCRIPTIVES: RECRUITMENT
########################################################################################################################
data_completed$Recruitment <- as.factor(data_completed$Recruitment)
recruitment_counts <- table(data_completed$Recruitment)
recruitment_proportions <- prop.table(recruitment_counts)
recruitment_proportions_numeric <- as.numeric(recruitment_proportions)

# PLOTTING
pie(recruitment_proportions_numeric,
    labels = paste0(c("Facebook", "Instagram", "Other", "Suggestion"), "\n", percent(recruitment_proportions_numeric)),
    border = "white",
    col = c("#A5C8E4", "#F8CBD6", "#C0ECCC", "#F9F0C1"))

########################################################################################################################
# DESCRIPTIVES: ENGLISH LEVEL
########################################################################################################################
# Calculate the count of each English level
english_levels_count <- table(data_completed$English.Proficienty)

# PLOTTING
ggplot(data_completed, aes(x = English.Proficienty)) +
  geom_bar(fill = "skyblue2", color = NA) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "", y = "Count") +
  theme_minimal()

########################################################################################################################
# DESCRIPTIVES: GENDER
########################################################################################################################
gender_count <- table(data_completed$Gender)

# PLOTTING
ggplot(data_completed, aes(x = Gender)) +
  geom_bar(fill = "skyblue2", color = NA) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "", y = "Count") +
  theme_minimal()

########################################################################################################################
# DESCRIPTIVES: AGE
########################################################################################################################
data_completed$Age <- as.numeric(data_completed$Age)

# PLOTTING
ggplot(data_completed, aes(x = "", y = Age)) +
  geom_violin(fill = "skyblue2", color = NA) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ylab("Age") +
  xlab("") +
  theme_minimal()

ggplot(data_completed, aes(x = "", y = Age)) +
  geom_boxplot(fill = "skyblue2", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ylab("Age") +
  xlab("") +
  theme_minimal()

########################################################################################################################
# DESCRIPTIVES: EDUCATION LEVEL
########################################################################################################################
data_completed <- data.frame(lapply(data_completed, function(x)
  ifelse(x == "Primary education ─ primary school until middle school (~ 6 - 11 years)", "Primary education", x)))
data_completed <- data.frame(lapply(data_completed, function(x)
  ifelse(x == "Secondary education ─ highschool (~ 12 - 18 years)", "Secondary education", x)))

education_count <- table(data_completed$Education.level)

# Create a factor with the desired order of the labels
data_completed$Education.level <- factor(data_completed$Education.level,
                                         levels = c("Primary education", "Secondary education","Bachelor's or equivalent",
                                                    "Master's or equivalent","Doctorate or equivalent"))

# PLOTTING
ggplot(data_completed, aes(x = Education.level)) +
  geom_bar(fill = "skyblue2", color = NA) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################################################################################################
# DESCRIPTIVES: PHQ-9
########################################################################################################################
phq_count <- table(data_completed$PHQ.9_category)
data_completed$PHQ.9_category <- factor(data_completed$PHQ.9_category,
                                        levels = c("minimal", "mild", "moderate","moderately severe", "severe"))

mean_phq <- mean(data_completed$PHQ.9_score) # 8.22973

# PLOTTING
ggplot(data_completed, aes(x = PHQ.9_category)) +
  geom_bar(fill = "skyblue2", color = NA) +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(x = "", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_completed, aes(x = "", y = PHQ.9_score)) +
  geom_boxplot(fill = "skyblue2", color = "black") +
  geom_jitter(width = 0.2, alpha = 0.5) +
  ylab("PHQ-9 score") +
  xlab("") +
  theme_minimal()

########################################################################################################################
# ANALYSIS 1: PREPARE DATA SET
########################################################################################################################
# Create a data frame only consisting of participant ID, pre IPQ-R and post IPQ-R scores
data_analysis_1 <- data_completed[, c(7, 13:46)]

# Re-code IPQ-R values (flip items that score in opposite direction) - positive IR = positive score
IPQ_right_columns <- c(2,5,7,8,9,10,12,18,19,22,24,25,26,27,29,35)

for (column in 2:ncol(data_analysis_1)){
  if (column %in% IPQ_right_columns){
    for (row in 1:nrow(data_analysis_1)){
      if (data_analysis_1[row,column] == "Strongly disagree"){
        data_analysis_1[row,column] <- -2
      } else if (data_analysis_1[row,column] == "Somewhat disagree"){
        data_analysis_1[row,column] <- -1
      } else if (data_analysis_1[row,column] == "Neither agree nor disagree"){
        data_analysis_1[row,column] <- 0
      } else if (data_analysis_1[row,column] == "Somewhat agree"){
        data_analysis_1[row,column] <- 1
      } else if (data_analysis_1[row,column] == "Strongly agree"){
        data_analysis_1[row,column] <- 2
      }
    }
  } else {
    for (row in 1:nrow(data_analysis_1)){
      if (data_analysis_1[row,column] == "Strongly disagree"){
        data_analysis_1[row,column] <- 2
      } else if (data_analysis_1[row,column] == "Somewhat disagree"){
        data_analysis_1[row,column] <- 1
      } else if (data_analysis_1[row,column] == "Neither agree nor disagree"){
        data_analysis_1[row,column] <- 0
      } else if (data_analysis_1[row,column] == "Somewhat agree"){
        data_analysis_1[row,column] <- -1
      } else if (data_analysis_1[row,column] == "Strongly agree"){
        data_analysis_1[row,column] <- -2
      }
    }
  }
}

# Summarize values per sub-scale (mean)
data_analysis_1[, 2:35] <- apply(data_analysis_1[, 2:35], 2, as.numeric)
data_analysis_1$Timeline_pre <- rowMeans(data_analysis_1[, 2:7], na.rm = TRUE)
data_analysis_1$Timeline_post <- rowMeans(data_analysis_1[, 19:24], na.rm = TRUE)
data_analysis_1$Control_pre <- rowMeans(data_analysis_1[, 8:13], na.rm = TRUE)
data_analysis_1$Control_post <- rowMeans(data_analysis_1[, 25:30], na.rm = TRUE)
data_analysis_1$Coherence_pre <- rowMeans(data_analysis_1[, 14:18], na.rm = TRUE)
data_analysis_1$Coherence_post <- rowMeans(data_analysis_1[, 31:35], na.rm = TRUE)

write.csv(data_analysis_1,"data_analysis_1.csv", row.names = FALSE)

########################################################################################################################
# ANALYSIS 1: PERFORM ONE-SIDED T-TESTS PER SUB-SCALE (FREQUENTIST)
########################################################################################################################
# Check for normality
shapiro.test(data_analysis_1$Timeline_pre) # normality holds
shapiro.test(data_analysis_1$Timeline_post) # normality holds
shapiro.test(data_analysis_1$Control_pre) # significant (!)
shapiro.test(data_analysis_1$Control_post) # significant (!)
shapiro.test(data_analysis_1$Coherence_pre) # significant (!)
shapiro.test(data_analysis_1$Coherence_post) # significant (!)

# Check how non-normal variables are skewed to decide how to transform them
skewness(data_analysis_1$Control_pre) # -0.7659814 left-skewed (negatively skewed)
skewness(data_analysis_1$Control_post) # -0.8204254 left-skewed (negatively skewed)
skewness(data_analysis_1$Coherence_pre) # -0.560237 left-skewed (negatively skewed)
skewness(data_analysis_1$Coherence_post) # -0.7566808 left-skewed (negatively skewed)

# Check for homogeneity of variances
leveneTest(data_analysis_1$Timeline_pre, data_analysis_1$Timeline_post) # homogeneity of variances holds
leveneTest(data_analysis_1$Control_pre, data_analysis_1$Control_post) # homogeneity of variances holds
leveneTest(data_analysis_1$Coherence_pre, data_analysis_1$Coherence_post) # significant (!) - var.equal = FALSE

# Perform paired t-test for Timeline (improvement)
ttest_Timeline <- t.test(data_analysis_1$Timeline_post, data_analysis_1$Timeline_pre,
                         alternative = "greater", paired = TRUE)
ttest_Control <- t.test(data_analysis_1$Control_post, data_analysis_1$Control_pre,
                        alternative = "greater", paired = TRUE)
ttest_Coherence <- t.test(data_analysis_1$Coherence_post, data_analysis_1$Coherence_pre,
                          alternative = "greater", var.equal = FALSE, paired = TRUE)

# Adjust p-value for multiple testing with FDR
adjusted_p_ttest <- p.adjust(c(ttest_Timeline$p.value, ttest_Control$p.value, ttest_Coherence$p.value), method = "fdr")

# Calculate effect sizes
diff_Timeline <- data_analysis_1$Timeline_post - data_analysis_1$Timeline_pre
effect_size_Timeline <- mean(diff_Timeline) / sd(diff_Timeline, na.rm = TRUE) # 0.3377561 (moderate effect size)
diff_Control <- data_analysis_1$Control_post - data_analysis_1$Control_pre
effect_size_Control <- mean(diff_Control) / sd(diff_Control, na.rm = TRUE) # 0.5204718 (medium effect size)
diff_Coherence <- data_analysis_1$Coherence_post - data_analysis_1$Coherence_pre
effect_size_Coherence <- mean(diff_Coherence) / sd(diff_Coherence, na.rm = TRUE) # 0.4143921 (moderate effect size)

# Wilcoxon tests for Control & Coherence (as normality does not hold)
wilcox_Control <- wilcox.test(data_analysis_1$Control_post, data_analysis_1$Control_pre,
                              alternative = "greater", paired = TRUE)
wilcox_Coherence <- wilcox.test(data_analysis_1$Coherence_post, data_analysis_1$Coherence_pre,
                                alternative = "greater", paired = TRUE)

# Adjust p-values for Wilcoxon test
p.adjust(c(wilcox_Control$p.value, wilcox_Coherence$p.value), method = "fdr")

# Calculate post-hoc power
mean_effect_size <- mean(c(effect_size_Timeline, effect_size_Control, effect_size_Coherence))
posthoc_power <- pwr.t.test(d = mean_effect_size, n = 2*n, sig.level = 0.05, type = "two.sample",
                            alternative = "greater")$power # 0.9514445

########################################################################################################################
# ANALYSIS 1: BAYESIAN T-TESTS PER SUB-SCALE
########################################################################################################################
bf_ttest_Timeline <-ttestBF(data_analysis_1$Timeline_post, data_analysis_1$Timeline_pre,
                            paired = TRUE, nullInterval = c(0, Inf))
bf_ttest_Control <-ttestBF(data_analysis_1$Control_post, data_analysis_1$Control_pre,
                           paired = TRUE, nullInterval = c(0, Inf))
bf_ttest_Coherence <-ttestBF(data_analysis_1$Coherence_post, data_analysis_1$Coherence_pre,
                             paired = TRUE, nullInterval = c(0, Inf))

########################################################################################################################
# ANALYSIS 1: PLOTTING SUBSCALE-LEVEL
########################################################################################################################
# Calculate mean and standard deviation for "pre" and "post" groups
mean_pre_timeline <- mean(data_analysis_1$Timeline_pre)
mean_post_timeline <- mean(data_analysis_1$Timeline_post)
sd_pre_timeline <- sd(data_analysis_1$Timeline_pre)
sd_post_timeline <- sd(data_analysis_1$Timeline_post)
mean_pre_control <- mean(data_analysis_1$Control_pre)
mean_post_control <- mean(data_analysis_1$Control_post)
sd_pre_control <- sd(data_analysis_1$Control_pre)
sd_post_control <- sd(data_analysis_1$Control_post)
mean_pre_coherence <- mean(data_analysis_1$Coherence_pre)
mean_post_coherence <- mean(data_analysis_1$Coherence_post)
sd_pre_coherence <- sd(data_analysis_1$Coherence_pre)
sd_post_coherence <- sd(data_analysis_1$Coherence_post)

# PLOTTING: PRE AND POST  MEAN & SD PER SUBSCALE
x_levels <- c("Timline pre", "Timeline post", "Control pre", "Control post", "Coherence pre", "Coherence post")
x_values <- factor(x_levels, levels = x_levels)

ggplot() +
  geom_errorbar(
    aes(x = x_values,
        ymin = c(mean_pre_timeline - sd_pre_timeline, mean_post_timeline - sd_post_timeline,
                 mean_pre_control - sd_pre_control, mean_post_control - sd_post_control,
                 mean_pre_coherence - sd_pre_coherence, mean_post_coherence - sd_post_coherence),
        ymax = c(mean_pre_timeline + sd_pre_timeline, mean_post_timeline + sd_post_timeline,
                 mean_pre_control + sd_pre_control, mean_post_control + sd_post_control,
                 mean_pre_coherence + sd_pre_coherence, mean_post_coherence + sd_post_coherence)),
    width = 0.2, color = "black"
  ) +
  geom_point(
    aes(x = x_values,
        y = c(mean_pre_timeline, mean_post_timeline,
              mean_pre_control, mean_post_control,
              mean_pre_coherence, mean_post_coherence)),
    size = 3, color = c(rep("lightslateblue",2), rep("darkorchid",2), rep("palevioletred3",2))
  ) +
  labs(x = "", y = "mean +/- SD") +
  theme_minimal()

# PLOTTING: PRE AND POST DENSITIES PER SUBSCALE
# TIMELINE
ggplot(data_analysis_1) +
  geom_density(aes(x = Timeline_pre, fill = "Timeline_pre"), alpha = 0.5, color = NA) +
  geom_density(aes(x = Timeline_post, fill = "Timeline_post"), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c("Timeline_pre" = "blue", "Timeline_post" = "red"),
                    labels = c("post", "pre")) +
  labs(x = "Timeline", y = "Density", fill = "") +
  theme_minimal()

# CONTROL
ggplot(data_analysis_1) +
  geom_density(aes(x = Control_pre, fill = "Control_pre"), alpha = 0.5, color = NA) +
  geom_density(aes(x = Control_post, fill = "Control_post"), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c("Control_pre" = "blue", "Control_post" = "red"),
                    labels = c("post", "pre")) +
  labs(x = "Personal control", y = "", fill = "") +
  theme_minimal()

# COHERENCE
ggplot(data_analysis_1) +
  geom_density(aes(x = Coherence_pre, fill = "Coherence_pre"), alpha = 0.5, color = NA) +
  geom_density(aes(x = Coherence_post, fill = "Coherence_post"), alpha = 0.5, color = NA) +
  scale_fill_manual(values = c("Coherence_pre" = "blue", "Coherence_post" = "red"),
                    labels = c("post", "pre")) +
  labs(x = "Illness coherence", y = "", fill = "") +
  theme_minimal()

########################################################################################################################
# ANALYSIS 1: PLOTTING ITEM-LEVEL
########################################################################################################################
mean_pre_I1 <- mean(data_analysis_1$IPQ.R_1)
mean_post_I1 <- mean(data_analysis_1$IPQ.R.02_1)
sd_pre_I1 <- sd(data_analysis_1$IPQ.R_1)
sd_post_I1 <- sd(data_analysis_1$IPQ.R.02_1)
mean_pre_I2 <- mean(data_analysis_1$IPQ.R_2)
mean_post_I2 <- mean(data_analysis_1$IPQ.R.02_2)
sd_pre_I2 <- sd(data_analysis_1$IPQ.R_2)
sd_post_I2 <- sd(data_analysis_1$IPQ.R.02_2)
mean_pre_I3 <- mean(data_analysis_1$IPQ.R_3)
mean_post_I3 <- mean(data_analysis_1$IPQ.R.02_3)
sd_pre_I3 <- sd(data_analysis_1$IPQ.R_3)
sd_post_I3 <- sd(data_analysis_1$IPQ.R.02_3)
mean_pre_I4 <- mean(data_analysis_1$IPQ.R_4)
mean_post_I4 <- mean(data_analysis_1$IPQ.R.02_4)
sd_pre_I4 <- sd(data_analysis_1$IPQ.R_4)
sd_post_I4 <- sd(data_analysis_1$IPQ.R.02_4)
mean_pre_I5 <- mean(data_analysis_1$IPQ.R_5)
mean_post_I5 <- mean(data_analysis_1$IPQ.R.02_5)
sd_pre_I5 <- sd(data_analysis_1$IPQ.R_5)
sd_post_I5 <- sd(data_analysis_1$IPQ.R.02_5)
mean_pre_I6 <- mean(data_analysis_1$IPQ.R_6)
mean_post_I6 <- mean(data_analysis_1$IPQ.R.02_6)
sd_pre_I6 <- sd(data_analysis_1$IPQ.R_6)
sd_post_I6 <- sd(data_analysis_1$IPQ.R.02_6)
mean_pre_I7 <- mean(data_analysis_1$IPQ.R_7)
mean_post_I7 <- mean(data_analysis_1$IPQ.R.02_7)
sd_pre_I7 <- sd(data_analysis_1$IPQ.R_7)
sd_post_I7 <- sd(data_analysis_1$IPQ.R.02_7)
mean_pre_I8 <- mean(data_analysis_1$IPQ.R_8)
mean_post_I8 <- mean(data_analysis_1$IPQ.R.02_8)
sd_pre_I8 <- sd(data_analysis_1$IPQ.R_8)
sd_post_I8 <- sd(data_analysis_1$IPQ.R.02_8)
mean_pre_I9 <- mean(data_analysis_1$IPQ.R_9)
mean_post_I9 <- mean(data_analysis_1$IPQ.R.02_9)
sd_pre_I9 <- sd(data_analysis_1$IPQ.R_9)
sd_post_I9 <- sd(data_analysis_1$IPQ.R.02_9)
mean_pre_I10 <- mean(data_analysis_1$IPQ.R_10)
mean_post_I10 <- mean(data_analysis_1$IPQ.R.02_10)
sd_pre_I10 <- sd(data_analysis_1$IPQ.R_10)
sd_post_I10 <- sd(data_analysis_1$IPQ.R.02_10)
mean_pre_I11 <- mean(data_analysis_1$IPQ.R_11)
mean_post_I11 <- mean(data_analysis_1$IPQ.R.02_11)
sd_pre_I11 <- sd(data_analysis_1$IPQ.R_11)
sd_post_I11 <- sd(data_analysis_1$IPQ.R.02_11)
mean_pre_I12 <- mean(data_analysis_1$IPQ.R_12)
mean_post_I12 <- mean(data_analysis_1$IPQ.R.02_12)
sd_pre_I12 <- sd(data_analysis_1$IPQ.R_12)
sd_post_I12 <- sd(data_analysis_1$IPQ.R.02_12)
mean_pre_I13 <- mean(data_analysis_1$IPQ.R_13)
mean_post_I13 <- mean(data_analysis_1$IPQ.R.02_13)
sd_pre_I13 <- sd(data_analysis_1$IPQ.R_13)
sd_post_I13 <- sd(data_analysis_1$IPQ.R.02_13)
mean_pre_I14 <- mean(data_analysis_1$IPQ.R_14)
mean_post_I14 <- mean(data_analysis_1$IPQ.R.02_14)
sd_pre_I14 <- sd(data_analysis_1$IPQ.R_14)
sd_post_I14 <- sd(data_analysis_1$IPQ.R.02_14)
mean_pre_I15 <- mean(data_analysis_1$IPQ.R_15)
mean_post_I15 <- mean(data_analysis_1$IPQ.R.02_15)
sd_pre_I15 <- sd(data_analysis_1$IPQ.R_15)
sd_post_I15 <- sd(data_analysis_1$IPQ.R.02_15)
mean_pre_I16 <- mean(data_analysis_1$IPQ.R_16)
mean_post_I16 <- mean(data_analysis_1$IPQ.R.02_16)
sd_pre_I16 <- sd(data_analysis_1$IPQ.R_16)
sd_post_I16 <- sd(data_analysis_1$IPQ.R.02_16)
mean_pre_I17 <- mean(data_analysis_1$IPQ.R_17)
mean_post_I17 <- mean(data_analysis_1$IPQ.R.02_17)
sd_pre_I17 <- sd(data_analysis_1$IPQ.R_17)
sd_post_I17 <- sd(data_analysis_1$IPQ.R.02_17)

x_levels <- c("I1 pre", "I1 post", "I2 pre", "I2 post", "I3 pre", "I3 post", "I4 pre", "I4 post",
              "I5 pre", "I5 post", "I6 pre", "I6 post", "I7 pre", "I7 post", "I8 pre", "I8 post",
              "I9 pre", "I9 post", "I10 pre", "I10 post", "I11 pre", "I11 post", "I12 pre", "I12 post",
              "I13 pre", "I13 post", "I14 pre", "I14 post", "I15 pre", "I15 post", "I16 pre", "I16 post",
              "I17 pre", "I17 post")
x_values <- factor(x_levels, levels = x_levels)

means <- c(mean_pre_I1, mean_post_I1, mean_pre_I2, mean_post_I2, mean_pre_I3, mean_post_I3, mean_pre_I4, mean_post_I4,
           mean_pre_I5, mean_post_I5, mean_pre_I6, mean_post_I6, mean_pre_I7, mean_post_I7, mean_pre_I8, mean_post_I8,
           mean_pre_I9, mean_post_I9, mean_pre_I10, mean_post_I10, mean_pre_I11, mean_post_I11, mean_pre_I12,
           mean_post_I12, mean_pre_I13, mean_post_I13, mean_pre_I14, mean_post_I14, mean_pre_I15, mean_post_I15,
           mean_pre_I16, mean_post_I16, mean_pre_I17, mean_post_I17)

sds <- c(sd_pre_I1, sd_post_I1, sd_pre_I2, sd_post_I2, sd_pre_I3, sd_post_I3, sd_pre_I4, sd_post_I4,
         sd_pre_I5, sd_post_I5, sd_pre_I6, sd_post_I6, sd_pre_I7, sd_post_I7, sd_pre_I8, sd_post_I8,
         sd_pre_I9, sd_post_I9, sd_pre_I10, sd_post_I10, sd_pre_I11, sd_post_I11, sd_pre_I12, sd_post_I12,
         sd_pre_I13, sd_post_I13, sd_pre_I14, sd_post_I14, sd_pre_I15, sd_post_I15, sd_pre_I16, sd_post_I16,
         sd_pre_I17, sd_post_I17)

ggplot() +
  geom_errorbar(
    aes(x = x_values,
        ymin = means - sds,
        ymax = means + sds),
    width = 0.2, color = "black"
  ) +
  geom_point(
    aes(x = x_values,
        y = means),
    size = 3, color = ifelse(seq_along(x_values) %% 2 == 1, "blue", "red")
  ) +
  labs(x = "", y = "mean +/- SD") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate difference scores per item
# Make new new column per items with difference score for specific item per participant
data_analysis_1$I1_diff <- data_analysis_1$IPQ.R.02_1 - data_analysis_1$IPQ.R_1
data_analysis_1$I2_diff <- data_analysis_1$IPQ.R.02_2 - data_analysis_1$IPQ.R_2
data_analysis_1$I3_diff <- data_analysis_1$IPQ.R.02_3 - data_analysis_1$IPQ.R_3
data_analysis_1$I4_diff <- data_analysis_1$IPQ.R.02_4 - data_analysis_1$IPQ.R_4
data_analysis_1$I5_diff <- data_analysis_1$IPQ.R.02_5 - data_analysis_1$IPQ.R_5
data_analysis_1$I6_diff <- data_analysis_1$IPQ.R.02_6 - data_analysis_1$IPQ.R_6
data_analysis_1$I7_diff <- data_analysis_1$IPQ.R.02_7 - data_analysis_1$IPQ.R_7
data_analysis_1$I8_diff <- data_analysis_1$IPQ.R.02_8 - data_analysis_1$IPQ.R_8
data_analysis_1$I9_diff <- data_analysis_1$IPQ.R.02_9 - data_analysis_1$IPQ.R_9
data_analysis_1$I10_diff <- data_analysis_1$IPQ.R.02_10 - data_analysis_1$IPQ.R_10
data_analysis_1$I11_diff <- data_analysis_1$IPQ.R.02_11 - data_analysis_1$IPQ.R_11
data_analysis_1$I12_diff <- data_analysis_1$IPQ.R.02_12 - data_analysis_1$IPQ.R_12
data_analysis_1$I13_diff <- data_analysis_1$IPQ.R.02_13 - data_analysis_1$IPQ.R_13
data_analysis_1$I14_diff <- data_analysis_1$IPQ.R.02_14 - data_analysis_1$IPQ.R_14
data_analysis_1$I15_diff <- data_analysis_1$IPQ.R.02_15 - data_analysis_1$IPQ.R_15
data_analysis_1$I16_diff <- data_analysis_1$IPQ.R.02_16 - data_analysis_1$IPQ.R_16
data_analysis_1$I17_diff <- data_analysis_1$IPQ.R.02_17 - data_analysis_1$IPQ.R_17

# PLOTTING: CHANGE SCORES ON ITEM-LEVEL
# Calculate the mean values for each column
mean_values <- colMeans(data_analysis_1[, grep("I[0-9]+_diff", colnames(data_analysis_1))])

mean_df <- data.frame(Variable = names(mean_values), Mean = mean_values)
mean_df$Scale <- c(rep("Timeline",6), rep("Control", 6), rep("Coherence",5))
mean_df$Variable <- factor(mean_df$Variable, levels = paste0("I", 1:17, "_diff"))
mean_df$Scale <- as.factor(mean_df$Scale)

# Assign colors to the groups
color_palette <- c("Timeline" = "lightslateblue", "Control" = "darkorchid", "Coherence" = "palevioletred3")

ggplot(mean_df, aes(x = Variable, y = Mean)) +
  geom_bar(aes(fill = Scale), stat = "identity", width = 0.6) +
  scale_fill_manual(values = color_palette, labels = c("Timeline", "Personal control", "Illness coherence")) +
  labs(x = "", y = "Mean Value") +
  theme_minimal() +
  scale_x_discrete(labels = c("Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Item 7",
                              "Item 8", "Item 9", "Item 10", "Item 11", "Item 12", "Item 13", "Item 14",
                              "Item 15", "Item 16", "Item 17")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################################################################################################
# ANALYSIS 1: ITEM CHANGE SCORE NETWORK
########################################################################################################################
# Create a correlation matrix using the difference scores
cor_matrix <- cor(data_analysis_1[, grep("I[0-9]+_diff", colnames(data_analysis_1))])

# Plot network using the correlation matrix
qgraph(cor_matrix, layout = "spring", minimum = 0.1, theme = "colorblind",
       labels = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11",
                  "I12", "I13", "I14", "I15", "I16", "I17"),
       color = c(rep("lightslateblue",6), rep("darkorchid",6), rep("palevioletred3",5)))

qgraph(cor_matrix, layout = "spring", minimum = 0.2, theme = "colorblind",
       labels = c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11",
                  "I12", "I13", "I14", "I15", "I16", "I17"),
       color = c(rep("lightslateblue",6), rep("darkorchid",6), rep("palevioletred3",5)))

########################################################################################################################
# ANALYSIS 2: PREPARE DATA
########################################################################################################################
data_ANCOVA <- data_analysis_1[,c("ResponseId", "Timeline_pre", "Timeline_post", "Control_pre", "Control_post",
                                  "Coherence_pre", "Coherence_post")]
data_ANCOVA$phq <- data_completed$PHQ.9_score
data_ANCOVA$education <- data_completed$Education.level
data_ANCOVA$mean_pre <- rowMeans(data_ANCOVA[,c("Timeline_pre", "Control_pre", "Coherence_pre")])
data_ANCOVA$mean_post <- rowMeans(data_ANCOVA[,c("Timeline_post", "Control_post", "Coherence_post")])

write.csv(data_ANCOVA,"data_ANCOVA.csv", row.names = FALSE)

########################################################################################################################
# ANALYSIS 2: CORRELATIONS - EXPLORE RELATIONSHIPS
########################################################################################################################
# Correlate PHQ-9 scores with (1) change scores and (2) pre-scores
cor(data_ANCOVA$phq, (data_ANCOVA$Timeline_post - data_ANCOVA$Timeline_pre), method = "spearman") # 0.4160198
cor(data_ANCOVA$phq, (data_ANCOVA$Control_post - data_ANCOVA$Control_pre), method = "spearman") # -0.04031198
cor(data_ANCOVA$phq, (data_ANCOVA$Coherence_post - data_ANCOVA$Coherence_pre), method = "spearman") # 0.1038553

cor(data_ANCOVA$phq, data_ANCOVA$Timeline_pre, method = "spearman") # -0.5561126
cor(data_ANCOVA$phq, data_ANCOVA$Control_pre, method = "spearman") # -0.2876434
cor(data_ANCOVA$phq, data_ANCOVA$Coherence_pre, method = "spearman") # -0.3440799

# Correlate education-level with (1) change scores and (2) pre-scores
# re-code education to numeric
ed_to_numeric <- c("Primary education" = 0,
                   "Secondary education" = 1,
                   "Bachelor's or equivalent" = 2,
                   "Master's or equivalent" = 3,
                   "Doctorate or equivalent" = 4)

data_ANCOVA$education_num <- ed_to_numeric[data_ANCOVA$education]

cor(data_ANCOVA$education_num, (data_ANCOVA$Timeline_post - data_ANCOVA$Timeline_pre), method = "spearman") # -0.095
cor(data_ANCOVA$education_num, (data_ANCOVA$Control_post - data_ANCOVA$Control_pre), method = "spearman") # -0.042
cor(data_ANCOVA$education_num, (data_ANCOVA$Coherence_post - data_ANCOVA$Coherence_pre), method = "spearman") # -0.007

cor(data_ANCOVA$education_num, data_ANCOVA$Timeline_pre, method = "spearman") # -0.09220751
cor(data_ANCOVA$education_num, data_ANCOVA$Control_pre, method = "spearman") # 0.02387912
cor(data_ANCOVA$education_num, data_ANCOVA$Coherence_pre, method = "spearman") # -0.09900878

########################################################################################################################
# ANALYSIS 2: PERFORM ANCOVAS FOR EACH SUBSCALE - CHECK JASP
########################################################################################################################
# Fit the repeated measures ANCOVA model - NEW
ancova_Timeline <- aov(Timeline_post - Timeline_pre ~ education + phq, data = data_ANCOVA)
ancova_Control <- aov(Control_post - Control_pre ~ education + phq, data = data_ANCOVA)
ancova_Coherence <- aov(Coherence_post - Coherence_pre ~ education + phq, data = data_ANCOVA)

# Get p-values
summary(ancova_Timeline) # education: 0.411; phq: 0.265 (non-significant)
summary(ancova_Control) # education: 0.593; phq: 0.818 (non-significant)
summary(ancova_Coherence) # education: 0.675; phq: 0.350 (non-significant)

# Get r-squared
summary(lm(Timeline_post - Timeline_pre ~ education + phq, data = data_ANCOVA)) # mult. R²: 0.1052; adj. R²: -0.005755
summary(lm(Control_post - Control_pre ~ education + phq, data = data_ANCOVA)) # mult. R²: 0.05218; adj. R²: -0.05313
summary(lm(Coherence_post - Coherence_pre ~ education + phq, data = data_ANCOVA)) # mult. R²: 0.06346; adj. R²: -0.0406

# Check for assumptions: Linearity
plot(data_ANCOVA$phq, fitted(ancova_Timeline), xlab = "Covariate", ylab = "Fitted Values") # linear
plot(data_ANCOVA$education, fitted(ancova_Timeline), xlab = "Education", ylab = "Fitted Values") # non-linear (!)
plot(data_ANCOVA$phq, fitted(ancova_Control), xlab = "Covariate", ylab = "Fitted Values") # linear
plot(data_ANCOVA$education, fitted(ancova_Control), xlab = "Education", ylab = "Fitted Values") # non-linear (!)
plot(data_ANCOVA$phq, fitted(ancova_Coherence), xlab = "Covariate", ylab = "Fitted Values") # linear
plot(data_ANCOVA$education, fitted(ancova_Coherence), xlab = "Education", ylab = "Fitted Values") # non-linear (!)

# Check for assumptions: Homogeneity of variances
plot(fitted(ancova_Timeline), rstandard(ancova_Timeline), xlab = "Fitted Values", ylab = "Standardized Residuals")
plot(fitted(ancova_Control), rstandard(ancova_Control), xlab = "Fitted Values", ylab = "Standardized Residuals")
plot(fitted(ancova_Coherence), rstandard(ancova_Coherence), xlab = "Fitted Values", ylab = "Standardized Residuals")

# Check for assumptions: Normality
shapiro.test(residuals(ancova_Timeline)) # significant (!)
shapiro.test(residuals(ancova_Control)) # normality holds
shapiro.test(residuals(ancova_Coherence)) # normality holds

########################################################################################################################
# ANALYSIS 3: PREPARE DATA
########################################################################################################################
# Data frame for analysis 3
data_analysis_3 <- data_completed[, c(7, 47:57)]
data_analysis_3 <- data_analysis_3[- nrow(data_analysis_3), ]
colnames(data_analysis_3) <- c("ID", "Explainability-1", "Explainability-2", "Scope-1", "Scope-2", "Scope-3",
                               "Length-1", "Utility-1", "Utility-2", "Utility-3", "Utility-4", "Feedback")

label_to_numeric <- c("Strongly disagree" = -2,
                      "Somewhat disagree" = -1,
                      "Neither agree nor disagree" = 0,
                      "Somewhat agree" = 1,
                      "Strongly agree" = 2)

# Convert the categorical labels to numerical values
data_analysis_3[, c(2:11)] <- lapply(data_analysis_3[, c(2:11)], function(x) label_to_numeric[x])

########################################################################################################################
# ANALYSIS 3: CRONBACH'S ALPHA FOR ACCEPTABILITY
########################################################################################################################
alpha_results_explainability <- alpha(data_analysis_3[, c(2:3)]) ; alpha_results_explainability$alpha # low
alpha_results_scope <- alpha(data_analysis_3[, c(4:6)]) ; alpha_results_scope$alpha # good
alpha_results_utility <- alpha(data_analysis_3[, c(8:11)]) ; alpha_results_utility$alpha # good

# Overall
alpha_results_acceptability<- alpha(data_analysis_3[, c(2:11)]) ; alpha_results_acceptability$alpha # good

########################################################################################################################
# ANALYSIS 3: MEAN PER ACCEPTABILITY ITEM
########################################################################################################################
round(mean(data_analysis_3$'Explainability-1'),3) ; round(sd(data_analysis_3$'Explainability-1'),3)
round(mean(data_analysis_3$'Explainability-2'),3) ; round(sd(data_analysis_3$'Explainability-2'),3)
round(mean(data_analysis_3$'Scope-1'),3) ; round(sd(data_analysis_3$'Scope-1'),3)
round(mean(data_analysis_3$'Scope-2'),3) ; round(sd(data_analysis_3$'Scope-2'),3)
round(mean(data_analysis_3$'Scope-3'),3) ; round(sd(data_analysis_3$'Scope-3'),3)
round(mean(data_analysis_3$'Length-1'),3) ; round(sd(data_analysis_3$'Length-1'),3)
round(mean(data_analysis_3$'Utility-1'),3) ; round(sd(data_analysis_3$'Utility-1'),3)
round(mean(data_analysis_3$'Utility-2'),3) ; round(sd(data_analysis_3$'Utility-2'),3)
round(mean(data_analysis_3$'Utility-3'),3) ; round(sd(data_analysis_3$'Utility-3'),3)
round(mean(data_analysis_3$'Utility-4'),3) ; round(sd(data_analysis_3$'Utility-4'),3)

########################################################################################################################
# ANALYSIS 3: PLOTTING
########################################################################################################################
df_long <- data_analysis_3 %>% pivot_longer(cols = 2:11, names_to = "Item", values_to = "Value")

ggplot(df_long, aes(x = Item, y = Value, color = Item)) +
  geom_boxplot(color = "black") +
  geom_jitter(position = position_dodge(0.5)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = "", y = "Response") +
  ggtitle("") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none")

