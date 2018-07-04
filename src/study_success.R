# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(scales); library(MASS); library(pastecs)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data (choose between the next two files)
subject_info <- read.csv("../data/subject_info.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]

# Rename columns and colume values
colnames(subject_info)[colnames(subject_info)=="Natio1"] <- "Nationality"
colnames(subject_info)[colnames(subject_info)=="TrackNatio1"] <- "Group"
subject_info$Nationality <- revalue(subject_info$Nationality, c("NL" = "Dutch", "DU" = "German"))
subject_info$Track <- revalue(subject_info$Track, c("NL" = "Dutch", "EN" = "English"))
subject_info$Group <- revalue(subject_info$Group, c("DU_in_NL" = "German in Dutch track", "NL_in_NL" = "Dutch in Dutch track", "DU_in_EN" = "German in English track", "NL_in_EN" = "Dutch in English track"))

# Relevel (for better visualisation)
subject_info$Track <- factor(subject_info$Track, levels = c("Dutch", "English"))
subject_info$Nationality <- factor(subject_info$Nationality, levels = c("Dutch", "German"))
subject_info$Group <- factor(subject_info$Group, levels = c("Dutch in Dutch track", "Dutch in English track", "German in Dutch track", "German in English track"))
subject_info$DropOut <- factor(subject_info$DropOut, levels = c("DuringYear1", "AfterYear1", "No"))

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]


### -------------------------------------
### Study success: Descriptive statistics
### -------------------------------------

### Calculate summary outcomes

# Indices
index1_grade <- which(colnames(no_dropout)=="Course1_Grade")
index13_grade <- which(colnames(no_dropout)=="Course13_Grade")
index1_worth <- which(colnames(no_dropout)=="Course1_EC_Worth")
index13_worth <- which(colnames(no_dropout)=="Course13_EC_Worth")

# Calculate obtained ECs
obtained_ecs <- (no_dropout[, c(index1_grade:index13_grade)] > 5.5) * no_dropout[, c(index1_worth:index13_worth)] # Multiply grades by worth
colnames(obtained_ecs) <- gsub('Worth', 'Obtained', colnames(obtained_ecs), fixed=TRUE) # Change column names
obtained_ecs[is.na(obtained_ecs)] <- 0 # Replace NA by 0
no_dropout <- cbind(no_dropout, obtained_ecs)

# Calculate weighted grades
weighted_grades <- (no_dropout[, c(index1_grade:index13_grade)]) * no_dropout[, c(index1_worth:index13_worth)] # Multiply grades by worth
colnames(weighted_grades) <- gsub('Grade', 'Weighted', colnames(weighted_grades), fixed=TRUE) # Change column names
weighted_grades[is.na(weighted_grades)] <- 0 # Replace NA by 0
no_dropout <- cbind(no_dropout, weighted_grades)

# More indices
index1_ec <- which(colnames(no_dropout)=="Course1_EC_Obtained")
index13_ec <- which(colnames(no_dropout)=="Course13_EC_Obtained")
index1_weighted <- which(colnames(no_dropout)=="Course1_Weighted")
index13_weighted <- which(colnames(no_dropout)=="Course13_Weighted")

# Calculate sum of obtained ECs
no_dropout$EC_Obtained <- rowSums(no_dropout[,c(index1_ec:index13_ec)])

# Calculate sum of weighted grades
no_dropout$Weighted_Grade <- rowSums(no_dropout[,c(index1_weighted:index13_weighted)])

# Calculate the number of ECs taken
ecs_taken <- (no_dropout[, c(index1_grade:index13_grade)] >= 1) * no_dropout[, c(index1_worth:index13_worth)]
no_dropout$EC_Taken <- rowSums(ecs_taken, na.rm = TRUE)

# Calculate mean grade
no_dropout$Mean_Grade <- no_dropout$Weighted_Grade / no_dropout$EC_Taken


### Functions

# Function to calculate standard error of the median
se_median <- function(dep_var){
  sd <- sd(dep_var)
  n <- length(dep_var)
  se <- 1.253 * sd / sqrt(n)
  return(se)
}

# Function to calculate standard error of the median absolute deviation
se_mad <- function(dep_var){
  sd <- sd(dep_var)
  n <- length(dep_var)
  se <- 1.67 * sd / sqrt(2*n)
  return(se)
}

# Function to bootstrap the precision of test statistics
bootstrap <- function(data, func, iter){
  data <- na.omit(data)
  boot_sample <- boot(data, function(x,i) func(x[i]), iter)
  print("The original statistic is:")
  print(boot_sample$t0)
  print("The bootstrapped standard error of the statistic is:")
  se <- sd(boot_sample$t)
  print(se)
  print("The bootstrapped standard deviation of the statistic is:")
  sd <- se * sqrt(length(data))
  print(sd)
  ci <- boot.ci(boot_sample, type = "bca")
  print("The BCa 95% confidence intervals of the statistic are:")
  print(ci$bca[,4]); print(ci$bca[,5])
}

### ECs

# Mean with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$EC_Obtained, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$EC_Obtained, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$EC_Obtained, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$EC_Obtained, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$EC_Obtained, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$EC_Obtained, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$EC_Obtained)
se_median(no_dropout$EC_Obtained)
mad(no_dropout$EC_Obtained)
se_mad(no_dropout$EC_Obtained)

# Histogram of total number of ECs obtained
ECs_hist <- ggplot(data = no_dropout, aes(EC_Obtained, fill = Group)) +
  geom_histogram(col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nNumber of ECs obtained", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)); ECs_hist

ECs_hist_perc <- ggplot(data = no_dropout, aes(EC_Obtained, fill = Group)) +
  geom_histogram(aes(y=5*..density..*100), col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nNumber of ECs obtained", y = "Percentage of students (%)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)); ECs_hist_perc


### Mean grade

# Mean with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$Mean_Grade, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$Mean_Grade, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$Mean_Grade, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$Mean_Grade, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$Mean_Grade, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$Mean_Grade, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$Mean_Grade)
se_median(no_dropout$Mean_Grade)
mad(no_dropout$Mean_Grade)
se_mad(no_dropout$Mean_Grade)

# Histogram of mean grades
mean_hist <- ggplot(data = no_dropout, aes(Mean_Grade, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nMean grade", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); mean_hist

mean_hist_perc <- ggplot(data = no_dropout, aes(Mean_Grade, fill = Group)) +
  geom_histogram(aes(y=0.5*..density..*100), col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nMean grade", y = "Percentage of students (%)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); mean_hist_perc


### Weighted grade

# Mean with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$Weighted_Grade, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$Weighted_Grade, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$Weighted_Grade, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$Weighted_Grade, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$Weighted_Grade, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$Weighted_Grade, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$Weighted_Grade)
se_median(no_dropout$Weighted_Grade)
mad(no_dropout$Weighted_Grade)
se_mad(no_dropout$Weighted_Grade)

# Histogram of weighted grades
weighted_hist <- ggplot(data = no_dropout, aes(Weighted_Grade, fill = Group)) +
  geom_histogram(col = "white", binwidth = 50) +
  facet_grid(~Group) +
  labs(x = "\nWeighted grade", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=3)); weighted_hist

weighted_hist_perc <- ggplot(data = no_dropout, aes(Weighted_Grade, fill = Group)) +
  geom_histogram(aes(y=50*..density..*100), col = "white", binwidth = 50) +
  facet_grid(~Group) +
  labs(x = "\nWeighted grade", y = "Percentage of students (%)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=3)); weighted_hist_perc


### Passing the BSA

# All students
bsa_all <- table(subject_info$Group, subject_info$PassedBSA); bsa_all
prop.table(bsa_all, 2)

# No drop-outs
bsa_no_dropout <- table(no_dropout$Group, no_dropout$PassedBSA); bsa_no_dropout
prop.table(bsa_no_dropout, 2)


### Drop-out

# Three categories (during year 1, after year 1, no)
drop <- table(subject_info$DropOut, subject_info$Group); drop
round(prop.table(drop, 2)*100, 2) # Per group
round(prop.table(table(subject_info$DropOut))*100,2) # Overall

# Two categories (yes, no)
drop2 <- table(subject_info$DropOutBinary, subject_info$Group); drop2
round(prop.table(drop2, 2)*100, 2) # Per group
round(prop.table(table(subject_info$DropOutBinary))*100,2) # Overall


### -------------------------------------
### Study success: Inferential statistics
### -------------------------------------

### Total number of ECs

## Checking assumptions
ECs_hist # Data are not normally distributed
tapply(no_dropout$EC_Obtained, no_dropout$Group, shapiro.test)

# Kruskal-Wallis test
kruskal.test(EC_Obtained ~ Group, data = no_dropout)
no_dropout$Rank <- rank(no_dropout$EC_Obtained)
by(no_dropout$Rank, no_dropout$Group, mean)

## Robust ANOVA

# Transform data to wide format
wilcox_wide_ECs <- dcast(no_dropout, SubjectCode ~ Group, value.var = "EC_Obtained")
wilcox_wide_ECs$SubjectCode <- NULL

# Load functions from Rand Wilcox
source("Rallfun-v35.txt")

# Perform robust ANOVA with bootstrapping
t1waybt(wilcox_wide_ECs, tr = 0, nboot = 10000)
med1way(wilcox_wide_ECs) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."

## Mixed-effects model

# Transform to long data format
grades_long <- melt(no_dropout, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c(21:32), value.name = "Grade")
colnames(grades_long)[colnames(grades_long)=="variable"] <- "Course"
grades_long$Course <- as.factor(gsub("\\D", "", grades_long$Course))

EC_long <- melt(no_dropout, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c(33:44), value.name = "EC_Worth")
colnames(EC_long)[colnames(EC_long)=="variable"] <- "Course"
EC_long$Course <- as.factor(gsub("\\D", "", EC_long$Course))

weighted_long <- melt(no_dropout, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c(45:56), value.name = "Weighted")
colnames(weighted_long)[colnames(weighted_long)=="variable"] <- "Course"
weighted_long$Course <- as.factor(gsub("\\D", "", weighted_long$Course))

# Merge
subject_long <- merge(grades_long, EC_long, by=c("SubjectCode", "Track", "Nationality", "Group", "Course"))
subject_long <- merge(subject_long, weighted_long, by=c("SubjectCode", "Track", "Nationality", "Group", "Course"))



### Mean grade

## Checking assumptions
mean_hist # Data seem normally distributed
tapply(no_dropout$Mean_Grade, no_dropout$Group, shapiro.test) # Significant
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "Dutch in Dutch track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "Dutch in English track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "German in Dutch track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "German in English track"])

# Levene's test of homogeneity of variance
leveneTest(no_dropout$Mean_Grade, no_dropout$Group) # Not significant

# Is EC_Obtained different between the groups?
lm_mean <- lm(Mean_Grade ~ Group, data = no_dropout)
summary(lm_mean)

aov_mean <- aov(Mean_Grade ~ Group, data = no_dropout)
summary(aov_mean)
plot(aov_mean)

# Kruskal-Wallis test
kruskal.test(Mean_Grade ~ Group, data = no_dropout)
no_dropout$RankMean_Grade <- rank(no_dropout$Mean_Grade)
by(no_dropout$RankMean_Grade, no_dropout$Group, mean)

## Robust ANOVA

# Transform data to wide format
wilcox_wide_mean <- dcast(no_dropout, SubjectCode ~ Group, value.var = "Mean_Grade")
wilcox_wide_mean$SubjectCode <- NULL

# Perform robust ANOVA
t1waybt(wilcox_wide_mean, tr = 0, nboot = 10000)
med1way(wilcox_wide_mean) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### Weighted grade

## Checking assumptions
weighted_hist # Data seem skewed
tapply(no_dropout$Weighted_Grade, no_dropout$Group, shapiro.test) # Data are non-normally distributed

# Levene's test of homogeneity of variance
leveneTest(no_dropout$Weighted_Grade, no_dropout$Group) # Not significant

# Kruskal-Wallis test
kruskal.test(Weighted_Grade ~ Group, data = no_dropout)
no_dropout$RankWeighted_Grade <- rank(no_dropout$Weighted_Grade)
by(no_dropout$Weighted_Grade, no_dropout$Group, mean)

# Transform data to wide format
wilcox_wide_weighted <- dcast(no_dropout, SubjectCode ~ Group, value.var = "Weighted_Grade")
wilcox_wide_weighted$SubjectCode <- NULL

# Perform robust ANOVA
t1waybt(wilcox_wide_weighted, tr = 0, nboot = 10000)
med1way(wilcox_wide_weighted) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### Passing the BSA

# All students
bsa_all <- table(subject_info$Group, subject_info$PassedBSA); bsa_all
chisq.test(bsa_all)

# No drop-outs
bsa_no_dropout <- table(no_dropout$Group, no_dropout$PassedBSA); bsa_no_dropout
chisq.test(bsa_no_dropout)


### Drop-out

# Chi-square tests
chisq.test(drop)
chisq.test(drop2)

## Predict who will drop out
dropout_model <- glm(DropOutBinary ~ Group, family = binomial (link = "logit"), data = subject_info)
summary(dropout_model)

dropout_model2 <- glm(DropOutBinary ~ Group + Gender, family = binomial (link = "logit"), data = subject_info)
summary(dropout_model2)

## Lexical richness as predictor

# Merge subject info and LCA data
lex <- merge(lca_data, subject_info, all.x = TRUE) # Get lca_data from lexical_richness script

# Average lexical richness scores over the three exams
lex$LD <- rowMeans(cbind(lex$ld_oct, lex$ld_feb, lex$ld_apr))
lex$LS2 <- rowMeans(cbind(lex$ls2_oct, lex$ls2_feb, lex$ls2_apr))
lex$NDWESZ <- rowMeans(cbind(lex$ndwesz_oct, lex$ndwesz_feb, lex$ndwesz_apr))

# Models and model comparisons
ld_first <- glm(DropOutBinary ~ Group + Gender + LD, family = binomial (link = "logit"), data = lex)
summary(ld_first)

ls_second <- glm(DropOutBinary ~ Group + Gender + LD + LS2, family = binomial (link = "logit"), data = lex)
summary(ls_second)
anova(ld_first, ls_second, test = "Chisq")

ls_first <- glm(DropOutBinary ~ Group + Gender + LS2, family = binomial (link = "logit"), data = lex)
summary(ls_first)

ld_second <- glm(DropOutBinary ~ Group + Gender + LS2 + LD, family = binomial (link = "logit"), data = lex)
summary(ld_second)
anova(ls_first, ld_second, test = "Chisq")

lv_model <- glm(DropOutBinary ~ Group + Gender + LD + LS2 + NDWESZ, family = binomial (link = "logit"), data = lex)
summary(lv_model)
anova(ls_model, lv_model, test = "Chisq")

## To do: lsmeans / correct for multiple testing


### --------------------------------------------
### Correlations between the dependent variables
### --------------------------------------------

## Three continuous measures of study success 

# Create dataset
cor_data <- no_dropout[,cbind("EC_Obtained", "Mean_Grade", "Weighted_Grade")]

# Check whether the three dependent variables are normally distributed
apply(cor_data, 2, shapiro.test) # 2 to loop through columns

# Create a correlation matrix using Spearman's correlation coefficient
rcorr(as.matrix(cor_data), type = "spearman")

## Lexical richness measures

# Create dataset
lex_rich_measures <- lex[,cbind("LD", "LS2", "NDWESZ")]

# Check whether the three dependent variables are normally distributed
apply(lex_rich_measures, 2, shapiro.test) # 2 to loop through columns

# Create a correlation matrix using Spearman's correlation coefficient
rcorr(as.matrix(lex_rich_measures), type = "spearman")


### --------------------------------------------------------
### Do the 'better' Dutch students choose the English track?
### --------------------------------------------------------

# Select Dutch students only
dutch_data <- subject_info[subject_info$Nationality == "Dutch",]

# Descriptives per track
tapply(dutch_data$SchoolMean, dutch_data$Track, stat.desc)
tapply(dutch_data$SchoolMean, dutch_data$Track, t.test) # To obtain 95% CI
tapply(dutch_data$SchoolEnglish, dutch_data$Track, stat.desc)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, t.test) # To obtain 95% CI

# Plot distribution of grades
hist(dutch_data$SchoolMean[dutch_data$Track == "English"], breaks=12)
hist(dutch_data$SchoolMean[dutch_data$Track == "Dutch"], breaks=12)

hist(dutch_data$SchoolEnglish[dutch_data$Track == "English"], breaks=12)
hist(dutch_data$SchoolEnglish[dutch_data$Track == "Dutch"], breaks=12)

# Are grades normally distributed? --> Not in the Dutch track
tapply(dutch_data$SchoolMean, dutch_data$Track, shapiro.test)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, shapiro.test)

# Use non-parametric testing
wilcox.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
wilcox.test(dutch_data$SchoolEnglish[dutch_data$Track=="English"], dutch_data$SchoolEnglish[dutch_data$Track=="Dutch"])

# Since the p-value for SchoolMean is so close to significance (.07), also do a t-test for further exploration
t.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
