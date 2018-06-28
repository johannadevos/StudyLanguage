# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(scales); library(MASS); library(pastecs)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data
subject_info <- read.csv("../data/subject_info.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Rename columns and colume values
colnames(subject_info)[colnames(subject_info)=="Natio1"] <- "Nationality"
colnames(subject_info)[colnames(subject_info)=="TrackNatio1"] <- "Group"
subject_info$Nationality <- revalue(subject_info$Nationality, c("NL" = "Dutch", "DU" = "German"))
subject_info$Track <- revalue(subject_info$Track, c("NL" = "Dutch", "EN" = "English"))
subject_info$Group <- revalue(subject_info$Group, c("DU_in_NL" = "German in Dutch", "NL_in_NL" = "Dutch in Dutch", "DU_in_EN" = "German in English", "NL_in_EN" = "Dutch in English"))

# Relevel (for better visualisation)
subject_info$Track <- factor(subject_info$Track, levels = c("Dutch", "English"))
subject_info$Nationality <- factor(subject_info$Nationality, levels = c("Dutch", "German"))
subject_info$Group <- factor(subject_info$Group, levels = c("Dutch in Dutch", "Dutch in English", "German in Dutch", "German in English"))
subject_info$DropOut <- factor(subject_info$DropOut, levels = c("DuringYear1", "AfterYear1", "No"))

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]


### -------------------------------------
### Study success: Descriptive statistics
### -------------------------------------

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
tapply(no_dropout$ECsTotal, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$ECsTotal, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$ECsTotal, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$ECsTotal, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$ECsTotal, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$ECsTotal, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$ECsTotal, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$ECsTotal, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$ECsTotal)
se_median(no_dropout$ECsTotal)
mad(no_dropout$ECsTotal)
se_mad(no_dropout$ECsTotal)

# Histogram of total number of ECs obtained
ECs_hist <- ggplot(data = no_dropout, aes(ECsTotal, fill = Group)) +
  geom_histogram(col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nTotal number of ECs obtained", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)); ECs_hist

ECs_hist_perc <- ggplot(data = no_dropout, aes(ECsTotal, fill = Group)) +
  geom_histogram(aes(y=5*..density..*100), col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nTotal number of ECs obtained", y = "Percentage of students (%)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)); ECs_hist_perc

### Mean grade

# Mean with bootstrapped precision estimates
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$MeanPsyWeighted, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$MeanPsyWeighted, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$MeanPsyWeighted)
se_median(no_dropout$MeanPsyWeighted)
mad(no_dropout$MeanPsyWeighted)
se_mad(no_dropout$MeanPsyWeighted)

# Histogram of mean grades
mean_hist <- ggplot(data = no_dropout, aes(MeanPsyWeighted, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nMean grade", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); mean_hist

mean_hist_perc <- ggplot(data = no_dropout, aes(MeanPsyWeighted, fill = Group)) +
  geom_histogram(aes(y=0.5*..density..*100), col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nMean grade", y = "Percentage of students (%)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); mean_hist_perc


### Weighted grade

# Mean with bootstrapped precision estimates
tapply(no_dropout$WeightedGrade, no_dropout$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(no_dropout$WeightedGrade, func=mean, iter=10000)

# Median with bootstrapped precision estimates
tapply(no_dropout$WeightedGrade, no_dropout$Group, bootstrap, func=median, iter=10000) # Per group
bootstrap(no_dropout$WeightedGrade, func=median, iter=10000)

# Other descriptives
tapply(no_dropout$WeightedGrade, no_dropout$Group, stat.desc) # Summary
tapply(no_dropout$WeightedGrade, no_dropout$Group, se_median) # SE of median
tapply(no_dropout$WeightedGrade, no_dropout$Group, mad) # Median Absolute Deviation
tapply(no_dropout$WeightedGrade, no_dropout$Group, se_mad) # SE of Median Absolute Deviation

stat.desc(no_dropout$WeightedGrade)
se_median(no_dropout$WeightedGrade)
mad(no_dropout$WeightedGrade)
se_mad(no_dropout$WeightedGrade)

# Histogram of weighted grades
weighted_hist <- ggplot(data = no_dropout, aes(WeightedGrade, fill = Group)) +
  geom_histogram(col = "white", binwidth = 50) +
  facet_grid(~Group) +
  labs(x = "\nWeighted grade", y = "Number of students (absolute)\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=3)); weighted_hist

weighted_hist_perc <- ggplot(data = no_dropout, aes(WeightedGrade, fill = Group)) +
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

drop <- table(subject_info$Group, subject_info$DropOut); drop
prop.table(drop, 2)

# Collapse during and after year 1
subject_info$DropOutBinary[subject_info$DropOut != "No"] <- "Yes"
subject_info$DropOutBinary[subject_info$DropOut == "No"] <- "No"

drop2 <- table(subject_info$Group, subject_info$DropOutBinary); drop2
prop.table(drop2, 2)


### -------------------------------------
### Study success: Inferential statistics
### -------------------------------------

### Total number of ECs

## Checking assumptions
ECs_hist # Data are not normally distributed
tapply(no_dropout$ECsTotal, no_dropout$Group, shapiro.test)

# Kruskal-Wallis test
kruskal.test(ECsTotal ~ Group, data = no_dropout)
no_dropout$Rank <- rank(no_dropout$ECsTotal)
by(no_dropout$Rank, no_dropout$Group, mean)

## Robust ANOVA

# Transform data to wide format
wilcox_wide_ECs <- dcast(no_dropout, SubjectCode ~ Group, value.var = "ECsTotal")
wilcox_wide_ECs$SubjectCode <- NULL

# Load functions from Rand Wilcox
source("Rallfun-v35.txt")

# Perform robust ANOVA with bootstrapping
t1waybt(wilcox_wide_ECs, tr = 0, nboot = 10000)
med1way(wilcox_wide_ECs) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### Mean grade

## Checking assumptions
mean_hist # Data seem normally distributed
tapply(subject_info$MeanPsyWeighted, subject_info$Group, shapiro.test) # Significant
qplot(sample = subject_info$MeanPsyWeighted[subject_info$Group == "Dutch in Dutch"])
qplot(sample = subject_info$MeanPsyWeighted[subject_info$Group == "Dutch in English"])
qplot(sample = subject_info$MeanPsyWeighted[subject_info$Group == "German in Dutch"])
qplot(sample = subject_info$MeanPsyWeighted[subject_info$Group == "German in English"])

# Levene's test of homogeneity of variance
leveneTest(no_dropout$MeanPsyWeighted, no_dropout$Group) # Not significant

# Is ECsTotal different between the groups?
lm_mean <- lm(MeanPsyWeighted ~ Group, data = no_dropout)
summary(lm_mean)

aov_mean <- aov(MeanPsyWeighted ~ Group, data = no_dropout)
summary(aov_mean)
plot(aov_mean)

# Kruskal-Wallis test
kruskal.test(MeanPsyWeighted ~ Group, data = no_dropout)
no_dropout$RankMeanPsyWeighted <- rank(no_dropout$MeanPsyWeighted)
by(no_dropout$RankMeanPsyWeighted, no_dropout$Group, mean)

## Robust ANOVA

# Transform data to wide format
wilcox_wide_mean <- dcast(no_dropout, SubjectCode ~ Group, value.var = "MeanPsyWeighted")
wilcox_wide_mean$SubjectCode <- NULL

# Perform robust ANOVA
t1waybt(wilcox_wide_mean, tr = 0, nboot = 10000)
med1way(wilcox_wide_mean) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### Weighted grade

## Checking assumptions
weighted_hist # Data seem skewed
tapply(subject_info$WeightedGrade, subject_info$Group, shapiro.test) # Data are non-normally distributed

# Levene's test of homogeneity of variance
leveneTest(no_dropout$WeightedGrade, no_dropout$Group) # Not significant

# Kruskal-Wallis test
kruskal.test(WeightedGrade ~ Group, data = no_dropout)
no_dropout$RankWeightedGrade <- rank(no_dropout$WeightedGrade)
by(no_dropout$WeightedGrade, no_dropout$Group, mean)

# Transform data to wide format
wilcox_wide_weighted <- dcast(no_dropout, SubjectCode ~ Group, value.var = "WeightedGrade")
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

drop <- table(subject_info$Group, subject_info$DropOut); drop
chisq.test(drop)

drop2 <- table(subject_info$Group, subject_info$DropOutBinary); drop2
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
cor_data <- no_dropout[,cbind("ECsTotal", "MeanPsyWeighted", "WeightedGrade")]

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
