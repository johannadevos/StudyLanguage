# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(fBasics); library(scales); library(MASS)

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
subject_info$Group <- revalue(subject_info$Group, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))

# Relevel (for better visualisation)
subject_info$Track <- factor(subject_info$Track, levels = c("Dutch", "English"))
subject_info$Nationality <- factor(subject_info$Nationality, levels = c("Dutch", "German"))
subject_info$Group <- factor(subject_info$Group, levels = c("Dutch_in_Dutch", "Dutch_in_English", "German_in_Dutch", "German_in_English"))
subject_info$DropOut <- factor(subject_info$DropOut, levels = c("DuringYear1", "AfterYear1", "No"))

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

# Inspect dataframe
str(subject_info)
# All first year results are currently coded as factors
# This is because there are also non-numeric data, such as "ND"


### -------------------------------------
### Study success: Descriptive statistics
### -------------------------------------

### ECs

# All students
tapply(subject_info$ECsTotal, subject_info$Group, mean)
tapply(subject_info$ECsTotal, subject_info$Group, sd)
descr_all_ec <- tapply(subject_info$ECsTotal, subject_info$Group, basicStats); descr_all_ec
basicStats(subject_info$ECsTotal)

# No drop-outs
tapply(no_dropout$ECsTotal, no_dropout$Group, mean)
tapply(no_dropout$ECsTotal, no_dropout$Group, sd)
descr_sub_ec <- tapply(no_dropout$ECsTotal, no_dropout$Group, basicStats); descr_sub_ec
basicStats(no_dropout$ECsTotal)

# Histogram of total number of ECs obtained
ECs_hist <- ggplot(data = no_dropout, aes(ECsTotal, fill = Group)) +
  geom_histogram(col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of total number of ECs obtained", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10)); ECs_hist


### Mean grade

# All students
tapply(subject_info$MeanPsyWeighted, subject_info$Group, mean, na.rm = TRUE)
tapply(subject_info$MeanPsyWeighted, subject_info$Group, sd, na.rm = TRUE)
descr_all_mean <- tapply(subject_info$MeanPsyWeighted, subject_info$Group, basicStats); descr_all_mean
basicStats(subject_info$MeanPsyWeighted)

# No drop-outs
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, mean, na.rm = TRUE)
tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, sd, na.rm = TRUE)
descr_sub_mean <- tapply(no_dropout$MeanPsyWeighted, no_dropout$Group, basicStats); descr_sub_mean
basicStats(no_dropout$MeanPsyWeighted)

# Histogram of mean grades
mean_hist <- ggplot(data = no_dropout, aes(MeanPsyWeighted, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of mean grades", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); mean_hist


### Weighted grade

# All students
tapply(subject_info$WeightedGrade, subject_info$Group, mean, na.rm = TRUE)
tapply(subject_info$WeightedGrade, subject_info$Group, sd, na.rm = TRUE)
descr_all_weighted <- tapply(subject_info$WeightedGrade, subject_info$Group, basicStats); descr_all_weighted
basicStats(subject_info$WeightedGrade)

# No drop-outs
tapply(no_dropout$WeightedGrade, no_dropout$Group, mean, na.rm = TRUE)
tapply(no_dropout$WeightedGrade, no_dropout$Group, sd, na.rm = TRUE)
descr_sub_weighted <- tapply(no_dropout$WeightedGrade, no_dropout$Group, basicStats); descr_sub_weighted
basicStats(no_dropout$WeightedGrade)

# Histogram of weighted grades
weighted_hist <- ggplot(data = no_dropout, aes(WeightedGrade, fill = Group)) +
  geom_histogram(col = "white", binwidth = 50) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of weighted grades", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=3)); weighted_hist


### Drop-out
drop <- table(subject_info$DropOut, subject_info$Group); drop
prop.table(drop, 2)

# Collapse during and after year 1
subject_info$DropOut2[subject_info$DropOut != "No"] <- "Yes"
subject_info$DropOut2[subject_info$DropOut == "No"] <- "No"

drop2 <- table(subject_info$DropOut2, subject_info$Group); drop2
prop.table(drop2, 2)


### -------------------------------------
### Study success: Inferential statistics
### -------------------------------------

### Total number of ECs

## Checking assumptions
ECs_hist # Data are not normally distributed

# Kruskal-Wallis test
kruskal.test(ECsTotal ~ Group, data = no_dropout)
no_dropout$Rank <- rank(no_dropout$ECsTotal)
by(no_dropout$Rank, no_dropout$Group, mean)

## Robust ANOVA

# Transform data to wide format
wilcox_wide <- dcast(no_dropout, SubjectCode ~ Group, value.var = "ECsTotal")
str(wilcox_wide)
wilcox_wide$SubjectCode <- NULL

# Load functions from Rand Wilcox
source("Rallfun-v35.txt")

# Perform robust ANOVA
t1way(wilcox_wide, tr = 0)
t1way(wilcox_wide, tr = 0.1)
t1way(wilcox_wide, tr = 0.2)
med1way(wilcox_wide) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."
t1waybt(wilcox_wide, tr = 0.1)


### Mean grade

## Checking assumptions
mean_hist # Data seem normally distributed
tapply(subject_info$MeanPsyWeighted, subject_info$Group, shapiro.test) # Shapiro test says data are non-normally distributed

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


### Passing the BSA

# All students
bsa_all <- table(subject_info$PassedBSA, subject_info$Group); bsa_all
prop.table(bsa_all, 2)
chisq.test(bsa_all)

# No drop-outs
bsa_no_dropout <- table(no_dropout$PassedBSA, no_dropout$Group); bsa_no_dropout
prop.table(bsa_no_dropout, 2)
chisq.test(bsa_no_dropout)


### Do certain groups drop out more often?
drop <- table(subject_info$DropOut, subject_info$Group); drop
chisq.test(drop)

## Collapse during and after year 1
subject_info$DropOut2[subject_info$DropOut != "No"] <- "Yes"
subject_info$DropOut2[subject_info$DropOut == "No"] <- "No"

drop2 <- table(subject_info$DropOut2, subject_info$Group); drop2
chisq.test(drop2)


### --------------------------------------------------------
### Do the 'better' Dutch students choose the English track?
### --------------------------------------------------------

# Select Dutch students only
dutch_data <- subject_info[subject_info$Nationality == "Dutch",]

# Descriptives per track
tapply(dutch_data$SchoolMean, dutch_data$Track, length)
tapply(dutch_data$SchoolMean, dutch_data$Track, summary)
tapply(dutch_data$SchoolMean, dutch_data$Track, sd, na.rm=TRUE)

tapply(dutch_data$SchoolEnglish, dutch_data$Track, length)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, summary)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, sd, na.rm=TRUE)

# Plot distribution of grades
hist(dutch_data$SchoolMean[dutch_data$Track == "English"], breaks=12)
hist(dutch_data$SchoolMean[dutch_data$Track == "Dutch"], breaks=12)

hist(dutch_data$SchoolEnglish[dutch_data$Track == "English"])
hist(dutch_data$SchoolEnglish[dutch_data$Track == "Dutch"])

# Are grades normally distributed? --> Not in the Dutch track
tapply(dutch_data$SchoolMean, dutch_data$Track, shapiro.test)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, shapiro.test)

# Use non-parametric testing
wilcox.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
wilcox.test(dutch_data$SchoolEnglish[dutch_data$Track=="English"], dutch_data$SchoolEnglish[dutch_data$Track=="Dutch"])

# Since the p-value for SchoolMean is so close to significance (.07), also do a t-test for further exploration
t.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
