# Import libraries
library(ggplot2) # Plotting
library(scales) # To use pretty breaks in ggplot
library(reshape2) # Use dcast (long -> wide) and melt (wide -> long)
library(Hmisc) # For rcorr function
library(plyr); library(dplyr); library(gridExtra)
library(car); library(MASS); library(pastecs)
library(lme4) # Linear mixed-effects models
library(boot) # For bootstrapping
library(arm) # To create the binned residual plot
library(influence.ME) # To compute Cook's distance
library(WRS) # Wilcox's functions for robust statistics

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data (choose between the next two files)
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]
subject_info$Exemption <- NULL

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]
subject_info$CoursesOutsideProgramme <- NULL

# Are LCA measures available?
subject_info$LCA <- ifelse(!is.na(subject_info$LD), 1, 0)

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

# Indices
index1_grade <- which(colnames(no_dropout)=="Course1_Grade")
index13_grade <- which(colnames(no_dropout)=="Course13_Grade")
index1_worth <- which(colnames(no_dropout)=="Course1_EC_Worth")
index13_worth <- which(colnames(no_dropout)=="Course13_EC_Worth")
index1_ec <- which(colnames(no_dropout)=="Course1_EC_Obtained")
index13_ec <- which(colnames(no_dropout)=="Course13_EC_Obtained")
index1_weighted <- which(colnames(no_dropout)=="Course1_Weighted")
index13_weighted <- which(colnames(no_dropout)=="Course13_Weighted")
index1_passed <- which(colnames(no_dropout)=="Course1_Passed")
index13_passed <- which(colnames(no_dropout)=="Course13_Passed")


### ----------------------
### Descriptive statistics
### ----------------------

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


### -------------------------------------------------------------------------
### Inferential statistics per outcome variable (no-mixed effects models yet)
### -------------------------------------------------------------------------

### OBTAINED ECS

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

# Load functions from Rand Wilcox (only if WRS library cannot be imported)
#source("Rallfun-v35.txt")

# Perform robust ANOVA with bootstrapping
t1waybt(wilcox_wide_ECs, tr = 0, nboot = 10000)
#med1way(wilcox_wide_ECs) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."

## Ordinary regression
reg_group <- lm(EC_Obtained ~ Group, data = no_dropout)
summary(reg_group)

# Further predictors: gender, LD, LS, LV

## Robust ANCOVA

# Function to draw histograms per group for the lexical richness variables
lr_hist <- function(dep_var){
  par(mfrow=c(2,2))
  hist(no_dropout[[dep_var]][no_dropout$Group=="Dutch in Dutch track"], main = dep_var, xlab = "Dutch in Dutch track")
  hist(no_dropout[[dep_var]][no_dropout$Group=="Dutch in English track"], main = dep_var, xlab = "Dutch in English track")
  hist(no_dropout[[dep_var]][no_dropout$Group=="German in Dutch track"], main = dep_var, xlab = "German in Dutch track")
  hist(no_dropout[[dep_var]][no_dropout$Group=="German in English track"], main = dep_var, xlab = "German in English track")
}

# Normally distributed?
lr_hist("LD")
lr_hist("LS")
lr_hist("LV")

## Group differences in lexical richness? (assumption for ANCOVA)

# Descriptives
options(scipen=999) # No scientific notation (enable again by setting it to 0)
tapply(no_dropout$LD, no_dropout$Group, stat.desc)
tapply(no_dropout$LS, no_dropout$Group, stat.desc)
tapply(no_dropout$LV, no_dropout$Group, stat.desc)

# Check assumptions for ANOVA
leveneTest(no_dropout$LD, no_dropout$Group)

# Conduct ANOVA
ld_lm <- lm(LD ~ Group, data = no_dropout); summary(ld_lm)
ls_lm <- lm(LS ~ Group, data = no_dropout); summary(ls_lm)
lv_lm <- lm(LV ~ Group, data = no_dropout); summary(lv_lm)

ld_aov <- aov(LD ~ Group, data = no_dropout, na.action = na.exclude); summary(ld_aov)
ls_aov <- aov(LS ~ Group, data = no_dropout, na.action = na.exclude); summary(ls_aov)
lv_aov <- aov(LV ~ Group, data = no_dropout, na.action = na.exclude); summary(lv_aov)

# Conduct ANCOVA

# NB: No robust ANCOVA for more than two groups seems to be available

## Robust regression

# Define function to do robust regression (taken from Andy Field)
bootReg <- function (formula, data, i)
  {d <- data [i,] # i refers to a particular bootstrap sample
  fit <- lm(formula, data = d)
  return(coef(fit))
}

# Run robust regression
boot_group <- boot(statistic = bootReg, formula = EC_Obtained ~ Group, data = no_dropout, R = 10000)

# Relevel
no_dropout$Group <- factor(no_dropout$Group, levels = c("Dutch in Dutch track", "Dutch in English track", "German in Dutch track", "German in English track"))
no_dropout$Group <- factor(no_dropout$Group, levels = c("Dutch in English track", "Dutch in Dutch track", "German in Dutch track", "German in English track"))
no_dropout$Group <- factor(no_dropout$Group, levels = c("German in Dutch track", "Dutch in Dutch track", "Dutch in English track", "German in English track"))

# Results
boot_group$t0 # Intercept and slope coefficients
boot.ci(boot_group, type = "bca", conf = 0.991, index = 1) # Confidence intervals for intercept
boot.ci(boot_group, type = "bca", conf = 0.991, index = 2) # Confidence intervals for group: Dutch in English track
boot.ci(boot_group, type = "bca", conf = 0.991, index = 3) # Confidence intervals for group: German in Dutch track
boot.ci(boot_group, type = "bca", conf = 0.991, index = 4) # Confidence intervals for group: German in English track

# Other analyses (to be continued)
boot_gender <- boot(statistic = bootReg, formula = EC_Obtained ~ Group + Gender, data = no_dropout, R = 2000)

bootResults2 <- boot(statistic = bootReg, formula = EC_Obtained ~ Group, data = lr, R = 2000)
bootResults3 <- boot(statistic = bootReg, formula = EC_Obtained ~ Group + LD_Centered, data = lr, R = 2000)
bootResults2$t0 # Intercept and slope coefficients
boot.ci(bootResults3, type = "bca", conf = 0.991, index = 1) # Confidence intervals for intercept
boot.ci(bootResults3, type = "bca", conf = 0.991, index = 2) # Confidence intervals for group: Dutch in English track
boot.ci(bootResults3, type = "bca", index = 3) # Confidence intervals for group: German in Dutch track
boot.ci(bootResults3, type = "bca", index = 4) # Confidence intervals for group: German in English track

## Lexical richness

# Create dataframe
lr <- no_dropout[!is.na(no_dropout$LD),]
lr$LD_Centered <- lr$LD - mean(lr$LD)

# Are the lexical richness measures normally distribued?
hist(lr$LD) # Yes
hist(lr$LS) # So-so (little bit of positive skew)
hist(lr$LV) # Yes

# Are the lexical richness measures correlated?
rcorr(as.matrix(lr[,cbind("LD", "LS", "LV")]), type = "pearson")

## Regression
boot_lr <- boot(statistic = bootReg, formula = EC_Obtained ~ LV + LS + LD, data = lr, R = 2000)

# Results
boot_lr$t0 # Intercept and slope coefficients
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 1) # Confidence intervals for intercept
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 2) # Confidence intervals for group: Dutch in English track
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 3) # Confidence intervals for group: German in Dutch track
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 4) # Confidence intervals for group: German in English track


### MEAN GRADE

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


### WEIGHTED GRADE

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


### PASSING THE BSA

# All students
bsa_all <- table(subject_info$Group, subject_info$PassedBSA); bsa_all
chisq.test(bsa_all)

# No drop-outs
bsa_no_dropout <- table(no_dropout$Group, no_dropout$PassedBSA); bsa_no_dropout
chisq.test(bsa_no_dropout)


### DROP-OUT

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


### -----------------------------------
### Study success: Mixed-effects models
### -----------------------------------

# Transform to long data format
EC_long <- melt(no_dropout, id.vars=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "LCA"), measure.vars = c(index1_ec:index13_ec), value.name = "EC_Obtained")
colnames(EC_long)[colnames(EC_long)=="variable"] <- "Course"
EC_long$Course <- as.factor(gsub("\\D", "", EC_long$Course))

grades_long <- melt(no_dropout, id.vars=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "LCA"), measure.vars = c(index1_grade:index13_grade), value.name = "Grade")
colnames(grades_long)[colnames(grades_long)=="variable"] <- "Course"
grades_long$Course <- as.factor(gsub("\\D", "", grades_long$Course))

weighted_long <- melt(no_dropout, id.vars=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "LCA"), measure.vars = c(index1_weighted:index13_weighted), value.name = "Weighted_Grade")
colnames(weighted_long)[colnames(weighted_long)=="variable"] <- "Course"
weighted_long$Course <- as.factor(gsub("\\D", "", weighted_long$Course))

passed_long <- melt(no_dropout, id.vars=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "LCA"), measure.vars = c(index1_passed:index13_passed), value.name = "Passed")
colnames(passed_long)[colnames(passed_long)=="variable"] <- "Course"
passed_long$Course <- as.factor(gsub("\\D", "", passed_long$Course))

# Merge
subject_long <- merge(EC_long, grades_long, by=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "Course", "LCA"))
subject_long <- merge(subject_long, weighted_long, by=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "Course", "LCA"))
subject_long <- merge(subject_long, passed_long, by=c("SubjectCode", "Gender", "Track", "Nationality", "Group", "Course", "LCA"))

# Remove dataframes
rm(EC_long, grades_long, weighted_long, passed_long)
rm(list=ls(pattern="index"))

# Also create a long dataframe with the lexical richness measures, using a subset of all data
lr_long <- subject_long
lr_long$LD <- subject_info$LD[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LS <- subject_info$LS[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LV <- subject_info$LV[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long <- lr_long[!is.na(lr_long$LD),]


### Models

## Obtained ECs

# Investigate Group and Gender on the full dataset
ec_null <- glmer(Passed ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(ec_null)
ec_group <- update(ec_null, ~. + Group); summary(ec_group)
ec_lca <- update(ec_group, ~. + LCA); summary(ec_lca)
ec_gender <- update(ec_group, ~. + Gender); summary(ec_gender)
anova(ec_null, ec_group, ec_gender)

## Do the students for whom LR measures are available differ from the other students?

# Create binary variable



# Also use lexical richness as predictor, on the subset of the data for which these measures are available
ec_null_lr <- lmer(EC_Obtained ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(ec_null_lr)
ec_ld <- update(ec_null_lr, ~. + LD); summary(ec_ld)
anova(ec_null_lr, ec_ld)

ec_ls <- update(ec_null_lr, ~. + LS); summary(ec_ls)
anova(ec_null_lr, ec_ls)

ec_lv <- update(ec_null_lr, ~. + LV); summary(ec_lv)
anova(ec_null_lr, ec_lv)


# Investigate Group and Gender on the full dataset
ec_null <- lmer(EC_Obtained ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(ec_null)
ec_group <- update(ec_null, ~. + Group); summary(ec_group)
ec_gender <- update(ec_group, ~. + Gender); summary(ec_gender)
anova(ec_null, ec_group, ec_gender)

# Also use lexical richness as predictor, on the subset of the data for which these measures are available
ec_null_lr <- lmer(EC_Obtained ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(ec_null_lr)
ec_ld <- update(ec_null_lr, ~. + LD); summary(ec_ld)
anova(ec_null_lr, ec_ld)

ec_ls <- update(ec_null_lr, ~. + LS); summary(ec_ls)
anova(ec_null_lr, ec_ls)

ec_lv <- update(ec_null_lr, ~. + LV); summary(ec_lv)
anova(ec_null_lr, ec_lv)

## Mean grade

# Investigate Group and Gender on the full dataset
mean_null <- lmer(Grade ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(mean_null)
mean_group <- update(mean_null, ~. + Group); summary(mean_group)
mean_gender <- update(mean_group, ~. + Gender); summary(mean_gender)
anova(mean_null, mean_group, mean_gender)

# Also use lexical richness as predictor, on the subset of the data for which these measures are available
mean_null_lr <- lmer(Mean_Grade ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(mean_null_lr)
mean_ld <- update(mean_null_lr, ~. + LD); summary(mean_ld)
anova(mean_null_lr, mean_ld)

mean_ls <- update(mean_null_lr, ~. + LS); summary(mean_ls)
anova(mean_null_lr, mean_ls)

mean_lv <- update(mean_null_lr, ~. + LV); summary(mean_lv)
anova(mean_null_lr, mean_lv)

## Weighted grade

# Investigate Group and Gender on the full dataset
weighted_null <- lmer(Weighted_Grade ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(weighted_null)
weighted_group <- update(weighted_null, ~. + Group); summary(weighted_group)
weighted_gender <- update(weighted_group, ~. + Gender); summary(weighted_gender)
anova(weighted_null, weighted_group, weighted_gender)

# Also use lexical richness as predictor, on the subset of the data for which these measures are available
weighted_null_lr <- lmer(Weighted_Grade ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(weighted_null_lr)
weighted_ld <- update(weighted_null_lr, ~. + LD); summary(weighted_ld)
anova(weighted_null_lr, weighted_ld)

weighted_ls <- update(weighted_null_lr, ~. + LS); summary(weighted_ls)
anova(weighted_null_lr, weighted_ls)

weighted_lv <- update(weighted_null_lr, ~. + LV); summary(weighted_lv)
anova(weighted_null_lr, weighted_lv)


### Check assumptions

# Choose the model you want to check the assumptions for
model <- weighted_gender

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals

## Homoskedasticity
# The standard deviations of the residuals should not depend on the x-value

# Residual plot
plot(fitted(model), residuals(model))
abline(h = c(0, sd(residuals(model)), -sd(residuals(model))))

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(model), resid(model), cex.pts=1, col.int="black", xlab = "Predicted values")

# Correlation test (in case a pattern is visible)
cor.test(fitted(model), residuals(model))

## Absence of collinearity

## Normality of residuals
hist(residuals(model)) 
qqnorm(residuals(model))

## Absence of influential data points

# Calculate Cook's distance and visualise outcomes
no_dropout$Cook <- cooks.distance.estex(influence(model, group = 'SubjectCode'))
plot(no_dropout$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)

## Independence
# Is taken care of by the random intercepts at the subject level



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
