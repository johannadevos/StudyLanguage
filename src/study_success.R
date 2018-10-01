# Import libraries
library(arm) # To create the binned residual plot
library(boot) # For bootstrapping
library(car) # Levene's test
library(emmeans) # To perform mulitple comparisons
library(ggplot2) # Plotting
library(Hmisc) # For rcorr function
library(influence.ME) # To compute Cook's distance in mixed-effects models
library(lme4) # Linear mixed-effects models
library(pastecs) # stat.desc function
library(reshape2) # Use dcast (long -> wide) and melt (wide -> long)
library(scales) # To use pretty breaks in ggplot
library(WRS) # Wilcox's functions for robust statistics

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")
subject_info$SubjectCode <- as.factor(subject_info$SubjectCode)

# Are LCA measures available?
subject_info$LCA <- ifelse(!is.na(subject_info$LD), 1, 0)

# Function to bootstrap the precision of test statistics
bootstrap <- function(data, func, iter, alpha){
  data <- na.omit(data)
  boot_sample <- boot(data, function(x,i) func(x[i]), iter)
  print("The estimated value of the statistic is:")
  print(round(boot_sample$t0, 2))
  print("The bootstrapped standard error of the statistic is:")
  se <- sd(boot_sample$t)
  print(round(se, 2))
  print("The bootstrapped standard deviation of the statistic is:")
  sd <- se * sqrt(length(data))
  print(round(sd,2))
  ci <- boot.ci(boot_sample, type = "bca", conf = 1-alpha)
  print("The BCa confidence intervals of the statistic are:")
  print(round(ci$bca[,4],2)); print(round(ci$bca[,5],2))
}


### --------------------------------------------------------------------
### Question 4: Do the 'better' Dutch students choose the English track?
### --------------------------------------------------------------------

# Select Dutch students only
dutch_data <- subject_info[subject_info$Nationality == "Dutch",]

# Alpha according to Li & Ji (2005); cited in Nyholt (2004)
alpha_4 = .0253

# Are the grades normally distributed? --> Not in the Dutch track
tapply(dutch_data$SchoolMean, dutch_data$Track, shapiro.test)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, shapiro.test)

# Plot distribution of grades
hist(dutch_data$SchoolMean[dutch_data$Track == "English"], breaks=12) # Overall grade
hist(dutch_data$SchoolMean[dutch_data$Track == "Dutch"], breaks=12)

hist(dutch_data$SchoolEnglish[dutch_data$Track == "English"], breaks=12) # English grade
hist(dutch_data$SchoolEnglish[dutch_data$Track == "Dutch"], breaks=12)

# Descriptives per track
tapply(dutch_data$SchoolMean, dutch_data$Track, stat.desc)
tapply(dutch_data$SchoolMean, dutch_data$Track, t.test, conf.level = (1-alpha_4)) # To obtain CI
tapply(dutch_data$SchoolEnglish, dutch_data$Track, stat.desc)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, t.test, conf.level = (1-alpha_4)) # To obtain CI

# Bootstrapped descriptives per dependent variable and track
tapply(dutch_data$SchoolMean, dutch_data$Track, bootstrap, func=mean, iter=10000, alpha = alpha_4) # SchoolMean
tapply(dutch_data$SchoolEnglish, dutch_data$Track, bootstrap, func=mean, iter=10000, alpha = alpha_4) # SchoolEnglish

# Use a non-parametric test to compare means
wilcox.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
wilcox.test(dutch_data$SchoolEnglish[dutch_data$Track=="English"], dutch_data$SchoolEnglish[dutch_data$Track=="Dutch"])

# Since the p-value for SchoolMean is so close to significance (.07), also do a t-test for further exploration
t.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])


### --------------------------------------------------------------------------------------
### Question 5: Is there a relation between nationality, study language and study success?
### --------------------------------------------------------------------------------------

### PREPROCESSING

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]
subject_info$Exemption <- NULL

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]
subject_info$CoursesOutsideProgramme <- NULL


### MATCH DUTCH STUDENTS ON ENGLISH GRADE

# Exclude Dutch students for whom high school grades are not available
subject_info <- subject_info[!is.na(subject_info$SchoolEnglish) | subject_info$Nationality=="German",]

# Create subsets for each Dutch group
d_in_d <- subject_info[subject_info$Group=="Dutch in Dutch track",]
d_in_e <- subject_info[subject_info$Group=="Dutch in English track",]

# Sort the English grades in the largest group (Dutch in Dutch track) ascendingly
d_in_d <- d_in_d[order(d_in_d$SchoolEnglish),]

# Delete rows until the English grades in the two groups match
for(i in 1:nrow(d_in_d)){
  print(paste("Index:", i))
  
  if(round(mean(d_in_d[-0:-i,]$SchoolEnglish, na.rm=TRUE),2) == round(mean(d_in_e$SchoolEnglish, na.rm = TRUE),2)){
    print("Mean English grade in each group:")
    print(mean(d_in_d[-0:-i,]$SchoolEnglish, na.rm=TRUE))
    print(mean(d_in_e$SchoolEnglish, na.rm=TRUE))
    
    print("Mean overall school grade in each group:")
    print(round(mean(d_in_d[-0:-i,]$SchoolMean, na.rm=TRUE), 2))
    print(round(mean(d_in_e$SchoolMean, na.rm=TRUE), 2))
    
    cut_off <- i
    
    break
  }
}

# Which subjects should be excluded in order to get matching English grades?
excl_subj <- d_in_d[1:cut_off,]$SubjectCode

# Exclude students so that the English grades end up the same
matched_all <- subject_info[!subject_info$SubjectCode %in% excl_subj,]

# Exclude students whose English school grade was unknown (this leaves only Dutch students)
matched_dutch <- matched_all[!is.na(matched_all$SchoolEnglish),]

# Delete unused Group levels (i.e., the two German groups)
matched_dutch$Group <- factor(matched_dutch$Group)

# To replicate the results for Question 4: set dutch_data to matched_dutch and run the above commands
dutch_data <- matched_dutch

# To replicate the results for Question 5: set no_dropout to matched or matched_dutch and run the above commands
subject_info <- matched_all

# Remove variables that are no longer needed
rm(excl_subj, d_in_d, d_in_e, matched_dutch, cut_off, i)


### CONTINUE PREPROCESSING

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

# Alpha according to Li & Ji (2005); cited in Nyholt (2004)
alpha_5 = 0.0202

## Create long data frames

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

# Remove data frames
rm(EC_long, grades_long, weighted_long, passed_long)
rm(list=ls(pattern="index"))


### TOTAL NUMBER OF OBTAINED ECs

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$EC_Obtained, func=mean, iter=10000, alpha = alpha_5) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$EC_Obtained, func=median, iter=10000, alpha = alpha_5) # Overall

# Summary statistics
tapply(no_dropout$EC_Obtained, no_dropout$Group, stat.desc) # Per group
stat.desc(no_dropout$EC_Obtained) # Overall

# Histograms
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

## Inferential statistics

# Checking the assumption of normality
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
wilcox_wide_ECs <- as.list(wilcox_wide_ECs)

# Load functions from Rand Wilcox (only if you have problems importing the WRS library)
#source("Rallfun-v35.txt")

# Perform robust ANOVA with bootstrapping
t1waybt(wilcox_wide_ECs, tr = 0, nboot = 10000)
#med1way(wilcox_wide_ECs) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."

# Post-hoc tests
mcppb20(wilcox_wide_ECs, tr = 0, crit = alpha_5/2, nboot = 10000) # Data need to be in list mode

## Mixed-effects models

# Run and compare models
ec_null <- glmer(Passed ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(ec_null)
ec_group <- update(ec_null, ~. + Group); summary(ec_group)
anova(ec_null, ec_group)

## Check assumptions of mixed-effects models

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(ec_group), resid(ec_group), cex.pts=1, col.int="black", xlab = "Predicted values")

# Normality of residuals
hist(residuals(ec_group))

# Are the random coefficients normally distributed?
subject_intercepts <- ranef(ec_group)[[1]]
subject_intercepts <- as.vector(subject_intercepts$`(Intercept)`)
hist(subject_intercepts)

course_intercepts <- ranef(ec_group)[[2]]
course_int_vec <- as.vector(course_intercepts$`(Intercept)`)
hist(course_intercepts)

# Influential data points (Cook's distance)
no_dropout$Cook_EC <- cooks.distance.estex(influence(ec_group, group = 'SubjectCode'))
plot(no_dropout$Cook_EC, ylab = "Cook's distance")

# Remove models
rm(list=ls(pattern="ec_"))


### MEAN GRADE

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$Mean_Grade, func=mean, iter=10000, alpha = alpha_5) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$Mean_Grade, func=median, iter=10000, alpha = alpha_5) # Overall

# Summary statistics
tapply(no_dropout$Mean_Grade, no_dropout$Group, stat.desc)
stat.desc(no_dropout$Mean_Grade)

# Histograms
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

## Inferential statistics

# Checking assumptions
mean_hist # Data seem normally distributed
tapply(no_dropout$Mean_Grade, no_dropout$Group, shapiro.test) # Significant
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "Dutch in Dutch track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "Dutch in English track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "German in Dutch track"])
qplot(sample = no_dropout$Mean_Grade[no_dropout$Group == "German in English track"])

# Levene's test of homogeneity of variance
leveneTest(no_dropout$Mean_Grade, no_dropout$Group) # Not significant

# Regular ANOVA
lm_mean <- lm(Mean_Grade ~ Group, data = no_dropout)
summary(lm_mean)

aov_mean <- aov(Mean_Grade ~ Group, data = no_dropout)
summary(aov_mean)
plot(aov_mean)

# Kruskal-Wallis test
kruskal.test(Mean_Grade ~ Group, data = no_dropout)
no_dropout$RankMean_Grade <- rank(no_dropout$Mean_Grade)
by(no_dropout$RankMean_Grade, no_dropout$Group, mean)

# Robust ANOVA

# Transform data to wide format
wilcox_wide_mean <- dcast(no_dropout, SubjectCode ~ Group, value.var = "Mean_Grade")
wilcox_wide_mean$SubjectCode <- NULL
wilcox_wide_mean <- as.list(wilcox_wide_mean)

# Perform robust ANOVA
t1waybt(wilcox_wide_mean, tr = 0, nboot = 10000)
#med1way(wilcox_wide_mean) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."

# Post-hoc tests
mcppb20(wilcox_wide_mean, tr = 0, crit = alpha_5/2, nboot = 10000) # Data need to be in list mode

# Mixed-effects models
mean_null <- lmer(Grade ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(mean_null)
mean_group <- update(mean_null, ~. + Group); summary(mean_group)
anova(mean_null, mean_group)

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(mean_group), resid(mean_group), cex.pts=1, col.int="black", xlab = "Predicted values")

# Normality of residuals
hist(residuals(mean_group))


### WEIGHTED GRADE

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$Weighted_Grade, func=mean, iter=10000, alpha = alpha_5) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_5) # Per group
bootstrap(no_dropout$Weighted_Grade, func=median, iter=10000, alpha = alpha_5) # Overall

# Summary statistics
tapply(no_dropout$Weighted_Grade, no_dropout$Group, stat.desc) # Per group
stat.desc(no_dropout$Weighted_Grade) # Overall

# Histograms
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

## Inferential statistics

# Checking assumptions
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
wilcox_wide_weighted <- as.list(wilcox_wide_weighted)

# Perform robust ANOVA
t1waybt(wilcox_wide_weighted, tr = 0, nboot = 10000)
#med1way(wilcox_wide_weighted) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."

# Post-hoc tests
mcppb20(wilcox_wide_weighted, tr = 0, crit = alpha_5/2, nboot = 10000)

# Mixed-effects models
weighted_null <- lmer(Weighted_Grade ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(weighted_null)
weighted_group <- update(weighted_null, ~. + Group); summary(weighted_group)
anova(weighted_null, weighted_group)

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(weighted_group), resid(weighted_group), cex.pts=1, col.int="black", xlab = "Predicted values")

# Normality of residuals
hist(residuals(weighted_group))


### Drop-out

## Descriptive statistics 

# Three categories (during year 1, after year 1, no)
drop3 <- table(subject_info$Group, subject_info$DropOut); drop3
round(prop.table(drop3, 1)*100, 2) # Per group
round(prop.table(table(subject_info$DropOut))*100,2) # Overall

# Two categories (yes, no)
drop2 <- table(subject_info$Group, subject_info$DropOutBinary); drop2
round(prop.table(drop2, 1)*100, 2) # Per group
round(prop.table(table(subject_info$DropOutBinary))*100,2) # Overall

# Confidence intervals around percentages
ci_perc <- function(prop, alpha, z, n)
{upper <- prop + z*sqrt((prop*((1-prop))/n))
lower <- prop - z*sqrt((prop*((1-prop))/n))
level <- (1-alpha)*100
print(paste(level, "% confidence interval: ", round(lower, 4), "-", round(upper, 4)))
}

ci_perc(0.1915, 0.02, 2.33, nrow(subject_info[subject_info$Group=="Dutch in Dutch track",]))
ci_perc(0.2121, 0.02, 2.33, nrow(subject_info[subject_info$Group=="Dutch in English track",]))
ci_perc(0.3400, 0.02, 2.33, nrow(subject_info[subject_info$Group=="German in Dutch track",]))
ci_perc(0.3323, 0.02, 2.33, nrow(subject_info[subject_info$Group=="German in English track",]))
ci_perc(0.2988, 0.02, 2.33, nrow(subject_info)) # Total

## Inferential statistics

# Chi-square tests
chisq.test(drop3)
chisq.test(drop2)

# Predict who will drop out
dropout_model <- glm(DropOutBinary ~ Group, family = binomial (link = "logit"), data = subject_info); summary(dropout_model)

emm_dropout <- emmeans(dropout_model, ~ Group); emm_dropout
pairs(emm_dropout, adjust = "none")
confint(pairs(emm_dropout, adjust = "none"))

# Check assumptions

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(dropout_model), resid(dropout_model), nclass = 24, cex.pts=1, col.int="black", xlab = "Predicted values")
# Only 4 dots, but that's because there are 4 groups and within each group all predictions are the same

# Histogram of residuals
hist(residuals(dropout_model)) # There are 8 possible values for the residuals (four groups * two outcomes)

# Investigate outliers

# Standardized residuals
subject_info$StandardizedResiduals <- rstandard(dropout_model)
plot(subject_info$StandardizedResiduals)
summary(subject_info$StandardizedResiduals)
length(which(subject_info$StandardizedResiduals > 1.96 | subject_info$StandardizedResiduals < -1.96)) / nrow(subject_info) # Only 5% should lie outside +- 1.96. We have 0%.
length(which(subject_info$StandardizedResiduals > 2.58 | subject_info$StandardizedResiduals < -2.58)) / nrow(subject_info) # Only 1% should lie outside +- 2.58. We have 0%.

# Store large residuals for further investigation
subject_info$LargeResidual <- rstudent(dropout_model) > 1.96 | rstudent(dropout_model) < -1.96

# Investigate influential cases

# Leverage
subject_info$Leverage <- hatvalues(dropout_model)
av_lev <- length(dropout_model$coefficients)/nrow(subject_info) # Average leverage
length(which(subject_info$Leverage > (2*av_lev)))
length(which(subject_info$Leverage > (3*av_lev)))
plot(subject_info$Leverage)
abline(h=2*av_lev); abline(h=3*av_lev)
subject_info$LargeLeverage <- subject_info["Leverage"] > (3*av_lev)

# DF beta
subject_info$dfBeta <- dfbeta(dropout_model)
summary(subject_info$dfBeta)

# Cook's distance
subject_info$Cook <- cooks.distance(dropout_model)
plot(subject_info$Cook) # No values above 0.85
subject_info$LargeCook <- subject_info["Cook"] > 0.85

# Covariance ratio
subject_info$CovRatio <- covratio(dropout_model)
high_cov <- 1 + 3*(3+1)/nrow(subject_info)
low_cov <- 1 - 3*(3+1)/nrow(subject_info)
subject_info$LargeCovRatio <- subject_info["CovRatio"] > high_cov | subject_info["CovRatio"] < low_cov

# Are there any cases with large residuals that are overly influential?
which(subject_info$LargeResidual==TRUE & (subject_info$LargeCook==TRUE | subject_info$LargeLeverage==TRUE | subject_info$LargeCovRatio==TRUE))

# Robust logistic regression

# Function to do bootstrapping
bootLogReg <- function (formula, data, i)
{d <- data [i,] # i refers to a particular bootstrap sample
fit <- glm(formula, data = d, family = "binomial")
return(coef(fit))
}

# Run robust regression
dropout_model_robust <- boot(statistic = bootLogReg, formula = DropOutBinary ~ Group, data = subject_info, R = 10000)

# Inspect results
dropout_model_robust$t0 # Intercept and slope coefficients
boot.ci(dropout_model_robust, type = "bca", conf = (1-alpha_5), index = 1) # Confidence intervals for intercept
boot.ci(dropout_model_robust, type = "bca", conf = (1-alpha_5), index = 2) # Confidence intervals for Dutch in English track
boot.ci(dropout_model_robust, type = "bca", conf = (1-alpha_5), index = 3) # Confidence intervals for German in Dutch track
boot.ci(dropout_model_robust, type = "bca", conf = (1-alpha_5), index = 4) # Confidence intervals for German in English track


### PASSING THE BSA

## Descriptive statistics 

# All students
bsa_all <- table(subject_info$Group, subject_info$PassedBSA); bsa_all
prop.table(bsa_all, 1)

# No drop-outs
bsa_no_dropout <- table(no_dropout$Group, no_dropout$PassedBSA); bsa_no_dropout
prop.table(bsa_no_dropout, 1)

## Inferential statistics

# All students
chisq.test(bsa_all)

# No drop-outs
chisq.test(bsa_no_dropout)


### ----------------------------------------------------------------------------------------------
### Question 6: Does students' lexical richness in the study language predict their study success?
### ----------------------------------------------------------------------------------------------

### Data preprocessing

# Read in data (go back to unmatched data)
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")
subject_info$SubjectCode <- as.factor(subject_info$SubjectCode)

# Are LCA measures available?
subject_info$LCA <- ifelse(!is.na(subject_info$LD), 1, 0)

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]
subject_info$Exemption <- NULL

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]
subject_info$CoursesOutsideProgramme <- NULL

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

# Dataset with just LCA
lca <- subject_info[subject_info$LCA==1,] # Incidentally, no-one dropped out during year 1

# Center the LCA measures
lca$LD_Centered <- lca$LD - mean(lca$LD)
lca$LS_Centered <- lca$LS - mean(lca$LS)
lca$LV_Centered <- lca$LV - mean(lca$LV)

## Create long data frames

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

# Remove data frames
rm(EC_long, grades_long, weighted_long, passed_long)
rm(list=ls(pattern="index"))

# Create a long data frame with the lexical richness measures, for mixed-effects modelling
lr_long <- subject_long
lr_long$LD <- subject_info$LD[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LS <- subject_info$LS[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LV <- subject_info$LV[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long <- lr_long[!is.na(lr_long$LD),]
lr_long$LCA <- NULL

### Alpha
alpha_6 <- .0253

### Define functions to do robust regression (taken Field, Miles & Field, 2012, p. 299)
bootReg <- function (formula, data, i)
{d <- data [i,] # i refers to a particular bootstrap sample
fit <- lm(formula, data = d)
return(coef(fit))
}

bootLogReg <- function (formula, data, i)
{d <- data [i,] # i refers to a particular bootstrap sample
fit <- glm(formula, data = d, family = "binomial")
return(coef(fit))
}


### TOTAL NUMBER OF OBTAINED ECs

## Mixed-effects model
lca_passed_me_group <- glmer(Passed ~ 1 + Group + (1|SubjectCode) + (1|Course), data = lr_long, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(lca_passed_me_group)
lca_passed_me_ld <- update(lca_passed_me_group, . ~ + LD + .); summary(lca_passed_me_ld)
lca_passed_me_ls <- update(lca_passed_me_ld, . ~ + LS + .); summary(lca_passed_me_ls)
lca_passed_me_lv <- update(lca_passed_me_ls, . ~ + LV + .); summary(lca_passed_me_lv)
anova(lca_passed_me_group, lca_passed_me_ld, lca_passed_me_ls, lca_passed_me_lv)

# Check assumptions

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(lca_passed_me_lv), resid(lca_passed_me_lv), cex.pts=1, col.int="black", xlab = "Predicted values")

# Normality of residuals
hist(residuals(lca_passed_me_lv))

# Are the random coefficients normally distributed?
subject_intercepts <- ranef(lca_passed_me_lv)[[1]]
subject_intercepts <- as.vector(subject_intercepts$`(Intercept)`)
hist(subject_intercepts)

course_intercepts <- ranef(lca_passed_me_lv)[[2]]
course_int_vec <- as.vector(course_intercepts$`(Intercept)`)
hist(course_intercepts)

## Robust regression
lca_ECs <- boot(statistic = bootReg, formula = EC_Obtained ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_ECs$t0 # Intercept and slope coefficients
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_6), index = 1) # Confidence intervals for intercept
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_6), index = 2) # Confidence intervals for LD
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_6), index = 3) # Confidence intervals for LS
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_6), index = 4) # Confidence intervals for LV

## OLS regression
lca_ECs_ols <- lm(EC_Obtained ~ LD_Centered + LS_Centered + LV_Centered, data = lca); summary(lca_ECs_ols)

# With polynomial terms
LD2 <- lca$LD_Centered^2
LS2 <- lca$LS_Centered^2
LV2 <- lca$LV_Centered^2

lca_ECs_ols2 <- lm(EC_Obtained ~ LD2 + LS2 + LV2 + LD_Centered + LS_Centered + LV_Centered, data = lca); summary(lca_ECs_ols2)
predictions <- predict(lca_ECs_ols2)
plot(lca$LS_Centered, predictions)

# Check assumptions

# Residual plot
plot(fitted(lca_ECs_ols), residuals(lca_ECs_ols)) # Does not look good - heteroscedasticity, also non-linearity? (stripe?)
abline(h = c(0, sd(residuals(lca_ECs_ols)), -sd(residuals(lca_ECs_ols))))

# Absence of collinearity (see earlier)

# Normality of residuals
hist(residuals(lca_ECs_ols)) # Skewed to the left
qqnorm(residuals(lca_ECs_ols))

# Absence of influential data points
# To do: see Field et al. (2012, p. 288)


### MEAN GRADE

## Robust regression
lca_mean <- boot(statistic = bootReg, formula = Mean_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_mean$t0 # Intercept and slope coefficients
boot.ci(lca_mean, type = "bca", conf = (1-alpha_6), index = 1) # Confidence intervals for intercept
boot.ci(lca_mean, type = "bca", conf = (1-alpha_6), index = 2) # Confidence intervals for LD
boot.ci(lca_mean, type = "bca", conf = (1-alpha_6), index = 3) # Confidence intervals for LS
boot.ci(lca_mean, type = "bca", conf = (1-alpha_6), index = 4) # Confidence intervals for LV

## OLS regression
lca_mean_ols <- lm(Mean_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca); summary(lca_mean_ols)

# Check assumptions

# Residual plot
plot(fitted(lca_mean_ols), residuals(lca_mean_ols)) # Looks good, no evidence of non-linearity or heteroscedasticity
abline(h = c(0, sd(residuals(lca_mean_ols)), -sd(residuals(lca_mean_ols))))

# Absence of collinearity
hist(lca$LD_Centered)
hist(lca$LS_Centered)
hist(lca$LV_Centered)
apply(lca[,cbind("LD", "LS", "LV")], 2, shapiro.test) # Significant for LS and LV, but histograms seem quite normal
rcorr(as.matrix(lca[,cbind("LD", "LS", "LV")]), type = "pearson") # Highest correlation is r = .38

# Normality of residuals
hist(residuals(lca_mean_ols)) # Looks good
qqnorm(residuals(lca_mean_ols))

# Absence of influential data points
# To do: see Field et al. (2012, p. 288)

## Mixed-effects model
lca_mean_me_null <- lmer(Grade ~ 1 + Group + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(lca_mean_me_null)
lca_mean_me_ld <- update(lca_mean_me_null, . ~ + LD + .); summary(lca_mean_me_ld)
lca_mean_me_ls <- update(lca_mean_me_ld, . ~ + LS + .); summary(lca_mean_me_ls)
lca_mean_me_lv <- update(lca_mean_me_ls, . ~ + LV + .); summary(lca_mean_me_lv)
anova(lca_mean_me_null, lca_mean_me_ld, lca_mean_me_ls, lca_mean_me_lv)

# Check assumptions

# Residual plot
plot(fitted(lca_mean_me_lv), residuals(lca_mean_me_lv)) # Stripes, because data are not strictly continuous. Otherwise okay.
abline(h = c(0, sd(residuals(lca_mean_me_lv)), -sd(residuals(lca_mean_me_lv))))

# Absence of collinearity
rcorr(as.matrix(lca[,cbind("LD", "LS", "LV")]), type = "pearson") # Highest correlation is r = .38

# Normality of residuals
hist(residuals(lca_mean_me_lv)) # Looks good
qqnorm(residuals(lca_mean_me_lv))

# Cook's distance: absence of influential data points
lca$Cook <- cooks.distance.estex(influence(lca_mean_me_lv, group = 'SubjectCode'))
plot(lca$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)

# Are the random coefficients normally distributed?
subject_intercepts <- ranef(lca_mean_me_lv)[[1]]
subject_intercepts <- as.vector(subject_intercepts$`(Intercept)`)
hist(subject_intercepts)

course_intercepts <- ranef(lca_mean_me_lv)[[2]]
course_int_vec <- as.vector(course_intercepts$`(Intercept)`)
hist(course_intercepts)

## Robust mixed-effects model
lca_mean_rme_null <- rlmer(Grade ~ 1 + Group + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(lca_mean_rme_null)
lca_mean_rme_ld <- update(lca_mean_rme_null, . ~ + LD + .); summary(lca_mean_rme_ld)
lca_mean_rme_ls <- update(lca_mean_rme_ld, . ~ + LS + .); summary(lca_mean_rme_ls)
lca_mean_rme_lv <- update(lca_mean_rme_ls, . ~ + LV + .); summary(lca_mean_rme_lv)
compare(lca_mean_rme_null, lca_mean_rme_ld, lca_mean_rme_ls, lca_mean_rme_lv)


### WEIGHTED GRADE

# Robust regression
lca_weighted <- boot(statistic = bootReg, formula = Weighted_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_weighted$t0 # Intercept and slope coefficients
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_6), index = 1) # Confidence intervals for intercept
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_6), index = 2) # Confidence intervals for LD
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_6), index = 3) # Confidence intervals for LS
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_6), index = 4) # Confidence intervals for LV

## OLS regression
lca_weighted_ols <- lm(Weighted_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca); summary(lca_weighted_ols)

# Check assumptions

# Residual plot
plot(fitted(lca_weighted_ols), residuals(lca_weighted_ols)) # Looks good, no evidence of non-linearity or heteroscedasticity
abline(h = c(0, sd(residuals(lca_weighted_ols)), -sd(residuals(lca_weighted_ols))))

# Absence of collinearity (see earlier)

# Normality of residuals
hist(residuals(lca_weighted_ols)) # Looks alright, not great
qqnorm(residuals(lca_weighted_ols))

# Absence of influential data points
# To do: see Field et al. (2012, p. 288)

## Mixed-effects model
lca_weighted_me <- lmer(Weighted_Grade ~ LD + LS + LV + (1|SubjectCode) + (1|Course), data = lr_long); summary(lca_weighted_me)

# Check assumptions

# Residual plot
plot(fitted(lca_weighted_me), residuals(lca_weighted_me)) # Stripes, some patterns visible.
abline(h = c(0, sd(residuals(lca_weighted_me)), -sd(residuals(lca_weighted_me))))

# Normality of residuals
hist(residuals(lca_weighted_me)) # Meh
qqnorm(residuals(lca_weighted_me))

# Absence of influential data points
# To do


### DROP-OUT

# Run robust regression
lca_drop <- boot(statistic = bootLogReg, formula = DropOutBinary ~ Group + LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_drop$t0 # Intercept and slope coefficients
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 1) # Confidence intervals for intercept
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 2) # Confidence intervals for Dutch in English track
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 3) # Confidence intervals for German in Dutch track
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 4) # Confidence intervals for German in English track
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 5) # Confidence intervals for LD
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 6) # Confidence intervals for LS
boot.ci(lca_drop, type = "bca", conf = (1-alpha_6), index = 7) # Confidence intervals for LV

# Logistic regression (see Field, Miles & Field, 2012, p. 337)
lca_drop_glm_null <- glm(DropOutBinary ~ Group, data = lca, family = "binomial"); summary(lca_drop_glm_null)
lca_drop_glm_ld <- update(lca_drop_glm_null, . ~ + LD_Centered + .); summary(lca_drop_glm_ld)
lca_drop_glm_ls <- update(lca_drop_glm_ld, . ~ + LS_Centered + .); summary(lca_drop_glm_ls)
lca_drop_glm_lv <- update(lca_drop_glm_ls, . ~ + LV_Centered + .); summary(lca_drop_glm_lv)

anova(lca_drop_glm_null, lca_drop_glm_ld, lca_drop_glm_ls, lca_drop_glm_lv)

modelChi <- lca_drop_glm_null$deviance - lca_drop_glm_ld$deviance
chidf <- lca_drop_glm_null$df.residual - lca_drop_glm_ld$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

# Check assumptions

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(lca_drop_glm), resid(lca_drop_glm), cex.pts=1, col.int="black", xlab = "Predicted values")

# Histogram of residuals
hist(residuals(lca_drop_glm))

# Investigate outliers

# Standardized residuals
lca$StandardizedResiduals <- rstandard(lca_drop_glm)
plot(lca$StandardizedResiduals)
abline(h=1.96); abline(h=2.58)
length(which(lca$StandardizedResiduals > 1.96 | lca$StandardizedResiduals < -1.96)) / nrow(lca) # Only 5% should lie outside +- 1.96. We have 8.5%.
length(which(lca$StandardizedResiduals > 2.58 | lca$StandardizedResiduals < -2.58)) / nrow(lca) # Only 1% should lie outside +- 2.58. We have 0%.

# Store large residuals for further investigation
lca$LargeResidual <- rstudent(lca_drop_glm) > 1.96 | rstudent(lca_drop_glm) < -1.96

# Investigate influential cases

# Leverage
lca$Leverage <- hatvalues(lca_drop_glm)
av_lev <- length(lca_drop_glm$coefficients)/nrow(lca) # Average leverage
plot(lca$Leverage)
abline(h=2*av_lev); abline(h=3*av_lev)
length(which(lca$Leverage > 2*av_lev)) / nrow(lca)
lca$LargeLeverage <- lca["Leverage"] > (2*av_lev)
which(lca$Leverage > 2*av_lev)

# DF Beta
lca$DFBeta <- dfbeta(lca_drop_glm) # This results in a matrix within the data frame. Only the first dimension of the matrix is visible.
summary(dfbeta(lca_drop_glm))

large_betas <- NULL # Create empty vector

for(predictor_index in 1:ncol(lca$DFBeta)){ # Loop through matrix to find df beta values > 1
  print(paste("Predictor index: ", predictor_index))
  print(which(lca$DFBeta[,predictor_index] > 1))
  print(lca$DFBeta[which(lca$DFBeta[,predictor_index] > 1), predictor_index])
  large_betas <- append(large_betas, which(lca$DFBeta[,predictor_index] > 1))
}

lca$LargeDFBeta <- FALSE # Create row with all LargeDFBeta values set to false
lca$LargeDFBeta[large_betas] <- TRUE # Set rows with large df beta values in one (or more) of the dimensions to true

# Exclude cases with large residuals that are overly influential
influ_cases <- which(lca$LargeResidual==TRUE & (lca$LargeLeverage==TRUE | lca$LargeDFBeta==TRUE))
lca_no_infl_cases <- lca[-influ_cases,]

# Rerun model for sensitivity analysis
lca_drop_glm_no_infl <- glm(DropOutBinary ~ Group + LD_Centered + LS_Centered + LV_Centered, data = lca_no_infl_cases, family = "binomial"); summary(lca_drop_glm_no_infl)
binnedplot(fitted(lca_drop_glm_no_infl), resid(lca_drop_glm_no_infl), cex.pts=1, col.int="black", xlab = "Predicted values")
hist(residuals(lca_drop_glm_no_infl)) # Histogram of residuals

# Cook's distance (not used in manuscript)
lca$Cook <- cooks.distance(lca_drop_glm) # No values above 0.85
plot(lca$Cook)
lca$LargeCook <- lca["Cook"] > 0.85

# Covariance ratio (not used in manuscript)
lca$CovRatio <- covratio(lca_drop_glm)
high_cov <- 1 + 3*(6+1)/305
low_cov <- 1 - 3*(6+1)/305
lca$LargeCovRatio <- lca["CovRatio"] > high_cov | lca["CovRatio"] < low_cov


### -----
### Other
### -----

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

options(scipen=999) # No scientific notation (enable again by setting it to 0)

## Are the lexical richness measures normally distribued?
hist(lca$LD) # Yes
hist(lca$LS) # So-so (little bit of positive skew)
hist(lca$LV) # Yes

## Contrasts

# Treatment coding
dd_vs_de <- c(0,1,0,0)
dd_vs_gd <- c(0,0,1,0)
dd_vs_ge <- c(0,0,0,1)

contrasts(subject_info$Group) <- cbind (dd_vs_de, dd_vs_gd, dd_vs_ge)
contrasts(subject_info$Group)

# Relevel
subject_info$Group <- factor(subject_info$Group, levels = c("Dutch in Dutch track", "Dutch in English track", "German in Dutch track", "German in English track"))
subject_info$Group <- factor(subject_info$Group, levels = c("German in English track", "Dutch in Dutch track", "Dutch in English track", "German in Dutch track"))

# Own contrasts: first Dutch versus German nationality, then Dutch versus English track
d_vs_g <- c(-1,-1,1,1)
dd_vs_de <- c(-1,1,0,0)
gd_vs_ge <- c(0,0,-1,1)

contrasts(subject_info$Group) <- cbind (d_vs_g, dd_vs_de, gd_vs_ge)
contrasts(subject_info$Group)

# Own contrasts: first L1 vs L2, then Dutch versus German (within the English track)
# The last contrast is German in Dutch versus Dutch in English and German in English (needed to arrive at three contrasts)
dd_vs_rest <- c(-1,0.33,0.33,0.33)
de_vs_ge <- c(0,-1,0,1)
gd_vs_english <- c(0,0.5,-1,0.5)

contrasts(subject_info$Group) <- cbind (dd_vs_rest, gd_vs_english, de_vs_ge)
contrasts(subject_info$Group)

## Correlations between the dependent variables

# Create dataset
cor_data <- no_dropout[,cbind("EC_Obtained", "Mean_Grade", "Weighted_Grade")]

# Check whether the three dependent variables are normally distributed
apply(cor_data, 2, shapiro.test) # 2 to loop through columns

# Create a correlation matrix using Spearman's correlation coefficient
rcorr(as.matrix(cor_data), type = "spearman")
