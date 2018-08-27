# Import libraries
library(emmeans) # To perform mulitple comparisons
library(ggplot2) # Plotting
library(scales) # To use pretty breaks in ggplot
library(reshape2) # Use dcast (long -> wide) and melt (wide -> long)
library(Hmisc) # For rcorr function
library(car) # Levene's test
library(pastecs) # stat.desc function
library(lme4) # Linear mixed-effects models
library(boot) # For bootstrapping
library(arm) # To create the binned residual plot
library(influence.ME) # To compute Cook's distance
library(WRS) # Wilcox's functions for robust statistics
library(plyr); library(dplyr); library(gridExtra); library(MASS)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Are LCA measures available?
subject_info$LCA <- ifelse(!is.na(subject_info$LD), 1, 0)

# Function to bootstrap the precision of test statistics
bootstrap <- function(data, func, iter, alpha){
  data <- na.omit(data)
  boot_sample <- boot(data, function(x,i) func(x[i]), iter)
  print("The estimated value of the statistic is:")
  print(boot_sample$t0)
  print("The bootstrapped standard error of the statistic is:")
  se <- sd(boot_sample$t)
  print(se)
  print("The bootstrapped standard deviation of the statistic is:")
  sd <- se * sqrt(length(data))
  print(sd)
  ci <- boot.ci(boot_sample, type = "bca", conf = 1-alpha)
  print("The BCa confidence intervals of the statistic are:")
  print(ci$bca[,4]); print(ci$bca[,5])
}


### --------------------------------------------------------------------
### Question 4: Do the 'better' Dutch students choose the English track?
### --------------------------------------------------------------------

# Select Dutch students only
dutch_data <- subject_info[subject_info$Nationality == "Dutch",]

# Alpha according to Li & Ji (2005); cited in Nyholt (2004)
alpha_school = .0253

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
tapply(dutch_data$SchoolMean, dutch_data$Track, t.test, conf.level = (1-alpha_school)) # To obtain CI
tapply(dutch_data$SchoolEnglish, dutch_data$Track, stat.desc)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, t.test, conf.level = (1-alpha_school)) # To obtain CI

# Bootstrapped descriptives per dependent variable and track
tapply(dutch_data$SchoolMean, dutch_data$Track, bootstrap, func=mean, iter=10000, alpha = alpha_school) # SchoolMean
tapply(dutch_data$SchoolEnglish, dutch_data$Track, bootstrap, func=mean, iter=10000, alpha = alpha_school) # SchoolEnglish

# Use a non-parametric test to compare means
wilcox.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])
wilcox.test(dutch_data$SchoolEnglish[dutch_data$Track=="English"], dutch_data$SchoolEnglish[dutch_data$Track=="Dutch"])

# Since the p-value for SchoolMean is so close to significance (.07), also do a t-test for further exploration
t.test(dutch_data$SchoolMean[dutch_data$Track=="English"], dutch_data$SchoolMean[dutch_data$Track=="Dutch"])


### --------------------------------------------------------------------------------------
### Question 5: Is there a relation between nationality, study language and study success?
### --------------------------------------------------------------------------------------

### Preprocessing

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]
subject_info$Exemption <- NULL

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]
subject_info$CoursesOutsideProgramme <- NULL

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

# Alpha according to Li & Ji (2005); cited in Nyholt (2004)
alpha_success = 0.0170


### TOTAL NUMBER OF OBTAINED ECs

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$EC_Obtained, func=mean, iter=10000, alpha = alpha_success) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$EC_Obtained, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$EC_Obtained, func=median, iter=10000, alpha = alpha_success) # Overall

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

# Checking assumptions
ECs_hist # Data are not normally distributed
tapply(no_dropout$EC_Obtained, no_dropout$Group, shapiro.test)

# Kruskal-Wallis test
kruskal.test(EC_Obtained ~ Group, data = no_dropout)
no_dropout$Rank <- rank(no_dropout$EC_Obtained)
by(no_dropout$Rank, no_dropout$Group, mean)

# Robust ANOVA

# Transform data to wide format
wilcox_wide_ECs <- dcast(no_dropout, SubjectCode ~ Group, value.var = "EC_Obtained")
wilcox_wide_ECs$SubjectCode <- NULL

# Load functions from Rand Wilcox (only if you have problems importing the WRS library)
#source("Rallfun-v35.txt")

# Perform robust ANOVA with bootstrapping
t1waybt(wilcox_wide_ECs, tr = 0, nboot = 10000)
#med1way(wilcox_wide_ECs) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### MEAN GRADE

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$Mean_Grade, func=mean, iter=10000, alpha = alpha_success) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$Mean_Grade, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$Mean_Grade, func=median, iter=10000, alpha = alpha_success) # Overall

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

# Perform robust ANOVA
t1waybt(wilcox_wide_mean, tr = 0, nboot = 10000)
#med1way(wilcox_wide_mean) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


### WEIGHTED GRADE

## Descriptive statistics 

# Mean with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=mean, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$Weighted_Grade, func=mean, iter=10000, alpha = alpha_success) # Overall

# Median with bootstrapped precision estimates
tapply(no_dropout$Weighted_Grade, no_dropout$Group, bootstrap, func=median, iter=10000, alpha = alpha_success) # Per group
bootstrap(no_dropout$Weighted_Grade, func=median, iter=10000, alpha = alpha_success) # Overall

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

# Perform robust ANOVA
t1waybt(wilcox_wide_weighted, tr = 0, nboot = 10000)
#med1way(wilcox_wide_weighted) # "WARNING: tied values detected. Estimate of standard error might be highly inaccurate, even with n large."


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

## Inferential statistics

# Chi-square tests
chisq.test(drop3)
chisq.test(drop2)

# Predict who will drop out
dropout_model <- glm(DropOutBinary ~ Group, family = binomial (link = "logit"), data = subject_info)
summary(dropout_model)

emm_dropout <- emmeans(dropout_model, ~ Group); emm_dropout
pairs(emm_dropout, adjust = "none")
confint(pairs(emm_dropout, adjust = "none"))


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

# Define functions to do robust regression (taken Field, Miles & Field, 2012, p. 299)
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

# Dataset with just LCA
lca <- subject_info[subject_info$LCA==1,] # Incidentally, no-one dropped out during year 1

# Center the LCA measures
lca$LD_Centered <- lca$LD - mean(lca$LD)
lca$LS_Centered <- lca$LS - mean(lca$LS)
lca$LV_Centered <- lca$LV - mean(lca$LV)


### TOTAL NUMBER OF OBTAINED ECs

## Robust regression
lca_ECs <- boot(statistic = bootReg, formula = EC_Obtained ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_ECs$t0 # Intercept and slope coefficients
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_success), index = 1) # Confidence intervals for intercept
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_success), index = 2) # Confidence intervals for LD
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_success), index = 3) # Confidence intervals for LS
boot.ci(lca_ECs, type = "bca", conf = (1-alpha_success), index = 4) # Confidence intervals for LV

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
boot.ci(lca_mean, type = "bca", conf = (1-alpha_success), index = 1) # Confidence intervals for intercept
boot.ci(lca_mean, type = "bca", conf = (1-alpha_success), index = 2) # Confidence intervals for LD
boot.ci(lca_mean, type = "bca", conf = (1-alpha_success), index = 3) # Confidence intervals for LS
boot.ci(lca_mean, type = "bca", conf = (1-alpha_success), index = 4) # Confidence intervals for LV

## OLS regression
lca_mean_ols <- lm(Mean_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca); summary(lca_mean_ols)

# Check assumptions

# Residual plot
plot(fitted(lca_mean_ols), residuals(lca_mean_ols)) # Looks good, no evidence of non-linearity or heteroscedasticity
abline(h = c(0, sd(residuals(lca_mean_ols)), -sd(residuals(lca_mean_ols))))

# Absence of collinearity
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


### WEIGHTED GRADE

# Robust regression
lca_weighted <- boot(statistic = bootReg, formula = Weighted_Grade ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_weighted$t0 # Intercept and slope coefficients
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_success), index = 1) # Confidence intervals for intercept
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_success), index = 2) # Confidence intervals for LD
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_success), index = 3) # Confidence intervals for LS
boot.ci(lca_weighted, type = "bca", conf = (1-alpha_success), index = 4) # Confidence intervals for LV

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
lca_drop <- boot(statistic = bootLogReg, formula = DropOutBinary ~ LD_Centered + LS_Centered + LV_Centered, data = lca, R = 10000)

# Inspect results
lca_drop$t0 # Intercept and slope coefficients
boot.ci(lca_drop, type = "bca", conf = (1-alpha_success), index = 1) # Confidence intervals for intercept
boot.ci(lca_drop, type = "bca", conf = (1-alpha_success), index = 2) # Confidence intervals for LD
boot.ci(lca_drop, type = "bca", conf = (1-alpha_success), index = 3) # Confidence intervals for LS
boot.ci(lca_drop, type = "bca", conf = (1-alpha_success), index = 4) # Confidence intervals for LV

lca_drop_glm <- glm(DropOutBinary ~ Group + LD_Centered + LS_Centered + LV_Centered, data = lca, family = "binomial"); summary(lca_drop_glm)

# Check assumptions

# Binned residual plot (see Gelman & Hill, 2007)
binnedplot(fitted(lca_drop_glm), resid(lca_drop_glm), cex.pts=1, col.int="black", xlab = "Predicted values")

# Investigate outliers

# Standardized residuals
lca$StandardizedResiduals <- rstandard(lca_drop_glm)
plot(lca$StandardizedResiduals)
length(which(lca$StandardizedResiduals > 1.96 | lca$StandardizedResiduals < -1.96)) / nrow(lca) # Only 5% should lie outside +- 1.96. We have 8.5%.
length(which(lca$StandardizedResiduals > 2.58 | lca$StandardizedResiduals < -2.58)) / nrow(lca) # Only 1% should lie outside +- 2.58. We have 0%.

# Store large residuals for further investigation
lca$LargeResidual <- rstudent(lca_drop_glm) > 1.96 | rstudent(lca_drop_glm) < -1.96

# Investigate influential cases

# Cook's distance
lca$Cook <- cooks.distance(lca_drop_glm) # No values above 0.85
plot(lca$Cook)
lca$LargeCook <- lca["Cook"] > 0.85

# Leverage
lca$Leverage <- hatvalues(lca_drop_glm)
av_lev <- (6+1)/305 # Average leverage
lca$LargeLeverage <- lca["Leverage"] > (3*av_lev)

# Covariance ratio
lca$CovRatio <- covratio(lca_drop_glm)
high_cov <- 1 + 3*(6+1)/305
low_cov <- 1 - 3*(6+1)/305
lca$LargeCovRatio <- lca["CovRatio"] > high_cov | lca["CovRatio"] < low_cov

# Exclude cases with large residuals that are overly influential
influ_cases <- which(lca$LargeResidual==TRUE & (lca$LargeCook==TRUE | lca$LargeLeverage==TRUE | lca$LargeCovRatio==TRUE))
lca_no_infl_cases <- lca[-influ_cases,]

# Rerun model for sensitivity analysis
lca_drop_glm_no_infl <- glm(DropOutBinary ~ Group + LD_Centered + LS_Centered + LV_Centered, data = lca_no_infl_cases, family = "binomial"); summary(lca_drop_glm_no_infl)
binnedplot(fitted(lca_drop_glm_no_infl), resid(lca_drop_glm_no_infl), cex.pts=1, col.int="black", xlab = "Predicted values")
plot(fitted(lca_drop_glm_no_infl), resid(lca_drop_glm_no_infl))

# Investigate outliers

# Standardized residuals
lca_no_infl_cases$StandardizedResiduals <- rstandard(lca_drop_glm_no_infl)
plot(lca_no_infl_cases$StandardizedResiduals)
length(which(lca_no_infl_cases$StandardizedResiduals > 1.96 | lca_no_infl_cases$StandardizedResiduals < -1.96)) / nrow(lca_no_infl_cases) # Only 5% should lie outside +- 1.96. We have 8.5%.
length(which(lca_no_infl_cases$StandardizedResiduals > 2.58 | lca_no_infl_cases$StandardizedResiduals < -2.58)) / nrow(lca_no_infl_cases) # Only 1% should lie outside +- 2.58. We have 0%.

# Store large residuals for further investigation
lca_no_infl_cases$LargeResidual <- rstudent(lca_drop_glm_no_infl) > 1.96 | rstudent(lca_drop_glm_no_infl) < -1.96

# Investigate influential cases

# Cook's distance
lca_no_infl_cases$Cook <- cooks.distance(lca_drop_glm_no_infl) # No values above 0.85
plot(lca_no_infl_cases$Cook)
lca_no_infl_cases$LargeCook <- lca_no_infl_cases["Cook"] > 0.85

# Leverage
lca_no_infl_cases$Leverage <- hatvalues(lca_drop_glm_no_infl)
av_lev <- (6+1)/nrow(lca_no_infl_cases) # Average leverage
lca_no_infl_cases$LargeLeverage <- lca_no_infl_cases["Leverage"] > (3*av_lev)

# Covariance ratio
lca_no_infl_cases$CovRatio <- covratio(lca_drop_glm_no_infl)
high_cov <- 1 + 3*(6+1)/nrow(lca_no_infl_cases)
low_cov <- 1 - 3*(6+1)/nrow(lca_no_infl_cases)
lca_no_infl_cases$LargeCovRatio <- lca_no_infl_cases["CovRatio"] > high_cov | lca_no_infl_cases["CovRatio"] < low_cov

which(lca_no_infl_cases$LargeResidual==TRUE & (lca_no_infl_cases$LargeCook==TRUE | lca_no_infl_cases$LargeLeverage==TRUE | lca_no_infl_cases$LargeCovRatio==TRUE))


### -------------------------------------------------------------------------
### Inferential statistics per outcome variable (no-mixed effects models yet)
### -------------------------------------------------------------------------

### OBTAINED ECS

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

# Are the lexical richness measures normally distribued?
hist(lca$LD) # Yes
hist(lca$LS) # So-so (little bit of positive skew)
hist(lca$LV) # Yes

# Are the lexical richness measures correlated?
rcorr(as.matrix(lca[,cbind("LD", "LS", "LV")]), type = "pearson")

## Regression
boot_lr <- boot(statistic = bootReg, formula = EC_Obtained ~ LV + LS + LD, data = lr, R = 2000)

# Results
boot_lr$t0 # Intercept and slope coefficients
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 1) # Confidence intervals for intercept
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 2) # Confidence intervals for group: Dutch in English track
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 3) # Confidence intervals for group: German in Dutch track
boot.ci(boot_lr, type = "bca", conf = 0.991, index = 4) # Confidence intervals for group: German in English track


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

## Gender as predictor
dropout_model2 <- glm(DropOutBinary ~ Group + Gender, family = binomial (link = "logit"), data = subject_info)
summary(dropout_model2)


### ----------------------------------------------------------------------------------------------
### Question 6: Does students' lexical richness in the study language predict their study success?
### ----------------------------------------------------------------------------------------------

# Dataset with just LCA
lca <- subject_info[subject_info$LCA==1,] # Incidentally, no-one dropped out during year 1

# Center the LCA measures
lca$LD_Centered <- lca$LD - mean(lca$LD)
lca$LS_Centered <- lca$LS - mean(lca$LS)
lca$LV_Centered <- lca$LV - mean(lca$LV)

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


### ---------------------------
### Extra: Mixed-effects models
### ---------------------------

### PREPROCESSING

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

# Remove dataframes
rm(EC_long, grades_long, weighted_long, passed_long)
rm(list=ls(pattern="index"))

# Also create a long dataframe with the lexical richness measures, using a subset of all data
lr_long <- subject_long
lr_long$LD <- subject_info$LD[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LS <- subject_info$LS[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long$LV <- subject_info$LV[match(lr_long$SubjectCode, subject_info$SubjectCode)]
lr_long <- lr_long[!is.na(lr_long$LD),]
lr_long$LCA <- NULL


### Models

### TOTAL NUMBER OF OBTAINED ECs / PASSING COURSES

# Investigate Group and Gender on the full dataset
ec_null <- glmer(Passed ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, family = "binomial", control = glmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=1e5))); summary(ec_null)
ec_group <- update(ec_null, ~. + Group); summary(ec_group)
ec_gender <- update(ec_group, ~. + Gender); summary(ec_gender)
anova(ec_null, ec_group, ec_gender)

# Are the outcomes different for students who completed all three exams?
ec_lca <- update(ec_group, ~. + LCA); summary(ec_lca)
anova(ec_group, ec_lca)

# Also use lexical richness as predictor, on the subset of the data for which these measures are available
ec_null_lr <- glmer(Passed ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(ec_null_lr)

ec_ld <- update(ec_null_lr, ~. + LD); summary(ec_ld)
anova(ec_null_lr, ec_ld) # Not significant

ec_ls <- update(ec_null_lr, ~. + LS); summary(ec_ls)
anova(ec_null_lr, ec_ls) # Not significant

ec_lv <- update(ec_null_lr, ~. + LV); summary(ec_lv)
anova(ec_null_lr, ec_lv) # Not significant


### MEAN GRADE

# Investigate Group and Gender on the full dataset
mean_null <- lmer(Grade ~ 1 + (1|SubjectCode) + (1|Course), data = subject_long, REML = FALSE); summary(mean_null)
mean_group <- update(mean_null, ~. + Group); summary(mean_group)
mean_gender <- update(mean_group, ~. + Gender); summary(mean_gender)
anova(mean_null, mean_group, mean_gender)

# Also use lexical richness as predictor, on the subset of the data for which these measures are available
mean_null_lr <- lmer(Grade ~ 1 + (1|SubjectCode) + (1|Course), data = lr_long, REML = FALSE); summary(mean_null_lr)

mean_ld <- update(mean_null_lr, ~. + LD); summary(mean_ld)
anova(mean_null_lr, mean_ld)

mean_ls <- update(mean_null_lr, ~. + LS); summary(mean_ls)
anova(mean_null_lr, mean_ls)

mean_lv <- update(mean_null_lr, ~. + LV); summary(mean_lv)
anova(mean_null_lr, mean_lv)


### WEIGHTED GRADE

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
model <- ec_group

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
subject_long$Cook <- cooks.distance.estex(influence(model, group = 'SubjectCode'))
plot(subject_long$Cook, ylab = "Cook's distance")
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
