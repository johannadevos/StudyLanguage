# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(fBasics); library(scales); library(MASS); library(pastecs); library(lme4); 
library(boot); library(nlme); library(influence.ME)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 


### ------------------------
### Read and preprocess data
### ------------------------

# Read in data
lca_data <- read.csv("../data/lexical_richness.txt", header=TRUE, sep="\t")

# For each measure, transform to a long data frame
ld_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ld_oct", "ld_feb", "ld_apr"), value.name = "LD")
colnames(ld_melted)[colnames(ld_melted)=="variable"] <- "Exam"
ld_melted$Exam <- revalue(ld_melted$Exam, c("ld_oct"="1 (Oct)", "ld_feb"="2 (Feb)", "ld_apr" = "3 (Apr)"))

ls_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ls2_oct", "ls2_feb", "ls2_apr"), value.name = "LS")
colnames(ls_melted)[colnames(ls_melted)=="variable"] <- "Exam"
ls_melted$Exam <- revalue(ls_melted$Exam, c("ls2_oct"="1 (Oct)", "ls2_feb"="2 (Feb)", "ls2_apr" = "3 (Apr)"))

lv_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ndwesz_oct", "ndwesz_feb", "ndwesz_apr"), value.name = "LV")
colnames(lv_melted)[colnames(lv_melted)=="variable"] <- "Exam"
lv_melted$Exam <- revalue(lv_melted$Exam, c("ndwesz_oct"="1 (Oct)", "ndwesz_feb"="2 (Feb)", "ndwesz_apr" = "3 (Apr)"))

# Merge long dataframes
lca_long <- merge(ld_melted, ls_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Exam"))
lca_long <- merge(lca_long, lv_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Exam"))

# Remove unused dataframes
#rm(list=ls(pattern="_melted"))


### -----------------------------------------
### Exam descriptives: Text length and grades
### -----------------------------------------

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

# Text length descriptives
tapply(lca_data$wordtokens_oct, lca_data$Group, bootstrap, func=mean, iter=10000) # Per group
bootstrap(lca_data$wordtokens_oct, func=mean, iter=10000) # Overall

tapply(lca_data$wordtokens_feb, lca_data$Group, bootstrap, func=mean, iter=10000)
bootstrap(lca_data$wordtokens_feb, func=mean, iter=10000)

tapply(lca_data$wordtokens_apr, lca_data$Group, bootstrap, func=mean, iter=10000)
bootstrap(lca_data$wordtokens_apr, func=mean, iter=10000)

# Grade descriptives
tapply(lca_data$grade_oct, lca_data$Group, bootstrap, func=mean, iter=10000)
bootstrap(lca_data$grade_oct, func=mean, iter=10000)

tapply(lca_data$grade_feb, lca_data$Group, bootstrap, func=mean, iter=10000)
bootstrap(lca_data$grade_feb, func=mean, iter=10000)

tapply(lca_data$grade_apr, lca_data$Group, bootstrap, func=mean, iter=10000)
bootstrap(lca_data$grade_apr, func=mean, iter=10000)

# Visualise grade distributions
hist_grades_oct <- ggplot(data = lca_data, aes(grade_oct, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of grades on Exam 1", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); hist_grades_oct

hist_grades_feb <- ggplot(data = lca_data, aes(grade_feb, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of grades on Exam 2", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); hist_grades_feb

hist_grades_apr <- ggplot(data = lca_data, aes(grade_apr, fill = Group)) +
  geom_histogram(col = "white", binwidth = 0.5) +
  facet_grid(~Group) +
  labs(x = "\nHistograms of grades on Exam 3", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=6)); hist_grades_apr


### ----------------------------------------------------
### Descriptives for the development of lexical richness
### ----------------------------------------------------

## Summary statistics
tapply(lca_data$ld_oct, lca_data$Group, length) # n

dplyr::select(lca_data, Group, ld_oct, ld_feb, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("mean")  

dplyr::select(lca_data, Group, ld_oct, ld_feb, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("sd")


## Visualisation

# Lexical density
ggplot(ld_melted, aes(x = Exam, y = LD, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Density\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

# Lexical sophistication
ggplot(ls_melted, aes(x = Exam, y = LS, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Sophistication\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

# Lexical variation
ggplot(lv_melted, aes(x = Exam, y = LV, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical variation\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))
#NB: LV can be divided by 20 to obtain TTR. This is not the original measure.


### --------------------------------------------------------------
### Investigate how the lexical richness variables are distributed
### --------------------------------------------------------------

# Don't use German students in Dutch track
three_gr <- lca_data[lca_data$Group != "German_in_Dutch",]

## Lexical density
ld_oct <- ggplot(data = three_gr, aes(three_gr$ld_oct)) +
  geom_histogram(col = "white", fill = "steelblue3") +
  facet_grid(~Group) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_feb <- ggplot(data = three_gr, aes(three_gr$ld_feb)) +
  geom_histogram(col = "white", fill = "orange") +
  facet_grid(~Group) +
  labs(x = "\nFebruary", y = "Count\n") +
  ggtitle("Lexical density\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_apr <- ggplot(data = three_gr, aes(three_gr$ld_apr)) +
  geom_histogram(col = "white", fill = "mediumpurple4") +
  facet_grid(~Group) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ld_oct, ld_feb, ld_apr, nrow=1, ncol=3)

## Lexical sophistication
ls2_oct <- ggplot(data = three_gr, aes(three_gr$ls2_oct)) +
  geom_histogram(col = "white", fill = "steelblue3") +
  facet_grid(~Group) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_feb <- ggplot(data = three_gr, aes(three_gr$ls2_feb)) +
  geom_histogram(col = "white", fill = "orange") +
  facet_grid(~Group) +
  labs(x = "\nFebruary", y = "Count\n") +
  ggtitle("Lexical sophistication\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_apr <- ggplot(data = three_gr, aes(three_gr$ls2_apr)) +
  geom_histogram(col = "white", fill = "mediumpurple4") +
  facet_grid(~Group) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ls2_oct, ls2_feb, ls2_apr, nrow=1, ncol=3)


### ----------------------
### Inferential statistics
### ----------------------

# --------------------------------------------------------------------
# Is Dutch students' lexical richness higher in Dutch than in English?
# --------------------------------------------------------------------

# Select Dutch students only
dutch_lex <- lca_data[lca_data$Nationality == "Dutch",]

# Average lexical richness over the three exams
dutch_lex$LD <- rowMeans(cbind(dutch_lex$ld_oct, dutch_lex$ld_feb, dutch_lex$ld_apr))
dutch_lex$LS <- rowMeans(cbind(dutch_lex$ls2_oct, dutch_lex$ls2_feb, dutch_lex$ls2_apr))
dutch_lex$LV <- rowMeans(cbind(dutch_lex$ndwesz_oct, dutch_lex$ndwesz_feb, dutch_lex$ndwesz_apr))

# Descriptives
tapply(dutch_lex$LD, dutch_lex$Track, stat.desc)
tapply(dutch_lex$LS, dutch_lex$Track, stat.desc)
tapply(dutch_lex$LV, dutch_lex$Track, stat.desc)

## Histograms
ld_hist <- ggplot(data = dutch_lex, aes(dutch_lex$LD)) +
  geom_histogram(col = "white", fill = "steelblue3") +
  facet_grid(~Group) +
  labs(x = "\nLD", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)); ld_hist

ls_hist <- ggplot(data = dutch_lex, aes(dutch_lex$LS)) +
  geom_histogram(col = "white", fill = "orange") +
  facet_grid(~Group) +
  labs(x = "\nLS", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)); ls_hist

lv_hist <- ggplot(data = dutch_lex, aes(dutch_lex$LV)) +
  geom_histogram(col = "white", fill = "mediumpurple4") +
  facet_grid(~Group) +
  labs(x = "\nLV", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)); lv_hist

# To plot the three graphs in one picture
grid.arrange(ld_hist, ls_hist, lv_hist, nrow=1, ncol=3)

## Check assumptions

# Homogeneity of covariance (for MANOVA)
by(dutch_lex[, 52:54], dutch_lex$Track, cov)

# Multivariate normality (for MANOVA)
library(RVAideMemoire)
dutch_wide <- dutch_lex[dutch_lex$Track=="Dutch", 52:54]
dutch_wide <- as.matrix(dutch_wide)
mshapiro.test(dutch_wide)

# Is lexical richness normally distributed in each group?
tapply(dutch_lex$LD, dutch_lex$Track, shapiro.test) # Yes
tapply(dutch_lex$LS, dutch_lex$Track, shapiro.test) # No
tapply(dutch_lex$LV, dutch_lex$Track, shapiro.test) # Yes

# Investigate correlations between dependent variables
cor(dutch_lex[, 52:54], method = "pearson") # Highest correlation is .20
cor(dutch_lex[, 52:54], method = "spearman") # Highest correlation is .22

## Comparing two means (NB: no correction for multiple testing)
t.test(dutch_lex$LD[dutch_lex$Track=="Dutch"], dutch_lex$LD[dutch_lex$Track=="English"])
wilcox.test(dutch_lex$LS[dutch_lex$Track=="Dutch"], dutch_lex$LV[dutch_lex$Track=="English"])
t.test(dutch_lex$LV[dutch_lex$Track=="Dutch"], dutch_lex$LV[dutch_lex$Track=="English"])


# --------------------------------------------------------
# How does lexical richness develop during the first year?
# --------------------------------------------------------

# Relevel
lca_long$Group <- factor(lca_long$Group, levels = c("Dutch in Dutch track", "Dutch in English track", "German in Dutch track", "German in English track"))
lca_long$Group <- factor(lca_long$Group, levels = c("German in Dutch track", "German in English track", "Dutch in Dutch track", "Dutch in English track"))
lca_long$Exam <- factor(lca_long$Exam, levels = c("1 (Oct)", "2 (Feb)", "3 (Apr)"))
lca_long$Exam <- factor(lca_long$Exam, levels = c("2 (Feb)", "1 (Oct)", "3 (Apr)"))

### Lexical density

# Question 1
group_model <- lme(LD ~ 1 + Group, random = ~1|SubjectCode, data = lca_long); summary(group_model)
group_model <- lmer(LD ~ 1 + Group + (1|SubjectCode), data = lca_long); summary(group_model)

# Question 2 and 3
exam_group_model <- lme(LD ~ 1 + Exam + Group + Exam:Group, random = ~1|SubjectCode, data = lca_long); summary(exam_group_model)

# Model comparisons
exam_model <- lme(LD ~ 1 + Exam, random = ~1|SubjectCode, data = lca_long, method = "ML")
group_model <- update(exam_model, .~. + Group)
exam_group_model <- update(group_model, .~. + Exam:Group)
anova(exam_model, group_model, exam_group_model)

## Check assumptions (see Winter, 2013)

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals
plot(fitted(exam_group_model), residuals(exam_group_model))
plot(lca_long$LD, residuals(exam_group_model))
abline(h = 0)

# A weak relationship is visible: smaller fitted values tend to have negative residuals,
# larger fitted values seem to have positive residuals
cor.test(fitted(exam_group_model), residuals(exam_group_model))
summary(lm(residuals(exam_group_model) ~ fitted(exam_group_model)))

## Absence of collinearity
# Exam and Group are not correlated, because all groups took the same exams

## Homoskedasticity
# The standard deviations of the residuals should not depend on the x-value
plot(fitted(exam_group_model), residuals(exam_group_model))
abline(h = 0)
# There doesn't seem to be heteroscedasticity - higher fitted values don't have smaller/larger residuals

## Normality of residuals
hist(residuals(exam_group_model)) 
qqnorm(residuals(exam_group_model))
# Almost perfectly normal

## Absence of influential data points

# Calculate Cook's distance and visualise outcomes
lca_data$Cook <- cooks.distance.estex(influence(eg_model, group = 'SubjectCode'))
plot(lca_data$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)

## Independence
# Is taken care of by the random intercepts at the subject level


### Lexical sophistication
group_model <- lme(LS ~ 1 + Group, random = ~1|SubjectCode, data = lca_long)
summary(group_model)

exam_group_model <- lme(LS ~ 1 + Exam + Group + Exam:Group, random = ~1|SubjectCode, data = lca_long)
summary(exam_group_model)

### Lexical variation
group_model <- lme(LV ~ 1 + Group, random = ~1|SubjectCode, data = lca_long)
summary(group_model)

exam_group_model <- lme(LV ~ 1 + Exam + Group + Exam:Group, random = ~1|SubjectCode, data = lca_long)
summary(exam_group_model)


### KLAD

## Exploration of contrasts, following Andy Field

# Set contrasts
DD_vs_DE <- c(1, -1, 0, 0)
GD_vs_GE <- c(0, 0, 1, -1)
contrasts(lca_long$Group) <- cbind(DD_vs_DE, GD_vs_GE)

oct_feb <- c(1, -1, 0)
oct_apr <- c(1, 0, -1)
feb_apr <- c(0, 1, -1)
contrasts(lca_long$Exam) <- cbind(oct_feb, feb_apr, oct_apr)

# Models
baseline_model <- lme(LD ~ 1, random = ~1|SubjectCode, data = lca_long, method = "ML")
# Use ML rather than REML to be able to do model comparisons with different fixed effects
summary(baseline_model)

exam_model <- update(baseline_model, .~. + Exam)
summary(exam_model)

group_model <- update(exam_model, .~. + Group)
summary(group_model)

exam_group_model <- update(group_model, .~. + Exam:Group)
summary(exam_group_model)

anova(baseline_model, exam_model, group_model, exam_group_model)

library(multcomp)
exams_pairwise <- glht(exam_group_model, linfct = mcp(Exam = "Tukey"))
summary(exams_pairwise)
confint(exams_pairwise)

groups_pairwise <- glht(exam_group_model, linfct = mcp(Group = "Tukey"))
summary(groups_pairwise)
confint(groups_pairwise)

## Other

# Define outcome variable
outcome <- cbind(long_data$LD, long_data$LS, long_data$LV)

# Run MANOVA
manova <- manova(outcome ~ Group + Month + Group * Month, data = long_data)
summary(manova)
summary.lm(manova)
summary.aov(manova)
