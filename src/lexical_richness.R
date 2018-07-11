# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(fBasics); library(scales); library(MASS); library(pastecs); library(lme4); 
library(boot); library(nlme); library(emmeans)

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

# Add grades
lca_long$Grade <- ifelse(lca_long$Exam == "1 (Oct)", lca_data$grade_oct[match(lca_long$SubjectCode, lca_data$SubjectCode)], 
                  ifelse(lca_long$Exam == "2 (Feb)", lca_data$grade_feb[match(lca_long$SubjectCode, lca_data$SubjectCode)],
                  ifelse(lca_long$Exam == "3 (Apr)", lca_data$grade_apr[match(lca_long$SubjectCode, lca_data$SubjectCode)], NA)))
  
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
ld <- ggplot(ld_melted, aes(x = Exam, y = LD, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Density\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)); ld

# Lexical sophistication
ls <- ggplot(ls_melted, aes(x = Exam, y = LS, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Sophistication\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)); ls

# Lexical variation
lv <- ggplot(lv_melted, aes(x = Exam, y = LV, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical variation\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1)); lv
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

# Plot the three graphs in one picture
grid.arrange(ls2_oct, ls2_feb, ls2_apr, nrow=1, ncol=3)


### --------------------------------------------------------
### How does lexical richness develop during the first year?
### --------------------------------------------------------

### Lexical density
# (These same values can also be obtained by relevelling the model)

# Model comparisons
exam_LD <- lmer(LD ~ 1 + Exam + (1|SubjectCode), data = lca_long, REML = FALSE)
grade_LD <- update(exam_LD, .~. + Grade); summary(grade_LD)
anova(exam_LD, grade_LD)

exam_group_LD <- update(exam_LD, .~. + Group); summary(exam_group_LD)
exam_group_int_LD <- update(exam_group_LD, .~. + Exam:Group); summary(exam_group_int_LD)
anova(exam_LD, exam_group_LD, exam_group_int_LD)

## Multiple comparisons

# Research question 1: Compare overall LD scores
group.emm_LD <- emmeans(exam_group_int_LD, ~ Group); group.emm_LD
pairs(group.emm_LD, adjust = "none")
confint(pairs(group.emm_LD, adjust = "none"))

# Research questions 2 and 3: Compare development of LD scores
exam_group.emm_LD <- emmeans(exam_group_int_LD, ~ Exam*Group); exam_group.emm_LD
pairs(exam_group.emm_LD, simple = c("Group", "Exam"), adjust = "none", interaction = TRUE)
pairs(exam_group.emm_LD, by = "Exam", adjust = "none")

## Check assumptions (see Winter, 2013)

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals
plot(fitted(exam_group_int_LD), residuals(exam_group_int_LD))
abline(h = 0)

# A weak relationship is visible: smaller fitted values tend to have negative residuals,
# larger fitted values seem to have positive residuals
cor.test(fitted(exam_group_int_LD), residuals(exam_group_int_LD))
summary(lm(residuals(exam_group_int_LD) ~ fitted(exam_group_int_LD)))

## Absence of collinearity
# Exam and Group are not correlated, because all groups took the same exams

## Homoskedasticity
# The standard deviations of the residuals should not depend on the x-value
plot(fitted(exam_group_int_LD), residuals(exam_group_int_LD))
abline(h = 0)
# There doesn't seem to be heteroscedasticity - higher fitted values don't have smaller/larger residuals

## Normality of residuals
hist(residuals(exam_group_int_LD)) 
qqnorm(residuals(exam_group_int_LD))
# Almost perfectly normal

## Absence of influential data points

# Calculate Cook's distance and visualise outcomes
lca_data$Cook <- cooks.distance.estex(influence(exam_group_int_LD, group = 'SubjectCode'))
plot(lca_data$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)

## Independence
# Is taken care of by the random intercepts at the subject level


### Lexical sophistication

# Model comparisons
exam_LS <- lmer(LS ~ 1 + Exam + (1|SubjectCode), data = lca_long, REML = FALSE)
grade_LS <- update(exam_LS, .~. + Grade); summary(grade_LS)
anova(exam_LS, grade_LS)

exam_group_LS <- update(grade_LS, .~. + Group); summary(exam_group_LS)
exam_group_int_LS <- update(exam_group_LS, .~. + Exam:Group); summary(exam_group_int_LS)
anova(grade_LS, exam_group_LS, exam_group_int_LS)

## Multiple comparisons

# Research question 1: Compare overall LS scores
group.emm_LS <- emmeans(exam_group_int_LS, ~ Group); group.emm_LS
pairs(group.emm_LS, adjust = "none")

# Research questions 2 and 3: Compare development of LS scores
exam_group.emm_LS <- emmeans(exam_group_int_LS, ~ Exam*Group); exam_group.emm_LS
pairs(exam_group.emm_LS, simple = c("Group", "Exam"), adjust = "none", interaction = TRUE)
pairs(exam_group.emm_LS, by = "Exam", adjust = "none")

## Check assumptions (see Winter, 2013)

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals
plot(fitted(exam_group_int_LS), residuals(exam_group_int_LS))
abline(h = 0)
# No obvious relationship between the fitted and residual values is visible,
# although apparently there are few fitted values around 0.14.

## Homoskedasticity
# There doesn't seem to be heteroscedasticity - higher fitted values don't have smaller/larger residuals

## Normality of residuals
hist(residuals(exam_group_int_LS)) 
qqnorm(residuals(exam_group_int_LS))
# Almost perfectly normal

## Absence of influential data points

# Calculate Cook's distance and visualise outcomes
lca_data$Cook <- cooks.distance.estex(influence(exam_group_int_LS, group = 'SubjectCode'))
plot(lca_data$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)


### Lexical variation

# Model comparisons
exam_LV <- lmer(LV ~ 1 + Exam + (1|SubjectCode), data = lca_long, REML = FALSE)
grade_LV <- update(exam_LV, .~. + Grade); summary(grade_LV)
anova(exam_LV, grade_LV)

exam_group_LV <- update(exam_LV, .~. + Group); summary(exam_group_LV)
exam_group_int_LV <- update(exam_group_LV, .~. + Exam:Group); summary(exam_group_int_LV)
anova(exam_LV, exam_group_LV, exam_group_int_LV)

## Multiple comparisons

# Research question 1: Compare overall LV scores
group.emm_LV <- emmeans(exam_group_int_LV, ~ Group); group.emm_LV
pairs(group.emm_LV, adjust = "none")

# Research questions 2 and 3: Compare development of LV scores
exam_group.emm_LV <- emmeans(exam_group_int_LV, ~ Exam*Group); exam_group.emm_LV
pairs(exam_group.emm_LV, simple = c("Group", "Exam"), adjust = "none", interaction = TRUE)
pairs(exam_group.emm_LV, by = "Exam", adjust = "none")

## Check assumptions (see Winter, 2013)

## Is there a linear relationship between the dependent and independent variables?
# The plot should not show any obvious pattern in the residuals
plot(fitted(exam_group_int_LV), residuals(exam_group_int_LV))
abline(h = 0)
# No obvious relationship between the fitted and residual values is visible,

## Homoskedasticity
# There doesn't seem to be heteroscedasticity - higher fitted values don't have smaller/larger residuals

## Normality of residuals
hist(residuals(exam_group_int_LV)) 
qqnorm(residuals(exam_group_int_LV))
# Seems normal

## Absence of influential data points

# Calculate Cook's distance and visualise outcomes
lca_data$Cook <- cooks.distance.estex(influence(exam_group_int_LV, group = 'SubjectCode'))
plot(lca_data$Cook, ylab = "Cook's distance")
# Different guidelines. Either, Cook's distance should not be >1 or >0.85 (assumption met)
# Or, it shouldn't be >4/n (assumption not met, but no real outliers)


### ------------
### LexTALE data
### ------------

lex <- read.delim("../data/lextale.txt", header = TRUE, sep = "\t")

# Different in English skills?
tapply(lex$SchoolEnglish, lex$Track, stat.desc)
tapply(lex$SchoolEnglish, lex$Track, shapiro.test)
wilcox.test(lex$SchoolEnglish ~ lex$Track)

# Subset matched data
lex_matched <- lex[lex$Include == "Yes",]

# Check that the English school grade is the same for both groups
tapply(lex_matched$SchoolEnglish, lex_matched$Track, stat.desc)

# Different in LexTALE?
tapply(lex_matched$LexTALE, lex_matched$Track, stat.desc)
tapply(lex_matched$LexTALE, lex_matched$Track, shapiro.test)

hist(lex_matched$LexTALE[lex_matched$Track=="Dutch"])
hist(lex_matched$LexTALE[lex_matched$Track=="English"])

wilcox.test(lex_matched$LexTALE ~ lex_matched$Track)
t.test(lex_matched$LexTALE ~ lex_matched$Track)
