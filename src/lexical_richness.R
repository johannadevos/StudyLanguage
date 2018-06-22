# Import libraries
library(ggplot2); library(plyr); library(dplyr); library(reshape2); library(Hmisc); library(gridExtra)
library(car); library(fBasics); library(scales); library(MASS); library(pastecs)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data
lca_data <- read.csv("../data/r_data.txt", header=TRUE, sep=",")

# Rename columns and colume values
colnames(lca_data)[colnames(lca_data)=="Natio1"] <- "Nationality"
colnames(lca_data)[colnames(lca_data)=="TrackNatio1"] <- "Group"
lca_data$Nationality <- revalue(lca_data$Nationality, c("NL" = "Dutch", "DU" = "German"))
lca_data$Track <- revalue(lca_data$Track, c("NL" = "Dutch", "EN" = "English"))
lca_data$Group <- revalue(lca_data$Group, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))

# Relevel (for better visualisation)
lca_data$Track <- factor(lca_data$Track, levels = c("Dutch", "English"))
lca_data$Nationality <- factor(lca_data$Nationality, levels = c("Dutch", "German"))
lca_data$Group <- factor(lca_data$Group, levels = c("Dutch_in_Dutch", "Dutch_in_English", "German_in_Dutch", "German_in_English"))


### -----------------------------------------
### Exam descriptives: Text length and grades
### -----------------------------------------

# Read in grades
grades <- read.csv("../data/grades.txt", header=TRUE, sep=",")

# Merge with LCA file
lca_data <- merge(lca_data, grades, by=c("SubjectCode"))

# Text length descriptives
tapply(lca_data$wordtokens_oct, lca_data$Group, stat.desc); stat.desc(lca_data$wordtokens_oct)
tapply(lca_data$wordtokens_feb, lca_data$Group, stat.desc); stat.desc(lca_data$wordtokens_feb)
tapply(lca_data$wordtokens_apr, lca_data$Group, stat.desc); stat.desc(lca_data$wordtokens_apr)

# Grade descriptives
tapply(lca_data$grade_oct, lca_data$Group, stat.desc); stat.desc(lca_data$grade_oct)
tapply(lca_data$grade_feb, lca_data$Group, stat.desc); stat.desc(lca_data$grade_feb)
tapply(lca_data$grade_apr, lca_data$Group, stat.desc); stat.desc(lca_data$grade_apr)

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


### --------------------------------------------------------------
### How do L1 and L2 lexical richness develop during the 1st year?
### --------------------------------------------------------------

### Descriptives
tapply(lca_data$ld_oct, lca_data$Group, length) # n

select(lca_data, Group, ld_oct, ld_feb, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("mean")  

select(lca_data, Group, ld_oct, ld_feb, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("sd")


### Visualisation

## Lexical density

# Reshape data
ld_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ld_oct", "ld_feb", "ld_apr"), value.name = "LD")
colnames(ld_melted)[colnames(ld_melted)=="variable"] <- "Month"
ld_melted$Month <- revalue(ld_melted$Month, c("ld_oct"="1 (Oct)", "ld_feb"="2 (Feb)", "ld_apr" = "3 (Apr)"))

# Visualise
ggplot(ld_melted, aes(x = Month, y = LD, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Density\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

## Lexical sophistication

# Reshape data
ls2_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ls2_oct", "ls2_feb", "ls2_apr"), value.name = "LS2")
colnames(ls2_melted)[colnames(ls2_melted)=="variable"] <- "Month"
ls2_melted$Month <- revalue(ls2_melted$Month, c("ls2_oct"="1 (Oct)", "ls2_feb"="2 (Feb)", "ls2_apr" = "3 (Apr)"))

# Visualise
ggplot(ls2_melted, aes(x = Month, y = LS2, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Sophistication\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

## NDWESZ

# Reshape data
ndwesz_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ndwesz_oct", "ndwesz_feb", "ndwesz_apr"), value.name = "NDWESZ")
colnames(ndwesz_melted)[colnames(ndwesz_melted)=="variable"] <- "Month"
ndwesz_melted$Month <- revalue(ndwesz_melted$Month, c("ndwesz_oct"="1 (Oct)", "ndwesz_feb"="2 (Feb)", "ndwesz_apr" = "3 (Apr)"))

# Visualise
ggplot(ndwesz_melted, aes(x = Month, y = NDWESZ/20, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical variation\n(NDW-ES / 20)\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

#NB: I divided NDWESZ by 20 to obtain TTR. This is not the original measure.

## MSTTR

# Reshape data
msttr_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("msttr_oct", "msttr_feb", "msttr_apr"), value.name = "MSTTR")
colnames(msttr_melted)[colnames(msttr_melted)=="variable"] <- "Month"
msttr_melted$Month <- revalue(msttr_melted$Month, c("msttr_oct"="1 (Oct)", "msttr_feb"="2 (Feb)", "msttr_apr" = "3 (Apr)"))

# Visualise
ggplot(msttr_melted, aes(x = Month, y = MSTTR, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nExam", y = "Lexical Variation\n(MSTTR)\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))


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

# Is their a main effect of track on Lexical richness measures for the Dutch students?

# Select Dutch students only
dutch_lex <- lca_data[lca_data$Nationality == "Dutch",]

# Average lexical richness over the three exams
dutch_lex$ld <- rowMeans(cbind(dutch_lex$ld_oct, dutch_lex$ld_feb, dutch_lex$ld_apr))
dutch_lex$ls2 <- rowMeans(cbind(dutch_lex$ls2_oct, dutch_lex$ls2_feb, dutch_lex$ls2_apr))
dutch_lex$ndwesz <- rowMeans(cbind(dutch_lex$ndwesz_oct, dutch_lex$ndwesz_feb, dutch_lex$ndwesz_apr))

# Descriptives
tapply(dutch_lex$ld, dutch_lex$Track, stat.desc)
tapply(dutch_lex$ls2, dutch_lex$Track, stat.desc)
tapply(dutch_lex$ndwesz, dutch_lex$Track, stat.desc)

## Check assumptions

# Homogeneity of covariance
by(dutch_lex[, cbind(dutch_lex$ld, dutch_lex$ls2)], dutch_lex$Track, cov)
by(dutch_lex[, 49:51], dutch_lex$Track, cov)

# Multivariate normality
dutch_wide <- dutch_lex[dutch_lex$Track=="Dutch", 49:51]
dutch_wide <- as.matrix(dutch_wide)
mshapiro.test(dutch_wide) # "System is computationally singular"
# https://stats.stackexchange.com/questions/76488/error-system-is-computationally-singular-when-running-a-glm
# "This results from linearly dependent columns, i.e. strongly correlated variables."

# Investigate correlations between dependent variables
shapiro.test(dutch_lex$ld) # Not significant
shapiro.test(dutch_lex$ls2) # Not significant
shapiro.test(dutch_lex$ndwesz) # Not significant

cor(dutch_lex[, 49:51], method = "pearson") # Highest correlation is .20




### KLAD

# Is lexical richness normally distributed in each group?
tapply(dutch_lex$ld, dutch_lex$Track, shapiro.test) # Yes
tapply(dutch_lex$ls2, dutch_lex$Track, shapiro.test) # No
tapply(dutch_lex$ndwesz, dutch_lex$Track, shapiro.test) # Yes

# Comparing two means (NB: no correction for multiple testing)
t.test(dutch_lex$ld[dutch_lex$Track=="Dutch"], dutch_lex$ld[dutch_lex$Track=="English"])
wilcox.test(dutch_lex$ls2[dutch_lex$Track=="Dutch"], dutch_lex$ls2[dutch_lex$Track=="English"])
t.test(dutch_lex$ndwesz[dutch_lex$Track=="Dutch"], dutch_lex$ndwesz[dutch_lex$Track=="English"])




# Merge long dataframes
long_data <- merge(ld_melted, ls2_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Month"))
long_data <- merge(long_data, ndwesz_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Month"))
long_data$Month <- factor(long_data$Month, levels = c("October", "February", "April"))

# Exclude Germans in the Dutch track
long_data <- long_data[long_data$Group != "German_in_Dutch",]

# Define outcome variable
outcome <- cbind(long_data$LD, long_data$LS2, long_data$NDWESZ)

# Run MANOVA
manova <- manova(outcome ~ Group + Month + Group * Month, data = long_data)
summary(manova)
summary.lm(manova)
summary.aov(manova)
