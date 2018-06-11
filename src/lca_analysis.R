# Import libraries
library(ggplot2); library(dplyr); library(reshape2); library(plyr); library(Hmisc); library(gridExtra); library(fBasics)
library(car)

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data
subject_info <- read.csv("../data/subject_info.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")
lca_data <- read.csv("../data/r_data.txt", header=TRUE, sep=",")

# Rename columns and colume values
colnames(subject_info)[colnames(subject_info)=="Natio1"] <- "Nationality"
colnames(lca_data)[colnames(lca_data)=="Natio1"] <- "Nationality"
colnames(subject_info)[colnames(subject_info)=="TrackNatio1"] <- "Group"
colnames(lca_data)[colnames(lca_data)=="TrackNatio1"] <- "Group"
subject_info$Nationality <- revalue(subject_info$Nationality, c("NL" = "Dutch", "DU" = "German"))
subject_info$Track <- revalue(subject_info$Track, c("NL" = "Dutch", "EN" = "English"))
subject_info$Group <- revalue(subject_info$Group, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))
lca_data$Nationality <- revalue(lca_data$Nationality, c("NL" = "Dutch", "DU" = "German"))
lca_data$Track <- revalue(lca_data$Track, c("NL" = "Dutch", "EN" = "English"))
lca_data$Group <- revalue(lca_data$Group, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))

# Create new dataframes
all_data <- merge(lca_data, subject_info, all.y = TRUE)
no_dropout <- all_data[all_data$DropOut!="DuringYear1",]

# Relevel (for better visualisation)
lca_data$Track <- factor(lca_data$Track, levels = c("Dutch", "English"))
lca_data$Nationality <- factor(lca_data$Nationality, levels = c("Dutch", "German"))
all_data$Group <- factor(all_data$Group, levels = c("Dutch_in_Dutch", "Dutch_in_English", "German_in_Dutch", "German_in_English"))
no_dropout$Group <- factor(no_dropout$Group, levels = c("Dutch_in_Dutch", "Dutch_in_English", "German_in_Dutch", "German_in_English"))

# Recode
str(subject_info)
# All first year results are currently coded as factors
# This is because there are also non-numeric data, such as "ND"
#subject_info$Stat1 <- as.numeric(subject_info$Stat1) # Doesn't work without preprocessing


### How can study success be predicted?
hist(no_dropout$ECTSTotal, breaks = 120)
table(no_dropout$ECTSTotal)

model_ects <- lm(ECTSTotal ~ Gender + Track + Nationality + Track:Nationality, data = all_data)
summary(model_ects)

model_ects2 <- lm(ECTSTotal ~ Gender + Track + Nationality + Track:Nationality + ld_oct + ls2_oct + ndwesz_oct, data = all_data)
summary(model_ects2)


### Study success: descriptive statistics

# All students
tapply(all_data$ECTSTotal, all_data$Group, mean)
tapply(all_data$ECTSTotal, all_data$Group, sd)
descr_all <- tapply(all_data$ECTSTotal, all_data$Group, basicStats)
basicStats(all_data$ECTSTotal)

# No drop-outs
tapply(no_dropout$ECTSTotal, no_dropout$Group, mean)
tapply(no_dropout$ECTSTotal, no_dropout$Group, sd)
descr_sub <- tapply(no_dropout$ECTSTotal, no_dropout$Group, basicStats)
basicStats(no_dropout$ECTSTotal)

# Calculate drop-out rates per group
groups <- levels(all_data$Group)
for (i in groups) {
  print(i)
  print("Percentage of drop-outs during year 1:")
  print(nrow(all_data[all_data$Group==i & all_data$DropOut=="DuringYear1",]) / nrow(all_data[all_data$Group==i,])*100)
  print("Percentage of drop-outs after year 1:")
  print(nrow(all_data[all_data$Group==i & all_data$DropOut=="AfterYear1",]) / nrow(all_data[all_data$Group==i,])*100)
  print("Total percentage of drop-outs:")
  print(nrow(all_data[all_data$Group==i & all_data$DropOut!="No",]) / nrow(all_data[all_data$Group==i,])*100)
}

# Calculate overall drop-out rates
print(nrow(all_data[all_data$DropOut=="DuringYear1",]) / nrow(all_data)*100)
print(nrow(all_data[all_data$DropOut=="AfterYear1",]) / nrow(all_data)*100)
print(nrow(all_data[all_data$DropOut!="No",]) / nrow(all_data)*100)

# Histogram of total number of ECTS obtained
ects_hist <- ggplot(data = no_dropout, aes(ECTSTotal, fill = Group)) +
  geom_histogram(col = "white", binwidth = 5) +
  facet_grid(~Group) +
  labs(x = "\nTotal number of ECTS obtained", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=pretty_breaks(n=5)) +
  scale_y_continuous(breaks=pretty_breaks(n=10))


### Study success: inferential statistics

## Checking assumptions

# Levene's test of homogeneity of variance
leveneTest(no_dropout$ECTSTotal, no_dropout$Group)

# Is ECTSTotal different between the groups?
ects_lm <- lm(ECTSTotal ~ Group, data = no_dropout)
summary(ects_lm)

ects_aov <- aov(ECTSTotal ~ Group, data = no_dropout)
summary(ects_aov)
plot(ects_aov)

# Kruskal-Wallis test
kruskal.test(ECTSTotal ~ Group, data = no_dropout)
no_dropout$Rank <- rank(no_dropout$ECTSTotal)
by(no_dropout$Rank, no_dropout$Group, mean)


### Do the 'better' Dutch students choose the English track?

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


### How do L1 and L2 lexical richness develop during the 1st year?

## Descriptives
tapply(lca_data$ld_oct, lca_data$Group, length) # n

lca_data %>%
  select(Group, ld_oct, ld_jan, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("mean")

lca_data %>%
  select(Group, ld_oct, ld_jan, ld_apr) %>%
  group_by(Group) %>%
  summarise_all("sd")

## Visualisation: Lexical density

# Reshape data
ld_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ld_oct", "ld_jan", "ld_apr"), value.name = "LD")
colnames(ld_melted)[colnames(ld_melted)=="variable"] <- "Month"
ld_melted$Month <- revalue(ld_melted$Month, c("ld_oct"="October", "ld_jan"="January", "ld_apr" = "April"))

# Visualise
ggplot(ld_melted, aes(x = Month, y = LD, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nMonth", y = "Lexical Density\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

## Visualisation: Lexical sophistication

# Reshape data
ls2_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ls2_oct", "ls2_jan", "ls2_apr"), value.name = "LS2")
colnames(ls2_melted)[colnames(ls2_melted)=="variable"] <- "Month"
ls2_melted$Month <- revalue(ls2_melted$Month, c("ls2_oct"="October", "ls2_jan"="January", "ls2_apr" = "April"))

# Visualise
ggplot(ls2_melted, aes(x = Month, y = LS2, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nMonth", y = "Lexical Sophistication\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

## Visualisation: NDWESZ

# Reshape data
ndwesz_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("ndwesz_oct", "ndwesz_jan", "ndwesz_apr"), value.name = "NDWESZ")
colnames(ndwesz_melted)[colnames(ndwesz_melted)=="variable"] <- "Month"
ndwesz_melted$Month <- revalue(ndwesz_melted$Month, c("ndwesz_oct"="October", "ndwesz_jan"="January", "ndwesz_apr" = "April"))

# Visualise
ggplot(ndwesz_melted, aes(x = Month, y = NDWESZ/20, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nMonth", y = "Lexical Variation 1\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

#NB: I divided NDWESZ by 20 to obtain TTR. This is not the original measure.

## Visualisation: MSTTR

# Reshape data
msttr_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "Group"), measure.vars = c("msttr_oct", "msttr_jan", "msttr_apr"), value.name = "MSTTR")
colnames(msttr_melted)[colnames(msttr_melted)=="variable"] <- "Month"
msttr_melted$Month <- revalue(msttr_melted$Month, c("msttr_oct"="October", "msttr_jan"="January", "msttr_apr" = "April"))

# Visualise
ggplot(msttr_melted, aes(x = Month, y = MSTTR, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", linetype = "solid", alpha = 0.75, size = 1, width = 0.5, position = position_dodge(width = 0.05)) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nMonth", y = "Lexical Variation 2\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  guides(linetype=guide_legend(keywidth = 2, keyheight = 1),
         colour=guide_legend(keywidth = 2, keyheight = 1))

## Histograms

# Don't use German students in Dutch track
three_gr <- lca_data[lca_data$Group != "German_in_Dutch",]

# Investigate how the variables are distributed

# Lexical density
ld_oct <- ggplot(data = three_gr, aes(three_gr$ld_oct)) +
  geom_histogram(fill = "steelblue3") +
  facet_grid(~Group) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_jan <- ggplot(data = three_gr, aes(three_gr$ld_jan)) +
  geom_histogram(fill = "orange") +
  facet_grid(~Group) +
  labs(x = "\nJanuary", y = "Count\n") +
  ggtitle("Lexical density\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_apr <- ggplot(data = three_gr, aes(three_gr$ld_apr)) +
  geom_histogram(fill = "mediumpurple4") +
  facet_grid(~Group) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ld_oct, ld_jan, ld_apr, nrow=1, ncol=3)

# Lexical sophistication
ls2_oct <- ggplot(data = three_gr, aes(three_gr$ls2_oct)) +
  geom_histogram(fill = "steelblue3") +
  facet_grid(~Group) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_jan <- ggplot(data = three_gr, aes(three_gr$ls2_jan)) +
  geom_histogram(fill = "orange") +
  facet_grid(~Group) +
  labs(x = "\nJanuary", y = "Count\n") +
  ggtitle("Lexical sophistication\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_apr <- ggplot(data = three_gr, aes(three_gr$ls2_apr)) +
  geom_histogram(fill = "mediumpurple4") +
  facet_grid(~Group) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ls2_oct, ls2_jan, ls2_apr, nrow=1, ncol=3)

## Analyse grades
grades <- read.csv("data/grades.txt", header=TRUE, sep=",")

# Only use the grades of those students whose writing samples are used
good_subjects <- lca_data$SubjectCode

# Filter grades
grades <- grades[grades$SubjectCode %in% good_subjects,] # Probleem: lengte 317 in plaats van 314?


## Statistical analysis

# Merge long dataframes
long_data <- merge(ld_melted, ls2_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Month"))
long_data <- merge(long_data, ndwesz_melted, by=c("SubjectCode", "Track", "Nationality", "Group", "Month"))
long_data$Month <- factor(long_data$Month, levels = c("October", "January", "April"))

# Exclude Germans in the Dutch track
long_data <- long_data[long_data$Group != "German_in_Dutch",]

# Define outcome variable
outcome <- cbind(long_data$LD, long_data$LS2, long_data$NDWESZ)

# Run MANOVA
manova <- manova(outcome ~ Group + Month + Group * Month, data = long_data)
summary(manova)
summary.lm(manova)
summary.aov(manova)
