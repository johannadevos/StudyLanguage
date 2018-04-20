# Import libraries
library(ggplot2); library(dplyr); library(reshape2); library(plyr); library(Hmisc); library(gridExtra)

# Set working directory
#setwd("C:/Users/johan/Documents/GitHub/StudyLanguage/")
setwd("U:/GitHub/StudyLanguage/")

# Read in data
subject_info <- read.csv("data/subject_info.txt", header=TRUE, sep="\t")
lca_data <- read.csv("data/r_data.txt", header=TRUE, sep=",")

# Rename columns and colume values
colnames(subject_info)[colnames(subject_info)=="Natio1"] <- "Nationality"
colnames(lca_data)[colnames(lca_data)=="Natio1"] <- "Nationality"
subject_info$Nationality <- revalue(subject_info$Nationality, c("NL" = "Dutch", "DU" = "German"))
subject_info$Track <- revalue(subject_info$Track, c("NL" = "Dutch", "EN" = "English"))
subject_info$TrackNatio1 <- revalue(subject_info$TrackNatio1, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))
lca_data$Nationality <- revalue(lca_data$Nationality, c("NL" = "Dutch", "DU" = "German"))
lca_data$Track <- revalue(lca_data$Track, c("NL" = "Dutch", "EN" = "English"))
lca_data$TrackNatio1 <- revalue(lca_data$TrackNatio1, c("DU_in_NL" = "German_in_Dutch", "NL_in_NL" = "Dutch_in_Dutch", "DU_in_EN" = "German_in_English", "NL_in_EN" = "Dutch_in_English"))

# Relevel (for better visualisation)
lca_data$Track <- factor(lca_data$Track, levels = c("Dutch", "English"))
lca_data$Nationality <- factor(lca_data$Nationality, levels = c("Dutch", "German"))

### Do the 'better' Dutch students choose the English track?

# Select Dutch students only
dutch_data <- subject_info[subject_info$Nationality == "Dutch",]

# Descriptives per track
tapply(dutch_data$SchoolMean, dutch_data$Track, length)
tapply(dutch_data$SchoolMean, dutch_data$Track, summary)

tapply(dutch_data$SchoolEnglish, dutch_data$Track, length)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, summary)

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
tapply(lca_data$ld_oct, lca_data$TrackNatio1, length) # n

lca_data %>%
  select(TrackNatio1, ld_oct, ld_jan, ld_apr) %>%
  group_by(TrackNatio1) %>%
  summarise_all("mean")

lca_data %>%
  select(TrackNatio1, ld_oct, ld_jan, ld_apr) %>%
  group_by(TrackNatio1) %>%
  summarise_all("sd")

## Visualisation: Lexical density

# Reshape data
ld_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "TrackNatio1"), measure.vars = c("ld_oct", "ld_jan", "ld_apr"), value.name = "LD")
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
ls2_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "TrackNatio1"), measure.vars = c("ls2_oct", "ls2_jan", "ls2_apr"), value.name = "LS2")
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
ndwesz_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "TrackNatio1"), measure.vars = c("ndwesz_oct", "ndwesz_jan", "ndwesz_apr"), value.name = "NDWESZ")
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
msttr_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality", "TrackNatio1"), measure.vars = c("msttr_oct", "msttr_jan", "msttr_apr"), value.name = "MSTTR")
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
three_gr <- lca_data[lca_data$TrackNatio1 != "German_in_Dutch",]

# Investigate how the variables are distributed

# Lexical density
ld_oct <- ggplot(data = three_gr, aes(three_gr$ld_oct)) +
  geom_histogram(fill = "steelblue3") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_jan <- ggplot(data = three_gr, aes(three_gr$ld_jan)) +
  geom_histogram(fill = "orange") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nJanuary", y = "Count\n") +
  ggtitle("Lexical density\n") +
  theme(plot.title = element_text(hjust = 0.5))

ld_apr <- ggplot(data = three_gr, aes(three_gr$ld_apr)) +
  geom_histogram(fill = "mediumpurple4") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ld_oct, ld_jan, ld_apr, nrow=1, ncol=3)

# Lexical sophistication
ls2_oct <- ggplot(data = three_gr, aes(three_gr$ls2_oct)) +
  geom_histogram(fill = "steelblue3") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nOctober", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_jan <- ggplot(data = three_gr, aes(three_gr$ls2_jan)) +
  geom_histogram(fill = "orange") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nJanuary", y = "Count\n") +
  ggtitle("Lexical sophistication\n") +
  theme(plot.title = element_text(hjust = 0.5))

ls2_apr <- ggplot(data = three_gr, aes(three_gr$ls2_apr)) +
  geom_histogram(fill = "mediumpurple4") +
  facet_grid(~TrackNatio1) +
  labs(x = "\nApril", y = "Count\n") +
  ggtitle("\n") +
  theme(plot.title = element_text(hjust = 0.5))

# To plot the three graphs in one picture
grid.arrange(ls2_oct, ls2_jan, ls2_apr, nrow=1, ncol=3)

## Statistical analysis

# Merge long dataframes
man_data <- merge(ld_melted, ls2_melted, by=c("SubjectCode", "Track", "Nationality", "TrackNatio1", "Month"))
man_data <- merge(man_data, ndwesz_melted, by=c("SubjectCode", "Track", "Nationality", "TrackNatio1", "Month"))
man_data$Month <- factor(man_data$Month, levels = c("October", "January", "April"))

# Exclude Germans in the Dutch track
man_data <- man_data[man_data$TrackNatio1 != "German_in_Dutch",]

# Define outcome variable
outcome <- cbind(man_data$LD, man_data$LS2, man_data$NDWESZ)

# Run MANOVA
manova <- manova(outcome ~ TrackNatio1 + Month, data = man_data)
summary(manova)







