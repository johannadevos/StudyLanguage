# Import libraries
library(ggplot2); library(dplyr); library(reshape2); library(plyr)

# Set working directory
setwd("C:/Users/johan/Documents/GitHub/StudyLanguage/")

# Read in data
subject_info <- read.csv("data/subject_info.txt", header=TRUE, sep="\t")
lca_data <- read.csv("data/r_data.txt", header=TRUE, sep=",")

# Small changes to the dataset
colnames(lca_data)[colnames(lca_data)=="Natio1"] <- "Nationality"
lca_data$Track <- factor(lca_data$Track, levels = c("NL", "EN"))


### Do the 'better' Dutch students choose the English track?

# Select Dutch students only
dutch_data <- subject_info[subject_info$Natio1 == "NL",]

# Descriptives per track
tapply(dutch_data$SchoolMean, dutch_data$Track, length)
tapply(dutch_data$SchoolMean, dutch_data$Track, summary)

tapply(dutch_data$SchoolEnglish, dutch_data$Track, length)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, summary)

# Plot distribution of grades
hist(dutch_data$SchoolMean[dutch_data$Track == "EN"], breaks=12)
hist(dutch_data$SchoolMean[dutch_data$Track == "NL"], breaks=12)

hist(dutch_data$SchoolEnglish[dutch_data$Track == "EN"])
hist(dutch_data$SchoolEnglish[dutch_data$Track == "NL"])

# Are grades normally distributed? --> Not in the Dutch track
tapply(dutch_data$SchoolMean, dutch_data$Track, shapiro.test)
tapply(dutch_data$SchoolEnglish, dutch_data$Track, shapiro.test)

# Use non-parametric testing
wilcox.test(dutch_data$SchoolMean[dutch_data$Track=="EN"], dutch_data$SchoolMean[dutch_data$Track=="NL"])
wilcox.test(dutch_data$SchoolEnglish[dutch_data$Track=="EN"], dutch_data$SchoolEnglish[dutch_data$Track=="NL"])

# Since the p-value for SchoolMean is so close to significance (.07), also do a t-test for further exploration
t.test(dutch_data$SchoolMean[dutch_data$Track=="EN"], dutch_data$SchoolMean[dutch_data$Track=="NL"])


### How do L1 and L2 lexical richness develop during the 1st year?

# Descriptives
tapply(lca_data$ld_oct, lca_data$TrackNatio1, length) # n

lca_data %>%
  select(TrackNatio1, ld_oct, ld_jan, ld_apr) %>%
  group_by(TrackNatio1) %>%
  summarise_all("mean")

lca_data %>%
  select(TrackNatio1, ld_oct, ld_jan, ld_apr) %>%
  group_by(TrackNatio1) %>%
  summarise_all("sd")

# Reshape data
ld_melted <- melt(lca_data, id.vars=c("SubjectCode", "Track", "Nationality"), measure.vars = c("ld_oct", "ld_jan", "ld_apr"), value.name = "LD")
colnames(ld_melted)[colnames(ld_melted)=="variable"] <- "Month"
ld_melted$Month <- revalue(ld_melted$Month, c("ld_oct"="October", "ld_jan"="January", "ld_apr" = "April"))

# Visualise
ggplot(ld_melted, aes(x = Month, y = LD, linetype = Track, colour = Nationality, group = interaction(Track, Nationality))) +
  stat_summary(fun.y = mean, geom = "point", size = 4) + 
  stat_summary(fun.y = mean, geom = "line", size = 2) +
  theme(text = element_text(size = 20), axis.text.y = element_text(size = 18), axis.text.x = element_text(size = 18), strip.text = element_text(size=18)) +
  labs(x = "\nMonth", y = "Lexical Density\n") +
  scale_color_manual(values=c("orange", "steelblue3")) +
  scale_linetype_manual("Track", values=c("NL"=2,"EN"=1)) +
  guides(linetype=guide_legend(keywidth = 4, keyheight = 1),
         colour=guide_legend(keywidth = 4, keyheight = 1))


# Don't use German students in Dutch track
three_gr <- lca_data[lca_data$TrackNatio1 != "DU_in_NL",]

# Investigate how the variables are distributed
ggplot(data = three_gr, aes(three_gr$ld_oct, fill = three_gr$TrackNatio1)) +
  geom_histogram()
