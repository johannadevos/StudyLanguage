# Set working directory
setwd("C:/Users/johan/Documents/GitHub/StudyLanguage/")

# Read in data
subject_info <- read.csv("data/subject_info.txt", header=TRUE, sep="\t")
lca_data <- read.csv("data/r_data.txt", header=TRUE, sep=",")


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



