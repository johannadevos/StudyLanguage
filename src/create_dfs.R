### Creating the data files for analysis

# Libraries
library(plyr) # To use revalue for renaming factor levels
library(dplyr) # To move selected columns to the end of the dataframe

# Clear workspace
rm(list=ls())

# Read in data
subject_info <- read.csv("../data/subject_info.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Calculate measures of study success

# Indices
index1_grade <- which(colnames(subject_info)=="Course1_Grade")
index13_grade <- which(colnames(subject_info)=="Course13_Grade")
index1_worth <- which(colnames(subject_info)=="Course1_EC_Worth")
index13_worth <- which(colnames(subject_info)=="Course13_EC_Worth")

# Calculate obtained ECs
obtained_ecs <- (subject_info[, c(index1_grade:index13_grade)] > 5.5) * subject_info[, c(index1_worth:index13_worth)] # Multiply grades by worth
colnames(obtained_ecs) <- gsub('Worth', 'Obtained', colnames(obtained_ecs), fixed=TRUE) # Change column names
obtained_ecs[is.na(obtained_ecs)] <- 0 # Replace NA by 0
subject_info <- cbind(subject_info, obtained_ecs)

# Calculate weighted grades
weighted_grades <- (subject_info[, c(index1_grade:index13_grade)]) * subject_info[, c(index1_worth:index13_worth)] # Multiply grades by worth
colnames(weighted_grades) <- gsub('Grade', 'Weighted', colnames(weighted_grades), fixed=TRUE) # Change column names
weighted_grades[is.na(weighted_grades)] <- 0 # Replace NA by 0
subject_info <- cbind(subject_info, weighted_grades)

# More indices
index1_ec <- which(colnames(subject_info)=="Course1_EC_Obtained")
index13_ec <- which(colnames(subject_info)=="Course13_EC_Obtained")
index1_weighted <- which(colnames(subject_info)=="Course1_Weighted")
index13_weighted <- which(colnames(subject_info)=="Course13_Weighted")

# Calculate pass/fail per course
courses_passed <- (subject_info[, c(index1_ec:index13_ec)] > 0) * 1
colnames(courses_passed) <- gsub('EC_Obtained', 'Passed', colnames(courses_passed), fixed=TRUE) # Change column names
subject_info <- cbind(subject_info, courses_passed)

# Calculate sum of obtained ECs
subject_info$EC_Obtained <- rowSums(subject_info[,c(index1_ec:index13_ec)])

# Calculate sum of weighted grades
subject_info$Weighted_Grade <- rowSums(subject_info[,c(index1_weighted:index13_weighted)])

# Calculate the number of ECs taken
ecs_taken <- (subject_info[, c(index1_grade:index13_grade)] >= 1) * subject_info[, c(index1_worth:index13_worth)]
subject_info$EC_Taken <- rowSums(ecs_taken, na.rm = TRUE)

# Calculate mean grade
subject_info$Mean_Grade <- subject_info$Weighted_Grade / subject_info$EC_Taken

# Read in LCA data
lca_raw <- read.csv("../data/r_data.txt", header=TRUE, sep=",")

# Read in exam grades
grades <- read.csv("../data/grades.txt", header=TRUE, sep=",")

# Merge LCA file with exam grades
lca_grades <- merge(lca_raw, grades, by="SubjectCode")

# Merge with subject info
all_data <- merge(subject_info, lca_grades, all = TRUE)

# Rename columns and colume values
colnames(all_data)[colnames(all_data)=="Natio1"] <- "Nationality"
colnames(all_data)[colnames(all_data)=="TrackNatio1"] <- "Group"
all_data$Nationality <- revalue(all_data$Nationality, c("NL" = "Dutch", "DU" = "German"))
all_data$Track <- revalue(all_data$Track, c("NL" = "Dutch", "EN" = "English"))
all_data$Group <- revalue(all_data$Group, c("DU_in_NL" = "German in Dutch track", "NL_in_NL" = "Dutch in Dutch track", "DU_in_EN" = "German in English track", "NL_in_EN" = "Dutch in English track"))

# Relevel (for better visualisation)
all_data$Track <- factor(all_data$Track, levels = c("Dutch", "English"))
all_data$Nationality <- factor(all_data$Nationality, levels = c("Dutch", "German"))
all_data$Group <- factor(all_data$Group, levels = c("Dutch in Dutch track", "Dutch in English track", "German in Dutch track", "German in English track"))
all_data$DropOut <- factor(all_data$DropOut, levels = c("DuringYear1", "AfterYear1", "No"))

# Lexical richness data

# Delete columns with repeated measures of study success
lca <- all_data[,-c(index1_ec:index13_ec, index1_grade:index13_grade, index1_weighted:index13_weighted, index1_worth:index13_worth)]

# Only keep students for whom LCA measures are available
lca <- lca[!is.na(lca$wordtokens_oct),]

# Study success data

study_success <- all_data

# Average lexical richness over the three exams
study_success$LD <- rowMeans(cbind(study_success$ld_oct, study_success$ld_feb, study_success$ld_apr))
study_success$LS <- rowMeans(cbind(study_success$ls2_oct, study_success$ls2_feb, study_success$ls2_apr))
study_success$LV <- rowMeans(cbind(study_success$ndwesz_oct, study_success$ndwesz_feb, study_success$ndwesz_apr))

# Remove repeated lexical measures
study_success <- study_success [, -grep("_oct", colnames(study_success))]
study_success <- study_success [, -grep("_feb", colnames(study_success))]
study_success <- study_success [, -grep("_apr", colnames(study_success))]

# Move repeated study success measures to the end
# This is done four times because the formerly established indices no longer apply
study_success <- study_success%>%dplyr::select(-cbind(index1_grade:index13_grade),everything())
study_success <- study_success%>%dplyr::select(-cbind(index1_grade:index13_grade),everything())
study_success <- study_success%>%dplyr::select(-cbind(index1_grade:index13_grade),everything())
study_success <- study_success%>%dplyr::select(-cbind(index1_grade:index13_grade),everything())
study_success <- study_success%>%dplyr::select(-cbind(index1_grade:index13_grade),everything())

# Write the new dataframes to text files
write.table(study_success,"../data/study_success.txt",sep="\t",row.names=FALSE)
write.table(lca,"../data/lexical_richness.txt",sep="\t",row.names=FALSE)

# Remove everything except the data needed
rm(list=setdiff(ls(), c("study_success", "lca")))