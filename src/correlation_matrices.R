### Create correlation matrices for Matrix Spectral Decomposition (matSpD)
### See https://neurogenetics.qimrberghofer.edu.au/matSpD/

# Read in data
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")


## Study success - Research question 1: high school grades

# New dataframe with just the school grades
school <- subject_info[c("SchoolMean", "SchoolEnglish")]
school <- na.omit(school)

# Histograms to gauge normality
hist(school$SchoolMean)
hist(school$SchoolEnglish)

# Correlation matrix
cor_school <- cor(school)

# Write correlation matrix to file
write.table(cor_school, "../results/corr_matrices/school.txt", col.names = FALSE, row.names=FALSE)


## Continue preprocessing

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]
subject_info$Exemption <- NULL

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]
subject_info$CoursesOutsideProgramme <- NULL

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]


## Study success - Research question 2 and 3: relationship group / lexical richness and study success

# New dataframe with the dependent variables
success <- no_dropout[c("EC_Obtained", "Mean_Grade", "Weighted_Grade", "DropOutBinary")]
success <- na.omit(success)

# Measures are not normally distributed

# Correlation matrix
cor_success <- cor(success, method = "spearman"); cor_success

# Write correlation matrix to file
write.table(cor_success, "../results/corr_matrices/success.txt", col.names = FALSE, row.names=FALSE)


## Lexical richness

# Read in data
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# New dataframe with the dependent variables
lca <- subject_info[c("LD", "LV", "LS")]
lca <- na.omit(lca)

# Histograms to gauge normality
hist(lca$LD) # Normal
hist(lca$LS) # So-so
hist(lca$LV) # Quite normal

# Correlation matrix
cor_lca <- cor(lca, method = "pearson"); cor_lca
rcorr(as.matrix(lca)) # To also obtain p-values

# Write correlation matrix to file
write.table(cor_lca, "../results/corr_matrices/lca.txt", col.names = FALSE, row.names=FALSE)
