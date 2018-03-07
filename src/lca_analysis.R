# First, set working directory to where this file is stored
# In RStudio, uses Session --> Set Working Directory

# Read in data
aip_a <- read.csv("../data/lca_results/lca_AIP_A_EN_corrected.txt", header=TRUE, sep=",", dec=".", fileEncoding="UTF-16LE")