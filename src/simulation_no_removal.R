# Read in data
lca_data <- read.csv("../data/lexical_richness.txt", header=TRUE, sep="\t")
lca_data$SubjectCode <- as.factor(lca_data$SubjectCode)

# Calculate overall lexical richness scores and average number of word tokens
lca_data$LD <- rowMeans(cbind(lca_data$ld_oct, lca_data$ld_feb, lca_data$ld_apr))
lca_data$LS <- rowMeans(cbind(lca_data$ls2_oct, lca_data$ls2_feb, lca_data$ls2_apr))
lca_data$LV <- rowMeans(cbind(lca_data$ndwesz_oct, lca_data$ndwesz_feb, lca_data$ndwesz_apr))
lca_data$tokens <- rowMeans(cbind(lca_data$wordtokens_oct, lca_data$wordtokens_feb, lca_data$wordtokens_apr))

# On average, the Dutch texts contained 77 words
d_len <- mean(lca_data$tokens[lca_data$Track=="Dutch"])

# How many Dutch writing samples?
d_sam <- length(which(lca_data$Track == "Dutch")) * 3

# On average, how many words were removed per Dutch text?
prop_rem <- 110 / d_sam

# One word per how many words was removed?
one_per <- d_len / prop_rem


## Suppose that one word was removed per text, and that this word was always a content word
## How does this removal impact lexical density scores? (LD)

# Proportion of content words
prop <- mean(lca_data$LD) # or manually:
prop <- 0.510708994708995

# Absolute number of content words
abs <- round(prop * one_per, 0)

# Vector with zeroes and ones, where the ones represent unique words
vec <- c(rep(0, one_per-abs), rep(1, abs))
mean(vec)

# Same, but add one content word
vec2 <- c(rep(0, one_per-abs), rep(1, abs+1))
mean(vec2)

# Check impact on lexical variation score
mean(vec)
mean(vec2)

# Percent change
(mean(vec) - mean(vec2)) / mean(vec2) * 100

## Suppose that one word was removed per text, and that this word was always unique
## How does this removal impact lexical variation scores? (NDW-ESZ)

# Proportion of unique words
prop <- mean(lca_data$LV) / 20 # or manually:
prop <- 17.31343 / 20

# Absolute number of unique words
abs <- round(prop * one_per, 0)

# Vector with zeroes and ones, where the ones represent unique words
vec <- c(rep(0, one_per-abs), rep(1, abs))
mean(vec)

# Same, but add one unique word
vec2 <- c(rep(0, one_per-abs), rep(1, abs+1))
mean(vec2)

# Check impact on lexical variation score
mean(vec) * 20
mean(vec2) * 20

# Percent change
(mean(vec) * 20 - mean(vec2) * 20) / (mean(vec2) * 20) * 100
