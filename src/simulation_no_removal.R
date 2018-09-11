# On average, less than one English word was removed from all Dutch texts
# On average, the Dutch texts contained 77 words
# Suppose that one word was removed per text, and that this word was always unique
# How does this removal impact lexical variation scores? (NDW-ESZ)

# Proportion of unique words
prop <- mean(lca_data$LV) # or alternatively:
prop <- 17.31343 / 20

# Absolute number of unique words
abs <- round(prop * 77, 0)

# Vector with zeroes and ones, where the ones represent unique words
vec <- c(rep(0, 77-abs), rep(1, abs))
mean(vec)

# Same, but add one unique word
vec2 <- c(rep(0, 77-abs), rep(1, abs+1))
mean(vec2)

# Check impact on lexical variation score
mean(vec) * 20
mean(vec2) * 20
