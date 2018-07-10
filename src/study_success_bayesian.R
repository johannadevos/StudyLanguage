library("tidyverse")  # for plotting and data manipulation
library("ggrepel")    # for plotting text labels that don't overlap
library("ggridges")   # for rows of density plots 
library("brms")       # for modelling
library("HDInterval") # for HDIs
library("car")
rstan::rstan_options(autowrite=TRUE)
# most modern computers have at least cores, so this should speed things up
options(mc.cores=2)
options(contrasts = c("contr.Sum","contr.Poly"))
# my preferred ggplot theme
theme_set(theme_light())

library('lme4')

# https://www.tidyverse.org/articles/2017/12/workflow-vs-script/

# variable: Track, levels: Dutch, English
# variable: Nationality, levels: Dutch, German
# four-level variable called Group (levels: Dutch in Dutch track, Dutch in English track, German in Dutch track, German in English track). 

# 1) "DropOutBinary", a 0/1 measure that indicates whether or not someone dropped out in the first year
# 2) "EC_Obtained", a continuous measure that indicates how many European Credits someone obtained in the first year (range: 0-60, because one year is 60 credits)
# 3) "Mean_Grade", a continuous measure with range 0-10
# 4) "Weighted_Grade", a continuous measure that is the product of the grades that someone obtained and the number of courses they participated in

# The first year of Psychology at Radboud University consists of 13 courses. Thus, variables 2-4 are repeated measures based on 13 observations. Therefore, I'm using mixed-effects models to investigate these measures. As said, the predictor called Group is the most important. I'm also exploring further predictors, namely Gender, LD, LS, and LV. The latter three are measures of lexical proficiency in the study language. These three measures I only have for a subset of the participants.

# You should run lines 1-31 of study_success.R to read and preprocess the data. Then run lines 365-391 to get the data in long format, and lines 398-451 for the mixed-effects models. This does not include DropOutBinary but I'm still working on that based on the e-mail you sent me last week.

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data (choose between the next two files)
vos <- read.table("../data/study_success.txt", header=TRUE, fileEncoding = "UTF-8-BOM") %>% 
    as_tibble() %>%
    # Exclude students who received exemptions for one or more courses
    subset(Exemption !=1) %>%
    # Exclude students who took courses outside of the Psychology programme
    subset(CoursesOutsideProgramme == 0) %>%
    gather(key="Course",value="EC_Obtained", Course1_EC_Obtained:Course13_EC_Obtained) %>%
    gather(key="Course",value="EC_Worth", Course1_EC_Worth:Course13_EC_Worth) %>%
    gather(key="Course",value="Grade", Course1_Grade:Course13_Grade) %>%
    gather(key="Course",value="Weighted_Grade", Course1_Weighted:Course13_Weighted) %>%
    mutate(Course = str_replace(Course,"Course([1-9])+_Weighted","\\1")) %>% 
    mutate(Course = factor(Course),
           SubjectCode = factor(SubjectCode))

vos[,c("Course","EC_Obtained", "EC_Worth", "Grade", "Weighted_Grade")]

vos.agg <- vos %>% 
    select(SubjectCode:EC_Taken) %>% 
    distinct() 

vos.binom <- brm(DropOutBinary ~ 1 + Track * Nationality,
                 family=bernoulli(),
                 data=vos.agg,
                 prior=set_prior("normal(0,2)", class="b"),
                 algorithm="sampling",
                 iter=2e3,
                 chains=2,
                 save_all_pars=TRUE,
                 sample_prior=FALSE) 
plot(vos.binom)

vos.binom <- update(vos.binom, iter=15e3,thin=5)
plot(vos.binom)
vos.binom
pp_check(vos.binom, nsamples=50) + ggtitle("Posterior Predictive Check")
pp_check(vos.binom,type="hist") + ggtitle("Posterior Predictive Check")

plot(marginal_effects(vos.binom,method="fitted"))

vos.binom.gender <- update(vos.binom, . ~ . * Gender, newdata=vos.agg)

plot(vos.binom.gender)

vos.binom.gender

plot(marginal_effects(vos.binom.gender,method="fitted"))

bayes_factor(vos.binom.gender, vos.binom)
pp_check(vos.binom, nsamples=50) + ggtitle("Posterior Predictive Check")
