library("tidyverse")  # for plotting and data manipulation
library("ggrepel")    # for plotting text labels that don't overlap
library("ggridges")   # for rows of density plots 
library("brms")       # for modelling
library("HDInterval") # for HDIs
library("car")
library("rstan")
rstan::rstan_options(autowrite=TRUE)
# most modern computers have at least cores, so this should speed things up
options(mc.cores=2)
options(contrasts = c("contr.Sum","contr.Poly"))
# my preferred ggplot theme
theme_set(theme_light())

library('lme4')

# Clear workspace
rm(list=ls())

# Set working directory to the current file location
# Can be done through 'Session' tab in RStudio 

# Read in data (choose between the next two files)
subject_info <- read.csv("../data/study_success.txt", header=TRUE, sep="\t", fileEncoding="UTF-8-BOM")

# Exclude students who received exemptions for one or more courses
subject_info <- subject_info[subject_info$Exemption!=1,]

# Exclude students who took courses outside of the Psychology programme
subject_info <- subject_info[subject_info$CoursesOutsideProgramme==0,]

# Data frame without drop-outs
no_dropout <- subject_info[subject_info$DropOut!="DuringYear1",]

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
                 data=subject_info,
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
