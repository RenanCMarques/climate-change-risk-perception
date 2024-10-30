#------------------------------------------------------------------------------#
######################## Regressions and Robustness ############################
#------------------------------------------------------------------------------#

rm(list = ls())

# Setting directory: adjust accordingly

setwd()

#------------------------------------------------------------------------------#
### Loading and installing necessary packages ##################################
#------------------------------------------------------------------------------#

# Installing required packages 

packages <- c('tidyverse', 'readxl', 'stringr', 
              'lmtest', 'sandwich', 'stargazer',
              'haven', 'survey', 'GDAtools',
              'weights','knitr','broom','gtools',
              'huxtable', 'compareGroups', 'lfe',
              'kableExtra', 'interplot', 'gridExtra',
              'miceadds', 'ggforce', 'httr', 'gdata',
              'rmarkdown', 'fixest', 'naniar', 'psych',
              'GPArotation', 'jtools', 'formattable',
              'openxlsx', 'fastDummies', 'mfx', 'margins',
              'corrplot', 'grid', 'gridExtra', 'broom.mixed',
              'erer', 'fixest', 'ggstance', 'lme4', 'lmerTest',
              'nlme', 'psych', 'spatstat', 'marginaleffects', 'texreg',
              'ggstance')

# Checking if is installed (and install if not)

packages.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(packages, packages.check)

#------------------------------------------------------------------------------#
### Loading data base  #########################################################
#------------------------------------------------------------------------------#

Base <- readRDS(file = "data/Base.rds")

#------------------------------------------------------------------------------#
### Renaming Variables  ########################################################
#------------------------------------------------------------------------------#

Base <- Base %>%
  mutate(
    rpd = `Risk Perception - Spatial Dimension`,
    rpt = `Risk Perception - Temporal Dimension`,
    rp = `Risk Perception`,
    rp2 = `Risk Perception - Spatial Dimension (alt)`,
    pi_lf = `Political Ideology (Left-Right)`,
    pi_cs = `Political Ideology (Conservatism-Progressive)`, 
    sk = `Subjective knowledge`,
    ok = `Objective knowledge`,
    hk = `Human-Caused Knowledge`,
    wo = `Worry`,
    ha = `Holistic Affect`,
    nep = `The New Ecological Paradigm (NEP)`,
    ii = `Individualism index`,
    ei = `Egalitarianism index`,
    pe = `Personal Experience (extreme weather events)`,
    pv = `Perceived Vulnerability to extreme events`,
    dn = `Descriptive norm`, 
    pn = `Prescriptive norm`)

#------------------------------------------------------------------------------#
### Creating Dictionary Variables  #############################################
#------------------------------------------------------------------------------#

mydict <- c("pi_lf" = "Political ideology: Left",  
            "pi_cs" = "Political ideology: Progressive",
            "sk" = "Subjective knowledge", 
            "ok" = "Objective knowledge",
            'hk' = 'Human-caused knowledge',
            "wo" = "Worry",
            'ha' = 'Holistic affect',
            "nep" = "The New Ecological Paradigm (NEP)",
            "ii" = "Individualism worldview",
            "ei" = "Egalitarianism worldview",
            "pe" = "Personal experience (extreme weather events)",
            'pv' = 'Perceived vulnerability to extreme events',
            'dn' = 'Descriptive norm', 
            'pn' = 'Prescriptive norm',
            "FemaleYes" = "Female", 
            "Escolaridade_3Highschoolorequivalent" = "Education: High school or equivalent", 
            "Escolaridade_3Undergraduateormore" = "Education: Undergraduate or more",
            "ReligionCatholic" = "Religion: Catholic", 
            "ReligionEvangelicalPentecostalorotherevangelical" = "Religion: Evangelical Pentecostal or other evangelical", 
            "ReligionEvangelicalTraditional" = "Religion: Evangelical Traditional",
            "ReligionOthers/NoRelig." = "Religion: Others/No Relig.",
            "Income1-2minimumwages" = "Income: 1 - 2 minimum wages", 
            "Income2-3minimumwages" = "Income: 2 - 3 minimum wages",
            "Income3-5minimumwages" = "Income: 3 - 5 minimum wages", 
            "Income5-10minimumwages" = "Income: 5 - 10 minimum wages",
            "Income10minimumwagesormore" = "Income: 10 minimum wages or more", 
            "IncomeDonotknow/Prefertonotanswer" = "Income: Do not know/ Prefer to not answer", 
            "age" = "Age (Years)", 
            "Color4" = "Race: Black",
            "(Intercept)" = "Const")

#------------------------------------------------------------------------------#
################ Regressions, Robustness and Tables ############################
#------------------------------------------------------------------------------#

# Setting baseline for socio-demographic variables
Base <- Base %>%
  mutate(Income = as.factor(Income),
         Income = factor(Income, levels = c("0 - 1 minimum wages", "1 - 2 minimum wages",
                                            "2 - 3 minimum wages", "3 - 5 minimum wages",
                                            "5 - 10 minimum wages", "10 minimum wages or more",
                                            "Do not know/ Prefer to not answer")),
         Income = relevel(Income, ref = "0 - 1 minimum wages"),
         Escolaridade_3 = as.factor(Escolaridade_3),
         Escolaridade_3 = relevel(Escolaridade_3, "Elementary (Primary) or less"),
         Religion = as.factor(Religion),
         Religion = relevel(Religion, "Atheist"),
         Female = as.factor(Female),
         Female = relevel(Female, "No"))

#------------------------------------------------------------------------------#
### (I) Risk Perception - Spatial Dimension (OLS) ##############################
#------------------------------------------------------------------------------#

regs <- list()  
for (i in seq(1,7)){ 
  regs[[i]] <- feols(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                       Female + Escolaridade_3 + Religion + Income + age + Color4, 
                     data=Base[Base$Pais==i,], weights = ~ ponde2, vcov = "hetero")
}

regs_total_I <- feols(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                        Female + Escolaridade_3 + Religion + Income + age + Color4, 
                      data = Base, weights = ~ ponde2, vcov = "hetero")

etable(regs_total_I, dict = mydict)

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_I, dict = mydict)

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_I,
       title = "OLS Results - Risk Perception: Spatial Dimension",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/ols_I.tex")

#------------------------------------------------------------------------------#
### (II) Risk Perception - Temporal Dimension (OLS) ############################
#------------------------------------------------------------------------------#

regs <- list()  
for (i in seq(1,7)){ 
  regs[[i]] <- feols(rpt ~  pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                       Female + Escolaridade_3 + Religion + Income + age + Color4, 
                     data=Base[Base$Pais==i,], weights = ~ ponde2, vcov = "hetero")
}

regs_total_II <- feols(rpt ~  pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                         Female + Escolaridade_3 + Religion + Income + age + Color4, 
                       data = Base, weights = ~ ponde2, vcov = "hetero")

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_II, dict = mydict)

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_II,
       title = "OLS Results - Risk Perception: Temporal Dimension",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/ols_II.tex")

#------------------------------------------------------------------------------#
### (III) Risk Perception (OLS) ################################################
#------------------------------------------------------------------------------#

regs <- list()  
for (i in seq(1,7)){ 
  regs[[i]] <- feols(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                       Female + Escolaridade_3 + Religion + Income + age + Color4, 
                     data=Base[Base$Pais==i,], weights = ~ ponde2, vcov = "hetero")
}

regs_total_III <- feols(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                          Female + Escolaridade_3 + Religion + Income + age + Color4, 
                        data = Base, weights = ~ ponde2, vcov = "hetero")

etable(regs_total_III, dict = mydict)

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_III, dict = mydict)

etable(regs[2],regs[1],regs[3],regs[4],regs[5],regs[6],regs[7],regs_total_III,
       title = "OLS Results - Risk Perception",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/ols_III.tex")

#------------------------------------------------------------------------------#
### (I-III) Multilevel Model ###################################################
#------------------------------------------------------------------------------#

# (I) Risk Perception - Spatial Dimension

regs_total_I_ml <- lmer(rpd ~  pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                          Female + Escolaridade_3 + Religion + Income + age + Color4 + (1|Pais), data=Base, weights=ponde2)
summary(regs_total_I_ml)

# (II) Risk Perception - Temporal Dimension

regs_total_II_ml <- lmer(rpt ~  pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                           Female + Escolaridade_3 + Religion + Income + age + Color4 + (1|Pais), data=Base,weights=ponde2)
summary(regs_total_II_ml)

# (III) Risk Perception 

regs_total_III_ml <- lmer(rp ~  pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                            Female + Escolaridade_3 + Religion + Income + age + Color4 + (1|Pais), data=Base,weights=ponde2)
summary(regs_total_III_ml)

# Table Models: I-III

texreg(list(regs_total_I_ml, regs_total_II_ml, regs_total_III_ml),
       custom.coef.names = names_c,
       digits = 3)

#------------------------------------------------------------------------------#
### (I-III) Fixed Effects Models ###############################################
#------------------------------------------------------------------------------#

# (I) Risk Perception - Spatial Dimension

regs_total_I_fe <- feols(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                           Female + Escolaridade_3 + Religion + Income + age + Color4 | Pais, 
                         data = Base, weights = ~ ponde2, vcov = "hetero")

# (II) Risk Perception - Temporal Dimension

regs_total_II_fe <- feols(rpt ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn + 
                            Female + Escolaridade_3 + Religion + Income + age + Color4 | Pais, 
                          data = Base, weights = ~ ponde2, vcov = "hetero")

# (III) Risk Perception 

regs_total_III_fe <- feols(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                             Female + Escolaridade_3 + Religion + Income + age + Color4 | Pais, 
                           data = Base, weights = ~ ponde2, vcov = "hetero")

etable(regs_total_I_fe, regs_total_II_fe, regs_total_III_fe)

etable(regs_total_I_fe, regs_total_II_fe, regs_total_III_fe,
       title = "FE Results",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/fe_I_II_III.tex")

#------------------------------------------------------------------------------#
################ Plotting Results ##############################################
#------------------------------------------------------------------------------#

# Naming variables 

mydict_plot <- c("Political ideology: Left" = "pi_lf",
                 "Political ideology: Progressive" ="pi_cs",
                 "Subjective knowledge" = "sk",
                 "Objective knowledge" = "ok",
                 "Human-caused knowledge" = "hk",
                 "Worry" = "wo",
                 "Holistic affect" = "ha",
                 "Personal experience (extreme weather events)" = "pe",
                 "Perceived vulnerability to extreme events" = "pv",
                 "Individualism worldview" = "ii",
                 "Egalitarianism worldview" =  "ei",
                 "The New Ecological Paradigm (NEP)" = "nep",
                 "Descriptive norm" = "dn",
                 "Prescriptive norm" = "pn",
                 "Female" = "FemaleYes", 
                 "Education: High school or equivalent" = "Escolaridade_3Highschoolorequivalent", 
                 "Education: Undergraduate or more" = "Escolaridade_3Undergraduateormore",
                 "Religion: Catholic" = "ReligionCatholic", 
                 "Religion: Evangelical Pentecostal or other evangelical" = "ReligionEvangelicalPentecostalorotherevangelical", 
                 "Religion: Evangelical Traditional" = "ReligionEvangelicalTraditional",
                 "Religion: Others/No Relig." = "ReligionOthers/NoRelig.",
                 "Income: 1 - 2 minimum wages" = "Income1-2minimumwages", 
                 "Income: 2 - 3 minimum wages" = "Income2-3minimumwages",
                 "Income: 3 - 5 minimum wages" = "Income3-5minimumwages", 
                 "Income: 5 - 10 minimum wages" = "Income5-10minimumwages",
                 "Income: 10 minimum wages or more" = "Income10minimumwagesormore", 
                 "Income: Do not know/ Prefer to not answer" = "IncomeDonotknow/Prefertonotanswer", 
                 "Age (Years)" = "age", 
                 "Race: Black" = "Color4")

names <- c("Political ideology: Left",
           "Political ideology: Progressive",
           "Subjective knowledge",
           "Objective knowledge",
           "Human-caused knowledge",
           "Worry",
           "Holistic affect",
           "Personal experience (extreme weather events)",
           "Perceived vulnerability to extreme events",
           "Individualism worldview",
           "Egalitarianism worldview",
           "The New Ecological Paradigm (NEP)",
           "Descriptive norm",
           "Prescriptive norm",
           "Female", "Education: High school or equivalent", "Education: Undergraduate or more",
           "Religion: Catholic", "Religion: Evangelical Pentecostal or other evangelical", "Religion: Evangelical Traditional",
           "Religion: Others/No Relig.",
           "Income: 1 - 2 minimum wages", "Income: 2 - 3 minimum wages",
           "Income: 3 - 5 minimum wages", "Income: 5 - 10 minimum wages",
           "Income: 10 minimum wages or more", "Income: Do not know/ Prefer to not answer", "Age (Years)", "Race: Black", "0", "1", "2", "3", "4", "5", "6", "7")

names_c <- c("Constant", "Political ideology: Left",
             "Political ideology: Progressive",
             "Subjective knowledge",
             "Objective knowledge",
             "Human-caused knowledge",
             "Worry",
             "Holistic affect",
             "Personal experience (extreme weather events)",
             "Perceived vulnerability to extreme events",
             "Individualism worldview",
             "Egalitarianism worldview",
             "The New Ecological Paradigm (NEP)",
             "Descriptive norm",
             "Prescriptive norm",
             "Female", "Education: High school or equivalent", "Education: Undergraduate or more",
             "Religion: Catholic", "Religion: Evangelical Pentecostal or other evangelical", "Religion: Evangelical Traditional",
             "Religion: Others/No Relig.",
             "Income: 1 - 2 minimum wages", "Income: 2 - 3 minimum wages",
             "Income: 3 - 5 minimum wages", "Income: 5 - 10 minimum wages",
             "Income: 10 minimum wages or more", "Income: Do not know/ Prefer to not answer", "Age (Years)", "Race: Black")

indep <- c("Subjective knowledge" = "sk",
           "Objective knowledge" = "ok",
           "Human-caused knowledge" = "hk",
           "Worry" = "wo",
           "Holistic affect" = "ha",
           "Personal experience (extreme weather events)" = "pe",
           "Perceived vulnerability to extreme events" = "pv",
           "Individualism worldview" = "ii",
           "Egalitarianism worldview" =  "ei",
           "The New Ecological Paradigm (NEP)" = "nep",
           "Descriptive norm" = "dn",
           "Prescriptive norm" = "pn")

indep_sub <- c("Subjective knowledge",
               "Objective knowledge",
               "Human-caused knowledge",
               "Worry",
               "Holistic affect",
               "Personal experience (extreme weather events)",
               "Perceived vulnerability to extreme events",
               "Individualism worldview",
               "Egalitarianism worldview",
               "The New Ecological Paradigm (NEP)",
               "Descriptive norm",
               "Prescriptive norm")

socio_dem <- c("Political ideology: Left" = "pi_lf",
               "Political ideology: Progressive" ="pi_cs",
               "Female" = "FemaleYes", 
               "Education: High school or equivalent" = "Escolaridade_3High school or equivalent", 
               "Education: Undergraduate or more" = "Escolaridade_3Undergraduate or more",
               "Religion: Catholic" = "ReligionCatholic", 
               "Religion: Evangelical Pentecostal or other evangelical" = "ReligionEvangelical Pentecostal or other evangelical", 
               "Religion: Evangelical Traditional" = "ReligionEvangelical Traditional",
               "Religion: Others/No Relig." = "ReligionOthers/No Relig.",
               "Income: 1 - 2 minimum wages" = "Income1 - 2 minimum wages", 
               "Income: 2 - 3 minimum wages" = "Income2 - 3 minimum wages",
               "Income: 3 - 5 minimum wages" = "Income3 - 5 minimum wages", 
               "Income: 5 - 10 minimum wages" = "Income5 - 10 minimum wages",
               "Income: 10 minimum wages or more" = "Income10 minimum wages or more", 
               "Income: Do not know/ Prefer to not answer" = "IncomeDo not know/ Prefer to not answer", 
               "Age (Years)" = "age", 
               "Race: Black" = "Color4")

socio_dem_sub <- c("Political ideology: Left",
                   "Political ideology: Progressive",
                   "Female", 
                   "Education: High school or equivalent", 
                   "Education: Undergraduate or more",
                   "Religion: Catholic", 
                   "Religion: Evangelical Pentecostal or other evangelical", 
                   "Religion: Evangelical Traditional",
                   "Religion: Others/No Relig.",
                   "Income: 1 - 2 minimum wages", 
                   "Income: 2 - 3 minimum wages",
                   "Income: 3 - 5 minimum wages", 
                   "Income: 5 - 10 minimum wages",
                   "Income: 10 minimum wages or more", 
                   "Income: Do not know/ Prefer to not answer", 
                   "Age (Years)", 
                   "Race: Black")

#------------------------------------------------------------------------------#
### Plot: (I) Risk Perception - Spatial Dimension (OLS) ########################
#------------------------------------------------------------------------------#

label_regs_total_I <- tidy(regs_total_I, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(coef_ci = paste0(format(round(estimate, 2), nsmall = 2), " ", "[" , 
                          format(round(conf.low, 2), nsmall = 2), ",",
                          format(round(conf.high, 2), nsmall = 2), "]")) %>%
  mutate(term = names_c)

# Independent Variables
indep_I <- plot_coefs(regs_total_I, scale = TRUE,
                      coefs = indep, robust = TRUE) +
  xlim(c(-0.20, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_I, term %in% indep_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("\n Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Socio-economic Variables
socio_dem_I <- plot_coefs(regs_total_I, scale = TRUE,
                          coefs = socio_dem, robust = TRUE) +
  xlim(c(-0.20, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_I, term %in% socio_dem_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("Political ideology and \n Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Joining both plots
grid.arrange(indep_I, socio_dem_I,
             nrow = 1,
             widths = c(1,1.08)) 

#------------------------------------------------------------------------------#
### Plot: (II) Risk Perception - Temporal Dimension (OLS) ######################
#------------------------------------------------------------------------------#

label_regs_total_II <- tidy(regs_total_II, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(coef_ci = paste0(format(round(estimate, 2), nsmall = 2), " ", "[" , 
                          format(round(conf.low, 2), nsmall = 2), ",",
                          format(round(conf.high, 2), nsmall = 2), "]")) %>%
  mutate(term = names_c)

# Independent Variables
indep_II <- plot_coefs(regs_total_II, scale = TRUE,
                       coefs = indep, robust = TRUE) +
  xlim(c(-0.35, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_II, term %in% indep_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("\n Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Socio-economic Variables
socio_dem_II <- plot_coefs(regs_total_II, scale = TRUE,
                           coefs = socio_dem, robust = TRUE) +
  xlim(c(-0.35, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_II, term %in% socio_dem_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("Political ideology and \n Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Joining both plots
grid.arrange(indep_II, socio_dem_II,
             nrow = 1,
             widths = c(1,1.05)) 

#------------------------------------------------------------------------------#
### Plot: (III) Risk Perception (OLS) ##########################################
#------------------------------------------------------------------------------#

label_regs_total_III <- tidy(regs_total_III, conf.int = TRUE, conf.level = 0.95) %>%
  mutate_if(is.numeric, round, digits = 2) %>%
  mutate(coef_ci = paste0(format(round(estimate, 2), nsmall = 2), " ", "[" , 
                          format(round(conf.low, 2), nsmall = 2), ",",
                          format(round(conf.high, 2), nsmall = 2), "]")) %>%
  mutate(term = names_c)

# Independent Variables
indep_III <- plot_coefs(regs_total_III, scale = TRUE,
                        coefs = indep, robust = TRUE) +
  xlim(c(-0.25, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_III, term %in% indep_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("\n Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Socio-economic Variables
socio_dem_III <- plot_coefs(regs_total_III, scale = TRUE,
                            coefs = socio_dem, robust = TRUE) +
  xlim(c(-0.25, 0.45)) +
  geom_text(inherit.aes = FALSE, 
            data = subset(label_regs_total_III, term %in% socio_dem_sub),
            aes(x = estimate, y = term, label = coef_ci), 
            size = 3.2, 
            vjust = -1) +
  ggtitle("Political ideology and \n Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 11),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Joining both plots
grid.arrange(indep_III, socio_dem_III,
             nrow = 1, 
             widths = c(1,1.05)) 

#------------------------------------------------------------------------------#
### Plotting OLS per Country ###################################################
#------------------------------------------------------------------------------#

demog <- c("pi_lf", "pi_cs", "FemaleYes", "Escolaridade_3High school or equivalent", "Escolaridade_3Undergraduate or more", 
           "ReligionCatholic", "ReligionEvangelical Pentecostal or other evangelical", "ReligionEvangelical Traditional", 
           "ReligionOthers/No Relig.", "Income1 - 2 minimum wages", "Income2 - 3 minimum wages", 
           "Income3 - 5 minimum wages", "Income5 - 10 minimum wages", "Income10 minimum wages or more", 
           "IncomeDo not know/ Prefer to not answer", "age", "Color4")

indepen <- c("sk", "ok", "hk", "wo", "ha", "pe", "pv", "ii", "ei", "nep", "dn", "pn")

countries <- c("Brazil", "Argentina","Chile", "Colombia", "Ecuador", "Mexico", "Peru")

#------------------------------------------------------------------------------#
### Plot (I) : Risk Perception - Spatial Dimension (OLS) #######################
#------------------------------------------------------------------------------#

regI <- feols(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                Female + Escolaridade_3 + Religion + Income + age + Color4, 
              data = Base, weights = ~ ponde2, vcov = "hetero", split = ~ Pais)

names(regI) <- countries

# Saving data frame with estimates and ci of model for each country

# Creating function to add CI to tidy function
tidy_ci <- function(x){
  tidy(x, conf.int = TRUE, conf.level = 0.95)
} 

regI_df <- map_dfr(regI, tidy_ci, .id = "Pais") %>%
  filter(term != "(Intercept)")

# Plotting results for Independent variables
indepen_I <- regI_df %>%             
  mutate(term = factor(term, level=c("pn", "dn", "nep", "ei", "ii", "pv", "pe", "ha", "wo", "hk", "ok", "sk"))) %>%
  filter(term %in% indepen) %>%
  mutate(term = recode(term,
                       "sk" = "Subjective knowledge", 
                       "ok" = "Objective knowledge",
                       'hk' = 'Human-Caused Knowledge',
                       "wo" = "Worry",
                       'ha' = 'Holistic Affect',
                       "nep" = "The New Ecological Paradigm (NEP)",
                       "ii" = "Individualism worldview",
                       "ei" = "Egalitarianism worldview",
                       "pe" = "Personal Experience (extreme weather events)",
                       'pv' = 'Perceived Vulnerability to extreme events',
                       'dn' = 'Descriptive norm', 
                       'pn' = 'Prescriptive norm')) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)

# Plotting results for Socio-demographic variables
sociodem_I <- regI_df %>%
  mutate(term = factor(term, level=c("Color4", "age", "IncomeDo not know/ Prefer to not answer", "Income10 minimum wages or more", 
                                     "Income5 - 10 minimum wages","Income3 - 5 minimum wages", "Income2 - 3 minimum wages", "Income1 - 2 minimum wages", 
                                     "ReligionOthers/No Relig.", "ReligionEvangelical Traditional", "ReligionEvangelical Pentecostal or other evangelical", 
                                     "ReligionCatholic", "Escolaridade_3Undergraduate or more", "Escolaridade_3High school or equivalent", "FemaleYes", "pi_cs", "pi_lf"))) %>%
  filter(term %in% demog) %>%
  mutate(term = recode(term,
                       "pi_lf" = "Political Ideology: Left",  
                       "pi_cs" = "Political Ideology: Progressive",
                       "Color4" = "Race: Black", 
                       "age" = "Age (Years)", 
                       "IncomeDo not know/ Prefer to not answer" = "Income: Do not know / Prefer to not answer", 
                       "Income10 minimum wages or more" = "Income: 10 minimum wages or more", 
                       "Income5 - 10 minimum wages" = "Income: 5 - 10 minimum wages",
                       "Income3 - 5 minimum wages" = "Income: 3 - 5 minimum wages",
                       "Income2 - 3 minimum wages" = "Income: 2 - 3 minimum wages", 
                       "Income1 - 2 minimum wages" = "Income: 1 - 2 minimum wages", 
                       "ReligionOthers/No Relig." = "Religion: Others/No Relig.", 
                       "ReligionEvangelical Traditional" = "Religion: Evangelical Traditional", 
                       "ReligionEvangelical Pentecostal or other evangelical" = "Religion: Evangelical Pentecostal or other", 
                       "ReligionCatholic" = "Religion: Catholic",
                       "Escolaridade_3Undergraduate or more" = "Education: Undergraduate or more", 
                       "Escolaridade_3High school or equivalent" = "Education: High school or equivalent", 
                       "FemaleYes" = "Female")) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Political ideology and Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)

# Joining both plots  #size 9 x 12.5
grid.arrange(indepen_I, sociodem_I,
             ncol = 1)

#------------------------------------------------------------------------------#
### Plot (II) : Risk Perception - Temporal Dimension (OLS) #####################
#------------------------------------------------------------------------------#

regII <- feols(rpt ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 Female + Escolaridade_3 + Religion + Income + age + Color4, 
               data = Base, weights = ~ ponde2, vcov = "hetero", split = ~ Pais)

names(regII) <- countries

# Saving data frame with estimates and ci of model for each country

# Creating function to add CI to tidy function
tidy_ci <- function(x){
  tidy(x, conf.int = TRUE, conf.level = 0.95)
} 

regII_df <- map_dfr(regII, tidy_ci, .id = "Pais") %>%
  filter(term != "(Intercept)")

# Plotting results for Independent variables
indepen_II <- regII_df %>%
  mutate(term = factor(term, level=c("pn", "dn", "nep", "ei", "ii", "pv", "pe", "ha", "wo", "hk", "ok", "sk"))) %>%
  filter(term %in% indepen) %>%
  mutate(term = recode(term,
                       "sk" = "Subjective knowledge", 
                       "ok" = "Objective knowledge",
                       'hk' = 'Human-Caused Knowledge',
                       "wo" = "Worry",
                       'ha' = 'Holistic Affect',
                       "nep" = "The New Ecological Paradigm (NEP)",
                       "ii" = "Individualism worldview",
                       "ei" = "Egalitarianism worldview",
                       "pe" = "Personal Experience (extreme weather events)",
                       'pv' = 'Perceived Vulnerability to extreme events',
                       'dn' = 'Descriptive norm', 
                       'pn' = 'Prescriptive norm')) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)


# Plotting results for Socio-demographic variables
sociodem_II <- regII_df %>%
  mutate(term = factor(term, level=c("Color4", "age", "IncomeDo not know/ Prefer to not answer", "Income10 minimum wages or more", 
                                     "Income5 - 10 minimum wages","Income3 - 5 minimum wages", "Income2 - 3 minimum wages", "Income1 - 2 minimum wages", 
                                     "ReligionOthers/No Relig.", "ReligionEvangelical Traditional", "ReligionEvangelical Pentecostal or other evangelical", 
                                     "ReligionCatholic", "Escolaridade_3Undergraduate or more", "Escolaridade_3High school or equivalent", "FemaleYes", "pi_cs", "pi_lf"))) %>%
  filter(term %in% demog) %>%
  mutate(term = recode(term,
                       "pi_lf" = "Political Ideology: Left",  
                       "pi_cs" = "Political Ideology: Progressive",
                       "Color4" = "Race: Black", 
                       "age" = "Age (Years)", 
                       "IncomeDo not know/ Prefer to not answer" = "    Income: Do not know / Prefer to not answer", 
                       "Income10 minimum wages or more" = "Income: 10 minimum wages or more", 
                       "Income5 - 10 minimum wages" = "Income: 5 - 10 minimum wages",
                       "Income3 - 5 minimum wages" = "Income: 3 - 5 minimum wages",
                       "Income2 - 3 minimum wages" = "Income: 2 - 3 minimum wages", 
                       "Income1 - 2 minimum wages" = "Income: 1 - 2 minimum wages", 
                       "ReligionOthers/No Relig." = "Religion: Others/No Relig.", 
                       "ReligionEvangelical Traditional" = "Religion: Evangelical Traditional", 
                       "ReligionEvangelical Pentecostal or other evangelical" = "Religion: Evangelical Pentecostal or other", 
                       "ReligionCatholic" = "Religion: Catholic",
                       "Escolaridade_3Undergraduate or more" = "Education: Undergraduate or more", 
                       "Escolaridade_3High school or equivalent" = "Education: High school or equivalent", 
                       "FemaleYes" = "Female")) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Political ideology and Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)

# Joining both plots
grid.arrange(indepen_II, sociodem_II,
             ncol = 1)

#------------------------------------------------------------------------------#
### Plot (III) : Risk Perception (OLS) #########################################
#------------------------------------------------------------------------------#

regIII <- feols(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                  Female + Escolaridade_3 + Religion + Income + age + Color4, 
                data = Base, weights = ~ ponde2, vcov = "hetero", split = ~ Pais)

names(regIII) <- countries

# Saving data frame with estimates and ci of model for each country

# Creating function to add CI to tidy function
tidy_ci <- function(x){
  tidy(x, conf.int = TRUE, conf.level = 0.95)
} 

regIII_df <- map_dfr(regIII, tidy_ci, .id = "Pais") %>%
  filter(term != "(Intercept)")

# Plotting results for Independent variables
indepen_III <- regIII_df %>%
  mutate(term = factor(term, level=c("pn", "dn", "nep", "ei", "ii", "pv", "pe", "ha", "wo", "hk", "ok", "sk"))) %>%
  filter(term %in% indepen) %>%
  mutate(term = recode(term,
                       "sk" = "Subjective knowledge", 
                       "ok" = "Objective knowledge",
                       'hk' = 'Human-Caused Knowledge',
                       "wo" = "Worry",
                       'ha' = 'Holistic Affect',
                       "nep" = "The New Ecological Paradigm (NEP)",
                       "ii" = "Individualism worldview",
                       "ei" = "Egalitarianism worldview",
                       "pe" = "Personal Experience (extreme weather events)",
                       'pv' = 'Perceived Vulnerability to extreme events',
                       'dn' = 'Descriptive norm', 
                       'pn' = 'Prescriptive norm')) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Psychological variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)


# Plotting results for Socio-demographic variables
sociodem_III <- regIII_df %>%
  mutate(term = factor(term, level=c("Color4", "age", "IncomeDo not know/ Prefer to not answer", "Income10 minimum wages or more", 
                                     "Income5 - 10 minimum wages","Income3 - 5 minimum wages", "Income2 - 3 minimum wages", "Income1 - 2 minimum wages", 
                                     "ReligionOthers/No Relig.", "ReligionEvangelical Traditional", "ReligionEvangelical Pentecostal or other evangelical", 
                                     "ReligionCatholic","Escolaridade_3Undergraduate or more", "Escolaridade_3High school or equivalent", "FemaleYes", "pi_cs", "pi_lf"))) %>%
  filter(term %in% demog) %>%
  mutate(term = recode(term,
                       "pi_lf" = "Political Ideology: Left",  
                       "pi_cs" = "Political Ideology: Progressive",
                       "Color4" = "Race: Black", 
                       "age" = "Age (Years)", 
                       "IncomeDo not know/ Prefer to not answer" = "    Income: Do not know / Prefer to not answer", 
                       "Income10 minimum wages or more" = "Income: 10 minimum wages or more", 
                       "Income5 - 10 minimum wages" = "Income: 5 - 10 minimum wages",
                       "Income3 - 5 minimum wages" = "Income: 3 - 5 minimum wages",
                       "Income2 - 3 minimum wages" = "Income: 2 - 3 minimum wages", 
                       "Income1 - 2 minimum wages" = "Income: 1 - 2 minimum wages", 
                       "ReligionOthers/No Relig." = "Religion: Others/No Relig.", 
                       "ReligionEvangelical Traditional" = "Religion: Evangelical Traditional", 
                       "ReligionEvangelical Pentecostal or other evangelical" = "Religion: Evangelical Pentecostal or other", 
                       "ReligionCatholic" = "Religion: Catholic",
                       "Escolaridade_3Undergraduate or more" = "Education: Undergraduate or more", 
                       "Escolaridade_3High school or equivalent" = "Education: High school or equivalent", 
                       "FemaleYes" = "Female")) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2, size = 0.25) +
  geom_linerange(aes(x = estimate, 
                     xmin = conf.low,
                     xmax = conf.high),
                 lwd = 0.65) +
  geom_point(aes(x = estimate, 
                 y = term),
             shape = 21,
             size = 2,
             fill = "white") +
  xlab("Estimate") +
  ggtitle("Political ideology and Socio-Demographic variables") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 10),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 9),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_grid(. ~ Pais)

# Joining both plots
grid.arrange(indepen_III, sociodem_III,
             ncol = 1)
