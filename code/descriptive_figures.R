#------------------------------------------------------------------------------#
######################## Descriptive - Figures ############################
#------------------------------------------------------------------------------#

rm(list = ls())

# Setting directory: adjust accordingly to 

setwd()

#------------------------------------------------------------------------------#
# Loading and installing necessary packages ##################################
#------------------------------------------------------------------------------#

# Installing required packages 

packages <- c('tidyverse', 'fixest', 'sjPlot', 'modelsummary', 
              'performance', 'car', 'psych', 'REdaS')

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
# Loading data base  #########################################################
#------------------------------------------------------------------------------#

Base <- readRDS(file = "data/Base.rds")

#------------------------------------------------------------------------------#
# Renaming Variables  ########################################################
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
# 1. Dependent Variables  ####################################################
#------------------------------------------------------------------------------#

# Country labels

countries <- c(
  "2" = "Argentina",
  "1" = "Brazil",
  "3" = "Chile",
  "4" = "Colombia",
  "5" = "Ecuador",
  "6" = "Mexico",
  "7" = "Peru")

order <- c(
  "2",
  "1",
  "3",
  "4",
  "5",
  "6",
  "7")

# Risk Perception - Spatial Dimension
Base %>%
  drop_na(rpd) %>%
  mutate(Pais = as.factor(Pais)) %>%
  ggplot(aes(rpd)) +
  geom_histogram() +
  labs(x = "Risk Perception - Spatial Dimension",
       y = "Observations") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 9),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(. ~ factor(Pais, levels = order, labels = countries)) #8.5 x 6

# Risk Perception - Temporal Dimension
Base %>%
  drop_na(rpt) %>%
  mutate(Pais = as.factor(Pais)) %>%
  ggplot(aes(rpt)) +
  geom_bar() +
  labs(x = "Risk Perception - Temporal Dimension",
       y = "Observations") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 9),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(. ~ factor(Pais, levels = order, labels = countries)) 

# Risk Perception Index
Base %>%
  drop_na(rp) %>%
  mutate(Pais = as.factor(Pais)) %>%
  ggplot(aes(rp)) +
  geom_histogram(col="white") +
  labs(x = "Risk Perception Index",
       y = "Observations") +
  theme_bw() +
  theme(axis.text.y = element_text(color="black", size = 9),
        axis.text.x = element_text(color="black"),
        axis.title.y = element_text(size = 11, margin = margin(t = 0, r = 8, b = 0, l = 0)),
        axis.title.x = element_text(size = 11, margin = margin(t = 8, r = 0, b = 0, l = 0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  facet_wrap(. ~ factor(Pais, levels = order, labels = countries)) 

# Correlation Table
dependent <- Base %>%
  dplyr::select(rpd, rpt, rp) %>%
  drop_na()

tab_corr(dependent)

datasummary_correlation(dependent,
                        output = "latex_tabular",
                        method = "pearson")

# Alpha
Base %>%
  dplyr::select(rpd, rpt) %>%
  alpha()

#------------------------------------------------------------------------------#
# 2. Independent Variables  ####################################################
#------------------------------------------------------------------------------#

# Correlation Table of Pyschological variables
independent_psycho <- Base %>%
  dplyr::select(sk, ok, hk, wo, ha, pe, pv, ii, ei, nep, dn, pn) %>%
  drop_na()

tab_corr(independent_psycho) 

datasummary_correlation(independent_psycho,
                        output = "latex_tabular",
                        method = "pearson")

