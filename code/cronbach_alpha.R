#------------------------------------------------------------------------------#
################ Cronbach's Alpha: Psychological Variables #####################
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
              'nlme', 'psych', 'spatstat', 'marginaleffects', 'texreg')

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
### Calculating Alpha  #########################################################
#------------------------------------------------------------------------------#

# Descriptive Norm
Base %>% 
  dplyr::select(P18_1, P18_2, P18_3) %>%
  alpha()

# Prescriptive Norm
Base %>% 
  dplyr::select(P18_4, P18_5, P18_6) %>%
  alpha(c(1, 1, 1))

# Risk Perception - Extent Dimension 
Base %>% 
  dplyr::select(P8_1, P8_2, P8_3, P8_4, P8_5, P8_6, P8_7) %>%       
  alpha()


