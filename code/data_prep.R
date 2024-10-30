#------------------------------------------------------------------------------#
############################ Preparing the database ############################
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
### Loading survey data (5388 obs of 160 variables) ############################
#------------------------------------------------------------------------------#

Base <- read_xlsx("data/Base.xlsx", sheet= "Data", 
                  col_types = c(rep("numeric",160)))

#------------------------------------------------------------------------------#
### Wrangling and creating socio-demographic variables #########################
#------------------------------------------------------------------------------#

Base <- Base %>%
  mutate(Escolaridade_3_niveis = coalesce(ESCOLARIDAD_BR, ESCOLARIDAD_AR,ESCOLARIDAD_CL,
                                          ESCOLARIDAD_CO,ESCOLARIDAD_EC,ESCOLARIDAD_MX,ESCOLARIDAD_PE)) %>%
  mutate(Escolaridade_aux = coalesce(F3_BR, F3_AR,F3_CL,F3_CO,F3_EC,F3_MX,F3_PE))

Base <- Base[1:5388,] %>%
  mutate(Female = ifelse(F1==2,"Yes","No")) %>%
  mutate(Country = ifelse(Pais==1,"Brazil",
                          ifelse(Pais==2,"Argentina",
                                 ifelse(Pais==3,"Chile",
                                        ifelse(Pais==4,"Colombia",
                                               ifelse(Pais==5,"Ecuador",
                                                      ifelse(Pais==6,"Mexico",
                                                             ifelse(Pais==7,"Peru","")))))))) %>%
  mutate(Escolaridade_aux = coalesce(F3_BR, F3_AR,F3_CL,F3_CO,F3_EC,F3_MX,F3_PE))%>%
  mutate(Escolaridade= ifelse(Escolaridade_aux == 1,"Elementary (Primary) incomplete or less",
                              ifelse(Escolaridade_aux == 2,"Elementary (Primary) complete ",
                                     ifelse(Escolaridade_aux == 3,"Middle school incomplete",
                                            ifelse(Escolaridade_aux == 4,"Middle school complete ",
                                                   ifelse(Escolaridade_aux == 5,"High school incomplete",
                                                          ifelse(Escolaridade_aux == 6,"High school complete",
                                                                 ifelse(Escolaridade_aux == 7,"Undergraduate incomplete ",
                                                                        ifelse(Escolaridade_aux == 8 | Escolaridade_aux == 9,"Undergraduate complete or Post-graduate",""))))))))) %>%
  mutate(Escolaridade_3_niveis = coalesce(ESCOLARIDAD_BR, ESCOLARIDAD_AR,ESCOLARIDAD_CL,ESCOLARIDAD_CO,ESCOLARIDAD_EC,ESCOLARIDAD_MX,ESCOLARIDAD_PE)) %>%
  mutate(Escolaridade_3 = ifelse(Escolaridade_3_niveis == 1,"Elementary (Primary) or less", 
                                 ifelse(Escolaridade_3_niveis == 2,"High school or equivalent",
                                        ifelse(Escolaridade_3_niveis == 3,"Undergraduate or more","")))) %>%
  mutate(Escolaridade_5_niveis = coalesce(ESCOLARIDAD_BR, ESCOLARIDAD_AR,ESCOLARIDAD_CL,ESCOLARIDAD_CO,ESCOLARIDAD_EC,ESCOLARIDAD_MX,ESCOLARIDAD_PE)) %>%
  mutate(Escolaridade_5 = ifelse(Escolaridade_5_niveis <6,1, 
                                 ifelse(Escolaridade_5_niveis == 6,2,
                                        ifelse(Escolaridade_5_niveis == 7,3,
                                               ifelse(Escolaridade_5_niveis == 8,4,
                                                      ifelse(Escolaridade_5_niveis == 9,5,"")))))) %>%
  mutate(Income_aux = coalesce(P31_BR,P31_AR,P31_CL,P31_CO,P31_EC,P31_MX,P31_PE)) %>%
  mutate(Income = ifelse(Income_aux == 1,"0 - 1 minimum wages",
                         ifelse(Income_aux == 2,"1 - 2 minimum wages",
                                ifelse(Income_aux == 3,"2 - 3 minimum wages",
                                       ifelse(Income_aux == 4,"3 - 5 minimum wages",
                                              ifelse(Income_aux == 5,"5 - 10 minimum wages",
                                                     ifelse(Income_aux == 6 | Income_aux == 7 | Income_aux == 8,"10 minimum wages or more",
                                                            #                                                             ifelse(Income_aux == 7,"BRL 20.901,00 to BRL 52.250,00",
                                                            #                                                                      ifelse(Income_aux == 8,"BRL 52.251,00 or more",
                                                            ifelse(Income_aux %in% c(98,99),"Do not know/ Prefer to not answer","")))))))) %>%
  
  mutate(Color = ifelse(P33 == 1,"White",
                        ifelse(P33 == 2 |P33 == 6,"Black or 'pardo'",
                               ifelse(P33 == 3,"Indigenous person",
                                      ifelse(P33 == 4,"Mestizo",
                                             ifelse(P33 == 7|P33 == 5,"Other","")))))) %>%
  mutate(Color2 = ifelse(P33 == 2 | P33 == 6, 1, 0)) %>% 
  mutate(Color3 = ifelse(P33 == 1, 1, 0)) %>%  
  mutate(Color4 = ifelse(P33 == 2, 1, 0)) %>% 
  mutate(Black = ifelse(P33 == 2 | P33 == 6, 1, 0)) %>% 
  mutate(White = ifelse(P33 == 1, 1, 0)) %>%
  mutate(Others = ifelse(P33 == 3 | P33 == 4 | P33 == 5 | P33 == 7, 1, 0)) %>%
  mutate(Religion = ifelse(P35==1, "Catholic",
                           ifelse(P35==2|P35==3, "Evangelical Traditional",
                                  ifelse(P35==4| P35==5| P35==6 ,"Evangelical Pentecostal or other evangelical",
                                         ifelse(P35==13,"Atheist","Others/No Relig."))))) %>%
  mutate(Religion2 = ifelse(P35==1, "Catholic",
                            ifelse(P35==2 | P35==3 | P35==4 | P35==5| P35==6, "Evangelical",
                                   ifelse(P35==13,"Atheist","Others/No Relig."))))

#------------------------------------------------------------------------------#
### Wrangling and creating independent variables ###############################
#------------------------------------------------------------------------------#

Base <- Base %>%
  # Subjective knowledge
  replace_with_na_at(.vars = c("P4"), condition = ~.x== 98) %>%
  mutate('Subjective knowledge' = ifelse(P4==98,NA,
                                         ifelse(P4==4,1,
                                                ifelse(P4==3,2,
                                                       ifelse(P4==2,3,
                                                              ifelse(P4==1, 4, 0)))))) %>%
  
  # Objective Knowledge
  mutate_at(.vars = c("P16_1", "P16_2", "P16_3", "P16_4", "P16_5", "P16_6"), ~ifelse(.==98 |.==2, 0, .)) %>%
  mutate('Objective knowledge' = ifelse(P16_1==1,1,0) + ifelse(P16_2==1,1,0) + ifelse(P16_3==1,1,0) +
           ifelse(P16_4==2,1,0) + ifelse(P16_5==2,1,0) + ifelse(P16_3==2,1,0)) %>%
  
  # Human-Caused Knowledge
  replace_with_na_at(.vars = c("P5"), condition = ~.x== 98) %>%
  mutate('Human-Caused Knowledge' = ifelse(P5==1,1,0)) %>%
  
  # Scientific consensus
  replace_with_na_at(.vars = c("P6"), condition = ~.x== 98) %>%
  mutate('Scientific consensus' = ifelse(P6==1,1,0)) %>%
    
  # Holistic Affect
  replace_with_na_at(.vars = c("P20"), condition = ~.x== 98) %>%
  mutate('Holistic Affect' = P20) %>%
  
  # Affective Imagery
  mutate('Affective Imagery' = P22) %>%
  
  # Personal Experience (extreme weather events)
  replace_with_na_at(.vars = c("P15"), condition = ~.x== 98) %>%
  mutate('Personal Experience (extreme weather events)' = ifelse(P15==2|P15==3|P15==4,1,0)) %>%
  
  # Perceived Vulnerability to extreme events
  replace_with_na_at(.vars = c("P19A_1","P19A_2","P19A_3","P19A_4","P19A_5","P19A_6","P19B_1","P19B_2","P19B_3","P19B_4","P19B_5","P19B_6"), condition = ~.x== 98) %>%
  # mutate('Perceived Vulnerability to extreme events' = (P19A_1+P19A_2+P19A_3+P19A_4+P19A_5+P19A_6+P19B_1+P19B_2+P19B_3+P19B_4+P19B_5+P19B_6)/12) %>%
  mutate('Perceived Vulnerability to extreme events' = ((5-P19A_1)+(5-P19A_2)+(5-P19A_3)+(5-P19A_4)+(5-P19A_5)+(5-P19A_6)+
                                                          (5-P19B_1)+(5-P19B_2)+(5-P19B_3)+(5-P19B_4)+(5-P19B_5)+(5-P19B_6))/12) %>% #invertendo sinal (qnt maior, mais vulneralvel)
  
  # Individualism worldview
  replace_with_na_at(.vars = c("P23_1","P23_2","P23_3","P23_4","P23_5"), condition = ~.x== 98) %>%
  mutate('Individualism index' = (P23_1+P23_2+P23_3+P23_4+P23_5)/5) %>%
  
  # Egalitarianism worldview
  replace_with_na_at(.vars = c("P23_6", "P23_7", "P23_8", "P23_10"), condition = ~.x== 98) %>%
  mutate('Egalitarianism index' = (P23_6 + P23_7 + P23_8 + P23_10)/4) %>%
  
  # The New Ecological Paradigm (NEP)
  replace_with_na_at(.vars = c("P12_1", "P12_2", "P12_3", "P12_4"), condition = ~.x== 98) %>%
  #  mutate('The New Ecological Paradigm (NEP)' = (P12_1 + (5-P12_2) + P12_3 + P12_4)/4) %>%
  mutate('The New Ecological Paradigm (NEP)' = (P12_1 + P12_3 + P12_4)/3) %>% # excluding P12_2 
  
  # Political Ideology (Left-Right)
  mutate('Political Ideology (Left-Right)' = ifelse(P32<=5,1, 0)) %>%
  
  # Political Ideology (Conservatism-Progressive)
  mutate('Political Ideology (Conservatism-Progressive)' = ifelse(P34<3,1,
                                                                  ifelse(P34==6,NA,0))) %>%
  #  Descriptive norm
  replace_with_na_at(.vars = c("P18_1", "P18_2", "P18_3"), condition = ~.x== 98) %>%
  mutate('Descriptive norm' = (P18_1 + P18_2 + P18_3)/3) %>%
  
  #  Prescriptive norm
  replace_with_na_at(.vars = c("P18_4", "P18_5", "P18_6", "P18_7"), condition = ~.x== 98) %>%
  mutate('Prescriptive norm' = (P18_4 + P18_5 + P18_6/3)) %>% 
                              
  # Trust in Scientists
  mutate('Trust in scientists' = ifelse(P21_1==3| P21_1==4,1,0)) %>%
  
  # Worry
  mutate('Worry' = 5-P7)

#------------------------------------------------------------------------------#
### Creating dependent variables  ##############################################
#------------------------------------------------------------------------------#

Base <- Base %>%
  # Risk Perception - Spatial Dimension
  replace_with_na_at(.vars = c("P8_1", "P8_2", "P8_3", "P8_4", "P8_5", "P8_6", "P8_7", "P10"), condition = ~.x== 98) %>%
  mutate('Risk Perception - Spatial Dimension' = (P8_1 + P8_2 + P8_3 + P8_4 + P8_5 + P8_6 + P8_7)/7) %>%
  # Risk Perception - Extent Timing
  mutate('Risk Perception - Temporal Dimension' = 7-P10) %>%
  # Risk Perception
  mutate('Risk Perception' = (`Risk Perception - Temporal Dimension` + `Risk Perception - Spatial Dimension`)/2) %>%
  # Risk Perception (alternative)
  mutate('Risk Perception - Spatial Dimension (alt1)' = (P8_1 + P8_2 + P8_7)/3) %>%
  mutate('Risk Perception - Spatial Dimension (alt2)' = (P8_3 + P8_4 + P8_5 + P8_6)/4)

#------------------------------------------------------------------------------#
### Saving database "Base" as R Data file ######################################
#------------------------------------------------------------------------------#

saveRDS(Base, file = "data/Base.rds")

