#------------------------------------------------------------------------------#
############################ Descriptive Tables ################################
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
              'nlme', 'psych', 'spatstat', 'marginaleffects', 'table1', 'texreg')

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

################################################################################
# Descriptive Statistics with weights 
################################################################################

# Creating table
summ.stats <- Base %>%
  mutate(mulher = ifelse(F1==2,1,0),
         Escola_1 = as.numeric(Escolaridade_3 == "Elementary (Primary) or less"),
         Escola_2 = as.numeric(Escolaridade_3 == "High school or equivalent"),
         Escola_3 = as.numeric(Escolaridade_3 == "Undergraduate or more"),
         renda1 = as.numeric(Income=="0 - 1 minimum wages"),
         renda2 = as.numeric(Income=="1 - 2 minimum wages"),
         renda3 = as.numeric(Income=="2 - 3 minimum wages"),
         renda4 = as.numeric(Income=="3 - 5 minimum wages"),
         renda5 = as.numeric(Income=="5 - 10 minimum wages"),
         renda6 = as.numeric(Income=="10 minimum wages or more"),
         renda7 = ifelse(Income_aux %in% c(98,99),1,0),
         Color1 = as.numeric(Color=="White"),
         Color2 = as.numeric(Color=="Black or 'pardo'"),
         Color3 = as.numeric(Color=="Indigenous person"),
         Color4 = as.numeric(Color=="Mestizo"),
         Color5 = as.numeric(Color=="Other"),
         Religion_1 = as.numeric(Religion=="Atheist"),
         Religion_2 = as.numeric(Religion=="Catholic"),
         Religion_3 = as.numeric(Religion=="Evangelical Pentecostal or other evangelical"),
         Religion_4 = as.numeric(Religion=="Evangelical Traditional"),
         Religion_5 = as.numeric(Religion=="Others/No Relig.")) %>%
  group_by(Country) %>%
  summarise(mean_age = weighted.mean(age,w = ponde2),
            fem = percent(weighted.mean(mulher, w = ponde2)),
            "Elementary (Primary) or less" = percent(weighted.mean(Escola_1, w = ponde2)),
            "High school or equivalent" = percent(weighted.mean(Escola_2, w = ponde2)),
            "Undergraduate or more" = percent(weighted.mean(Escola_3, w = ponde2)),
            "0 - 1 minimum wages" = percent(weighted.mean(renda1, w = ponde2)),
            "1 - 2 minimum wages" = percent(weighted.mean(renda2, w = ponde2)),
            "2 - 3 minimum wages" = percent(weighted.mean(renda3, w = ponde2)),
            "3 - 5 minimum wages" = percent(weighted.mean(renda4, w = ponde2)),
            "5 - 10 minimum wages" = percent(weighted.mean(renda5, w = ponde2)),
            "10 minimum wages or more" = percent(weighted.mean(renda6, w = ponde2)),
            "Do not know/ Prefer to not answer" = percent(weighted.mean(renda7, w = ponde2)),
            "White" = percent(weighted.mean(Color1, w = ponde2)),
            "Black or 'pardo'" = percent(weighted.mean(Color2, w = ponde2)),
            "Indigenous person" = percent(weighted.mean(Color3, w = ponde2)),
            "Mestizo" = percent(weighted.mean(Color4, w = ponde2)),
            "Other" = percent(weighted.mean(Color5, w = ponde2)),
            "Atheist" = percent(weighted.mean(Religion_1, w = ponde2)),
            "Catholic" = percent(weighted.mean(Religion_2, w = ponde2)),
            "Evangelical Pentecostal or other evangelical" = percent(weighted.mean(Religion_3, w = ponde2)),
            "Evangelical Traditional" = percent(weighted.mean(Religion_4, w = ponde2)),
            "Others/No Relig." = percent(weighted.mean(Religion_5, w = ponde2)))%>%
  mutate_if(is.numeric, format, digits=3,nsmall = 0) %>%
  t() %>%
  as.data.frame()

#Saving table as LateX

summ.stats_tex <- kbl(summ.stats, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",6)))

writeLines(summ.stats_tex, 'tables/summ.stats.tex')

################################################################################
# Crossed Statistics (Distribution of age by sex and country) with weigths
################################################################################

# Argentina 
Pais <- Base %>% filter(Pais == 2)
prop.table(table(as.factor(Pais$F2), Pais$F1))  
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) 

# Brasil 
Br <- Base %>% filter(Pais == 1)
prop.table(table( as.factor(Br$F2), Br$F1))  
prop.table(tapply(Br$ponde2, list(as.factor(Br$F2), Br$F1), sum)) # Weighted

### Regions of Brazil
table1::table1(~ as.factor(ZONA_BR), data=Br)
prop.table(tapply(Br$ponde2, list(as.factor(Br$ZONA_BR)), sum)) # Weighted

# Chile 
Pais <- Base %>% filter(Pais == 3)
prop.table(table(as.factor(Pais$F2), Pais$F1))  
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) # Weighted

# Colombia
Pais <- Base %>% filter(Pais == 4)
prop.table(table(as.factor(Pais$F2), Pais$F1))  
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) # Weighted

# Equador
Pais <- Base %>% filter(Pais == 5)
prop.table(table(as.factor(Pais$F2), Pais$F1))  
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) # Weighted

# Mexico
Pais <- Base %>% filter(Pais == 6)
prop.table(table(as.factor(Pais$F2), Pais$F1))  
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) # Weighted

# Peru
Pais <- Base %>% filter(Pais == 7)
prop.table(table(as.factor(Pais$F2), Pais$F1)) 
prop.table(tapply(Pais$ponde2, list(as.factor(Pais$F2), Pais$F1), sum)) # Weighted

rm(Pais, Br)

# Variables with missing
table1::table1(~ as.factor(P2) + as.factor(P3) + as.factor(P36) + as.factor(P37_1)+as.factor(P37_2) +as.factor(P37_3)+as.factor(P37_4)+as.factor(P38)| Country, data=Base)
Br <- Base %>% filter(Pais == 1)
prop.table(table( as.factor(Br$P36), Br$P37_1))
prop.table(table( as.factor(Br$P36), Br$P37_2))
table1::table1(~ as.factor(ZONA_BR), data=Br)

################################################################################
# Dependent Variables with weigths
################################################################################

summ.stats_1 <- Base %>%
  group_by(Country) %>%
  summarise(`Risk Perception - Spatial Dimension` = weighted.mean(`Risk Perception - Spatial Dimension`, w = ponde2, na.rm = TRUE),
            `Risk Perception - Temporal Dimension`  = weighted.mean(`Risk Perception - Temporal Dimension`, w = ponde2, na.rm = TRUE),
            `Risk Perception` = weighted.mean(`Risk Perception`, w = ponde2, na.rm = TRUE)) %>%
  mutate_if(is.numeric, format, digits=3,nsmall = 0) %>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column()

summ.stats_1_tex <- kable(summ.stats_1, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",6)))

writeLines(summ.stats_1_tex, 'summ.stats_1.tex')

weighted.mean(Base$`Risk Perception - Spatial Dimension`, w = Base$ponde2, na.rm = TRUE)
weighted.mean(Base$`Risk Perception - Temporal Dimension`, w = Base$ponde2, na.rm = TRUE)
weighted.mean(Base$`Risk Perception`, w = Base$ponde2, na.rm = TRUE)

# Brazil
sqrt(wtd.var(Base[Base$Pais == 1,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 1,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 1,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 1,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 1,]$`Risk Perception`, w = Base[Base$Pais == 1,]$ponde2, na.rm = TRUE))

#Argentina
sqrt(wtd.var(Base[Base$Pais == 2,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 2,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 2,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 2,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 2,]$`Risk Perception`, w = Base[Base$Pais == 2,]$ponde2, na.rm = TRUE))

#Chile
sqrt(wtd.var(Base[Base$Pais == 3,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 3,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 3,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 3,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 3,]$`Risk Perception`, w = Base[Base$Pais == 3,]$ponde2, na.rm = TRUE))

#Colombia
sqrt(wtd.var(Base[Base$Pais == 4,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 4,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 4,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 4,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 4,]$`Risk Perception`, w = Base[Base$Pais == 4,]$ponde2, na.rm = TRUE))

#Ecuador
sqrt(wtd.var(Base[Base$Pais == 5,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 5,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 5,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 5,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 5,]$`Risk Perception`, w = Base[Base$Pais == 5,]$ponde2, na.rm = TRUE))

#Mexico
sqrt(wtd.var(Base[Base$Pais == 6,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 6,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 6,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 6,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 6,]$`Risk Perception`, w = Base[Base$Pais == 6,]$ponde2, na.rm = TRUE))

#Peru
sqrt(wtd.var(Base[Base$Pais == 7,]$`Risk Perception - Spatial Dimension`, w = Base[Base$Pais == 7,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 7,]$`Risk Perception - Temporal Dimension`, w = Base[Base$Pais == 7,]$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base[Base$Pais == 7,]$`Risk Perception`, w = Base[Base$Pais == 7,]$ponde2, na.rm = TRUE))

#All
sqrt(wtd.var(Base$`Risk Perception - Spatial Dimension`, w = Base$ponde2, na.rm = TRUE))
sqrt(wtd.var(Base$`Risk Perception - Temporal Dimension`, w = Base$ponde2, na.rm = TRUE))        
sqrt(wtd.var(Base$`Risk Perception`, w = Base$ponde2, na.rm = TRUE))    


################################################################################
# Independent Variables with weights
################################################################################
Base <-  Base %>%
  mutate(Subjective_knowledge = `Subjective knowledge`,
         Holistic_Affect = `Holistic Affect`,
         Affective_Imagery =`Affective Imagery`)

Base <- cbind(dummy_columns(Base, select_columns = c('Subjective_knowledge', 'Holistic_Affect', 'Affective_Imagery')))

summ.stats_2 <- Base %>%
  group_by(Country) %>%
  summarise(`Political Ideology (Left-Right)` = percent(weighted.mean(`Political Ideology (Left-Right)`  , w = ponde2, na.rm = TRUE)),
            `Political Ideology (Conservatism-Progressive)` = percent(weighted.mean( `Political Ideology (Conservatism-Progressive)` , w = ponde2, na.rm = TRUE)),
            `Subjective knowledge` = weighted.mean(`Subjective knowledge`,w = ponde2, na.rm = TRUE),
            `Nothing` = percent(weighted.mean(Subjective_knowledge_1, w = ponde2, na.rm = TRUE)),
            `A little` = percent(weighted.mean(Subjective_knowledge_2, w = ponde2, na.rm = TRUE)),
            `A moderate amount` = percent(weighted.mean(Subjective_knowledge_3, w = ponde2, na.rm = TRUE)),
            `A lot` = percent(weighted.mean(Subjective_knowledge_4, w = ponde2, na.rm = TRUE)),
            `Do not know` = percent(weighted.mean(Subjective_knowledge_NA, w = ponde2, na.rm = TRUE)),
            `Objective knowledge` = weighted.mean(`Objective knowledge`,w = ponde2, na.rm = TRUE),
            `Human-Caused Knowledge`  = percent(weighted.mean(`Human-Caused Knowledge`, w = ponde2, na.rm = TRUE)),
            `Worry` = weighted.mean(`Worry`,w = ponde2, na.rm = TRUE),
            `Scientific consensus` = percent(weighted.mean(`Scientific consensus`, w = ponde2, na.rm = TRUE)),
            `Trust in scientists` = percent(weighted.mean(`Trust in scientists` , w = ponde2, na.rm = TRUE)),
            `Very bad` = percent(weighted.mean(Holistic_Affect_0, w = ponde2, na.rm = TRUE)),
            `Bad` = percent(weighted.mean(Holistic_Affect_1, w = ponde2, na.rm = TRUE)),
            `A little bad` = percent(weighted.mean(Holistic_Affect_2, w = ponde2, na.rm = TRUE)),
            `Neutral` = percent(weighted.mean(Holistic_Affect_3, w = ponde2, na.rm = TRUE)),
            `Little good` = percent(weighted.mean(Holistic_Affect_4, w = ponde2, na.rm = TRUE)),
            `Good` = percent(weighted.mean(Holistic_Affect_5, w = ponde2, na.rm = TRUE)),
            `Very good` = percent(weighted.mean(Holistic_Affect_6, w = ponde2, na.rm = TRUE)),
            `Increase in respiratory diseases` = percent(weighted.mean(Affective_Imagery_1 , w = ponde2, na.rm = TRUE)),
            `Frequent occurrence of extreme weather events` = percent(weighted.mean(Affective_Imagery_2 , w = ponde2, na.rm = TRUE)),
            `Social problems such as hunger and unemployment` = percent(weighted.mean(Affective_Imagery_3 , w = ponde2, na.rm = TRUE)),
            `Lack of drinking water ` = percent(weighted.mean(Affective_Imagery_4 , w = ponde2, na.rm = TRUE)),
            `Increased pollution` = percent(weighted.mean(Affective_Imagery_5 , w = ponde2, na.rm = TRUE)),
            `Destruction of nature (animals, plants, forests and others)` = percent(weighted.mean(Affective_Imagery_6 , w = ponde2, na.rm = TRUE)),
            `Melting ice at the poles` = percent(weighted.mean(Affective_Imagery_7 , w = ponde2, na.rm = TRUE)),
            `Increase in sea leve` = percent(weighted.mean(Affective_Imagery_8 , w = ponde2, na.rm = TRUE)),
            `None ` = percent(weighted.mean(Affective_Imagery_98 , w = ponde2, na.rm = TRUE)),
            `I do not know ` = percent(weighted.mean(Affective_Imagery_99 , w = ponde2, na.rm = TRUE)),
            `Missing` = percent(weighted.mean(Affective_Imagery_NA , w = ponde2, na.rm = TRUE)),
            `Personal Experience (extreme weather events)` = percent(weighted.mean(`Personal Experience (extreme weather events)` , w = ponde2, na.rm = TRUE)),
            `Perceived Vulnerability to extreme events` = weighted.mean(`Perceived Vulnerability to extreme events` , w = ponde2, na.rm = TRUE),
            `Individualism index` = weighted.mean(`Individualism index` , w = ponde2, na.rm = TRUE),
            `Egalitarianism index`  = weighted.mean(`Egalitarianism index` , w = ponde2, na.rm = TRUE),
            `The New Ecological Paradigm (NEP)` = weighted.mean(`The New Ecological Paradigm (NEP)`, w = ponde2, na.rm = TRUE),
            `Descriptive norm` = weighted.mean(`Descriptive norm` , w = ponde2, na.rm = TRUE),
            `Prescriptive norm` = weighted.mean(`Prescriptive norm`  , w = ponde2, na.rm = TRUE)) %>%
  mutate_if(is.numeric, format, digits=3, nsmall = 0) %>%
  t()%>%
  as.data.frame()%>%
  rownames_to_column()

summ.stats_2_tex <- kable(summ.stats_2, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",6)))

writeLines(summ.stats_2_tex, 'tables/summ.stats_2.tex')
