#------------------------------------------------------------------------------#
######################## Robustness Checks ############################
#------------------------------------------------------------------------------#

rm(list = ls())

# Setting directory: adjust accordingly

setwd()

#------------------------------------------------------------------------------#
# Loading and installing necessary packages ##################################
#------------------------------------------------------------------------------#

# Installing required packages 

packages <- c('tidyverse', 'fixest', 'sjPlot', 'modelsummary', 
              'performance', 'car', 'psych', 'REdaS', 'knitr')

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
# 1. Multicollinearity  ############################
#------------------------------------------------------------------------------#

multicol <- lm(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 Female + Escolaridade_3 + Religion + Income + age + Color4, 
               data = Base, 
               weights = ponde2)

multicol_table <- multicollinearity(multicol)

kable(multicol_table, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",7)))

#------------------------------------------------------------------------------#
# 2. Multiple Hypotheses Testing  ############################
#------------------------------------------------------------------------------#

spatial <- lm(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                Female + Escolaridade_3 + Religion + Income + age + Color4, 
                data = Base, 
                weights = ponde2)

temporal <- lm(rpt ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 Female + Escolaridade_3 + Religion + Income + age + Color4,  
             data = Base, 
             weights = ponde2)

index <- lm(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
              Female + Escolaridade_3 + Religion + Income + age + Color4,  
             data = Base, 
             weights = ponde2)


# Teste F: modelo completo
summary(spatial)
summary(temporal)
summary(index)

# Teste F: apenas conjunto de variáveis psicológicas
spatial_psych <- linearHypothesis(spatial, c("sk = 0", "ok = 0", 
                                               "hk = 0", "wo = 0", "ha = 0", "pe = 0",
                                               "pv = 0", "ii = 0", "ei = 0", "nep = 0",
                                               "dn = 0", "pn = 0"), 
                                    white.adjust = "hc1")

temporal_psych <- linearHypothesis(temporal, c("sk = 0", "ok = 0", 
                                             "hk = 0", "wo = 0", "ha = 0", "pe = 0",
                                             "pv = 0", "ii = 0", "ei = 0", "nep = 0",
                                             "dn = 0", "pn = 0"), 
                                 white.adjust = "hc1")

index_psych <- linearHypothesis(index, c("sk = 0", "ok = 0", 
                                          "hk = 0", "wo = 0", "ha = 0", "pe = 0",
                                          "pv = 0", "ii = 0", "ei = 0", "nep = 0",
                                          "dn = 0", "pn = 0"), 
                                 white.adjust = "hc1")


f_psych <- bind_rows(spatial_psych, temporal_psych, index_psych) %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(Model = c("Spatial", "Temporal", "Index")) %>%
  dplyr::select(Model, "Res.Df", "Df", "F", "Pr(>F)")

kable(f_psych, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",6)))

# Teste F: apenas conjunto de variáveis politicas

spatial_pol <- linearHypothesis(spatial, c("pi_lf = 0", "pi_cs = 0"), 
                                  white.adjust = "hc1")

temporal_pol <- linearHypothesis(temporal, c("pi_lf = 0", "pi_cs = 0"), 
                               white.adjust = "hc1")

index_pol <- linearHypothesis(index, c("pi_lf = 0", "pi_cs = 0"), 
                               white.adjust = "hc1")


f_pol <- bind_rows(spatial_pol, temporal_pol, index_pol) %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(Model = c("Spatial", "Temporal", "Index")) %>%
  dplyr::select(Model, "Res.Df", "Df", "F", "Pr(>F)")

kable(f_pol, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",5)))

# Teste F: apenas conjunto de variáveis demograficas

spatial_soc <- linearHypothesis(spatial, c("FemaleYes = 0", "Escolaridade_3High school or equivalent = 0",
                                               "Escolaridade_3Undergraduate or more = 0", "ReligionCatholic = 0",
                                               "ReligionEvangelical Pentecostal or other evangelical = 0",
                                               "ReligionEvangelical Traditional = 0", "ReligionOthers/No Relig. = 0",
                                               "Income1 - 2 minimum wages = 0", "Income10 minimum wages or more = 0",
                                               "Income2 - 3 minimum wages = 0", "Income3 - 5 minimum wages = 0",
                                               "Income5 - 10 minimum wages = 0", "IncomeDo not know/ Prefer to not answer = 0",
                                               "age = 0", "Color4 = 0"), 
                                  white.adjust = "hc1")

temporal_soc <- linearHypothesis(temporal, c("FemaleYes = 0", "Escolaridade_3High school or equivalent = 0",
                                         "Escolaridade_3Undergraduate or more = 0", "ReligionCatholic = 0",
                                         "ReligionEvangelical Pentecostal or other evangelical = 0",
                                         "ReligionEvangelical Traditional = 0", "ReligionOthers/No Relig. = 0",
                                         "Income1 - 2 minimum wages = 0", "Income10 minimum wages or more = 0",
                                         "Income2 - 3 minimum wages = 0", "Income3 - 5 minimum wages = 0",
                                         "Income5 - 10 minimum wages = 0", "IncomeDo not know/ Prefer to not answer = 0",
                                         "age = 0", "Color4 = 0"), 
                               white.adjust = "hc1")

index_soc <- linearHypothesis(index, c("FemaleYes = 0", "Escolaridade_3High school or equivalent = 0",
                                         "Escolaridade_3Undergraduate or more = 0", "ReligionCatholic = 0",
                                         "ReligionEvangelical Pentecostal or other evangelical = 0",
                                         "ReligionEvangelical Traditional = 0", "ReligionOthers/No Relig. = 0",
                                         "Income1 - 2 minimum wages = 0", "Income10 minimum wages or more = 0",
                                         "Income2 - 3 minimum wages = 0", "Income3 - 5 minimum wages = 0",
                                         "Income5 - 10 minimum wages = 0", "IncomeDo not know/ Prefer to not answer = 0",
                                         "age = 0", "Color4 = 0"), 
                               white.adjust = "hc1")

f_soc <- bind_rows(spatial_soc, temporal_soc, index_soc) %>%
  as.data.frame() %>%
  drop_na() %>%
  mutate(Model = c("Spatial", "Temporal", "Index")) %>%
  dplyr::select(Model, "Res.Df", "Df", "F", "Pr(>F)")

kable(f_soc, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",5)))

#------------------------------------------------------------------------------#
# 3. Hypothesis Testing: Linear combination  ############################
#------------------------------------------------------------------------------#

## Spatial Dimension ####

# Worry x Objective Knowledge
sd1 <- linearHypothesis(spatial, c("wo - ok = 0"), 
                 white.adjust = "hc1") 

# Worry x Human-caused knowledge
sd2 <- linearHypothesis(spatial, c("wo - hk = 0"), 
                 white.adjust = "hc1")

# Worry x Holistic effect
sd3 <- linearHypothesis(spatial, c("wo + ha = 0"), 
                 white.adjust = "hc1")

# Worry x Personal experience
sd4 <- linearHypothesis(spatial, c("wo - pe = 0"), 
                 white.adjust = "hc1") 

# Worry x Perceived Vulnerability
sd5 <- linearHypothesis(spatial, c("wo - pv = 0"), 
                 white.adjust = "hc1") 

# Worry x NEP
sd6 <- linearHypothesis(spatial, c("wo - nep = 0"), 
                 white.adjust = "hc1") 

sd <-  bind_rows(sd1, sd2, sd3, sd4, sd5, sd6) %>%
  drop_na() %>%
  mutate(Variable = c("Objective knowledge", "Human-caused knowledge", "Holistic Affect" , "Personal experience (extreme weather events)",
                      "Perceived Vulnerability to extreme weather events" , "The New Ecological Paradigm (NEP)"),
         .before = Res.Df)

kable(sd, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",4)))

## Temporal Dimension ####

# Worry x Subjective Knowledge
td1 <- linearHypothesis(temporal, c("wo - sk = 0"), 
                        white.adjust = "hc1") 

# Worry x Objective Knowledge
td2 <- linearHypothesis(temporal, c("wo - ok = 0"), 
                        white.adjust = "hc1")

# Worry x Human-caused Knowledge
td3 <- linearHypothesis(temporal, c("wo - hk = 0"), 
                        white.adjust = "hc1")

# Worry x Personal experience with extreme
td4 <- linearHypothesis(temporal, c("wo - pe = 0"), 
                        white.adjust = "hc1")

# Worry x Perceived Vulnerability
td5 <- linearHypothesis(temporal, c("wo - pv = 0"), 
                        white.adjust = "hc1") 

# Worry x Individualism worldview
td6 <- linearHypothesis(temporal, c("wo + ii = 0"), 
                        white.adjust = "hc1") 

# Worry x NEP
td7 <- linearHypothesis(temporal, c("wo - nep = 0"), 
                        white.adjust = "hc1") 

td <-  bind_rows(td1, td2, td3, td4, td5, td6, td7) %>%
  drop_na() %>%
  mutate(Variable = c("Subjective knowledge", "Objective knowledge", "Human-caused knowledge",
                      "Personal experience (extreme weather events)", "Perceived vulnerability to extreme weather events" , 
                      "Individualism worldview", "The New Ecological Paradigm (NEP)"),
         .before = Res.Df)

kable(td, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",4)))

## Index ####

# Worry x Subjective Knowledge
in1 <- linearHypothesis(index, c("wo - sk = 0"), 
                        white.adjust = "hc1") 

# Worry x Objective Knowledge
in2 <- linearHypothesis(index, c("wo - ok = 0"), 
                        white.adjust = "hc1")

# Worry x Human-caused Knowledge
in3 <- linearHypothesis(index, c("wo - hk = 0"), 
                        white.adjust = "hc1")

# Worry x Holistic Affect
in4 <- linearHypothesis(index, c("wo + ha = 0"), 
                        white.adjust = "hc1")

# Worry x Personal experience with extreme
in5 <- linearHypothesis(index, c("wo - pe = 0"), 
                        white.adjust = "hc1")

# Worry x Perceived Vulnerability
in6 <- linearHypothesis(index, c("wo - pv = 0"), 
                        white.adjust = "hc1") 

# Worry x Individualism worldview
in7 <- linearHypothesis(index, c("wo + ii = 0"), 
                        white.adjust = "hc1") 

# Worry x NEP
in8 <- linearHypothesis(index, c("wo - nep = 0"), 
                        white.adjust = "hc1") 

ind <-  bind_rows(in1, in2, in3, in4, in5, in6, in7, in8) %>%
  drop_na() %>%
  mutate(Variable = c("Subjective knowledge", "Objective knowledge", "Human-caused knowledge", "Holistic affect",
                      "Personal experience (extreme weather events)", "Perceived vulnerability to extreme weather events" , 
                      "Individualism worldview", "The New Ecological Paradigm (NEP)"),
         .before = Res.Df)

kable(ind, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",4)))

#------------------------------------------------------------------------------#
# 4. Adjusted p-value  ############################
#------------------------------------------------------------------------------#

spatial_h <- feols(rpd ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                   pi_lf + pi_cs+ Female + Escolaridade_3 + Religion + Income + age + Color4,
                   data = Base, weights = ~ ponde2, vcov = "hetero")

spatial <- feols(rpd ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                     pi_lf + pi_cs+ Female + Escolaridade_3 + Religion + Income + age + Color4,
                   data = Base, weights = ~ ponde2)

temporal_h <- feols(rpt ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                    pi_lf + pi_cs  + Female + Escolaridade_3 + Religion + Income + age + Color4,
                data = Base, weights = ~ ponde2, vcov = "hetero")

temporal <- feols(rpt ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                      pi_lf + pi_cs  + Female + Escolaridade_3 + Religion + Income + age + Color4,
                    data = Base, weights = ~ ponde2)

index_h <- feols(rp ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 pi_lf + pi_cs  + Female + Escolaridade_3 + Religion + Income + age + Color4, 
               data = Base, weights = ~ ponde2, vcov = "hetero")

index <- feols(rp ~ sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 pi_lf + pi_cs  + Female + Escolaridade_3 + Religion + Income + age + Color4, 
               data = Base, weights = ~ ponde2)

# Spatial
spatial_std <- as.data.frame(spatial_h$coeftable) %>%
  select('Pr(>|t|)') %>%
  mutate_if(is.numeric, round, digits = 5)

spatial_bh <- as.data.frame(p.adjust(spatial$coeftable$`Pr(>|t|)`,
                                       method = "BH")) %>%
  mutate_if(is.numeric, round, digits = 4)

spatial_p <- spatial_std %>%
  bind_cols(spatial_bh) 

names(spatial_p)[1] = "standard"
names(spatial_p)[2] = "adjusted"

spatial_p <- spatial_p %>%
  mutate(signif = case_when(
    standard <= 0.01 ~ "***",
    standard > 0.01 & standard <= 0.05 ~ "**",
    standard > 0.05 & standard <= 0.1 ~ "*",
    standard > 0.1 ~ "-"),
    signif_adj = case_when(
      adjusted <= 0.01 ~ "***",
      adjusted > 0.01 & adjusted <= 0.05 ~ "**",
      adjusted > 0.05 & adjusted <= 0.1 ~ "*",
      adjusted > 0.1 ~ "-")) %>%
  dplyr::select(standard, signif, adjusted, signif_adj)

# Temporal
temporal_std <- as.data.frame(temporal_h$coeftable) %>%
  select('Pr(>|t|)') %>%
  mutate_if(is.numeric, round, digits = 5)

temporal_bh <- as.data.frame(p.adjust(temporal$coeftable$`Pr(>|t|)`,
                                    method = "BH")) %>%
  mutate_if(is.numeric, round, digits = 4)

temporal_p <- temporal_std %>%
  bind_cols(temporal_bh) 

names(temporal_p)[1] = "standard"
names(temporal_p)[2] = "adjusted"

temporal_p <- temporal_p %>%
  mutate(signif = case_when(
    standard <= 0.01 ~ "***",
    standard > 0.01 & standard <= 0.05 ~ "**",
    standard > 0.05 & standard <= 0.1 ~ "*",
    standard > 0.1 ~ "-"),
    signif_adj = case_when(
      adjusted <= 0.01 ~ "***",
      adjusted > 0.01 & adjusted <= 0.05 ~ "**",
      adjusted > 0.05 & adjusted <= 0.1 ~ "*",
      adjusted > 0.1 ~ "-")) %>%
  dplyr::select(standard, signif, adjusted, signif_adj)

# Index
index_std <- as.data.frame(index_h$coeftable) %>%
  select('Pr(>|t|)') %>%
  mutate_if(is.numeric, round, digits = 5)

index_bh <- as.data.frame(p.adjust(index$coeftable$`Pr(>|t|)`,
                                   method = "BH")) %>%
  mutate_if(is.numeric, round, digits = 4)

index_p <- index_std %>%
  bind_cols(index_bh) 

names(index_p)[1] = "standard"
names(index_p)[2] = "adjusted"

index_p <- index_p %>%
  mutate(signif = case_when(
    standard <= 0.01 ~ "***",
    standard > 0.01 & standard <= 0.05 ~ "**",
    standard > 0.05 & standard <= 0.1 ~ "*",
    standard > 0.1 ~ "-"),
    signif_adj = case_when(
      adjusted <= 0.01 ~ "***",
      adjusted > 0.01 & adjusted <= 0.05 ~ "**",
      adjusted > 0.05 & adjusted <= 0.1 ~ "*",
      adjusted > 0.1 ~ "-")) %>%
  dplyr::select(standard, signif, adjusted, signif_adj)

# Creating table

adjusted_p <- bind_cols(spatial_p, temporal_p, index_p)


kable(adjusted_p, "latex",  booktabs = T, digits = 4, align = c(rep("c",6)))
