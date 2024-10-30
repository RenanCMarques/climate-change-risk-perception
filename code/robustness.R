#------------------------------------------------------------------------------#
######################## Robustness Checks ############################
#------------------------------------------------------------------------------#

rm(list = ls())

# Renan's Directory

setwd("C:/Users/renan/Google Drive/FGV RI/Determinants of Individual-Level Climate Risk Perception in Latin America")

#------------------------------------------------------------------------------#
# Loading and installing necessary packages ##################################
#------------------------------------------------------------------------------#

# Installing required packages 

packages <- c('tidyverse', 'fixest', 'sjPlot', 'modelsummary', 
              'performance', 'car', 'psych', 'REdaS', 'knitr',
              'fastDummies', 'psych', 'factoextra', 'xtable',
              'pwr', 'broom', 'lm.beta', 'kableExtra')

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
# 1. Multicollinearity  ############################
#------------------------------------------------------------------------------#

## Non-standardized variables ####
multicol <- lm(rp ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 Female + Escolaridade_3 + Religion + Income + age + Color4, 
               data = Base, 
               weights = ponde2)

multicol_table <- multicollinearity(multicol)

kable(multicol_table, "latex",  booktabs = T, digits = 2, align = c("l", rep("c",7)))

## Standardized variables ####
Base_scaled <- Base %>%
  dummy_cols(select_columns = c("Female", "Escolaridade_3", "Religion", "Income")) %>%
  mutate(across(c(pi_lf, pi_cs, sk, ok, hk, wo, ha, pe, pv, ii, ei, nep, dn, pn,
                  219:235, age, Color4), scale))

replace_spaces <- function(colname) {
  gsub(" ", "_", colname)
}

Base_scaled <- Base_scaled %>%
  rename_with(.fn = replace_spaces, .cols = 205:235) %>%
  rename(educ1 = colnames(Base_scaled)[221], 
         educ2 = colnames(Base_scaled)[222], 
         educ3 = colnames(Base_scaled)[223], 
         regat = colnames(Base_scaled)[224],
         regcat = colnames(Base_scaled)[225],
         regevg = colnames(Base_scaled)[226],
         regevgtrad = colnames(Base_scaled)[227],
         religno = colnames(Base_scaled)[228],
         income1 = colnames(Base_scaled)[229],
         income2 = colnames(Base_scaled)[230],
         income3 = colnames(Base_scaled)[231],
         income4 = colnames(Base_scaled)[232],
         income5 = colnames(Base_scaled)[233],
         income6 = colnames(Base_scaled)[234],
         income_no = colnames(Base_scaled)[235])

multicol_scaled <- lm(rp ~ sk[,1] + ok[,1] + hk[,1] + wo[,1] + ha[,1] + pe[,1] + pv[,1] + ii[,1] + ei[,1] + nep[,1] + dn[,1] + pn[,1] +
                        pi_lf[,1] + pi_cs[,1] + 
                        Female_Yes[,1] +
                        educ2 +
                        educ3 +
                        regcat +
                        regevg +
                        regevgtrad +
                        religno +
                        income1 + 
                        income2 + 
                        income3 + 
                        income4 + 
                        income5 + 
                        income6 + 
                        income_no + 
                        age + Color4, 
                        data = Base_scaled, 
                        weights = ponde2)

# Checking for multicollinearity
multicol_table_scaled <- multicollinearity(multicol_scaled)

# Outputting the results in LaTeX format
kable(multicol_table_scaled, "latex", booktabs = T, digits = 2, align = c("l", rep("c", 7)))

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

# Teste F: apenas conjunto de vari?veis psicol?gicas
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

# Teste F: apenas conjunto de vari?veis politicas

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

# Teste F: apenas conjunto de vari?veis demograficas

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

#------------------------------------------------------------------------------#
# 5. Stepwise: Including climate-related independent variables ######
#------------------------------------------------------------------------------#

## Spatial ####
spatial_stepwise_climate <- feols(rpd ~ csw(sk, ok, hk, wo, ha, ii, ei, dn, pn,
                                   pi_lf, pi_cs, Female, Escolaridade_3, Religion, Income, age, Color4,
                                   nep, pe, pv), 
                         data = Base, 
                         weights = ~ ponde2, 
                         vcov = "hetero")

names(spatial_stepwise_climate) <- c(1:20)

spatial_stepwise_climate <- spatial_stepwise_climate[c(17,18,19,20)]

names(spatial_stepwise_climate) <- c(1:4)

etable(spatial_stepwise_climate,
       title = "OLS Results - Correlates of Risk Perception (Spatial Dimension)",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/spatial_stepwise_climate.tex")

modelsummary(spatial_stepwise,
             coef_omit = 'Interc',
             estimate = "{estimate}{stars} ({std.error})",
             statistic =  NULL,
             vcov = "robust",
             coef_rename = mydict,
             gof_map = c("nobs", "r.squared"),
             stars = c('*' = .10, '**' = .05, '***' = .01),
             output = "kableExtra")

## Temporal ####

temporal_stepwise_climate <- feols(rpt ~ csw(sk, ok, hk, wo, ha, ii, ei, dn, pn,
                                            pi_lf, pi_cs, Female, Escolaridade_3, Religion, Income, age, Color4,
                                            nep, pe, pv), 
                         data = Base, 
                         weights = ~ ponde2, 
                         vcov = "hetero")

names(temporal_stepwise_climate) <- c(1:20)

temporal_stepwise_climate <- temporal_stepwise_climate[c(17,18,19,20)]

names(temporal_stepwise_climate) <- c(1:4)

etable(temporal_stepwise_climate,
       title = "OLS Results - Correlates of Risk Perception (Temporal Dimension)",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/temporal_stepwise_climate.tex")

modelsummary(temporal_stepwise_climate,
             coef_omit = 'Interc',
             estimate = "{estimate}{stars} ({std.error})",
             statistic =  NULL,
             vcov = "robust",
             coef_rename = mydict,
             gof_map = c("nobs", "r.squared"),
             stars = c('*' = .10, '**' = .05, '***' = .01),
             output = "latex")

## Index ####

index_stepwise_climate <- feols(rp ~ csw(sk, ok, hk, wo, ha, ii, ei, dn, pn,
                                          pi_lf, pi_cs, Female, Escolaridade_3, Religion, Income, age, Color4,
                                          nep, pe, pv), 
                          data = Base, 
                          weights = ~ ponde2, 
                          vcov = "hetero")

names(index_stepwise_climate) <- c(1:20)

index_stepwise_climate <- index_stepwise_climate[c(17,18,19,20)]

names(index_stepwise_climate) <- c(1:4)

etable(index_stepwise_climate,
       title = "OLS Results - Correlates of Risk Perception (Index)",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/index_stepwise_climate.tex")

modelsummary(index_stepwise_climate,
             coef_omit = 'Interc',
             estimate = "{estimate}{stars} ({std.error})",
             statistic =  NULL,
             vcov = "robust",
             coef_rename = mydict,
             gof_map = c("nobs", "r.squared"),
             stars = c('*' = .10, '**' = .05, '***' = .01),
             output = "latex")

#------------------------------------------------------------------------------#
# 6. Stepwise: Only significant coefficients + add other variables ######
#------------------------------------------------------------------------------#

## Spatial #### 

step_spatial <- feols(rpd ~ csw(ok, hk, wo, ha, pe, pv, nep, Female, #Significant
                                 sk, ii, ei, dn, pn, pi_lf, pi_cs, Escolaridade_3, Religion, Income, age, Color4), #Insignificant 
                        data = Base, 
                        weights = ~ ponde2,
                        vcov = "hetero")

names(step_spatial) <- c(1:20)

step_spatial <- step_spatial[c(8,9,10,11,12,13,14,15,16,17,18,19,20)]

names(step_spatial) <- c(1:9)

etable(step_spatial,
       title = "OLS Results - Determinants of Risk Perception (Spatial Dimension)",
       digits = 3,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/stepwise_spatial.tex")

modelsummary(step_spatial,
             coef_omit = 'Interc',
             estimate = "{estimate}{stars} ({std.error})",
             statistic =  NULL,
             vcov = "robust",
             coef_rename = mydict,
             gof_map = c("nobs", "r.squared"),
             stars = c('*' = .10, '**' = .05, '***' = .01),
             output = "kableExtra")

## Temporal ####

step_temporal <- feols(rpt ~ csw(sk, ok, wo, pv, nep, Female, age, #Significant
                               hk, ha, ii, ei, pe, dn, pn, pi_lf, pi_cs, Escolaridade_3, Religion, Income, Color4), #Insignificant 
                     data = Base, 
                     weights = ~ ponde2, 
                     vcov = "hetero")

names(step_temporal) <- c(1:20)

step_temporal <- step_temporal[c(7,8,9,10,11,12,13,14,15,16,17,18,20)]

names(step_temporal) <- c(1:8)

#------------------------------------------------------------------------------#
# 7. Correlation Table (After standardization) ######
#------------------------------------------------------------------------------#

# Correlation Table of Pyschological variables
independent_psycho <- Base_scaled %>%
  dplyr::select(207:218) 

tab_corr(independent_psycho) 

datasummary_correlation(independent_psycho,
                        output = "latex_tabular",
                        method = "pearson")

#------------------------------------------------------------------------------#
# 8. Correlation Plots ######
#------------------------------------------------------------------------------#

## Worry and Hollistic Affect ####

Base %>%
  ggplot(aes(wo, ha)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(
    title = "Worry x Hollistic Affect",
    x = "Worry (1-4)",
    y = "Hollistic Affect (1-6)"
  )

## Political Ideology ####
Base %>%
  select(P32, P34) %>%
  mutate(P34 = ifelse(P34 == 6, NA, P34)) %>%
  ggplot(aes(P32, P34)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(
    title = "Political Ideology: Left-Right x Conservative-Progressive",
    x = "Left -> Right",
    y = "Progressive -> Conservative"
  )

## Worry x Perceived Vulnerability ####
cor1 <- lm(pv ~ wo, data = Base)
summary(cor1)

Base %>%
  ggplot(aes(wo, pv)) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(
    title = "Worry x Perceived Vulnerability",
    x = "Worry",
    y = "Perceived Vulnerability to extreme weather events"
  )

#------------------------------------------------------------------------------#
# 9. PCA ######
#------------------------------------------------------------------------------#

# Select only relevant columns
Base_pca <- Base_scaled %>%
  select(207:218) %>%
  drop_na()

# Run PCA
pca_result2 <- prcomp(Base_pca, scale. = TRUE)

# Get eigenvalues
eig.val <- get_eigenvalue(pca_result2) %>%
  xtable(caption = "Eigenvalues and Variance Explained by PCA Components")

# Scree plot
fviz_eig(pca_result2, addlabels = TRUE)

# Variable contributions
fviz_pca_var(pca_result2, col.var = "contrib")

#------------------------------------------------------------------------------#
# 10. Power Analysis ######
#------------------------------------------------------------------------------#

# Define your models
spatial <- lm(rpd ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                Female + Escolaridade_3 + Religion + Income + age + Color4, 
              data = Base, 
              weights = ponde2)

temporal <- lm(rpt ~ pi_lf + pi_cs + sk + ok + hk + wo + ha + pe + pv + ii + ei + nep + dn + pn +
                 Female + Escolaridade_3 + Religion + Income + age + Color4,  
               data = Base, 
               weights = ponde2)

# 1. Power analysis using pwr.f2.test (Cohen's f-squared effect size calculation)
# f2 = R^2 / (1 - R^2)
R2_spatial <- summary(spatial)$r.squared
R2_temporal <- summary(temporal)$r.squared

f2_spatial <- R2_spatial / (1 - R2_spatial)
f2_temporal <- R2_temporal / (1 - R2_temporal)

# Power analysis for each model
power_spatial <- pwr.f2.test(u = length(coef(spatial)) - 1,  # numerator degrees of freedom
                             v = nrow(Base) - length(coef(spatial)),  # denominator degrees of freedom
                             f2 = f2_spatial, 
                             sig.level = 0.05)

power_temporal <- pwr.f2.test(u = length(coef(temporal)) - 1, 
                              v = nrow(Base) - length(coef(temporal)), 
                              f2 = f2_temporal, 
                              sig.level = 0.05)

# Power results
power_spatial
power_temporal

#------------------------------------------------------------------------------#
# 11. Effect Size ######
#------------------------------------------------------------------------------#

# Get standardized coefficients
spatial_beta <- lm.beta(spatial)
temporal_beta <- lm.beta(temporal)

# Extract coefficients and confidence intervals
spatial_summary <- tidy(spatial, conf.int = TRUE)
temporal_summary <- tidy(temporal, conf.int = TRUE)

# Adding standardized coefficients (effect sizes) to the output
spatial_summary$std_estimate <- coef(spatial_beta)
temporal_summary$std_estimate <- coef(temporal_beta)

# View the results
print(spatial_summary)
print(temporal_summary)

# Create LaTeX tables
spatial_table <- spatial_summary %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, std_estimate) %>%
  kbl(caption = "Spatial Model Results", format = "latex", booktabs = TRUE) 

temporal_table <- temporal_summary %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high, std_estimate) %>%
  kbl(caption = "Temporal Model Results", format = "latex", booktabs = TRUE)

# Print LaTeX tables
print(spatial_table)
print(temporal_table)


#------------------------------------------------------------------------------#
# 6. Stepwise: Removing psychological variables ######
#------------------------------------------------------------------------------#

## Spatial #### 

step_spatial <- feols(rpd ~ csw(pi_lf, pi_cs, Female, Escolaridade_3, Religion, Income, age, Color4,
                                sk, ok, hk, wo, ha, pe, pv, ii, ei, nep, dn, pn), 
                      data = Base, 
                      weights = ~ ponde2,
                      vcov = "hetero")

names(step_spatial) <- c(1:17)

step_spatial <- step_spatial[c(8,9,10,11,12,13,14,15,16,17,18,19,20)]

names(step_spatial) <- c(1:9)

etable(step_spatial,
       title = "OLS Results - Determinants of Risk Perception (Spatial Dimension)",
       digits = 2,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/stepwise_spatial_psych.tex")

modelsummary(step_spatial,
             coef_omit = 'Interc',
             estimate = "{estimate}{stars} ({std.error})",
             statistic =  NULL,
             vcov = "robust",
             coef_rename = mydict,
             gof_map = c("nobs", "r.squared"),
             stars = c('*' = .10, '**' = .05, '***' = .01),
             output = "kableExtra")

## Temporal ####

step_temporal <- feols(rpt ~ csw(pi_lf, pi_cs, Female, Escolaridade_3, Religion, Income, age, Color4,
                                sk, ok, hk, wo, ha, pe, pv, ii, ei, nep, dn, pn), 
                      data = Base, 
                      weights = ~ ponde2,
                      vcov = "hetero")

names(step_temporal) <- c(1:17)

step_temporal <- step_temporal[c(8,9,10,11,12,13,14,15,16,17,18,19,20)]

names(step_temporal) <- c(1:9)

etable(step_temporal,
       title = "OLS Results - Determinants of Risk Perception (Temporal Dimension)",
       digits = 2,
       dict = mydict,
       signif.code = c(`***` = 0.01, `**` = 0.05, `*` = 0.1),
       file = "tables/stepwise_temporal_psych.tex")


