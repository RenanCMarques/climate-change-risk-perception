#------------------------------------------------------------------------------#
########################            Maps           #############################
#------------------------------------------------------------------------------#

#Loading packages to work with maps
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(pollster)

# Setting directory: adjust accordingly

setwd()

#------------------------------------------------------------------------------#
### Importing Base  ############################################################
#------------------------------------------------------------------------------#

Base <- readRDS("data/Base.rds")

#------------------------------------------------------------------------------#
### Renaming Variables  ########################################################
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
    ts = `Trust in scientists`, 
    nep = `The New Ecological Paradigm (NEP)`,
    ii = `Individualism index`,
    ei = `Egalitarianism index`,
    pe = `Personal Experience (extreme weather events)`,
    pv = `Perceived Vulnerability to extreme events`,
    dn = `Descriptive norm`, 
    pn = `Prescriptive norm`)

#------------------------------------------------------------------------------#
### Importing data of borders in the Americas ##################################
#------------------------------------------------------------------------------#

america <- ne_countries(scale = "medium", type = "countries", continent = c("south america", "north america"), returnclass = "sf")

# Plotting plain map, with zoom (i.e. coord_sf) in the latin american region
ggplot(data = america) +
  geom_sf() +
  coord_sf(xlim = c(-120,-33), ylim = c(-60,35), expand = FALSE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), #without crossed lines
        panel.grid.minor = element_blank())

#------------------------------------------------------------------------------#
### Risk Perception - Spatial Dimension ########################################
#------------------------------------------------------------------------------#
Base <- Base %>% mutate(rpd_median = median(rpd, na.rm = TRUE),
                        rpd_mean = mean(rpd, na.rm = TRUE),
                        rpd_abv_med = ifelse(rpd >= rpd_median, 1, 0),
                        rpd_abv_mean = ifelse(rpd > rpd_mean, 1, 0),
                        rpd_abv_3 = ifelse(rpd >= 3, 1, 0))

# Creating cross table of percentage of respondents who have an above the mean (or median) risk perception (spatial dimension)
spatial_risk <- crosstab(df = Base, x = Pais, y = rpd_abv_3, weight = ponde2) %>%
  mutate(name = case_when(
    Pais == 1 ~ "Brazil",
    Pais == 2 ~ "Argentina",
    Pais == 3 ~ "Chile",
    Pais == 4 ~ "Colombia",
    Pais == 5 ~ "Ecuador",
    Pais == 6 ~ "Mexico",
    Pais == 7 ~ "Peru",
  )) %>%
  rename(spatial_risk = '1') %>%
  dplyr::select(name, spatial_risk) 

# Joining risk perception (spatial dimension) data with america df
america <- america %>% full_join(spatial_risk, by = "name")

# Plotting map 
ggplot(data = america) +
  geom_sf(aes(fill = spatial_risk), color = "#808080") +  #
  coord_sf(xlim = c(-120,-33), ylim = c(-60,35), expand = FALSE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 8.5),
        legend.spacing.y = unit(0.4, 'cm')) +
  scale_fill_binned(name = "% of respondents that \n believe climate change will \n have widespread effects",
                    type = "viridis",
                    breaks = c(55, 65, 75, 85, 90, 95),
                    limits = c(55, 100),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE),
                    alpha = 0.8) +
   geom_sf_label(aes(label = round(spatial_risk, 1)), size = 3, label.padding = unit(0.20, "lines"), label.size = 0.1, alpha = 0.75)

#------------------------------------------------------------------------------#
### Risk Perception - Temporal Dimension #######################################
#------------------------------------------------------------------------------#

# Creating cross table of percentage of respondents
timing_risk <- crosstab(df = Base, x = Pais, y = rpt, weight = ponde2) %>%
  mutate(name = case_when(
    Pais == 1 ~ "Brazil",
    Pais == 2 ~ "Argentina",
    Pais == 3 ~ "Chile",
    Pais == 4 ~ "Colombia",
    Pais == 5 ~ "Ecuador",
    Pais == 6 ~ "Mexico",
    Pais == 7 ~ "Peru",
  )) %>%
  rename(rpt_6 = '6',
         rpt_5 = '5',
         rpt_4 = '4') %>%
  mutate(timing_risk = rpt_6 + rpt_5 + rpt_4) %>%
  dplyr::select(name, timing_risk)

# Joining belief data with america df
america <- america %>% full_join(timing_risk, by = "name")

# Plotting map
ggplot(data = america) +
  geom_sf(aes(fill = timing_risk), color = "#808080") +  
  coord_sf(xlim = c(-120,-33), ylim = c(-60,35), expand = FALSE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 8.5),
        legend.spacing.y = unit(0.4, 'cm')) +
  scale_fill_binned(name = "% of respondents who \n believe climate change \n will have negative \n effects in 25 years or less",
                    type = "viridis",
                    breaks = c(55, 65, 75, 85, 90, 95),
                    limits = c(55, 100),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE),
                    alpha = 0.8) +
  geom_sf_label(aes(label = round(timing_risk, 1)), size = 3, label.padding = unit(0.20, "lines"), label.size = 0.1, alpha = 0.75)


#------------------------------------------------------------------------------#
### Risk Perception ############################################################
#------------------------------------------------------------------------------#
Base <- Base %>% mutate(risk_median = median(rp, na.rm = TRUE),
                        risk_mean = mean(rp, na.rm = TRUE),
                        risk_abv_med = ifelse(rp >= rpd_median, 1, 0),
                        risk_abv_mean = ifelse(rp > rpd_mean, 1, 0),
                        risk_abv_3 = ifelse(rpd >= 3, 1, 0))

# Creating cross table of percentage of respondents who have an above the mean risk perception
risk <- crosstab(df = Base, x = Pais, y = risk_abv_3, weight = ponde2) %>%
  mutate(name = case_when(
    Pais == 1 ~ "Brazil",
    Pais == 2 ~ "Argentina",
    Pais == 3 ~ "Chile",
    Pais == 4 ~ "Colombia",
    Pais == 5 ~ "Ecuador",
    Pais == 6 ~ "Mexico",
    Pais == 7 ~ "Peru",
  )) %>%
  rename(risk = '1') %>%
  dplyr::select(name, risk) 

# Joining belief data with america df
america <- america %>% full_join(risk, by = "name")

# Plotting map 
ggplot(data = america) +
  geom_sf(aes(fill = risk), color = "#808080") +  
  coord_sf(xlim = c(-120,-33), ylim = c(-60,35), expand = FALSE) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.title = element_text(size = 8.5),
        legend.spacing.y = unit(0.4, 'cm')) +
  scale_fill_binned(name = "% of respondents with \n high risk perception",
                    type = "viridis",
                    breaks = c(55, 65, 75, 85, 90, 95),
                    limits = c(55, 100),
                    guide = guide_coloursteps(even.steps = TRUE,
                                              show.limits = TRUE),
                    alpha = 0.8)+
  geom_sf_label(aes(label = round(risk, 1)), size = 3, label.padding = unit(0.20, "lines"), label.size = 0.1, alpha = 0.75)


