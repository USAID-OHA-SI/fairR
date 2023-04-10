# PROJECT:  Equity Analysis
# AUTHOR:   P. Ranade | USAID
# PURPOSE:  Visualize disparities
# LICENSE:  MIT
# DATE:     2023-04-10

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glitr)
library(glamr)
library(gophr)
library(gagglr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(gophr)
library(janitor)
library(lubridate)
library(gt)
library(mindthegap)
library(summarytools)


# GLOBAL VARIABLES --------------------------------------------------------

# authors <- c("Prasann Ranade", "Nada Petrovic", "Karishma Srikanth")
# ref_id <- 00000

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY21-23") %>% 
  read_psd()  
get_metadata()

#pull unaids numbers
df_est <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Estimates", 
                      pepfar_only = TRUE)
df_tt <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Test & Treat", 
                     pepfar_only = TRUE)

# MUNGE -------------------------------------------------------------------

#get USAID indicator numbers by sex
df_tx <- df %>%
  filter(sex %in% c("Male", "Female"),
         indicator %in% c("TX_CURR", "TX_NEW", "HTS_TST_POS"),
         country == "Guatemala",
         funding_agency == "USAID",
         fiscal_year == 2022) %>% 
  group_by(country, indicator, sex) %>% 
  summarise(across(cumulative, sum, na.rm = TRUE)) %>%
  mutate(freq = round((cumulative / sum(cumulative)*100),2)) %>% 
  mutate(sex_color = ifelse(sex == "Female", denim, old_rose)) %>% 
  ungroup()

# get UNAIDS PLHIV estimates by sex and extend to 6 entries
df_plhiv <- df_est %>% 
  filter(indicator %in% c("Number PLHIV"),
         country %in% c("Guatemala"),
         year == 2021,
         age == "15+") %>% 
  group_by(country, indicator, sex) %>% 
  summarize(across(estimate, sum, na.rm=TRUE)) %>% 
  mutate(freq = round((estimate / sum(estimate)*100),2)) %>% 
  ungroup() 

# Duplicate df_plhiv to match df_tx
# df_plhiv <- df_plhiv %>% 
#   rbind(df_plhiv) %>% 
#   rbind(df_plhiv)

# add in reference column for geom_segment
df_tx <- df_tx %>% 
  mutate(plhiv_freq = rev(df_plhiv$freq))

# VISUALIZE-----------------------------------------------------

#visualize dumbbell plot and line segments
df_viz <- ggplot(df_tx) + 
  geom_vline(xintercept = unlist(df_plhiv$freq), linetype = "dotted") +
  geom_point(aes(x=freq, y=indicator), size = 3, color = df_tx$sex_color, show.legend = TRUE) +
  geom_text(aes(df_tx$freq,df_tx$indicator), label = round(df_tx$freq), nudge_y = .2) +
  geom_segment(x = df_tx$plhiv_freq, xend = df_tx$freq, y = df_tx$indicator, yend = df_tx$indicator) +
  labs(title = glue("HTS_TST_POS DISPLAYS THE GREATEST DISPARITY IN TREATMENT BY GENDER"),
       subtitle = glue("Treatment by  <span style = 'color: #2057a7;'>Female</span> and <span style = 'color: #c43d4d;'>Male</span> in {df_tx$country[1]} <br>
                       Dotted line represents UNAIDS PLHIV estimates for Female and Male"),
       caption = glue("Source: MER {metadata$curr_fy_lab}")) +
  si_style_xgrid() +
  theme(plot.subtitle = element_markdown())  

plot(df_viz)

#save plot
si_save("Images/equity_sex_guatemala.png")
si_save("Graphics/equity_sex_guatemala.svg")


  
  