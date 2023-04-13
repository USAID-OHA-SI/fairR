# PROJECT:  Equity Analysis
# AUTHOR:   P. Ranade | USAID
# PURPOSE:  Visualize disparities
# LICENSE:  MIT
# DATE:     2023-04-10

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(mindthegap)
library(glue)
library(ggtext)
library(extrafont)

# GLOBAL VARIABLES --------------------------------------------------------

# authors <- c("Prasann Ranade", "Nada Petrovic", "Karishma Srikanth")
# ref_id <- 00000

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY21-23") %>% 
  read_psd()  
get_metadata()

#pull unaids estimates
df_est <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Estimates", 
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
  mutate(sex_color = ifelse(sex == "Female", moody_blue, genoa)) %>% 
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
df_plhiv <- df_plhiv %>%
  rbind(df_plhiv) %>%
  rbind(df_plhiv)

# add in reference column for geom_segment
df_tx <- df_tx %>% 
  mutate(plhiv_freq = rev(df_plhiv$freq))

# VISUALIZE-----------------------------------------------------
#visualize dumbbell plot and line segments
df_viz <- ggplot(df_tx) + 
  geom_vline(xintercept = df_plhiv$freq[1], linetype = "dotted", color = genoa) +
  geom_vline(xintercept = df_plhiv$freq[2], linetype = "dotted", color = moody_blue) +
  geom_point(aes(x=freq, y=indicator), size = 3, color = df_tx$sex_color, show.legend = TRUE) +
  geom_text(aes(df_tx$freq,df_tx$indicator), label = round(df_tx$freq), size = 4, color = trolley_grey, nudge_y = .2, family = "Source Sans Pro") +
  geom_segment(x = df_tx$plhiv_freq, xend = df_tx$freq, y = df_tx$indicator, yend = df_tx$indicator) +
  labs(title = glue("HTS_TST_POS DISPLAYS THE GREATEST DISPARITY BY SEX"),
       subtitle = glue("Indicator results and UNAIDS PLHIV estimates by  <span style = 'color: #8980cb;'>
                        Female</span> and <span style = 'color: #287c6f;'>Male</span>
                        in {df_tx$country[1]}"),
       caption = glue("Source: MER {metadata$curr_fy_lab}"),
       x = "% Share of PLHIV by Sex",
       y = "") +
  si_style_xgrid() +
  scale_x_continuous(breaks = seq(10, 90, by = 10)) +
  theme(plot.subtitle = element_markdown())  

plot(df_viz)

#save plot
si_save("Images/equity_sex_guatemala.png")
si_save("Graphics/equity_sex_guatemala.svg")


  
  