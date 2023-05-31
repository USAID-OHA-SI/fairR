# PROJECT:  Equity Analysis
# AUTHOR:   P. Ranade | USAID
# PURPOSE:  KP and Age Facet Plots
# LICENSE:  MIT
# DATE:     2023-04-10

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(gagglr)
library(extrafont)
library(glue)
library(ggtext)
library(mindthegap)
library(scales)

# GLOBAL VARIABLES --------------------------------------------------------

#authors <- c("Prasann Ranade", "Nada Petrovic", "Karishma Srikanth")
#ref_id <- "0530547f"
#vrsn <- 1

# IMPORT ------------------------------------------------------------------

df <- si_path() %>% 
  return_latest("OU_IM_FY21-23") %>% 
  read_psd()  
get_metadata()

#pull unaids estimates
df_est <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Estimates", 
                      pepfar_only = TRUE)

# MUNGE -------------------------------------------------------------------

# filter to country, indicators, and disaggregates
ind_sel <- c("TX_NEW", "TX_CURR", "TX_PVLS", "PrEP_NEW", "PrEP_CT")

df_prep <- df %>% 
  filter(country == "Guatemala")

df_prep <- df_prep %>% 
  filter(
    standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "KeyPop/HIVStatus"),
    indicator %in% ind_sel,
    funding_agency=="USAID",
    fiscal_year >= 2021) 

#ageasentered %in% c("35-39","40-44","45-49","50+") ~ "35+",
df_prep <- df_prep %>% 
  # Combine age groups above 35+, rename People in Prisons
  mutate(ageasentered = case_when(ageasentered %in% c("<01","01-04","05-09", "10-14") ~ "<15",
                                  ageasentered %in% c("45-49","50-54","50+", "55-59", "60-64", "65+") ~ "45+", 
                                  TRUE ~ ageasentered),
         otherdisaggregate = case_when(otherdisaggregate %in% "People in prisons and other enclosed settings" ~ "Prisoners", 
                                       TRUE ~ otherdisaggregate)) %>%
  # Create index for faceting - either age or KP group
  mutate(facet_ind=case_when(standardizeddisaggregate == "Age/Sex/HIVStatus" ~ ageasentered, 
                             TRUE ~ otherdisaggregate)) %>%
  filter(facet_ind!="Unknown Age") %>%
  # Fill sex indicator with "KP" value for KP - will be used for color
  mutate(sex = case_when(is.na(sex)==TRUE  ~ "KP", TRUE ~ sex)) %>%
  group_by(fiscal_year, country, funding_agency, indicator, standardizeddisaggregate, sex, ageasentered, otherdisaggregate, facet_ind) %>% 
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop"
  ) %>% 
  reshape_msd() %>% 
  select(-period_type) %>% 
  arrange(period) 

df_plhiv <- df_est %>% 
  filter(indicator %in% c("Number PLHIV"),
         country %in% c("Guatemala"),
         year >= 2021) %>% 
  group_by(indicator, sex, age, estimate) %>%  
  ungroup() 

# plot only one indicator at a time
df_facet <- df_prep %>% 
  filter(indicator == "TX_NEW") 

# VISUALIZE --------------------------------------------------------------------

pd_brks <- unique(df_prep$period) %>% str_replace(".*(2|4)$", "")

# Add facets and labels
df_viz <- df_facet %>% 
  ggplot(aes(x=period, y=value)) + 
  geom_point(aes(color=sex), size = 4, alpha = .8) +
  geom_line(aes(group = sex, color=sex), linewidth=1, alpha = .8) +
  scale_x_discrete(labels = pd_brks) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Female" = moody_blue, "Male" = genoa, "KP"=burnt_sienna),
                     labels = function(x) str_to_upper(x)) +
  facet_wrap(facet_ind ~ ., scales = "fixed", ncol=4, dir="h") +
  geom_hline(data = df_plhiv, aes(yintercept = estimate[4]/100), linetype = "dashed") +
  si_style_xyline() +
  theme(strip.text.x = element_text(size = 12, face="bold"),
        axis.text.x = element_text(size=6),
        axis.text.y = element_text(size=10, angle=0))+
  theme(plot.subtitle = element_markdown()) +
  theme(legend.position = "none") +
  coord_cartesian(clip="off") +
  labs(x = NULL, y = NULL,
       title = glue("Are treatment numbers relatively consistent across demographics?"),
       subtitle = glue("USAID/Guatemala TX_NEW 
                      by <span style = 'color: #8980cb'>Female</span>, 
                      <span style = 'color: #287c6f;'>Male</span>, and
                      <span style = 'color: #e07653;'>Key Populations</span>"),
       caption = glue("Dashed line indicates UNAIDS PLHIV Estimates \n
                      {metadata$caption} | USAID/OHA/SIEI"))

plot(df_viz)

#save plot
si_save("Images/equity_disagg_guatemala_tx_new.png")
si_save("Graphics/equity_disagg_guatemala.svg")
