## PROJECT: COP_UGA
## AUTHOR:  N. Petrovic | USAID
## PURPOSE: DSD Analysis - descriptive analysis viz
## LICENSE: MIT
## DATE:    2022-02-23


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(glitr)
library(gophr)
library(extrafont)
library(scales)
library(tidytext)
library(glue)
library(janitor)
library(ggtext)
library(extrafont)
library(patchwork)
library(waffle)

# Reference ID to be used for searching GitHub
ref_id <- "c125e3f4"

## QC'ed with PANO for TX_CURR & TX_NEW


#IMPORT ------------------------------------------------------------------

#table of age/sex disaggs
df_disaggs <- tibble::tribble(
  ~indicator,      ~standardizeddisaggregate,
  "HTS_TST_POS",      "Modality/Age/Sex/Result",
  "TX_CURR",            "Age/Sex/HIVStatus",
  "TX_NEW",            "Age/Sex/HIVStatus",
  "TX_PVLS", "Age/Sex/Indication/HIVStatus",
  "PrEP_NEW",                      "Age/Sex")

# Load NATSUBNAT data set & get metadata
load_secrets()
merdata <- file.path(glamr::si_path("path_msd"))
file_path <- return_latest(folderpath = merdata,
                           pattern = "NAT_SUBNAT_FY21-23")
df_msd_natsubnat <- read_psd(file_path)
msd_source <- source_info(file_path)
get_metadata()
metadata_natsubnat<-metadata

#Load MER & get metadata
file_path <- return_latest(folderpath = merdata,
                           pattern = "OU_IM_FY21-23")
df_msd_all <- read_psd(file_path) 
msd_source <- source_info(file_path)
get_metadata() 


#### Select Filters

#ind_sel<-"TX_CURR"
#ind_sel<-"TX_NEW"
#ind_sel<- "HTS_TST_POS" 
#ind_sel<-"TX_PVLS"
ind_sel<-"PrEP_NEW" 
cntry_sel<-"Guatemala"
fisc_yr_sel<-2023
fund_ag_sel<-"USAID"
  
  
############### MUNGE

df_age_natsubnat <- df_msd_natsubnat %>% filter(country==cntry_sel, indicator=="PLHIV", fiscal_year==fisc_yr_sel) %>%
  filter(is.na(age_2019)==FALSE) %>%
  group_by(age_2019) %>%
  summarise(across(targets, sum),.groups="drop") 

df_msd <- df_msd_all %>%
  semi_join(df_disaggs, by = c("indicator", "standardizeddisaggregate"))   

df_msd <- df_msd %>% filter(country==cntry_sel, indicator==ind_sel, fiscal_year==fisc_yr_sel, funding_agency==fund_ag_sel) %>%
  group_by(age_2019) %>%
  summarise(across(cumulative, sum, na.rm=TRUE),.groups="drop") 

df_age <- df_msd %>%
  inner_join(df_age_natsubnat, by = c("age_2019")) %>%
  rename(mer_result=cumulative, unaids_est=targets) %>%
  mutate(mer_result=mer_result/sum(mer_result), unaids_est=unaids_est/sum(unaids_est)) %>%
  mutate(diff=mer_result-unaids_est) %>%
  mutate(path_color=case_when(abs(diff)>0.05 ~ trolley_grey,
                              TRUE ~ trolley_grey_light)) %>%
  mutate(label_left=case_when(age_2019 %in% c("<01","01-04","05-09")~NA,
                              diff<0 ~ mer_result, 
                              diff>0 ~ unaids_est)) %>%
  mutate(label_right=case_when(age_2019 %in% c("<01","01-04","05-09")~NA,
                              diff<0 ~ unaids_est, 
                              diff>0 ~ mer_result)) %>%
  pivot_longer(cols=c(mer_result,unaids_est), names_to="type") 
  
 

#DESCRIPTIVE ANALYTICS ---------------------------------------------------

df_age %>% 
  ggplot(aes(value, age_2019)) +
  geom_path(aes(color = path_color),linewidth=1) +
  geom_point(aes(fill = type, color = "white"), shape = 21, size=5, show.legend = FALSE ) +
  geom_text(aes(x=label_left, label = percent(label_left,1)), family = "Source Sans Pro", 
            color = trolley_grey, hjust = 1.5, na.rm = TRUE) +
  geom_text(aes(x=label_right, label = percent(label_right,1)), family = "Source Sans Pro", 
            color = trolley_grey, hjust = -0.5, na.rm = TRUE) +
  scale_fill_manual(values = c("mer_result" = burnt_sienna, "unaids_est" = scooter)) +
  scale_color_identity() +
  #expand_limits(x = 1) +
  si_style_xgrid() +
  labs(title = glue("{toupper(fund_ag_sel)} FY{str_sub(fisc_yr_sel, start = 3, end=4)} AGE BREAKDOWN FOR <span style = 'color: #e07653; font-weight: bold'>
                        {ind_sel} </span> COMPARED TO <span style = 'color: #1e87a5; font-weight: bold'> TOTAL PLHIV</span>
                        IN {toupper(cntry_sel)}"),
       caption = glue("Sources: {msd_source} and NAT_SUBNAT| USAID/OHA/SIEI | Ref ID: {ref_id}"),
       x = "Percent share by age group",
       y = "") +
  scale_x_continuous(breaks = seq(0, 1, by = .1), labels=label_percent()) +
  theme(
    panel.grid.major.y = element_blank(),
    #axis.text.x = element_blank(),
    plot.title = element_markdown()
  )

si_save(glue("Equity_Indicators_vs_PLHIV_age_bands_{cntry_sel}_{fisc_yr_sel}_{ind_sel}.png"), path = "Images")


#df_msd_natsubnat %>% filter(indicator=="PLHIV", fiscal_year=="2022") %>%
#filter(is.na(sex)==FALSE) %>% 
#group_by(sex) %>% 
#summarise(across(targets, sum),.groups="drop") %>% 
#mutate(freq = round((targets / sum(targets)),3))

