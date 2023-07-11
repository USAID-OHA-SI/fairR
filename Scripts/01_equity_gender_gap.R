# PROJECT: 
# PURPOSE: Munge and Analysis of
# AUTHOR: Prasann Ranade & Nada Petrovic| SI
# REF ID:   45b16bac
# LICENSE: MIT
# DATE: 2023-01-20
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
  library(tidyverse)
  library(glamr)
  library(glue)
  library(glitr)
  library(extrafont)
  library(scales)
  library(gophr)
  library(mindthegap)
  library(ggtext)

  ## QC'd with Panorama 6/23 using sex/age disagg in single OU table

  #table of age/sex disaggs
  df_disaggs <- tibble::tribble(
  ~indicator,      ~standardizeddisaggregate,
  "HTS_TST_POS",      "Modality/Age/Sex/Result",
  "TX_CURR",            "Age/Sex/HIVStatus",
  "TX_NEW",            "Age/Sex/HIVStatus",
  "TX_PVLS", "Age/Sex/Indication/HIVStatus",
  "PrEP_NEW",                      "Age/Sex")

  # Reference ID to be used for searching GitHub
  ref_id <- "3a7ad849"
  
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "OU_IM_FY21-23")
      
  # Grab metadata
   msd_source <- source_info(file_path)
   get_metadata() 
   
  # Read msd
   df_msd <- read_psd(file_path) 
   
  # Pull UNAIDS estimates
   df_est <- pull_unaids(orginal_unaids = TRUE, data_type = "HIV Estimates", 
                         pepfar_only = TRUE)
   
   # get UNAIDS PLHIV estimates by sex 
   df_plhiv <- df_est %>% 
     filter(indicator %in% c("Number PLHIV"),
            country %in% c("Guatemala"),
            year == 2021, age=="15+") %>% 
     group_by(country, indicator, sex) %>% 
     summarize(across(estimate, sum, na.rm=TRUE),.groups="drop") %>%
     pivot_wider(names_from = sex, values_from = estimate) %>%
     rename(Total=All) %>%
     mutate(Male=Total-Female) %>%
     mutate(Female=round(Female/Total,2), Male=round(Male/Total,2)) %>%
     select(-c("Total","indicator")) %>%
     pivot_longer(cols = Female:Male,
                  names_to = "sex",
                  values_to = "freq_UNAIDS") 
   
     unaids_female<-df_plhiv$freq_UNAIDS[df_plhiv$sex=="Female"]
     unaids_male<-df_plhiv$freq_UNAIDS[df_plhiv$sex=="Male"]

 ## Parameters
     cntry_sel<-"Guatemala"
     fisc_yr_sel<-2023
     
        
 ### Filter by indicatorlist
  df_msd <- df_msd %>% clean_indicator() %>%
  semi_join(df_disaggs, by = c("indicator", "standardizeddisaggregate"))  
   
  #get USAID indicator numbers by sex
    df_tx <- df_msd %>%
    filter(country == cntry_sel,
           funding_agency == "USAID",
           fiscal_year == fisc_yr_sel) %>% 
    group_by(country, indicator, sex) %>% 
    summarise(across(cumulative, sum, na.rm = TRUE),.groups="drop") %>%
    group_by(country, indicator) %>% 
    mutate(freq = round((cumulative / sum(cumulative)),2)) %>% 
    mutate(sex_color = ifelse(sex == "Female", moody_blue, genoa)) %>% 
    ungroup() %>%
    right_join(df_plhiv, by=c("country", "sex")) %>%
    mutate(indicator = factor(indicator, 
              levels = c("TX_PVLS", "TX_CURR", "TX_NEW", "HTS_TST_POS", "PrEP_NEW")))
    
    # VISUALIZE-----------------------------------------------------
    #visualize dumbbell plot and line segments
    df_viz <- df_tx %>%
      ggplot() +
      geom_point(aes(freq, indicator, color = sex), 
                size = 3, show.legend = FALSE) +
      geom_text(aes(freq, indicator, label = percent(freq)), 
                size = 4, color = trolley_grey, nudge_y = .2, nudge_x = 0, 
                family = "Source Sans Pro") +
      geom_richtext(aes(unaids_female, 5, label=glue("<span style = 'color: #8980cb;'> {percent(unaids_female)} PLHIV are <br> Female</span>")), 
                size = 4, color = trolley_grey, label.color = NA, nudge_y = .3, nudge_x = -.08, 
                family = "Source Sans Pro") +
      geom_richtext(aes(unaids_female, 5, label=glue("<span style = 'color: #287c6f;'> {percent(unaids_male)} PLHIV are <br> Male</span>")), 
                    size = 4, color = trolley_grey, label.color = NA, nudge_y = .3, nudge_x = .22, 
                    family = "Source Sans Pro") +
      geom_segment(aes(x = freq, xend = freq_UNAIDS, 
                   y = indicator, yend = indicator), color = trolley_grey) +
      geom_vline(xintercept = unaids_female, linetype = "dashed", color = moody_blue, linewidth = 1) +
      geom_vline(xintercept = unaids_male, linetype = "dashed", color = genoa, linewidth = 1)+ 
      si_style_xgrid(facet_space = 0.75)+ 
      scale_color_manual(values = c("Female" = moody_blue, "Male" = genoa),
                                                             labels = function(x) str_to_upper(x)) + 
      labs(title = glue("GREATER PROPORTION OF FEMALES LIVE WITH HIV THAN FOUND IN INDICATOR COHORTS"),
           subtitle = glue("FY{str_sub(fisc_yr_sel, start = 3, end=4)} indicator results and UNAIDS PLHIV estimates for <span style = 'color: #8980cb;'>
                        Females</span> and <span style = 'color: #287c6f;'>Males</span>
                        in {df_tx$country[1]}"),
           caption = glue("Sources: {msd_source}, UNAIDS Estimates 2021 | USAID/OHA/SIEI | Ref ID: {ref_id}"),
           x = "% Share by Sex",
           y = "") +
      si_style_nolines() +
      scale_x_continuous(breaks = seq(0, 1, by = .1), labels=label_percent()) +
      theme(plot.subtitle = element_markdown())  
    
    plot(df_viz)
    

    si_save("Images/Equity_Indicators_vs_PLHIV_Female_Male.png")
    
    
    
    
# caption = glue::glue("Source: {source} | {author} \n created on: {Sys.Date()}"))
      
    
    
     
    
    
  


