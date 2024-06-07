## ---------------------------------------------------------------------------------------------------------------------------
## Purpose: Get age composition of OSO (wild) N-O spawners from SiRE/ONA deadpitch data
##          and age composition from CRITFC sources for annual comparison of % at ocean age.
## Notes:   Pulls data directly from raw but 'tidied' ONA spawning ground (SG) fish samples in ok_riv_deadpitch
##          (which was previously input for thermal-mark analysis).  
##          Applies multi-year average (unknown yearspan - derived by Stockwell and Hyatt -- TBD)
##          for some years where age comp NA (e.g., 1980-1984, 1986)
##          Obtains CRITFC age composition proportions (Jeff Fryer).
##          Compares "past" annual ocean age comp assignment (Hyatt & Stockwell) with a "CRITFC-preferred"
##          annual age composition assignment, and a "ONA-preferred" assignment from SG data.
## Issues:  see statements where triple hash marks (###) exist.
## ---------------------------------------------------------------------------------------------------------------------------

work <- "C:\\Users\\StiffH\\Documents\\Rcode\\Osoyoos_recruits_SAR"             # working directory

filename <- paste(work, "\\DATA\\ok_riv_deadpitch_xlsx_240524.csv", sep = "")
ok_riv_deadpitch <- read.csv2(filename, sep=",") 

age_sample_lower <- ok_riv_deadpitch %>%                            # get lower Ok River Sockeye biosample data (SiRE/ONA) for age composition estimate...
  filter(fish == "sockeye") %>%        
  filter(section != "Skaha") %>%                                    # remove mid-OkRiver (Skaha) samples
  filter(section != "Penticton Channel") %>%                        # remove redundant Pen channel data 2017-18 as that is dup in Penticton dataset
  filter(age != "Unknown") %>%                                      # removing all rows where age is Unknown  (2000-2021)
  filter(!(age %in% c(1, 2, 3, 4, 5))) %>%                          # removing all rows where age is FW age only  (2000-2021)
  mutate(age = substr(age,1,3)) %>%                                 # cleanup age data (e.g. change 1.10000001 to 1.1)
  dplyr::select(year, thermal_mark, age, section, fish) %>% 
  mutate(section = ifelse(section == "Lower Okanagan", "Lower Ok River", "Middle Ok River")) %>% 
  arrange(section, year, age)
  
age_composition_natural <- age_sample_lower %>%                     # get age composition of N-O fish in lower river
  group_by(year, age, thermal_mark) %>%                             # by year and age group
  summarize(n = n()) %>%                                            # for all data where thermal_mark is NA (2000-2006) or Natural (2007-end)
  mutate(thermal_mark = ifelse(year == 2019, "Natural",             # no thermal marking in 2019 data (all Unknown),  
                               thermal_mark)) %>%                   # so reassign t_m in 2019 to "Natural" since most fish in lower river are N-O
  filter(thermal_mark %in% c(NA, "Natural")) %>%                    # removing all rows where thermal mark is Unknown or Hatchery (2007-2021)
  pivot_wider(names_from=age, names_prefix="age_", values_from=n)   # tabulate by year and age group

missing_ages_2006 <- data.frame(year = 2006, age_1.1=1,             ### use summary at age data from ONA (thanks Sam P 240527) for missing age comp in 2006
                                age_1.2=371, age_1.3=46, age_2.2=2, ### will need to incorporate 2006 raw data into source data later...
                                thermal_mark="Natural") 
missing_ages_2022 <- data.frame(year = 2022, age_1.1=178,           ### use summary % at age data from ONA (from AO 240322) for missing age comp in 2022
                                age_1.2=765, age_1.3=57,            ### will need to incorporate 2022 raw data into source data later...THESE ARE PERCENTx10, NOT SAMPLE SIZES
                                thermal_mark="Natural")            
missing_ages_2023 <- data.frame(year = 2023, age_1.1=89,            ### use summary % at age data from ONA (from AO 240322) for missing age comp in 2023
                                age_1.2=812, age_1.3=96, age_2.2=3, ### will need to incorporate 2023 raw data into source data later...THESE ARE PERCENTx10, NOT SAMPLE SIZES
                                thermal_mark="Natural")    

missing_ages_1980 <- data.frame(year = 1980, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1980-1984, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,            ### need to find out what is the source of this multi-year average age comp used for 980-1984... ###  
                                thermal_mark=NA)    
missing_ages_1981 <- data.frame(year = 1981, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1980-1984, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,             
                                thermal_mark=NA)    
missing_ages_1982 <- data.frame(year = 1982, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1980-1984, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,             
                                thermal_mark=NA)    
missing_ages_1983 <- data.frame(year = 1983, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1980-1984, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,             
                                thermal_mark=NA)    
missing_ages_1984 <- data.frame(year = 1984, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1980-1984, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,             
                                thermal_mark=NA)    
missing_ages_1986 <- data.frame(year = 1986, age_1.1=52,            ### use multi-year average PERCENTx10 for missing age comp in 1986, THESE ARE NOT SAMPLE SIZES 
                                age_1.2=825, age_1.3=83, 
                                age_2.1=23,  age_2.2=16,             
                                thermal_mark=NA)    

age_composition <- bind_rows(age_composition_natural, 
                             missing_ages_1980, missing_ages_1981, 
                             missing_ages_1982, missing_ages_1983, 
                             missing_ages_1984, missing_ages_1986,  # 1980-1984 assigned multi-year avg
                             missing_ages_2006, missing_ages_2022,
                             missing_ages_2023) %>%                 ### concatenate annual age comp data with provided numbers at age for 2006
  arrange(year)

age_composition$thermal_mark <- NULL                                # drop thermal_mark variable (which were all NA or Natural)
age_composition[is.na(age_composition)] <- 0                        # set NAs to 0 in pivot table
age_composition_setup <- age_composition %>% 
  rename(return_year = year) %>%
  dplyr::select(return_year, age_1.1, age_1.2, age_1.3, 
                age_1.4, age_2.1, age_2.2) %>%                      ### careful: this COLUMN SORT drops age_2.3 or older if these ages ever exist
  rowwise() %>% 
  mutate(total = sum(c_across(everything())))                       # Calculate the sum of N for each row

age_composition_ONA_wide <- age_composition_setup %>% 
  mutate(across(everything(), ~ . / total, .names = "prop_{.col}")) %>%         # Calculate the annual proportions for each age column
  mutate(source = "ONA")

age_comp_ONA_long <- age_composition_ONA_wide %>%                               # Pivot to list age composition by return year and age class
  pivot_longer(cols = c("prop_age_1.1", "prop_age_1.2", "prop_age_1.3",         ### caution: explicit selection of age classes may leave rare age classes 
                        "prop_age_1.4", "prop_age_2.1", "prop_age_2.2"),        ###          out in some years
               names_to = "age_class", values_to = "ONA_prop_at_age") %>%
  mutate(ONA_method = ifelse(return_year>=1987, "Deadpitch","MultiYearAvg")) %>%# Classify annual age comp data methods
  dplyr::select(-source, -total, -prop_total, -age_1.1, -age_1.2, -age_1.3,     # Drop sample size columns 
                -age_1.4, -age_2.1, -age_2.2)

## ------------------------------------------------------------------------------
## Purpose: Get age composition of Okanagan-bound SK from CRITFC data (Jeff Fryer)
## Notes:   Years 2006-present come from PIT-tag data confirmed Okanagan-bound SK
##          Years <2006 come from scale data (SPA), usually mixed-stock data from
##          Bonneville Dam that have been tweaked by JF to represent Oka-bound SK
##          by removing likely contributions of larger WEN SK... to be confirmed.
## ------------------------------------------------------------------------------

age_comp_critfc <- read_xlsx("C:\\DFO-MPO\\OneDrive\\OneDrive - DFO-MPO\\Sockeye Index Stocks\\Okanagan\\Harvest\\OKA Returns at Age from JF 24.05.27.xlsx",
          sheet = "CRITFC Age Comp", na="")                                     # read CRITFC age comp from workbook in OneDrive      ### fix folder spec
filename <- paste(work, "\\DATA\\age_comp_critfc_240527.csv", sep = "")         # CSV filename for CRITFC age composition proportions ### fix folder spec
write.csv(age_comp_critfc, filename)                                            # saves the data to filename

age_comp_critfc <- read.csv2(filename, sep=",")                                 # input saved CRITFC annual age composition  
age_comp_critfc_wide <- age_comp_critfc %>%    
  rename(return_year = Year) %>%
  dplyr::select(-Other,-TOTAL,-Confirmed,-Source,-Notes,-X,-Sample_Size) %>%    # remove meta-data columns
  mutate(source = "CRITFC")                                                     # add source data tag

age_comp_CRITFC_long <- age_comp_critfc_wide %>%                                # pivot to list proportions by return year and age class
  pivot_longer(cols = c("prop_age_1.1", "prop_age_1.2", "prop_age_1.3",
                        "prop_age_2.1", "prop_age_2.2", "prop_age_2.3",
                        "prop_age_3.1", "prop_age_3.2", "prop_age_3.3",
                        "prop_age_4.1"), 
               names_to = "age_class", values_to = "CRITFC_prop_at_age") %>%
  mutate(CRITFC_method = Age_Method) %>%
  dplyr::select(-source, -Age_Method)
 
## -----------------------------------------------------------------------------

age_comp_ONA_CRITFC <- merge(age_comp_CRITFC_long, age_comp_ONA_long,           # Merge (full join) CRITFC and ONA age composition data by year and age class
                       by = c("return_year", "age_class"), all = TRUE) %>%
  filter(age_class != "prop_age_1.4")                                           ### this filters out a record in 2007 where age 1.4 < 0.3%... 

## -----------------------------------------------------------------------------
## NEXT section generates an annual list of 'best source' (ONA spawning grounds or CRITFC mainstem dam samples) for age composition for each year.
## First part creates (and then reads) the annual 'best source' tags in use currently, based on past assumptions and classifications in SIS-OSO workbook.
## Next part creates (and then reads) a revised 'best source' list that basically tags all years since 2006 with CRITFC as 'best source', given PIT-tag-based method improvements since 2006. 
## That leaves 2001 as the only "ONA" 'best source' in recent years, which might as well be assigned to CRITFC since the two sources have nearly identical age comps for the major ages.
## However, it would be worth reviewing 2002-2003, which were years where CRITFC showed a potentially problematic number of older ages (3.x, 4.x), which JF has suggested may be misread scales.

best_age_comp_source_long <- age_data %>% dplyr::select(return_year,best_source)# Assemble annual 'best source' tag for age comp - based on past assumptions
best_age_comp_source_past <- distinct(best_age_comp_source_long, return_year,
                                      .keep_all = TRUE)                         # Pull 'best source' from old age_data frame for now
best_age_comp_source_past$best_source <- gsub("SiRE", "ONA",
                                         best_age_comp_source_past$best_source) # Sub ONA for SiRE in 'best source' tag
best_age_comp_source_past <- best_age_comp_source_past %>%
  dplyr::select(return_year, best_source)

  filename <- paste(work, "\\DATA\\best_age_comp_source_past_240527.csv",       ### fix folder spec...
                    sep = "")                                                   # CSV filename for CRITFC age composition proportions
write.csv(best_age_comp_source_past, filename)                                  # saves the data to filename
best_age_comp_source_past <- read.csv(filename)                                 # read CSV file; this stmt needs filename stmt above to be uncommented
best_age_comp_source_past <- best_age_comp_source_past %>%
  dplyr::select(return_year, best_source_PAST = best_source)

#-------------------------------------------------------------------------------

best_age_comp_source_CRITFC <- best_age_comp_source_past                        # initialize CRITFC version, based on past best source
best_age_comp_source_CRITFC$best_source =
  ifelse(best_age_comp_source_CRITFC$return_year >= 2006, "CRITFC",             # assign CRITFC as 'best source' for all years >=2006, for which CRITFC
         best_age_comp_source_CRITFC$best_source)                               # age comp was derived from Bonneville PIT-tag based stock comp & aging
best_age_comp_source_CRITFC$best_source =
  ifelse(best_age_comp_source_CRITFC$return_year == 2023, "ONA",                # that leaves only 2001 in the time-series as ONA-source; but ONA==CRITFC age comp
         best_age_comp_source_CRITFC$best_source)                               # for that year for major ages, plus has some 2.x that did not show up on SG, so assign 2001<-CRITFC
best_age_comp_source_CRITFC$best_source =
  ifelse(best_age_comp_source_CRITFC$return_year == 2001, "CRITFC",             # that leaves only 2001 in the time-series as ONA-source; but ONA==CRITFC age comp
         best_age_comp_source_CRITFC$best_source)                               # for that year for major ages, plus has some 2.x that did not show up on SG, so assign 2001<-CRITFC
best_age_comp_source_CRITFC$best_source =
  ifelse(best_age_comp_source_CRITFC$return_year == 1986, "ONA MultiYr",        # and 1986 as ONA MultiYr source 
         best_age_comp_source_CRITFC$best_source)                                    

best_age_comp_source_CRITFC <- best_age_comp_source_CRITFC %>%
  dplyr::select(return_year, best_source)

filename <- paste(work, "\\DATA\\best_age_comp_source_CRITFC_240527.csv",       ### fix folder spec... 
                  sep = "")                                                     # CSV filename for CRITFC age composition proportions
write.csv(best_age_comp_source_CRITFC, filename)                                # saves the data to filename
best_age_comp_source_CRITFC <- read.csv(filename)                               # this stmt needs filename stmt above to be uncommented
best_age_comp_source_CRITFC <- best_age_comp_source_CRITFC %>%
  dplyr::select(return_year, best_source_CRITFC = best_source)

#-------------------------------------------------------------------------------

best_age_comp_source_ONA <- best_age_comp_source_past                           # initialize ONA-focused version, based on past best source
best_age_comp_source_ONA$best_source =
  ifelse(best_age_comp_source_ONA$return_year >= 2000, "ONA",                   # assign ONA as 'best source' for all years >=2000, i.e. use Spawn Grd data
         best_age_comp_source_ONA$best_source)                                  # where available...

filename <- paste(work, "\\DATA\\best_age_comp_source_ONA_240527.csv",          ### fix folder spec... 
                  sep = "")                                                     # CSV filename for CRITFC age composition proportions
write.csv(best_age_comp_source_ONA, filename)                                   # saves the data to filename
best_age_comp_source_ONA <- read.csv(filename)                                  # this stmt needs filename stmt above to be uncommented
best_age_comp_source_ONA <- best_age_comp_source_ONA %>%
  dplyr::select(return_year, best_source_ONA = best_source)

#-------------------------------------------------------------------------------

age_comp_source_a   <- merge(best_age_comp_source_past, 
                             best_age_comp_source_ONA,                          # Merge (full join) "PAST" best source and "ONA-focused" source for age composition data by year
                             by = c("return_year"), all = TRUE)
age_comp_source_b   <- merge(age_comp_source_a, 
                             best_age_comp_source_CRITFC,                       # Merge (full join) CRITFC with "PAST/ONA" source info for age composition data by year 
                             by = c("return_year"), all = TRUE)                 # Good dataset to visually compare annual best sources
age_comp_source_all <- merge(age_comp_ONA_CRITFC, 
                             age_comp_source_b,                                 # Merge (full join) annual age composition data by year and age class
                             by = c("return_year"), all = TRUE) %>%             # Use this to compare/contrast affect of different 'best source' series on age composition time-series
  mutate(ocean_age = str_sub(age_class, start = -1))
  
#------
# 
# This section defines three age composition (proportions) time-series based on three different combinations of the source data to review the differences.
# The source data are annual age compositions from either SiRE/ONA sampling on the Okanagan River spawning grounds (based on raw data in ok_riv_deadpitch), 
# or CRITFC sampling at mainstem dams (BON and/or WELLS), for which age proportions were provided by Jeff Fryer (CRITFC; see OKA Returns at Age from JF 24.05.27.xlsx) 
# and largely confirmed against annual CRITFC reports.
#
# The three time-series are based on different assignments of the two age comp sources to a given return year.
# 1. age_comp_from_past_best_source is based on historical age comp assignments (Hyatt & Stockwell) for which annual age comps were applied (usually) in the order of
#    ONA/SiRE Spawning Grounds before CRITFC, and in the absence of either of those, a multi-year average (not sure what years were used!).
#    This approach appears to be based on the idea that the SG data were the most representative, since in the early years, the CRITFC age comps were either based on 
#    multi-stock sampling (at BON), or size-biased sampling (at WELLS). Generally-speaking, this resulted in early years largely assigned to CRITFC, and later years (since 2006)
#    assigned to ONA, and missing years assigned to a multi-year average. Some years were assigned to one or the other source for other reasons, possible concerns
#    with SG data being derived from hatchery-brood-stock (possibly size-biased), small sample size, etc.
#    In any case, there are a few years where ages found in the CRITFC data are non-existent in the ONA data, suggesting the SG data may not be the most representative of recruitment.
#    This may be due to various reasons: mainstem fishery and enroute natural mortality selection of certain sizes/ages; limitations in spawning ground surveys, crew changes, etc.
# 2. Since 2006, however, CRITFC age comp has been refined, in that it is based on biosampling SPA at BON but for which the stock composition is verified via PIT detection. 
#    This presumably avoids the pitfalls of sampling the survivors in the terminal area, while also avoiding the noise introduced by mixed-stock summaries.
#    age_comp_from_CRITFC_best_source therefore reverses the selection process, assigning CRITFC age comp to return years where available ahead of ONA SG sources.
#    It should be noted that prior to PIT-tag-based data (<2006), Fryer provides 'adjusted' age composition proportions based on some method of tweaking of the mixed stock
#    SPA data from Bonneville. Thus the proportions provided by Fryer don't correspond exactly to the annual CRITFC report tables at least for 1998-2005 (NEEDS TO BE CHECKED WITH JEFF)
#    and some of the earliest years (pre-1998) may be mixed stock proportions (TO BE CHECKED WITH JEFF).
# 3. An ONA SG focused time-series is also generated that preferentially assigns SG age comp where available, followed by CRITFC and M-Yr. 
#    age_comp_from_ONA_best_source therefore differs from age_comp_from_ONA_best_past only in a few years. See age_comp_source_all above for annual comparison.
#    age_comp_from_ONA_best_source is of interest to examine differences in returns at age depending on primary location of age classification...

age_comp_from_past_best_source <- age_comp_source_all %>%                       # Assign age comp proportions to year based on recent PAST age comp assignments (circa Hyatt & Stockwell)
  mutate(best_age_comp = ifelse(best_source_PAST == "CRITFC",                   # as specified in best_source_PAST column
                                CRITFC_prop_at_age, ONA_prop_at_age)) %>%       # 
  mutate(best_method = ifelse(best_source_PAST == "CRITFC", CRITFC_method,      # annotate with method
                              ONA_method)) %>%                                  #  
  mutate(best_source = ifelse(best_source_PAST == "CRITFC", "CRITFC", "ONA"))%>%# annotate with source 
  dplyr::select(return_year, age_class, best_age_comp, 
                best_source, best_method) %>%                                   # drop unselected columns
  mutate_at(3, as.numeric, na.rm = TRUE)                                        # convert age comp column to numeric
  age_comp_from_past_best_source$best_age_comp[is.na
                          (age_comp_from_past_best_source$best_age_comp)] <- 0  # change age comp NAs to 0

check_past <- age_comp_from_past_best_source %>%                                # check PAST data line-up of age comp proportions by year, annotated by source and method    
  pivot_wider(names_from = age_class, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_past$total_prop <- rowSums(check_past[, c(-1, -2, -3)], na.rm = TRUE)   # sum proportions, should add to 1.0

ocean_age_comp_past <- age_comp_from_past_best_source %>%
    mutate(ocean_age = str_sub(age_class, start = -1)) %>% 
    group_by(return_year, ocean_age, best_source, best_method) %>% 
    summarise(best_age_comp = sum(best_age_comp, na.rm = TRUE)) 

check_past_OA <- ocean_age_comp_past %>%                                        # check PAST data line-up of ocean age comp proportions by year, annotated by source and method    
  pivot_wider(names_from = ocean_age, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_past_OA$total_prop <- 
  rowSums(check_past_OA[, c(-1, -2, -3)], na.rm = TRUE)                         # sum ocean age proportions, should add to 1.0  
  
past_plot <- ggplot(ocean_age_comp_past, aes(x = return_year, y = best_age_comp))+           # stacked 100% bar plot of annual age composition    
    geom_point(data = ocean_age_comp_past %>% ungroup() %>% 
               dplyr::select(return_year, best_source) %>% 
               unique(), aes(x = return_year, y = -0.02, color = best_source), 
               size = 3, pch = 15)+
    geom_col(aes(fill = ocean_age))+
    scale_fill_viridis_d("Ocean Age")+
    scale_color_manual(values = c("red", "dodgerblue", "black"), "Source")+
    guides(colour = guide_legend(order = 2), 
           fill = guide_legend(order = 1))+
    coord_cartesian(expand = FALSE)+
    labs(title="Ocean Age Proportion", 
         subtitle = "Source: Past Age Comp Assignment (Hyatt & Stockwell)")+
    ylab(NULL)+
    xlab(NULL)+
    theme_pt()+
    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5))            # theme_pt centers main title but not subtitle
  
#-------

age_comp_from_CRITFC_best_source <- age_comp_source_all %>%                     # Assign age comp proportions to year based on CRITFC over ONA age comp assignments                                       
  mutate(best_age_comp = ifelse(best_source_CRITFC == "CRITFC", 
                                CRITFC_prop_at_age, ONA_prop_at_age)) %>%
  mutate(best_method = ifelse(best_source_CRITFC == "CRITFC", CRITFC_method,    # Annotate with method and source
                              ONA_method)) %>%
  mutate(best_source = ifelse(best_source_CRITFC == "CRITFC", "CRITFC", "ONA")) %>%
  dplyr::select(return_year, age_class, best_age_comp, best_source, best_method)%>% # drop unselected columns
  mutate_at(3, as.numeric, na.rm = TRUE)                                        # convert age comp column to numeric
  age_comp_from_CRITFC_best_source$best_age_comp[is.na(age_comp_from_CRITFC_best_source$best_age_comp)] <- 0  # change age comp NAs to 0

check_CRITFC <- age_comp_from_CRITFC_best_source %>%                            # check CRITFC-focused data line-up of age comp proportions by year, annotated by source and method
  pivot_wider(names_from = age_class, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method 
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_CRITFC$total_prop <- 
    rowSums(check_CRITFC[, c(-1, -2, -3)], na.rm = TRUE)                        # sum proportions, should add to 1.0

ocean_age_comp_CRITFC <- age_comp_from_CRITFC_best_source %>%
  mutate(ocean_age = str_sub(age_class, start = -1)) %>% 
  group_by(return_year, ocean_age, best_source, best_method) %>% 
  summarise(best_age_comp = sum(best_age_comp, na.rm = TRUE)) 
  
check_CRITFC_OA <- ocean_age_comp_CRITFC %>%                                    # check CRITFC data line-up of ocean age comp proportions by year, annotated by source and method    
  pivot_wider(names_from = ocean_age, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_CRITFC_OA$total_prop <- 
  rowSums(check_CRITFC_OA[, c(-1, -2, -3)], na.rm = TRUE)                       # sum ocean age proportions, should add to 1.0  
  
CRITFC_plot <- ggplot(ocean_age_comp_CRITFC, aes(x = return_year, y = best_age_comp))+         # stacked 100% bar plot of annual age composition    
  geom_point(data = ocean_age_comp_CRITFC %>% ungroup() %>% 
               dplyr::select(return_year, best_source) %>% 
               unique(), aes(x = return_year, y = -0.02, color = best_source), 
             size = 3, pch = 15)+
  geom_col(aes(fill = ocean_age))+
  scale_fill_viridis_d("Ocean Age")+
  scale_color_manual(values = c("red", "dodgerblue", "black"), "Source")+
  guides(colour = guide_legend(order = 2), 
         fill = guide_legend(order = 1))+
  coord_cartesian(expand = FALSE)+
  labs(title="Ocean Age Proportion", 
       subtitle = "Source: Primarily CRITFC Age Comp (Fryer et al.)")+
  ylab(NULL)+
  xlab(NULL)+
  theme_pt()+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5))              # theme_pt centers main title but not subtitle
  
#-------
  
age_comp_from_ONA_best_source <- age_comp_source_all %>%                        # Assign age comp proportions to year based on ONA over CRITFC age comp assignments                                
  mutate(best_age_comp = ifelse(best_source_ONA == "CRITFC", 
                                CRITFC_prop_at_age, ONA_prop_at_age)) %>%
  mutate(best_method = ifelse(best_source_ONA == "CRITFC", CRITFC_method,       # Annotate with method
                              ONA_method)) %>%
  mutate(best_source = ifelse(best_source_ONA == "CRITFC", "CRITFC", "ONA"))%>% # annotate with source
  dplyr::select(return_year, age_class, best_age_comp, best_source, 
                best_method)%>%                                                 # drop unselected columns
  mutate_at(3, as.numeric, na.rm = TRUE)                                        # convert age comp column to numeric
  age_comp_from_ONA_best_source$best_age_comp[is.na
                          (age_comp_from_ONA_best_source$best_age_comp)] <- 0   # change age comp NAs to 0
  
check_ONA <- age_comp_from_ONA_best_source %>%                                  # check ONA-focused data line-up of age comp proportions by year, annotated by source and method                     
  pivot_wider(names_from = age_class, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method 
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_ONA$total_prop <- rowSums(check_ONA[, c(-1, -2, -3)], na.rm = TRUE)     # sum proportions, should add to 1.0

ocean_age_comp_ONA <- age_comp_from_ONA_best_source %>%
  mutate(ocean_age = str_sub(age_class, start = -1)) %>% 
  group_by(return_year, ocean_age, best_source, best_method) %>% 
  summarise(best_age_comp = sum(best_age_comp, na.rm = TRUE)) 
  
check_ONA_OA <- ocean_age_comp_ONA %>%                                          # check ONA data line-up of ocean age comp proportions by year, annotated by source and method    
  pivot_wider(names_from = ocean_age, 
              values_from = best_age_comp) %>%
  filter(!is.na(best_method)) %>%                                               # filter out rows where there is no method
  select_if(~sum(!is.na(.)) > 0)                                                # drop proportions columns in which all years are NA
  check_ONA_OA$total_prop <- 
  rowSums(check_ONA_OA[, c(-1, -2, -3)], na.rm = TRUE)                          # sum ocean age proportions, should add to 1.0  
  
ONA_plot <- ggplot(ocean_age_comp_ONA, aes(x = return_year, y = best_age_comp))+# stacked 100% bar plot of annual age composition    
  geom_point(data = ocean_age_comp_ONA %>% ungroup() %>% 
               dplyr::select(return_year, best_source) %>% 
               unique(), aes(x = return_year, y = -0.02, color = best_source), 
             size = 3, pch = 15)+
  geom_col(aes(fill = ocean_age))+
  scale_fill_viridis_d("Ocean Age")+
  scale_color_manual(values = c("red", "dodgerblue", "black"), "Source")+
  guides(colour = guide_legend(order = 2), 
         fill = guide_legend(order = 1))+
  coord_cartesian(expand = FALSE)+
  labs(title="Ocean Age Proportion", 
       subtitle = "Source: Primarily Spawning Grounds (ONA, Hyatt et al.)")+
  ylab(NULL)+
  xlab(NULL)+
  theme_pt()+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5))              # theme_pt centers main title but not subtitle

past_plot + CRITFC_plot + ONA_plot +                                            # collate three age comp plots for comparison
  plot_annotation(tag_levels = 'A')+                                            # put A B C on Figure
  plot_layout(nrow = 3)                                                         # stack figures

filename <- paste(work, "\\FIGURES\\Age_Comp_Compare_", timestamp, ".png", sep = "") # figure output filename
ggsave(file = filename, width = 6, height = 8, units = "in")                    # saves the plot

#-----------------------  

setup_ONA_OA_comp <- ocean_age_comp_ONA %>% ungroup() %>%
  mutate(ONA_age_comp = best_age_comp) %>%
  filter(is.na(best_method) == FALSE) %>%                                       ### not sure where the extra rows of NAs come from (e.g., 1986)
  dplyr::select(return_year, ocean_age, ONA_age_comp) 

setup_CRITFC_OA_comp <- ocean_age_comp_CRITFC %>% ungroup() %>%
  mutate(CRITFC_age_comp = best_age_comp) %>%
  filter(is.na(best_method) == FALSE) %>%                                       ### not sure where the extra rows of NAs come from (e.g., 1986)
  dplyr::select(return_year, ocean_age, CRITFC_age_comp)

compare_OA_comp <-
  merge(setup_ONA_OA_comp, setup_CRITFC_OA_comp) %>%
  mutate(diff_OA_comp = abs(ONA_age_comp - CRITFC_age_comp))

ocean_age_1 <- compare_OA_comp %>%
  filter(ocean_age == 1)
ocean_age_2 <- compare_OA_comp %>%
  filter(ocean_age == 2)
ocean_age_3 <- compare_OA_comp %>%
  filter(ocean_age == 3)

ggplot(ocean_age_1, aes(x = return_year, y = diff_OA_comp)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Difference in Proportion") +
  ggtitle("Difference in Ocean Age 1 Composition") + 

ggplot(ocean_age_2, aes(x = return_year, y = diff_OA_comp)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Difference in Proportion") +
  ggtitle("Difference in Ocean Age 2 Composition") + 
  
ggplot(ocean_age_3, aes(x = return_year, y = diff_OA_comp)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Difference in Proportion") +
  ggtitle("Difference in Ocean Age 3 Composition") +   
  
  plot_annotation(tag_levels = 'A')+                                            # put A B C on Figure
  plot_layout(nrow = 3)                                                         # stack figures

#---- 
# Side-by-side bar chart comparison of CRITFC vs ONA proportions by ocean age classes 1 and 2

ocean_age_1_plotdata <- ocean_age_1 %>%                                         # focus on ocean age 1 (jacks)
  dplyr::select(-diff_OA_comp) %>%
  pivot_longer(cols = ends_with("_age_comp"),                                   # arranging the data to long format for plotting
                              names_to = "Source",
                              values_to = "Ocean_Age_1") %>%
  dplyr::filter(return_year >= 1990) 

ocean_age_2_plotdata <- ocean_age_2 %>%                                         # focus on ocean age 2 (42s and 53s)
  dplyr::select(-diff_OA_comp) %>%
  pivot_longer(cols = ends_with("_age_comp"),                                   #arranging the data to long format for plotting
               names_to = "Source",
               values_to = "Ocean_Age_2") %>%
  dplyr::filter(return_year >= 1990) 

ocean_age_3_plotdata <- ocean_age_3 %>%                                         # focus on ocean age 3 (52s and 63s
  dplyr::select(-diff_OA_comp) %>%
  pivot_longer(cols = ends_with("_age_comp"),                                   #arranging the data to long format for plotting
               names_to = "Source",
               values_to = "Ocean_Age_3") %>%
  dplyr::filter(return_year >= 1990) 

ggplot(ocean_age_1_plotdata, aes(x = factor(return_year), y = Ocean_Age_1, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.80) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Compare Proportions at Age 1") +
  scale_y_continuous(breaks = c(seq(from = 0, to = 1, by = 0.1)))+ ylim(0, 1)+
  scale_x_discrete(breaks = c(seq(from = 1990, to = 2024, by = 2)))+            # omit 1980-1999 as only CRITFC or M-Yr ONA
  scale_fill_discrete(name = "Source", labels = c("CRITFC", "ONA")) +
  theme_pt(major_grid = TRUE)+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5)) +            # theme_pt centers main title but not subtitle

ggplot(ocean_age_2_plotdata, aes(x = factor(return_year), y = Ocean_Age_2, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.80) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Compare Proportions at Age 2") +
  scale_y_continuous(breaks = c(seq(from = 0, to = 1, by = 0.1)))+ ylim(0, 1)+ 
  scale_x_discrete(breaks = c(seq(from = 1990, to = 2024, by = 2)))+            # omit 1980-1999 as only CRITFC or M-Yr ONA
  scale_fill_discrete(name = "Source", labels = c("CRITFC", "ONA")) +
  theme_pt(major_grid = TRUE)+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.75))+

ggplot(ocean_age_3_plotdata, aes(x = factor(return_year), y = Ocean_Age_3, fill = Source)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.80) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Compare Proportions at Age 3") +
  scale_y_continuous(breaks = c(seq(from = 0, to = 1, by = 0.1)))+ ylim(0, 1)+
  scale_x_discrete(breaks = c(seq(from = 1990, to = 2024, by = 2)))+            # omit 1980-1999 as only CRITFC or M-Yr ONA
  scale_fill_discrete(name = "Source", labels = c("CRITFC", "ONA")) +
  theme_pt(major_grid = TRUE)+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5))+
  
  plot_annotation(tag_levels = 'A')+                                            # put A B C on Figure
  plot_layout(nrow = 3)                                                         # stack figures


