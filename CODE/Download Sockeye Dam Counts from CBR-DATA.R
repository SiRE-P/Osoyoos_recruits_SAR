# --------------------------------------------------------------------------
# Program: Download Sockeye Dam Counts from CBR-DATA.R ####
#
# Title:   Download Sockeye Dam Counts from CBR-DART.R (mispelled in filename!)
# Purpose: Obtain raw total annual dam counts at Columbia mainstem dams from online web portal and adjust for 16-24-hr expansions where 
#          appropriate, as well as "negative mortality" issues at u/s dams vs d/s dams. Calculate annual mid-Columbia Sockeye stock composition
#          for Wenatchee- and Okanagan-bound Sockeye stocks.
# Author:  H Stiff 24.06.27
# Notes:   Annual dam count data retrieval queries (one for each dam) were obtained from: https://www.cbr.washington.edu/dart/query/adult_annual_sum  
#          Though this program retrieves annual totals data from all mainstem dams for all species, only some dams are needed for subsequent analyses, 
#          and only Sockeye data are processed.
#          Some dam/year combinations are based on 16-hr counts; these are adjusted up to full 24-hr day counts either
#          via negative binomial regression analysis (Bonneville) in another program, OR AS IS DONE IN THIS PROGRAM SO FAR, 
#          application of a simple multi-year mean multiplier based on years where both 16- and 24-hr counts are available.
#          --> Wells pre-1998: Estimated from 16-hr counts (pre-1998) + average annual difference 13.2% between 16- and 24-hour counts (1998-2022; Tom Kahler pers. comm.), i.e. 16-hr count x 1.132 [hs 2022-10-17] 
#          --> Rocky Reach pre-1994: Using 16-to-24-hr adj factor (1.12) up to 1993 based on multi-year avg 2004-2011 at RRH (from CW WDFW)
#          --> Rock Island is effectively 24-hr day counts, as is Tumwater
#          --> Not sure about John Day, McNary, Priest Rapids
#          --> Bonneville is most accurately adjusted as in Osoyoos Sox Recruit & Survival.Rmd {r oso recruitment} based on model from {r bonn 16 to 24 hr count adjustment}
#          *** BUT here the expansion factor is only approximated with an old 4% inflation factor till we decide how to incorporate the GAM in this program. ***
# --------------------------------------------------------------------------

library(ggplot2)
library(ggpubr)       # for adding text boxes with regression coefficients to plots
library(ggrepel)
library(rvest)
library(MASS)
library(mgcv)
library(patchwork)
library(psych)
library(readxl)
library(scales)
library(SiREfunctions)
library(splines)
library(stringi)
library(tidyverse)

timestamp <- substr(format(Sys.time(), "%Y%m%d"), 3, 8)                         # Get the current date and time and format as string to timestamp output files

#-------------------------------------------------------------------------------
# Bonneville Dam Counts ####

Bonn_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=BON&startdate=1%2F1&enddate=12%2F31&run=")
Bonn_data <- Bonn_html %>% html_table(fill = TRUE)
Bonn_data <- Bonn_data[[1]] 

Bonn_Sockeye <- Bonn_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Bonn_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Bonn_Sockeye)                             # NOTE: All years based on 16-hr daily counts only
  
#-------------------------------------------------------------------------------
# John Day Dam Counts  ####

JDay_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=JDA&startdate=1%2F1&enddate=12%2F31&run=")
JDay_data <- JDay_html %>% html_table(fill = TRUE)
JDay_data <- JDay_data[[1]] 

JDay_Sockeye <- JDay_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), JDay_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, JDay_Sockeye)                             # NOTE: UNKNOWN whether JDay based on 16-hr daily counts only

#-------------------------------------------------------------------------------
# McNary Dam Counts  ####

MCN_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=MCN&startdate=1%2F1&enddate=12%2F31&run=")
MCN_data <- MCN_html %>% html_table(fill = TRUE)
MCN_data <- MCN_data[[1]] 

MCN_Sockeye <- MCN_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), MCN_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, MCN_Sockeye)                              # NOTE: UNKNOWN whether McNary based on 16-hr daily counts only 

#-------------------------------------------------------------------------------
# Ice Harbor Dam Counts for Snake River Sockeye ####

ICE_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=IHR&startdate=1%2F1&enddate=12%2F31&run=")
ICE_data <- ICE_html %>% html_table(fill = TRUE)
ICE_data <- ICE_data[[1]] 

ICE_Sockeye <- ICE_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), ICE_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, ICE_Sockeye)                              # NOTE: UNKNOWN whether ICE HARBOR is based on 16-hr daily counts only; assume 24-hr 

#-------------------------------------------------------------------------------
# Priest Rapids Dam Counts  ####

PRD_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=PRD&startdate=1%2F1&enddate=12%2F31&run=")
PRD_data <- PRD_html %>% html_table(fill = TRUE)
PRD_data <- PRD_data[[1]] 

PRD_Sockeye <- PRD_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), PRD_Sockeye = as.numeric(Sockeye)) %>% # NOTE: UNKNOWN whether PRD based on 16-hr daily counts only
  dplyr::select(Project, Return_Year, PRD_Sockeye) %>%
  mutate(PRD_Sockeye = ifelse(PRD_Sockeye < 5, NA, PRD_Sockeye))                # Bad data (1 fish!) in 2023 ? 

#-------------------------------------------------------------------------------
# Rock Island Dam Counts ####

RockI_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RIS&startdate=1%2F1&enddate=12%2F31&run=")
RockI_data <- RockI_html %>% html_table(fill = TRUE)
RockI_data <- RockI_data[[1]] 

RockI_Sockeye <- RockI_data %>%                                                 # Based on actual 24-hour counts since 1993, and effective 24-hour counts pre-1993
  filter(Year != "Year") %>%                                                    # (since, pre-1993, 16 hours were observed and then gates closed for 8 hours, but no fish could pass till re-opened for next 16-hr count).  Source: Catherine Willard (WDFW), Oct 2022 
  mutate(Return_Year = as.numeric(Year), RockI_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RockI_Sockeye) 

#-------------------------------------------------------------------------------
# Rocky Reach Dam Counts  ####

RRH_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RRH&startdate=1%2F1&enddate=12%2F31&run=")
RRH_data <- RRH_html %>% html_table(fill = TRUE)
RRH_data <- RRH_data[[1]] 

RRH_Sockeye <- RRH_data %>%                                                     # NOTE: 16-hr counts (pre-1994), 24-hr counts since 1994. USE multi-year mean multiplier of 1.12 to expand 16- to 24-hr counts (C. Willard, Chelan PUD)
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), RRH_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RRH_Sockeye) 

#-------------------------------------------------------------------------------
# Tumwater (Wenatchee) Dam Counts  ####                                         # NOTE: CBR-DATA for TUM differ in year-span and value from SIS workbook entries which were provided by C.Willard (Chelan PUD, TUM mgmt)

Tum_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=TUM&startdate=1%2F1&enddate=12%2F31&run=")
Tum_data <- Tum_html %>% html_table(fill = TRUE)
Tum_data <- Tum_data[[1]] 

Tum_Sockeye <- Tum_data %>%                                                     # NOTE: Annual totals all based on 24-hr counts
  filter(Year != "Year") %>%                                                    # NOTE: CBR-DART data are limited to 1999-present (whereas C.Willard data in workbook start in 1989). 
  mutate(Return_Year = as.numeric(Year), Tum_Sockeye = as.numeric(Sockeye)) %>% #       (According to ChatGPT, TUM dam was operational in 1909; fish counting began in 1956!)
  dplyr::select(Project, Return_Year, Tum_Sockeye) 

#-------------------------------------------------------------------------------
# Wells Dam Counts  ####

Wells_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=WEL&startdate=1%2F1&enddate=12%2F31&run=")
Wells_data <- Wells_html %>% html_table(fill = TRUE)
Wells_data <- Wells_data[[1]] 

Wells_Sockeye <- Wells_data %>%                                                 # NOTE: 16-hr counts (pre-1998), 24-hr counts since 1998. USE multi-year mean multiplier of 1.13 to expand 16- to 24-hr counts (T. Kahler, DC PUD) 
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Wells_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Wells_Sockeye) 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_by_Year_All <- Bonn_Sockeye %>%                     # Collate all Sockeye dam count series by year  ####
  full_join(JDay_Sockeye,  by = "Return_Year") %>%
  full_join(MCN_Sockeye,   by = "Return_Year") %>%  
  full_join(ICE_Sockeye,   by = "Return_Year") %>%  
  full_join(PRD_Sockeye,   by = "Return_Year") %>%  
  full_join(RockI_Sockeye, by = "Return_Year") %>%
  full_join(Tum_Sockeye,   by = "Return_Year") %>%
  full_join(RRH_Sockeye,   by = "Return_Year") %>%
  full_join(Wells_Sockeye, by = "Return_Year") %>%
  dplyr::select(-starts_with("Project"))                                        # drop Project variables since columns are labelled with dam name

Columbia_Sockeye_Dam_Counts_by_Year_Raw <- Columbia_Sockeye_Dam_Counts_by_Year_All %>%    # filter raw data !938-present to years common to 
  filter(Return_Year >= 1977 & Return_Year != year(today()))      ### Note: End Year!?    # mainstem dams (1977-present), dropping current incomplete year  

filename <- paste("./data/Columbia_Sockeye_Dam_Counts_by_Year_", timestamp, 
                  ".csv", sep = "")                                             # filename for raw dam counts output 
write.csv(Columbia_Sockeye_Dam_Counts_by_Year_All, filename)                    # saves ALL YEARS (1938-present) dam counts data to filename

#-------------------------------------------------------------------------------
# Bonn 16-to-24 hr Model ####

bonn_16_v_24 <- read.csv("./data/Bonneville_16_v_24.csv")              # input data for years with 24 and 16 hour sockeye counts at Bonneville Dam
bonn_16_v_24 <- bonn_16_v_24 %>%                                       # BON has two fish ladders: Bradford (OR) and Washington (WA)
  mutate(tot_16 = ifelse(!is.na(tot_16), tot_16, bradford_16hr + washington_16hr),
         tot_24 = ifelse(!is.na(tot_24), tot_24, bradford_24hr + washington_24hr)) %>% 
  dplyr::select(ret_year, tot_16, tot_24) %>% 
  mutate(diff = tot_24 - tot_16) %>%                                   # derive response variable = difference between 16 and 24-hr counts
  arrange(ret_year)

bonn_model <- glm.nb((diff) ~ log(tot_16), data = bonn_16_v_24, link = "log") # predictive model: diff as func of log(16-hr counts); link=log applies only to response var

corrbonn <- corrr::correlate(log(bonn_16_v_24$tot_16), y = log(bonn_16_v_24$diff))    # r = 0.98 
stats_txt = "ln(Y) = -4.89 + 1.15 * ln(X); r2 = 0.96; dev = 18.2"                     # stats to annotate regression plot   

tot16_seq <- data.frame(tot_16 = seq(min(bonn_16_v_24$tot_16), max(bonn_16_v_24$tot_16), length=100))    # makes a sequence to predict over so we can get the line in the plot

bonn_pred_seq <- data.frame(tot_16 = tot16_seq, 
                            diff = predict(bonn_model, newdata = tot16_seq, type = "response"))                # type = "response" reverts to the scale of original data

ggplot(bonn_16_v_24, aes(y = diff/1000, x = tot_16/1000))+                      ### Now getting ERROR in stacked plot creation...
  geom_point()+                                                                 ###   Error in Ops.data.frame(guide_loc, panel_loc) : 
  # geom_smooth(method = "glm.nb", color="gray", se=TRUE) +                     ###   ‘==’ only defined for equally-sized data frames 
  geom_line(data = bonn_pred_seq, color="red", linewidth=1, linetype=1) +       ### ......................................[hs 240812]
  geom_point(colour="black", size=2.5)+
  # geom_jitter(height=1) +
  geom_label_repel(aes(label = ret_year), fill = "white", colour = "black", size = 3)+ #, nudge_y = 5, nudge_x = -80)+
  geom_text(x=0, y=36, label=stats_txt, hjust="inward", size=4) +
  labs(title="Bonneville Dam Sockeye Counts (1000s)", subtitle = "1995-2002, 2013-2022")+
  ylab("24 hr - 16 hr Count Difference")+
  xlab("16 hr Count")+
  theme_pt(major_grid=TRUE)+
  ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust=0.5))+             # theme_pt centers main title but not subtitle
  
  ggplot(bonn_16_v_24, aes(y = (diff + tot_16)/1000, x = tot_16/1000))+
  geom_line(data = bonn_pred_seq, color = "red", linewidth = 1.0)+                        # PT's predictive regression line
  # geom_smooth(method = "lm", se=TRUE, alpha = 0.5, color = "white", linewidth = 0.01)+  # alternative line option
  geom_point(colour="black", size=2.5)+
  geom_label_repel(aes(label = ret_year), fill = "white", color = "black", size = 3, nudge_y = 50, nudge_x = -80)+
  # labs(title="Bonneville Dam Sockeye Counts", subtitle = "1995-2002, 2013-2022")+                   
  ylab("24 hr Count")+
  xlab("16 hr Count")+
  theme_pt(major_grid=TRUE)+
  
  plot_annotation(tag_levels = 'A')+                                            # put A and B on Figure
  plot_layout(nrow = 2)                                                         # stack figures

filename <- paste("./figures/Bon_Dam_Counts_", timestamp, ".png", sep = "")     # figure output filename
ggsave(file = filename, width = 6, height = 6, units = "in")                    # saves the plot

#-------------------------------------------------------------------------------

# Mainstem Dam Counts 16-to-24 adjustments ####

bon_16_to_24 <- Columbia_Sockeye_Dam_Counts_by_Year_Raw %>%
  dplyr::select(ret_year = Return_Year, Bonn_Sockeye) %>%
  full_join(bonn_16_v_24, by = "ret_year") %>%
  mutate(Bonn_Sockeye_24hr_pred  = round(Bonn_Sockeye + predict(bonn_model,     # use bonn_model to estimate 24-hr counts from 16-hr counts          
                                   data.frame(tot_16 = Bonn_Sockeye),
                                   type = "response"),0)) %>% 
  mutate(Bonn_Sockeye_24hr = ifelse(is.na(tot_24), Bonn_Sockeye_24hr_pred,      # use known 24-hr counts instead of estimates where they exist (i.e., 1994-2000, 2002, 2013-2022)
                                    tot_24)) %>%
  rename(Return_Year = ret_year)

filename <- paste("./output/bon_16_to_24_", timestamp, 
                  ".csv", sep = "")                                             # filename for relevant adjusted dam counts output 
write.csv(bon_16_to_24, filename)                                               # saves the adj dam count data (1977-2023) to filename

Columbia_Sockeye_Dam_Counts_24hr <- Columbia_Sockeye_Dam_Counts_by_Year_Raw %>%           # expand dam counts from 16-to-24 hr counts where applicable
  full_join(bon_16_to_24, by = "Return_Year") %>%                                         # Bon 24-hr counts drawn from bon_16_to_24 data step
  mutate(RRH_Sockeye_24hr   = ifelse(Return_Year < 1994,                                
                                     round(RRH_Sockeye   * 1.120, 0), RRH_Sockeye  )) %>% # Using 16-to-24-hr adj factor (1.12) up to 1993 based on multi-year avg 2004-2011 at RRH (from CW WDFW)
  mutate(Wells_Sockeye_24hr = ifelse(Return_Year < 1998, 
                                     round(Wells_Sockeye * 1.132, 0), Wells_Sockeye)) %>% # Estimated from 16-hr counts (pre-1998) + average annual difference 13.2% between 16- and 24-hour counts (1998-2022; Tom Kahler pers. comm.), i.e. 16-hr count x 1.132 [hs 2022-10-17]
  mutate(RockI_Sockeye_24hr = RockI_Sockeye) %>%                                          # RockI dam counts are already 24hr based
  mutate(Tum_Sockeye_24hr   = Tum_Sockeye) %>%                                            # Tumwater dam counts are already 24hr based
  dplyr::select(Return_Year, Bonn_Sockeye_24hr, RockI_Sockeye_24hr, RRH_Sockeye_24hr,     # Keep just the variables based on 24-hr counts.
                Tum_Sockeye_24hr, Wells_Sockeye_24hr)

#-------------------------------------------------------------------------------

# Columbia_Sockeye_Dam_Counts_Adj <- Columbia_Sockeye_Dam_Counts_24hr %>%                   # Compare u/s dam totals to d/s dam totals ####
#   mutate(Well_gt_RRH_diff  = ifelse(Wells_Sockeye_24hr > RRH_Sockeye_24hr,                # and calculate dam count difference if u/s > d/s
#                                     Wells_Sockeye_24hr - RRH_Sockeye_24hr, NA)) %>%       # and flag the record by displaying the difference.
#   mutate(RRH_Sockeye_adj   = pmax(Wells_Sockeye_24hr, RRH_Sockeye_24hr)) %>%              # Set d/s dam to max of the two dams.
#   mutate(RRH_gt_RockI_diff = ifelse(RRH_Sockeye_adj > RockI_Sockeye_24hr,                 # Compare u/s Rocky Reach (RRH) to d/s Rock Island
#                                     RRH_Sockeye_adj - RockI_Sockeye_24hr, NA)) %>%        # and flag any records with the difference.
#   mutate(RockI_Sockeye_adj = pmax(RockI_Sockeye_24hr, RRH_Sockeye_adj)) %>%               # Set d/s dam to max of the two dams.
#   mutate(RockI_gt_Bonn_diff = ifelse(RockI_Sockeye_adj > Bonn_Sockeye_24hr,               # Compare u/s Rocky Reach (RRH) to d/s Rock Island
#                                      RockI_Sockeye_adj - Bonn_Sockeye_24hr, NA)) %>%      # and flag any records with the difference.
#   mutate(Bonn_Sockeye_adj = pmax(Bonn_Sockeye_24hr, RockI_Sockeye_adj)) %>%               # Set d/s dam to max of the two dams.
#   mutate(Wells_Sockeye_adj = Wells_Sockeye_24hr) %>%                                      # No further adjustments for Wells.
#   mutate(Tum_Sockeye_adj = Tum_Sockeye_24hr) %>%                                          # or Tumwater.
#   dplyr::select(Return_Year, Bonn_Sockeye_24hr, RockI_gt_Bonn_diff, Bonn_Sockeye_adj,
#                              RockI_Sockeye_24hr, RRH_gt_RockI_diff, RockI_Sockeye_adj,
#                              RRH_Sockeye_24hr, Well_gt_RRH_diff, RRH_Sockeye_adj, 
#                              Wells_Sockeye_24hr, Wells_Sockeye_adj, 
#                              Tum_Sockeye_24hr, Tum_Sockeye_adj) 

Columbia_Sockeye_Dam_Counts_Adj <- Columbia_Sockeye_Dam_Counts_24hr %>%                   # Compare u/s dam totals to d/s dam totals ####
  mutate(RockI_gt_Bonn_diff = ifelse(RockI_Sockeye_24hr > Bonn_Sockeye_24hr,              # Compare u/s Rocky Reach (RRH) to d/s Bonneville
                                     RockI_Sockeye_24hr - Bonn_Sockeye_24hr, NA)) %>%     # and flag any records with the difference.
  mutate(RockI_Sockeye_adj  = pmin(Bonn_Sockeye_24hr, RockI_Sockeye_24hr)) %>%            # Set d/s dam to min of the two dams.
  mutate(RRH_gt_RockI_diff  = ifelse(RRH_Sockeye_24hr > RockI_Sockeye_adj,                # Compare u/s Rocky Reach (RRH) to d/s Rock Island
                                     RRH_Sockeye_24hr - RockI_Sockeye_adj, NA)) %>%       # and flag any records with the difference.
  mutate(RRH_Sockeye_adj    = pmin(RockI_Sockeye_adj, RRH_Sockeye_24hr)) %>%              # Set d/s dam to max of the two dams.
  mutate(Well_gt_RRH_diff   = ifelse(Wells_Sockeye_24hr > RRH_Sockeye_adj,                # and calculate dam count difference if u/s > d/s
                                     Wells_Sockeye_24hr - RRH_Sockeye_adj, NA)) %>%       # and flag the record by displaying the difference.
  mutate(Wells_Sockeye_adj  = pmin(Wells_Sockeye_24hr, RRH_Sockeye_adj)) %>%              # Set d/s dam to max of the two dams.
  mutate(Bonn_Sockeye_adj   = Bonn_Sockeye_24hr) %>%                                      # No further adjustments for Bonn.
  mutate(Tum_Sockeye_adj    = Tum_Sockeye_24hr) %>%                                       # or Tumwater.
  dplyr::select(Return_Year,  Bonn_Sockeye_24hr, Bonn_Sockeye_adj, RockI_gt_Bonn_diff, 
                              RockI_Sockeye_24hr, RockI_Sockeye_adj, RRH_gt_RockI_diff, 
                              RRH_Sockeye_24hr, RRH_Sockeye_adj, Well_gt_RRH_diff, 
                              Wells_Sockeye_24hr, Wells_Sockeye_adj, 
                              Tum_Sockeye_24hr, Tum_Sockeye_adj) 

filename <- paste("./data/Columbia_Sockeye_Dam_Counts_Adj_", timestamp, 
                  ".csv", sep = "")                                                       # filename for relevant adjusted dam counts output 
write.csv(Columbia_Sockeye_Dam_Counts_Adj, filename)                                      # saves the adj dam count data (1977-2023) to filename

#-------------------------------------------------------------------------------

Columbia_Sockeye_Stock_Comp <- Columbia_Sockeye_Dam_Counts_Adj %>%                        # Get best mid-Columbia stock composition proportions ####
  mutate(Ok_Stock_Comp_1  = round(RRH_Sockeye_adj / RockI_Sockeye_adj,2)) %>%             # Ok-bound stock is typically calc'd from ratio of RRH:RockI dam counts.
  mutate(Wen_Stock_Comp_1 = round(1 - (RRH_Sockeye_adj / RockI_Sockeye_adj),2)) %>%       # Wen stock is usually calc'd by subtracting the Ok proportion from 1...
  mutate(Wen_Stock_Comp_2 = ifelse(is.na(Tum_Sockeye_24hr), 0,                            # but if counts at Tumwater (since 2000) are large enough to indicate a higher proportion
                                   round(Tum_Sockeye_24hr / RockI_Sockeye_adj, 2))) %>%   # of Wen fish than the RRH:RockI ratio, that then adds more information &
  mutate(Wen_Stock_Comp_Avg = (Wen_Stock_Comp_1 + Wen_Stock_Comp_2) / 2) %>%
  mutate(Wen_Stock_Comp_Best = ifelse(Return_Year < 2000,                                 # then, depending on year (Tumwater counts effective as of 2000)
                                      pmax(Wen_Stock_Comp_1, Wen_Stock_Comp_2),           # the RRH:RI ratio is used up to 1999 and
                                      Wen_Stock_Comp_Avg)) %>%                            # the annual average of the RRH:RI and TUM:RI ratios is used where both exist       
  mutate(Ok_Stock_Comp_Best = 1 - Wen_Stock_Comp_Best) %>%                                # and the best Ok stock comp is 100 - Best_Wen%.
  mutate(Stock_Comp_Total = Wen_Stock_Comp_Best + Ok_Stock_Comp_Best) %>%                 # This is all a bit arbitrary, as it is not clear without dam count error data,
  dplyr::select(Return_Year, RockI_Sockeye_adj, RRH_Sockeye_adj, Tum_Sockeye_adj,         # fall-back (and over-shoot) estimates, and spatially-resolved harvest data, to
                Wen_Stock_Comp_1, Ok_Stock_Comp_1, Wen_Stock_Comp_2,                      # know which of the three dams is contributing what to the noise.
                Wen_Stock_Comp_Best, Ok_Stock_Comp_Best, Stock_Comp_Total)                # Note also that the workbook contains somewhat different counts in the Tumwater column (starting in 
                                                                                          # 1989 not 1999) provided by C.Willard (Chelan PUD; Tum Dam mgmt), who apparently supplies the dam 
filename <- paste("./data/Columbia_Sockeye_Stock_Comp_", timestamp,                       # count data to CBR-DART; but her data may include rec harvest or spawning ground AUC estimates to 
                  ".csv", sep = "")                                                       # better approximate 'total Wen returns'. This prog uses published CBR-DART data for Tumwater only, 
write.csv(Columbia_Sockeye_Stock_Comp, filename)                                          # which means there will be some generally small diffs in stock comp from analyses based on workbook data.
#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_24hr_long <- Columbia_Sockeye_Dam_Counts_24hr %>%             # convert datasets to long to enable ggplot outputs
  pivot_longer(cols = c(Bonn_Sockeye_24hr, RockI_Sockeye_24hr, RRH_Sockeye_24hr, 
                        Wells_Sockeye_24hr), 
               names_to = "Mainstem_Dam", values_to = "Sockeye_Estimates") %>%
  dplyr::select(Return_Year, Mainstem_Dam, Sockeye_Estimates)

ggplot(Columbia_Sockeye_Dam_Counts_24hr_long, 
       aes(x = Return_Year, y = Sockeye_Estimates / 1000, color = Mainstem_Dam)) +
  labs(title = "Sockeye 24-hr Counts at Columbia Mainstem Dams (1000s)",
       subtitle = "by Return Year", x = NULL, y = NULL) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(trunc(min(Columbia_Sockeye_Dam_Counts_24hr_long$Return_Year)/10)*10, 
                                  max(Columbia_Sockeye_Dam_Counts_24hr_long$Return_Year), by = 4)) +
  theme_minimal() +
  theme(legend.position = "bottom")

Columbia_Sockeye_Dam_Counts_Adj_long <- Columbia_Sockeye_Dam_Counts_Adj %>% 
  pivot_longer(cols = c(Bonn_Sockeye_adj, RockI_Sockeye_adj, RRH_Sockeye_adj, Wells_Sockeye_adj), 
               names_to = "Mainstem_Dam", values_to = "Sockeye_Estimates") %>%
  dplyr::select(Return_Year, Mainstem_Dam, Sockeye_Estimates)

ggplot(Columbia_Sockeye_Dam_Counts_Adj_long, 
       aes(x = Return_Year, y = Sockeye_Estimates / 1000, color = Mainstem_Dam)) +
  labs(title = "Adjusted Sockeye Estimates at Columbia Mainstem Dams (1000s)",
       subtitle = "by Return Year", x = NULL, y = NULL) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(trunc(min(Columbia_Sockeye_Dam_Counts_24hr_long$Return_Year)/10)*10, 
                                  max(Columbia_Sockeye_Dam_Counts_Adj_long$Return_Year), by = 4)) +
  theme_minimal() +
  theme(legend.position = "bottom")

filename <- paste("./figures/Columbia_Sockeye_Dam_Counts_Adj_", 
                  timestamp, ".png", sep = "")                                  # figure output filename
ggsave(file = filename, width = 8, height = 6, units = "in")                    # saves the plot

  
  
  
  
  