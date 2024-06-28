## --------------------------------------------------------------------------
## Program: Download Sockeye Dam Counts from CBR-DATA.R
##
## Title:   Download Sockeye Dam Counts from CBR-DART.R (mispelled in filename!)
## Purpose: Obtain raw total annual dam counts at Columbia mainstem dams from online web portal and adjust for 16-24-hr expansions where 
##          appropriate, as well as "negative mortality" issues at u/s dams vs d/s dams. Calculate annual mid-Columbia Sockeye stock composition
##          for Wenatchee- and Okanagan-bound Sockeye stocks.
## Author:  H Stiff 24.06.27
## Notes:   Annual dam count data retrieval queries (one for each dam) were obtained from: https://www.cbr.washington.edu/dart/query/adult_annual_sum  
##          Though this program retrieves annual totals data from all mainstem dams for all species, only some dams are needed for subsequent analyses, 
##          and only Sockeye data are processed.
##          Some dam/year combinations are based on 16-hr counts; these are adjusted up to full 24-hr day counts either
##          via negative binomial regression analysis (Bonneville) in another program, OR AS IS DONE IN THIS PROGRAM SO FAR, 
##          application of a simple multi-year mean multiplier based on years where both 16- and 24-hr counts are available.
##          --> Wells pre-1998: Estimated from 16-hr counts (pre-1998) + average annual difference 13.2% between 16- and 24-hour counts (1998-2022; Tom Kahler pers. comm.), i.e. 16-hr count x 1.132 [hs 2022-10-17] 
##          --> Rocky Reach pre-1994: Using 16-to-24-hr adj factor (1.12) up to 1993 based on multi-year avg 2004-2011 at RRH (from CW WDFW)
##          --> Rock Island is effectively 24-hr day counts, as is Tumwater
##          --> Not sure about John Day, McNary, Priest Rapids
##          --> Bonneville is most accurately adjusted as in Osoyoos Sox Recruit & Survival.Rmd {r oso recruitment} based on model from {r bonn 16 to 24 hr count adjustment}
##          *** BUT here the expansion factor is only approximated with an old 4% inflation factor till we decide how to incorporate the GAM in this program. ***
## --------------------------------------------------------------------------

# install.packages("rvest")
library(rvest)

#-------------------------------------------------------------------------------
# Bonneville Dam Counts

Bonn_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=BON&startdate=1%2F1&enddate=12%2F31&run=")
Bonn_data <- Bonn_html %>% html_table(fill = TRUE)
Bonn_data <- Bonn_data[[1]] 

Bonn_Sockeye <- Bonn_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Bonn_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Bonn_Sockeye)                             # NOTE: All years based on 16-hr daily counts only
  
#-------------------------------------------------------------------------------
# John Day Dam Counts

JDay_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=JDA&startdate=1%2F1&enddate=12%2F31&run=")
JDay_data <- JDay_html %>% html_table(fill = TRUE)
JDay_data <- JDay_data[[1]] 

JDay_Sockeye <- JDay_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), JDay_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, JDay_Sockeye)                             # NOTE: UNKNOWN whether JDay based on 16-hr daily counts only

#-------------------------------------------------------------------------------
# McNary Dam Counts

MCN_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=MCN&startdate=1%2F1&enddate=12%2F31&run=")
MCN_data <- MCN_html %>% html_table(fill = TRUE)
MCN_data <- MCN_data[[1]] 

MCN_Sockeye <- MCN_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), MCN_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, MCN_Sockeye)                              # NOTE: UNKNOWN whether McNary based on 16-hr daily counts only 

#-------------------------------------------------------------------------------
# Priest Rapids Dam Counts

PRD_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=PRD&startdate=1%2F1&enddate=12%2F31&run=")
PRD_data <- PRD_html %>% html_table(fill = TRUE)
PRD_data <- PRD_data[[1]] 

PRD_Sockeye <- PRD_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), PRD_Sockeye = as.numeric(Sockeye)) %>% # NOTE: UNKNOWN whether PRD based on 16-hr daily counts only
  dplyr::select(Project, Return_Year, PRD_Sockeye) %>%
  mutate(PRD_Sockeye = ifelse(PRD_Sockeye < 5, NA, PRD_Sockeye))                # Bad data (1 fish!) in 2023 ? 

#-------------------------------------------------------------------------------
# Rock Island Dam Counts

RockI_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RIS&startdate=1%2F1&enddate=12%2F31&run=")
RockI_data <- RockI_html %>% html_table(fill = TRUE)
RockI_data <- RockI_data[[1]] 

RockI_Sockeye <- RockI_data %>%                                                 # Based on actual 24-hour counts since 1993, and effective 24-hour counts pre-1993
  filter(Year != "Year") %>%                                                    # (since, pre-1993, 16 hours were observed and then gates closed for 8 hours, but no fish could pass till re-opened for next 16-hr count).  Source: Catherine Willard (WDFW), Oct 2022 
  mutate(Return_Year = as.numeric(Year), RockI_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RockI_Sockeye) 

#-------------------------------------------------------------------------------
# Rocky Reach Dam Counts

RRH_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RRH&startdate=1%2F1&enddate=12%2F31&run=")
RRH_data <- RRH_html %>% html_table(fill = TRUE)
RRH_data <- RRH_data[[1]] 

RRH_Sockeye <- RRH_data %>%                                                     # NOTE: 16-hr counts (pre-1994), 24-hr counts since 1994. USE multi-year mean multiplier of 1.12 to expand 16- to 24-hr counts (C. Willard, Chelan PUD)
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), RRH_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RRH_Sockeye) 

#-------------------------------------------------------------------------------
# Tumwater (Wenatchee) Dam Counts                                               # NOTE: CBR-DATA for TUM differ in year-span and value from SIS workbook entries which were provided by C.Willard (Chelan PUD, TUM mgmt)

Tum_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=TUM&startdate=1%2F1&enddate=12%2F31&run=")
Tum_data <- Tum_html %>% html_table(fill = TRUE)
Tum_data <- Tum_data[[1]] 

Tum_Sockeye <- Tum_data %>%                                                     # NOTE: Annual totals all based on 24-hr counts
  filter(Year != "Year") %>%                                                    # CBR-DART data are limited to 1999-present (C.Willard data start in 1989). According to ChatGPT, TUM dam was operational in 1909; fish counting began in 1956!
  mutate(Return_Year = as.numeric(Year), Tum_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Tum_Sockeye) 

#-------------------------------------------------------------------------------
# Wells Dam Counts

Wells_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=WEL&startdate=1%2F1&enddate=12%2F31&run=")
Wells_data <- Wells_html %>% html_table(fill = TRUE)
Wells_data <- Wells_data[[1]] 

Wells_Sockeye <- Wells_data %>%                                                 # NOTE: 16-hr counts (pre-1998), 24-hr counts since 1998. USE multi-year mean multiplier of 1.13 to expand 16- to 24-hr counts (T. Kahler, DC PUD) 
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Wells_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Wells_Sockeye) 

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_by_Year_All <- Bonn_Sockeye %>%                     # collate all Sockeye dam count series by year
  full_join(JDay_Sockeye,  by = "Return_Year") %>%
  full_join(MCN_Sockeye,   by = "Return_Year") %>%
  full_join(PRD_Sockeye,   by = "Return_Year") %>%
  full_join(RockI_Sockeye, by = "Return_Year") %>%
  full_join(Tum_Sockeye,   by = "Return_Year") %>%
  full_join(RRH_Sockeye,   by = "Return_Year") %>%
  full_join(Wells_Sockeye, by = "Return_Year") %>%
  dplyr::select(-starts_with("Project"))                                        # drop Project variables since columns are labelled with dam name

Columbia_Sockeye_Dam_Counts_by_Year_Raw <- Columbia_Sockeye_Dam_Counts_by_Year_All %>%    # filter raw data !938-present to years common to 
  filter(Return_Year >= 1977 & Return_Year != year(today()))      ### Note: End Year!?    # mainstem dams (1977-present), dropping current incomplete year  

#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_24hr <- Columbia_Sockeye_Dam_Counts_by_Year_Raw %>%           # expand dam counts from 16-to-24 hr counts where applicable
  mutate(RRH_Sockeye_24hr   = ifelse(Return_Year < 1994,                                
                                     round(RRH_Sockeye   * 1.120, 0), RRH_Sockeye  )) %>% # Using 16-to-24-hr adj factor (1.12) up to 1993 based on multi-year avg 2004-2011 at RRH (from CW WDFW)
  mutate(Wells_Sockeye_24hr = ifelse(Return_Year < 1998, 
                                     round(Wells_Sockeye * 1.132, 0), Wells_Sockeye)) %>% # Estimated from 16-hr counts (pre-1998) + average annual difference 13.2% between 16- and 24-hour counts (1998-2022; Tom Kahler pers. comm.), i.e. 16-hr count x 1.132 [hs 2022-10-17]
  mutate(Bonn_Sockeye_24hr  = round(Bonn_Sockeye * 1.040, 0)) %>%                      ### WAIT: NOTE OLD EXPANSION FACTOR. Incorporate PT's Bonn-16-to-24-hr adjustment here too????
  mutate(RockI_Sockeye_24hr = RockI_Sockeye) %>%                                          # RockI dam counts are already 24hr based
  mutate(Tum_Sockeye_24hr   = Tum_Sockeye) %>%                                            # Tumwater dam counts are already 24hr based
  dplyr::select(Return_Year, Bonn_Sockeye_24hr, RockI_Sockeye_24hr, RRH_Sockeye_24hr,     # Keep just the variables based on 24-hr counts.
                Tum_Sockeye_24hr, Wells_Sockeye_24hr)

#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_Adj <- Columbia_Sockeye_Dam_Counts_24hr %>%                   # Compare u/s dam totals to d/s dam totals
  mutate(Well_gt_RRH_diff  = ifelse(Wells_Sockeye_24hr > RRH_Sockeye_24hr,                # and calculate dam count difference if u/s > d/s
                                    Wells_Sockeye_24hr - RRH_Sockeye_24hr, NA)) %>%       # and flag the record by displaying the difference.
  mutate(RRH_Sockeye_adj   = pmax(Wells_Sockeye_24hr, RRH_Sockeye_24hr)) %>%              # Set d/s dam to max of the two dams.
  mutate(RRH_gt_RockI_diff = ifelse(RRH_Sockeye_adj > RockI_Sockeye_24hr,                 # Compare u/s Rocky Reach (RRH) to d/s Rock Island
                                    RRH_Sockeye_adj - RockI_Sockeye_24hr, NA)) %>%        # and flag any records with the difference.
  mutate(RockI_Sockeye_adj = pmax(RockI_Sockeye_24hr, RRH_Sockeye_adj)) %>%               # Set d/s dam to max of the two dams.
  mutate(RockI_gt_Bonn_diff = ifelse(RockI_Sockeye_adj > Bonn_Sockeye_24hr,               # Compare u/s Rocky Reach (RRH) to d/s Rock Island
                                     RockI_Sockeye_adj - Bonn_Sockeye_24hr, NA)) %>%      # and flag any records with the difference.
  mutate(Bonn_Sockeye_adj = pmax(Bonn_Sockeye_24hr, RockI_Sockeye_adj)) %>%               # Set d/s dam to max of the two dams.
  dplyr::select(Return_Year, Bonn_Sockeye_24hr, RockI_gt_Bonn_diff, Bonn_Sockeye_adj,
                             RockI_Sockeye_24hr, RRH_gt_RockI_diff, RockI_Sockeye_adj,
                             RRH_Sockeye_24hr, Well_gt_RRH_diff, RRH_Sockeye_adj, 
                             Wells_Sockeye_24hr, Tum_Sockeye_24hr)

#-------------------------------------------------------------------------------

Columbia_Sockeye_Stock_Comp <- Columbia_Sockeye_Dam_Counts_Adj %>%                        # Get best mid-Columbia stock composition proportions...
  mutate(Ok_Stock_Comp_1  = round(RRH_Sockeye_adj / RockI_Sockeye_adj,2)) %>%             # Ok-bound stock is typically calc'd from ratio of RRH:RockI dam counts.
  mutate(Wen_Stock_Comp_1 = round(1 - (RRH_Sockeye_adj / RockI_Sockeye_adj),2)) %>%       # Wen stock is usually calc'd by subtracting the Ok proportion from 1...
  mutate(Wen_Stock_Comp_2 = ifelse(is.na(Tum_Sockeye_24hr), 0,                            # but if counts at Tumwater are large enough to indicate a higher proportion
                                   round(Tum_Sockeye_24hr / RockI_Sockeye_adj, 2))) %>%   # of Wen fish than the RRH:RockI ratio, then maybe that is a better indicator &
  mutate(Wen_Stock_Comp_Best = pmax(Wen_Stock_Comp_1, Wen_Stock_Comp_2)) %>%              # the best Wen stock comp is set to the max of the two estimates.
  mutate(Ok_Stock_Comp_Best = 1 - Wen_Stock_Comp_Best) %>%                                # and the best Ok stock comp is 100 - Wen%.
  mutate(Stock_Comp_Total = Wen_Stock_Comp_Best + Ok_Stock_Comp_Best) %>%                 # This is all a bit arbitrary, as it is not clear without dam count error data,
  dplyr::select(Return_Year, RockI_Sockeye_adj, RRH_Sockeye_adj, Tum_Sockeye_24hr,        # fall-back (and over-shoot) estimates, and spatially-resolved harvest data, to
                Wen_Stock_Comp_1, Ok_Stock_Comp_1, Wen_Stock_Comp_2,                      # know which of the three dams is contributing what to the noise.
                Wen_Stock_Comp_Best, Ok_Stock_Comp_Best, Stock_Comp_Total)                # Note also that the workbook contains somewhat different counts in the Tumwater column (starting in 
                                                                                          # 1989 not 1999) provided by C.Willard (Chelan PUD; Tum Dam mgmt), who apparently supplies the dam 
                                                                                          # count data to CBR-DART; but her data may include rec harvest or spawning ground AUC estimates to 
                                                                                          # better approximate 'total Wen returns'. This prog uses published CBR-DART data for Tumwater only, 
                                                                                          # which means there will be some generally small diffs in stock comp from analyses based on workbook data.




  
  
  
  
  