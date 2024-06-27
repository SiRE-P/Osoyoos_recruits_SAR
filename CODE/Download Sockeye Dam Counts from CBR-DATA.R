## --------------------------------------------------------------------------
## FILENAME.R
##
## Title:   
## Purpose: 
## Author:  H Stiff
## Date:    20.
## Notes:   
##          
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
  dplyr::select(Project, Return_Year, Bonn_Sockeye) 
  
#-------------------------------------------------------------------------------
# John Day Dam Counts

JDay_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=JDA&startdate=1%2F1&enddate=12%2F31&run=")
JDay_data <- JDay_html %>% html_table(fill = TRUE)
JDay_data <- JDay_data[[1]] 

JDay_Sockeye <- JDay_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), JDay_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, JDay_Sockeye)

#-------------------------------------------------------------------------------
# McNary Dam Counts

MCN_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=MCN&startdate=1%2F1&enddate=12%2F31&run=")
MCN_data <- MCN_html %>% html_table(fill = TRUE)
MCN_data <- MCN_data[[1]] 

MCN_Sockeye <- MCN_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), MCN_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, MCN_Sockeye)

#-------------------------------------------------------------------------------
# Priest Rapids Dam Counts

PRD_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=PRD&startdate=1%2F1&enddate=12%2F31&run=")
PRD_data <- PRD_html %>% html_table(fill = TRUE)
PRD_data <- PRD_data[[1]] 

PRD_Sockeye <- PRD_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), PRD_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, PRD_Sockeye)

#-------------------------------------------------------------------------------
# Rock Island Dam Counts

RockI_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RIS&startdate=1%2F1&enddate=12%2F31&run=")
RockI_data <- RockI_html %>% html_table(fill = TRUE)
RockI_data <- RockI_data[[1]] 

RockI_Sockeye <- RockI_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), RockI_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RockI_Sockeye) 

#-------------------------------------------------------------------------------
# Rocky Reach Dam Counts

RRH_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=RRH&startdate=1%2F1&enddate=12%2F31&run=")
RRH_data <- RRH_html %>% html_table(fill = TRUE)
RRH_data <- RRH_data[[1]] 

RRH_Sockeye <- RRH_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), RRH_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, RRH_Sockeye) 

#-------------------------------------------------------------------------------
# Tumwater (Wenatchee) Dam Counts

Tum_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=TUM&startdate=1%2F1&enddate=12%2F31&run=")
Tum_data <- Tum_html %>% html_table(fill = TRUE)
Tum_data <- Tum_data[[1]] 

Tum_Sockeye <- Tum_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Tum_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Tum_Sockeye) 

#-------------------------------------------------------------------------------
# Wells Dam Counts

Wells_html <- read_html("https://www.cbr.washington.edu/dart/cs/php/rpt/adult_annual.php?sc=1&outputFormat=html&proj=WEL&startdate=1%2F1&enddate=12%2F31&run=")
Wells_data <- Wells_html %>% html_table(fill = TRUE)
Wells_data <- Wells_data[[1]] 

Wells_Sockeye <- Wells_data %>%
  filter(Year != "Year") %>%
  mutate(Return_Year = as.numeric(Year), Wells_Sockeye = as.numeric(Sockeye)) %>%
  dplyr::select(Project, Return_Year, Wells_Sockeye) 

#-------------------------------------------------------------------------------

Columbia_Sockeye_Dam_Counts_by_Year <- Bonn_Sockeye %>%
  full_join(JDay_Sockeye, by = "Return_Year") %>%
  full_join(MCN_Sockeye, by = "Return_Year") %>%
  full_join(PRD_Sockeye, by = "Return_Year") %>%
  full_join(RockI_Sockeye, by = "Return_Year") %>%
  full_join(RRH_Sockeye, by = "Return_Year") %>%
  full_join(Wells_Sockeye, by = "Return_Year") %>%
  dplyr::select(-starts_with("Project"))

        

