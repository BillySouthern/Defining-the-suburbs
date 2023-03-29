#3/28, initiated by BS
#Goal:  To join the spatial data with ACS data for 2011 and 2021

#ACS	Total Population (B01003)	
#ACS	Hispanic or Latino Origin by Race (Table B03002)	
#ACS	Nativity in the United States (Table B05012)	
#ACS	Median Household Income (Table B19019)	


#Libraries
library(tigris)
library(tidycensus)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

#Select the state, scale/geography, year, CBSA, and central city
#(We include bordering states as many CBSAs cross boundaries)
ST = c("PA", "OH", "WV", "NC", "SC", "OR", "WA")
GEOG = "tract"
YR1 = 2011
YR2 = 2021


#2011 Total Pop
Population_11 <- get_acs(
  geography = GEOG, 
  variables = "B01003_001", 
  state = ST,
  year = YR1) %>%
  rename(total_11 = estimate,
         moe_11 = moe)


#2021 Total Pop
Population_21 <- get_acs(
  geography = GEOG, 
  variables = "B01003_001", 
  state = ST,
  year = YR2) %>%
  rename(total_21 = estimate,
         moe_21 = moe)

#NON-CENSUS DESIGNATED PLACE SUBURBS TOTAL POP
#loading 2011 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2011.Rdata")

Total_Pop_11 <- merge(Population_11, nonCDP_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_11, moe_11, City)

#Load 2021 nonDCP tract
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2021.Rdata")

#Join tracts to data
Total_Pop_21 <- merge(Population_21, nonCDP_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_21, moe_21, City)

#Join both years
NCDPTotal_Population <- merge(Total_Pop_11, Total_Pop_21, all = TRUE) %>%
  mutate(Type = "NCDP")


#POST-CR SUBURBS TOTAL POP
#2011
#load 2011 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2021.Rdata")

#Join tracts to data (and remove duplicate)
Total_Pop_11 <- merge(Population_11, PostCR_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_11, moe_11, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2021.Rdata")

#Join tracts to data (and remove duplicate)
Total_Pop_21 <- merge(Population_21, PostCR_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_21, moe_21, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#Join both years
PostCRTotal_Population <- merge(Total_Pop_11, Total_Pop_21, all = TRUE)


#INNER/OUTER Suburbs
#2011
#load 2011 INNER/OUTER tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2011.Rdata")

#Join tracts to data (and remove duplicate)
Total_Pop_11 <- merge(Population_11, InnerOuter_Suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_11, moe_11, Suburb) 

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2021.Rdata")

#Join tracts to data (and remove duplicate)
Total_Pop_21 <- merge(Population_21, InnerOuter_Suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, total_21, moe_21, Suburb) 

#Join both years
IOTotal_Population <- merge(Total_Pop_11, Total_Pop_21, all = TRUE)

#Combining all suburban populations into one object
Total_Population <- merge(IOTotal_Population, PostCRTotal_Population, by.x = "GEOID", by.y = "GEOID", all = TRUE)
Total_Population <- merge(Total_Population, NCDPTotal_Population, by.x = "GEOID", by.y = "GEOID", all = TRUE) 

#General tidying
Total_Population <- Total_Population %>%
  mutate(City = str_extract(GEOID, "^.{2}")) %>%
  mutate(City = case_when(City == 37 ~ "Charlotte",
                          City == 45 ~ "Charlotte",
                          City == 42 ~ "Pittsburgh",
                          City == 54 ~ "Pittsburgh",
                          City == 39 ~ "Pittsburgh",
                          City == 41 ~ "Portland",
                          City == 53 ~ "Portland")) %>%
  mutate(Total_2011 = coalesce(total_11.x, total_11.y, total_11),
         Total_2021 = coalesce(total_21.x, total_21.y, total_21)) %>%
  select(GEOID, City, Type, PostCRsuburb, Suburb, Total_2011, Total_2021)

#save(Total_Population,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/TotalPopulation.Rdata")


#2011 Median HH income
Med_inc_11 <- get_acs(
  geography = GEOG, 
  variables = "B19019_001", 
  state = ST,
  year = YR) %>%
  rename(income_11 = estimate,
         incomemoe_11 = moe)

#2011 Foreign born
NativeForeign_11 <- get_acs(
  geography = GEOG, 
  variables = c("B05012_002E", "B05012_003E"), 
  state = ST,
  summary_var = "B05012_001",
  year = YR) %>%
  rename(citizen_11 = estimate,
         citizenmoe_11 = moe)

#2011 Race
Race_11 <- get_acs(
  geography = GEOG, 
  variables = c(OverallRace_E = "B03002_001E",
                OverallRace_MOE = "B03002_001M",
                NHWhite_E = "B03002_003E",
                NHWhite_MOE = "B03002_003M",
                NHBlack_E = "B03002_004E",
                NHBlack_MOE = "B03002_004M",
                Hispanic_E = "B03002_012E",
                Hispanic_MOE = "B03002_012M",
                NHAsian_E ="B03002_006E",
                NHAsian_MOE ="B03002_006M",
                Native_E = "B03002_005E",
                Native_MOE = "B03002_005M",
                HawaiiPI_E = "B03002_007E",
                HawaiiPI_MOE = "B03002_007M",
                OtherRace_E = "B03002_008E",
                OtherRace_MOE = "B03002_008M"),
  state = ST,
  year = YR,
  output = "wide"
)

Race_11 <- Race_11 %>%
  mutate(Race_11, SmallGroups_E = Native_E + HawaiiPI_E + OtherRace_E) %>%
  mutate(Race_11, SmallGroups_MOE = Native_MOE + HawaiiPI_MOE + OtherRace_MOE) %>%
  select(GEOID, OverallRace_E, OverallRace_MOE, NHWhite_E, NHWhite_MOE, NHBlack_E, NHBlack_MOE, 
         Hispanic_E, Hispanic_MOE, NHAsian_E, NHAsian_MOE, SmallGroups_E, SmallGroups_MOE) %>%
  pivot_longer(!GEOID) %>%
  rename(Race = name)



