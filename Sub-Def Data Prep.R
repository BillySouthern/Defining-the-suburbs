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
NCDPTotal_Population <- merge(Total_Pop_11, Total_Pop_21, by.x = "GEOID", by.y = "GEOID", all = TRUE) %>%
  mutate(Type = "NCDP")


#POST-CR SUBURBS TOTAL POP
#2011
#load 2011 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2011.Rdata")

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
PostCRTotal_Population <- merge(Total_Pop_11, Total_Pop_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)


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
IOTotal_Population <- merge(Total_Pop_11, Total_Pop_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)

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
         Total_2021 = coalesce(total_21.x, total_21.y, total_21),
         PostCRSuburb = coalesce(PostCRsuburb.x, PostCRsuburb.y),
         Suburb = coalesce(Suburb.x, Suburb.y)) %>%
  select(GEOID, City, Type, PostCRSuburb, Suburb, Total_2011, Total_2021)

#save(Total_Population,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/TotalPopulation.Rdata")


--------

#2011 Median HH income
Med_inc_11 <- get_acs(
  geography = GEOG, 
  variables = "B19019_001", 
  state = ST,
  year = YR1,
  output = "wide") %>%
  rename("estimate_11" = B19019_001E,
         "moe_11" = B19019_001M)

#2021 Median HH income
Med_inc_21 <- get_acs(
  geography = GEOG, 
  variables = "B19019_001", 
  state = ST,
  year = YR2,
  output = "wide") %>%
  rename("estimate_21" = B19019_001E,
         "moe_21" = B19019_001M)

#NON-CENSUS DESIGNATED PLACE SUBURBS TOTAL POP
#loading 2011 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2011.Rdata")

Income_11 <- merge(Med_inc_11, nonCDP_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_11, moe_11, City)

#loading 2021 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2021.Rdata")

#Join tracts to data
Income_21 <- merge(Med_inc_21, nonCDP_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_21, moe_21, City)

#Join both years
NCDP_Income <- merge(Income_11, Income_21, by.x = "GEOID", by.y = "GEOID", all = TRUE) %>%
  mutate(Type = "NCDP")



#POST-CR SUBURBS TOTAL POP
#2011
#load 2011 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2011.Rdata")

#Join tracts to data (and remove duplicate)
Income_11 <- merge(Med_inc_11, PostCR_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_11, moe_11, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2021.Rdata")

#Join tracts to data (and remove duplicate)
Income_21 <- merge(Med_inc_21, PostCR_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_21, moe_21, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#Join both years
PostCR_Income <- merge(Income_11, Income_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)


#INNER/OUTER Suburbs
#2011
#load 2011 INNER/OUTER tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2011.Rdata")

#Join tracts to data (and remove duplicate)
Income_11 <- merge(Med_inc_11, InnerOuter_Suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_11, moe_11, Suburb) 

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2021.Rdata")

#Join tracts to data (and remove duplicate)
Income_21 <- merge(Med_inc_21, InnerOuter_Suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, estimate_21, moe_21, Suburb) 

#Join both years
IO_Income <- merge(Income_11, Income_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)

#Combining all suburban populations into one object
Median_Income <- merge(IO_Income, PostCR_Income, by.x = "GEOID", by.y = "GEOID", all = TRUE)
Median_Income <- merge(Median_Income, NCDP_Income, by.x = "GEOID", by.y = "GEOID", all = TRUE) 

#General tidying
Median_Income <- Median_Income %>%
  mutate(City = str_extract(GEOID, "^.{2}")) %>%
  mutate(City = case_when(City == 37 ~ "Charlotte",
                          City == 45 ~ "Charlotte",
                          City == 42 ~ "Pittsburgh",
                          City == 54 ~ "Pittsburgh",
                          City == 39 ~ "Pittsburgh",
                          City == 41 ~ "Portland",
                          City == 53 ~ "Portland")) %>%
  mutate(Income_11 = coalesce(estimate_11.x, estimate_11.y, estimate_11),
         Income_21 = coalesce(estimate_21.x, estimate_21.y, estimate_21),
         PostCRSuburb = coalesce(PostCRsuburb.x, PostCRsuburb.y),
         Suburb = coalesce(Suburb.x, Suburb.y)) %>%
  select(GEOID, City, Type, PostCRSuburb, Suburb, Income_11, Income_21)

#save(Median_Income,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/MedianIncome.Rdata")







-------

#2011 Foreign born
NativeForeign_11 <- get_acs(
  geography = GEOG, 
  variables = c("B05012_002E", "B05012_003E"), 
  state = ST,
  summary_var = "B05012_001",
  year = YR1,
  output = "wide") %>%
  rename(Native_11 = B05012_002E,
         Native_MOE = B05012_002M,
         Foreign_11 = B05012_003E,
         Foreign_MOE = B05012_003M) %>%
  mutate(NativePercent_11 = Native_11/summary_est) %>%
  mutate(ForeignPercent_11 = Foreign_11/summary_est) 
  

NativeForeign_21 <- get_acs(
  geography = GEOG, 
  variables = c("B05012_002E", "B05012_003E"), 
  state = ST,
  summary_var = "B05012_001",
  year = YR2,
  output = "wide") %>%
  rename(Native_21 = B05012_002E,
         Native_MOE = B05012_002M,
         Foreign_21 = B05012_003E,
         Foreign_MOE = B05012_003M) %>%
  mutate(NativePercent_21 = Native_21/summary_est) %>%
  mutate(ForeignPercent_21 = Foreign_21/summary_est) 

#loading 2011 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2011.Rdata")

Born_11 <- merge(NativeForeign_11, nonCDP_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_11, Foreign_11, City)

#loading 2021 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2021.Rdata")

#Join tracts to data
Born_21 <- merge(NativeForeign_21, nonCDP_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_21, Foreign_21, City)

#Join both years
NCDP_Born <- merge(Born_11, Born_21, by.x = "GEOID", by.y = "GEOID", all = TRUE) %>%
  mutate(Type = "NCDP")


#2011
#load 2011 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2011.Rdata")

#Join tracts to data (and remove duplicate)
Born_11 <- merge(NativeForeign_11, PostCR_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_11, Foreign_11, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2021.Rdata")

#Join tracts to data (and remove duplicate)
Born_21 <- merge(NativeForeign_21, PostCR_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_21, Foreign_21, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#Join both years
PostCR_Born <- merge(Born_11, Born_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)



#INNER/OUTER Suburbs
#2011
#load 2011 INNER/OUTER tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2011.Rdata")

#Join tracts to data (and remove duplicate)
Born_11 <- merge(NativeForeign_11, InnerOuter_Suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_11, Foreign_11, Suburb) 

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2021.Rdata")

#Join tracts to data (and remove duplicate)
Born_21 <- merge(NativeForeign_21, InnerOuter_Suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, Native_21, Foreign_21, Suburb) 

#Join both years
IO_Born <- merge(Born_11, Born_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)

#Combining all suburban populations into one object
Born <- merge(IO_Born, PostCR_Born, by.x = "GEOID", by.y = "GEOID", all = TRUE)
Born <- merge(Born, NCDP_Born, by.x = "GEOID", by.y = "GEOID", all = TRUE) 

#General tidying
Born <- Born %>%
  mutate(City = str_extract(GEOID, "^.{2}")) %>%
  mutate(City = case_when(City == 37 ~ "Charlotte",
                          City == 45 ~ "Charlotte",
                          City == 42 ~ "Pittsburgh",
                          City == 54 ~ "Pittsburgh",
                          City == 39 ~ "Pittsburgh",
                          City == 41 ~ "Portland",
                          City == 53 ~ "Portland")) %>%
  mutate(Native_11 = coalesce(Native_11.x, Native_11.y, Native_11),
         Native_21 = coalesce(Native_21.x, Native_21.y, Native_21),
         Foreign_11 = coalesce(Foreign_11.x, Foreign_11.y, Foreign_11),
         Foreign_21 = coalesce(Foreign_21.x, Foreign_21.y, Foreign_21),
         PostCRSuburb = coalesce(PostCRsuburb.x, PostCRsuburb.y),
         Suburb = coalesce(Suburb.x, Suburb.y)) %>%
  select(GEOID, City, Type, PostCRSuburb, Suburb, Native_11, Native_21, Foreign_11, Foreign_21)


save(Born,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/Born.rdata")


------------
#2011 Race
Race_11 <- get_acs(
  geography = GEOG, 
  variables = c(OverallRace_E_11 = "B03002_001E",
                OverallRace_MOE_11 = "B03002_001M",
                NHWhite_E_11 = "B03002_003E",
                NHWhite_MOE_11 = "B03002_003M",
                NHBlack_E_11 = "B03002_004E",
                NHBlack_MOE_11 = "B03002_004M",
                Hispanic_E_11 = "B03002_012E",
                Hispanic_MOE_11 = "B03002_012M",
                NHAsian_E_11 ="B03002_006E",
                NHAsian_MOE_11 ="B03002_006M",
                Native_E_11 = "B03002_005E",
                Native_MOE_11 = "B03002_005M",
                HawaiiPI_E_11 = "B03002_007E",
                HawaiiPI_MOE_11 = "B03002_007M",
                OtherRace_E_11 = "B03002_008E",
                OtherRace_MOE_11 = "B03002_008M"),
  state = ST,
  year = YR1,
  output = "wide"
)

Race_11 <- Race_11 %>%
  mutate(Race_11, SmallGroups_E_11 = Native_E_11 + HawaiiPI_E_11 + OtherRace_E_11) %>%
  mutate(Race_11, SmallGroups_MOE_11 = Native_MOE_11 + HawaiiPI_MOE_11 + OtherRace_MOE_11) %>%
  select(GEOID, NHWhite_E_11, NHWhite_MOE_11, NHBlack_E_11, NHBlack_MOE_11, 
         Hispanic_E_11, Hispanic_MOE_11, NHAsian_E_11, NHAsian_MOE_11, SmallGroups_E_11, SmallGroups_MOE_11) 




#2021 Race
Race_21 <- get_acs(
    geography = GEOG, 
    variables = c(OverallRace_E_21 = "B03002_001E",
                  OverallRace_MOE_21 = "B03002_001M",
                  NHWhite_E_21 = "B03002_003E",
                  NHWhite_MOE_21 = "B03002_003M",
                  NHBlack_E_21 = "B03002_004E",
                  NHBlack_MOE_21 = "B03002_004M",
                  Hispanic_E_21 = "B03002_012E",
                  Hispanic_MOE_21 = "B03002_012M",
                  NHAsian_E_21 ="B03002_006E",
                  NHAsian_MOE_21 ="B03002_006M",
                  Native_E_21 = "B03002_005E",
                  Native_MOE_21 = "B03002_005M",
                  HawaiiPI_E_21 = "B03002_007E",
                  HawaiiPI_MOE_21 = "B03002_007M",
                  OtherRace_E_21 = "B03002_008E",
                  OtherRace_MOE_21 = "B03002_008M"),
    state = ST,
    year = YR2,
    output = "wide"
  )
  
  Race_21 <- Race_21 %>%
    mutate(Race_21, SmallGroups_E_21 = Native_E_21 + HawaiiPI_E_21 + OtherRace_E_21) %>%
    mutate(Race_21, SmallGroups_MOE_21 = Native_MOE_21 + HawaiiPI_MOE_21 + OtherRace_MOE_21) %>%
    select(GEOID, NHWhite_E_21, NHWhite_MOE_21, NHBlack_E_21, NHBlack_MOE_21, 
           Hispanic_E_21, Hispanic_MOE_21, NHAsian_E_21, NHAsian_MOE_21, SmallGroups_E_21, SmallGroups_MOE_21) 


#NON-CENSUS DESIGNATED PLACE SUBURBS TOTAL POP
#loading 2011 nonCDP tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2011.Rdata")

Total_Race_11 <- merge(Race_11, nonCDP_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, NHWhite_E_11, NHBlack_E_11, 
         Hispanic_E_11, NHAsian_E_11, SmallGroups_E_11,
         City)

#Load 2021 nonDCP tract
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/NonCDP suburbs/nonCDP_suburbs_2021.Rdata")

#Join tracts to data
Total_Race_21 <- merge(Race_21, nonCDP_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, NHWhite_E_21, NHBlack_E_21, 
         Hispanic_E_21, NHAsian_E_21, SmallGroups_E_21,
         City)

#Join both years
NCDP_Race <- merge(Total_Race_11, Total_Race_21, by.x = "GEOID", by.y = "GEOID", all = TRUE) %>%
  mutate(Type = "NCDP")


#POST-CR SUBURBS TOTAL POP
#2011
#load 2011 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2011.Rdata")

#Join tracts to data (and remove duplicate)
Total_Race_11 <- merge(Race_11, PostCR_suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
    select(GEOID, NHWhite_E_11, NHBlack_E_11, 
           Hispanic_E_11, NHAsian_E_11, SmallGroups_E_11, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Post-CR Suburbs/PostCRsuburbs_2021.Rdata")

#Join tracts to data (and remove duplicate)
Total_Race_21 <- merge(Race_21, PostCR_suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, NHWhite_E_21, NHBlack_E_21, 
         Hispanic_E_21, NHAsian_E_21, SmallGroups_E_21, State, PostCRsuburb) %>%
  distinct(GEOID, .keep_all = TRUE)

#Join both years
PostCR_Race <- merge(Total_Race_11, Total_Race_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)


#INNER/OUTER Suburbs
#2011
#load 2011 INNER/OUTER tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2011.Rdata")

#Join tracts to data 
Total_Race_11 <- merge(Race_11, InnerOuter_Suburbs_2011, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, NHWhite_E_11, NHBlack_E_11, 
         Hispanic_E_11, NHAsian_E_11, SmallGroups_E_11, Suburb) 

#2021
#Load 2021 POST CR tracts
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Inner-Outer/InnerOuter_2021.Rdata")

#Join tracts to data 
Total_Race_21 <- merge(Race_21, InnerOuter_Suburbs_2021, by.x = "GEOID", by.y = "GEOID") %>%
  select(GEOID, NHWhite_E_21, NHBlack_E_21, 
         Hispanic_E_21, NHAsian_E_21, SmallGroups_E_21, Suburb) 

#Join both years
IO_Race <- merge(Total_Race_11, Total_Race_21, by.x = "GEOID", by.y = "GEOID", all = TRUE)

#Combining all suburban populations into one object
Total_Race <- merge(IO_Race, PostCR_Race, by.x = "GEOID", by.y = "GEOID", all = TRUE)
Total_Race <- merge(Total_Race, NCDP_Race, by.x = "GEOID", by.y = "GEOID", all = TRUE) 

#General tidying
Total_Race <- Total_Race %>%
  mutate(City = str_extract(GEOID, "^.{2}")) %>%
  mutate(City = case_when(City == 37 ~ "Charlotte",
                          City == 45 ~ "Charlotte",
                          City == 42 ~ "Pittsburgh",
                          City == 54 ~ "Pittsburgh",
                          City == 39 ~ "Pittsburgh",
                          City == 41 ~ "Portland",
                          City == 53 ~ "Portland")) %>%
  mutate(NHWhite_11 = coalesce(NHWhite_E_11.x, NHWhite_E_11.y, NHWhite_E_11),
         NHWhite_21 = coalesce(NHWhite_E_21.x, NHWhite_E_21.y, NHWhite_E_21),
         NHBlack_11 = coalesce(NHBlack_E_11.x, NHBlack_E_11.y, NHBlack_E_11),
         NHBlack_21 = coalesce(NHBlack_E_21.x, NHBlack_E_21.y, NHBlack_E_21),
         Hispanic_11 = coalesce(Hispanic_E_11.x, Hispanic_E_11.y, Hispanic_E_11),
         Hispanic_21 = coalesce(Hispanic_E_21.x, Hispanic_E_21.y, Hispanic_E_21),
         Asian_11 = coalesce(NHAsian_E_11.x, NHAsian_E_11.y, NHAsian_E_11),
         Asian_21 = coalesce(NHAsian_E_21.x, NHAsian_E_21.y, NHAsian_E_21),
         SG_11 = coalesce(SmallGroups_E_11.x, SmallGroups_E_11.y, SmallGroups_E_11),
         SG_21 = coalesce(SmallGroups_E_21.x, SmallGroups_E_21.y, SmallGroups_E_21),
         PostCRSuburb = coalesce(PostCRsuburb.x, PostCRsuburb.y),
         Suburb = coalesce(Suburb.x, Suburb.y)) %>%
  select(GEOID, City, Type, PostCRSuburb, Suburb, NHWhite_11, NHWhite_21,
         NHBlack_11, NHBlack_21, Hispanic_11, Hispanic_21, Asian_11, Asian_21,
         SG_11, SG_21)

#save(Total_Race,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/TotalRace.Rdata")

