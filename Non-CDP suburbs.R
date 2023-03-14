#Non census-designated place suburbs
#Choices focuses on the core based statistical area and the census designated place
#The code retains tracts that interact with the metropolitan statistical area boundary as we are interested in peripheral processes
#Tracts that intersect with the census designated place are removed as we are less interested in inner-urban processes

#This definition is inspired by Hall and Lee's (2010) paper which disaggregates the CMSA by CDP

#Libraries
library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

#Select the state, scale/geography, year, CBSA, and central city
#(We include bordering states as many CBSAs cross boundaries)
ST = c("PA", "OH", "WV", "NC", "SC", "OR", "WA")
GEOG = "tract"
YR = 2011

CBSA = c("Pittsburgh|Portland-Vancouver|Charlotte-Gastonia|Charlotte-Concord")
CENTRAL_CITY = c("Pittsburgh city|Portland city|Charlotte city")


#2011
#Download geographies of interest (in this case, the CBSA)
CBSA_2011 <- core_based_statistical_areas(resolution = "500k", year = 2011) %>%
  filter(str_detect(NAME, CBSA))

#Download tracts of interest
Tracts_2011 <- map_dfr(c(ST), ~{
  tracts(.x, year = YR)})

#Acquiring the central city geographies
CentralCities_2011 <- places(state = ST, year = YR) %>%
  filter(str_detect(NAMELSAD, CENTRAL_CITY)) 

#Spatial filter of tracts for the city of interest
#Here we include all tracts that border the CMSA boundary, as we're are interested in the periphery
Tracts_2011 <- Tracts_2011[CBSA_2011, ]

#Removing the CDP tracts from Pittsburgh
Tracts_2011 <-  Tracts_2011[lengths(st_intersects(Tracts_2011,CentralCities_2011))==0,]

#Erase water
nonCDP_Suburbs_2011 <- erase_water(Tracts_2011)

#General tidying
nonCDP_Suburbs_2011  <- nonCDP_Suburbs_2011 %>%
  mutate(State_Name = "PA",
         City = "Pittsburgh") %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP)

#For initial visualizing of Pittsburgh
ggplot() + 
  geom_sf(data = nonCDP_Suburbs_2011[nonCDP_Suburbs_2011$STATEFP == 42, ], fill = "black", color = "grey") + 
  geom_sf(data = CentralCities_2011[CentralCities_2011$NAME == "Pittsburgh", ], fill = NA, color = "blue") + 
  geom_sf(data = CBSA_2011[CBSA_2011$NAME == "Pittsburgh, PA", ], fill = NA, color = "red") + 
  theme_void()




#2021
YR = 2021

#Download geographies of interest (in this case, the CBSA)
CBSA_2021 <- core_based_statistical_areas(resolution = "500k", year = 2021) %>%
  filter(str_detect(NAME, CBSA))

#Download tracts of interest
Tracts_2021 <- map_dfr(c(ST), ~{
  tracts(.x, year = YR)})

#Acquiring the central city geographies
CentralCities_2021 <- places(state = ST, year = YR) %>%
  filter(str_detect(NAMELSAD, CENTRAL_CITY)) 

#Spatial filter of tracts for the city of interest
#Here we include all tracts that border the CMSA boundary, as we're are interested in the periphery
Tracts_2021 <- Tracts_2021[CBSA_2021, ]

#Removing the CDP tracts from Pittsburgh
Tracts_2021 <-  Tracts_2021[lengths(st_intersects(Tracts_2021,CentralCities_2021))==0,]

#Erase water
nonCDP_Suburbs_2021 <- erase_water(Tracts_2021)

#General tidying
nonCDP_Suburbs_2021  <- nonCDP_Suburbs_2021 %>%
  mutate(State_Name = "PA",
         City = "Pittsburgh") %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP)

#For a more detailed visualization of the new data (with a legend)
ggplot() + 
  geom_sf(data = nonCDP_Suburbs_2021[nonCDP_Suburbs_2011$STATEFP == 42, ], aes(fill = "black", color = "grey"), show.legend = "line") + 
  geom_sf(data = CentralCities_2021[CentralCities_2011$NAME == "Pittsburgh", ], aes(color = "blue"), fill = NA,  show.legend = "line")  + 
  geom_sf(data = CBSA_2011[CBSA_2021$NAME == "Pittsburgh, PA", ], aes(color = "dark red"), fill = NA,  show.legend = "line") + 
  theme_void() +
  scale_color_manual(values=c("blue", "red", "light grey"), 
                     labels=c("Census Designated Place", "Combined Statistical Area", "Census Tract")) +
  scale_fill_manual(values = c("black"), 
                    labels=c("Census Designated Place"),
                    guide = "none") +
  theme(legend.title=element_blank(),
        legend.position=c("bottom")) + 
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              override.aes = list(size=1)))





