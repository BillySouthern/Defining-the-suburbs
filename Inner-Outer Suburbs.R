#1/22, initiated by BS
#Goal:  To isolate suburban tracts and highlight whether they are "inner" or "outer" suburbs

#Post-Civil Rights suburbs (Pfeiffer, 2016)

#Libraries
library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

ST = c("PA", "OH", "WV", "NC", "SC", "OR", "WA")
GEOG = "tract"
YR = 2011

CBSA = c("Pittsburgh|Portland-Vancouver|Charlotte-Gastonia|Charlotte-Concord")
CENTRAL_CITY = c("Pittsburgh city|Portland city|Charlotte city")



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
#Filtering the census tracts for our geographies of interest
Tracts_2011 <- Tracts_2011[CBSA_2011, ]

#Removing the CDP tracts
#This removes the inner/urban area
Tracts_2011 <-  Tracts_2011[lengths(st_within(Tracts_2011,CentralCities_2011))==0,]

#To isolate the Inner suburban tracts
Inner_Suburbs <- Tracts_2011[CentralCities_2011, ]

Inner_Suburbs <-  Inner_Suburbs[lengths(st_within(Inner_Suburbs,CentralCities_2011))==0,]%>%
  mutate(Suburb = "Inner")

#Isolating the Outer suburban tracts
Outer_Suburbs <-  Tracts_2011[lengths(st_overlaps(Tracts_2011,Inner_Suburbs))==0,] 

Outer_Suburbs <-  Tracts_2011[lengths(st_overlaps(Tracts_2011,CentralCities_2011))==0,] %>%
  mutate(Suburb = "Outer")

#Joining Inner/Outer suburbs
InnerOuter_Suburbs <- rbind(Inner_Suburbs, Outer_Suburbs)

#Erase water bodies, rivers, lakes
InnerOuter_Suburbs <- erase_water(InnerOuter_Suburbs)

#Not showing PA inner suburbs, need to make it for 2021, make sure tracts are full
#May just be too small, PA tracts perfectly align too

ggplot() + 
  geom_sf(data = Inner_Suburbs[Inner_Suburbs$STATEFP == 41, ], aes(fill = Suburb)) + 
  geom_sf(data = CentralCities_2011[CentralCities_2011$NAME == "Portland", ], color = "yellow", fill = NA)  
  
  theme_void() +
  scale_fill_manual(values=c("#BFBFBF", "#1A1A1A"), 
                    labels=c("Pre-Civil Rights", "Post-Civil Rights"),
                    na.translate = F) +
  theme(legend.title=element_blank(),
        legend.position=c("bottom"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              override.aes = list(size=1)))
