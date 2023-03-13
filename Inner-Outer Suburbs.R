#1/22, initiated by BS
#Goal:  To isolate suburban tracts and highlight whether they are "inner" or "outer" suburbs

#Inner suburbs (Hanlon, 2008)

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
#Filtering the census tracts for our geographies of interest
Tracts_2011 <- Tracts_2011[CBSA_2011, ]

#Removing the CDP tracts
#This removes the inner/urban area
Tracts_2011 <-  Tracts_2011[lengths(st_within(Tracts_2011,CentralCities_2011))==0,]

#To isolate the Inner suburban tracts (tracts that are contiguous with the central city)
Inner_Suburbs <- Tracts_2011[CentralCities_2011, ]%>%
  mutate(Suburb = "Inner")

#Isolating the Outer suburban tracts
#Remove the inner tracts, then the central city tracts
Outer_Suburbs <-  Tracts_2011[lengths(st_within(Tracts_2011,Inner_Suburbs))==0,] %>%
  mutate(Suburb = "Outer")

#Joining Inner/Outer suburbs
InnerOuter_Suburbs <- rbind(Inner_Suburbs, Outer_Suburbs)

#Erase water bodies, rivers, lakes
InnerOuter_Suburbs_2011 <- erase_water(InnerOuter_Suburbs)


#To visualize Portland (OR and WA)
ggplot() + 
  geom_sf(data = InnerOuter_Suburbs_2011[InnerOuter_Suburbs_2011$STATEFP == 41, ], aes(fill = Suburb)) + 
  geom_sf(data = InnerOuter_Suburbs_2011[InnerOuter_Suburbs_2011$STATEFP == 53, ], aes(fill = Suburb)) + 
  theme_void() +
  scale_fill_manual(values=c("#1A1A1A", "#BFBFBF"), 
                    labels=c("Inner Suburbs", "Outer Suburbs"),
                    na.translate = F) +
  theme(legend.title=element_blank(),
        legend.position=c("bottom"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              override.aes = list(size=1)))

#To save the file
ggsave("Portland-IO-2011.png",
       path = "~/desktop",
       width = 6,
       height = 8,
       units = "in",
       dpi = 500)

#To save
save(InnerOuter_Suburbs_2011,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/InnerOuter_2011.Rdata")



#2021
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
#Filtering the census tracts for our geographies of interest
Tracts_2021 <- Tracts_2021[CBSA_2021, ]

#Removing the CDP tracts
#This removes the inner/urban area
Tracts_2021 <-  Tracts_2021[lengths(st_within(Tracts_2021,CentralCities_2021))==0,]

#To isolate the Inner suburban tracts (tracts that are contiguous with the central city)
Inner_Suburbs <- Tracts_2021[CentralCities_2021, ]%>%
  mutate(Suburb = "Inner")

#Isolating the Outer suburban tracts
#Remove the inner tracts, then the central city tracts
Outer_Suburbs <-  Tracts_2021[lengths(st_within(Tracts_2021,Inner_Suburbs))==0,] %>%
  mutate(Suburb = "Outer")

#Joining Inner/Outer suburbs
InnerOuter_Suburbs <- rbind(Inner_Suburbs, Outer_Suburbs)

#Erase water bodies, rivers, lakes
InnerOuter_Suburbs_2021 <- erase_water(InnerOuter_Suburbs)


#To visualize Portland (OR and WA)
ggplot() + 
  geom_sf(data = InnerOuter_Suburbs_2021[InnerOuter_Suburbs_2021$STATEFP == 41, ], aes(fill = Suburb)) + 
  geom_sf(data = InnerOuter_Suburbs_2021[InnerOuter_Suburbs_2021$STATEFP == 53, ], aes(fill = Suburb)) + 
  theme_void() +
  scale_fill_manual(values=c("#1A1A1A", "#BFBFBF"), 
                    labels=c("Inner Suburbs", "Outer Suburbs"),
                    na.translate = F) +
  theme(legend.title=element_blank(),
        legend.position=c("bottom"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              override.aes = list(size=1)))

#To save the file
ggsave("Portland-IO-2021.png",
       path = "~/desktop",
       width = 6,
       height = 8,
       units = "in",
       dpi = 500)

#To save
#save(InnerOuter_Suburbs_2021,file = "~/OneDrive - The Pennsylvania State University/Suburban typologies Paper/InnerOuter_2021.Rdata")



