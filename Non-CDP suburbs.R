#Non census-designated place suburbs
#Choices include the consolidated metropolitan statistical areas and census designated places
#The code retains tracts that interact with the CMSA boundary as we are interested in peripheral processes
#Tracts that intersect with the CDP are removed as we are less interested in inner-urban processes

#This definition is inspired by Hall and Lee's (2010) paper which disaggregates the CMSA by CDP

#Libraries
library(tigris)
library(tidyverse)
library(sf)

options(tigris_use_cache = TRUE)

#The suburbs of Pittsburgh, PA

#Pittsburgh tracts 2011
Pittsburgh_tracts_2011 <- map_dfr(c("PA", "OH", "WV"), ~{
  tracts(.x, year = 2011)
})

#Pittsburgh CBSA 2011
Pittsburgh_metro_2011 <- combined_statistical_areas(resolution = "500k", year = 2011) %>%
  filter(str_detect(NAME, "Pittsburgh")) 

#Pittsburgh Census Designated Place 2011
PittsburghCDP_2011 <- places(state = "PA", year = 2011) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  filter(!str_detect(NAME, "University")) %>%
  filter(!str_detect(NAME, "East")) 
  
#Here we include all tracts that border the CMSA boundary, as we're are interested in the periphery
Pittsburgh_2011 <- Pittsburgh_tracts_2011[Pittsburgh_metro_2011, ]

#Removing the CDP tracts from Pittsburgh
Pittsburgh_2011 <-  Pittsburgh_2011[lengths(st_intersects(Pittsburgh_2011,PittsburghCDP_2011))==0,]

#Erase water
Pittsburgh_2011 <- erase_water(Pittsburgh_2011)

#General tidying
Pittsburgh_2011  <- Pittsburgh_2011 %>%
  mutate(State_Name = "PA",
         City = "Pittsburgh") %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP)

#For initial visualizing
ggplot() + 
  geom_sf(data = Pittsburgh_2011, fill = "black", color = "grey") + 
  geom_sf(data = PittsburghCDP_2011, fill = NA, color = "blue") + 
  geom_sf(data = Pittsburgh_metro_2011, fill = NA, color = "red") + 
  theme_void()


#2021
#Pittsburgh tracts 2021
Pittsburgh_tracts_2021 <- map_dfr(c("PA", "OH", "WV"), ~{
  tracts(.x, cb = TRUE, year = 2021)
})

#Pittsburgh CBSA 2021
Pittsburgh_metro_2021 <- combined_statistical_areas(cb = TRUE, resolution = "500k") %>%
  filter(str_detect(NAME, "Pittsburgh")) 

#Pittsburgh Census Designated Place 2021
PittsburghCDP_2021 <- places(state = "PA", cb = TRUE) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  filter(!str_detect(NAME, "University")) %>%
  filter(!str_detect(NAME, "East")) 

#Here we include all tracts that border the CMSA boundary, as we're are interested in the periphery
Pittsburgh_2021 <- Pittsburgh_tracts_2021[Pittsburgh_metro_2021, ]

#Removing the CDP tracts from Pittsburgh
Pittsburgh_2021 <-  Pittsburgh_2021[lengths(st_intersects(Pittsburgh_2021,PittsburghCDP_2021))==0,]

#Erase water
Pittsburgh_2021 <- erase_water(Pittsburgh_2021)

#General tidying
Pittsburgh_2021  <- Pittsburgh_2021 %>%
  mutate(City = "Pittsburgh") %>%
  rename(State_Name = STUSPS) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  relocate(AFFGEOID, .before = State_Name) 
  
#For a more detailed visualization of the new data (with a legend)
ggplot() + 
  geom_sf(data = Pittsburgh_2021, aes(fill = "black", color = "grey"), show.legend = "line") + 
  geom_sf(data = PittsburghCDP_2021, aes(color = "blue"), fill = NA,  show.legend = "line")  + 
  geom_sf(data = Pittsburgh_metro_2021, aes(color = "dark red"), fill = NA,  show.legend = "line") + 
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



References
Hall, M., & Lee, B. (2010). How Diverse Are US Suburbs? Urban Studies, 47(1), 3–28. https://doi.org/10.1177/0042098009346862
Walker, K. (2022). Analyzing US Census Data: Methods, Maps, and Models in R (1st ed.). CRC Press.


