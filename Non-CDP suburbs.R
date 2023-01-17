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
#2020
#Pittsburgh 2020 tracts
Pittsburgh_tracts_2020 <- map_dfr(c("PA", "OH", "WV"), ~{
  tracts(.x, cb = TRUE, year = 2020)
})

#Pittsburgh CBSA
Pittsburgh_metro <- combined_statistical_areas(cb = TRUE, resolution = "500k") %>%
  filter(str_detect(NAME, "Pittsburgh")) 

#Pittsburgh Census Designated Place (and removing nearby areas beyond the city)
PittsburghCDP <- places(state = "PA", cb = TRUE) %>%
  filter(str_detect(NAME, "Pittsburgh")) %>%
  filter(!str_detect(NAME, "University")) 

#Here we include all tracts that border the CMSA boundary, as we're are interested in the periphery
Pittsburgh_2020 <- Pittsburgh_tracts_2020[Pittsburgh_metro, ]

#To remove the CDP tracts from Pittsburgh
Pittsburgh_2020 <-  Pittsburgh_2020[lengths(st_intersects(Pittsburgh_2020,PittsburghCDP))==0,]

#Erasing the water bodies in Pittsburgh
Pittsburgh_2020 <- erase_water(Pittsburgh_2020)

#General tidying
Pittsburgh_2020  <- Pittsburgh_2020 %>%
  mutate(City = "Pittsburgh") %>%
  rename(State_Name = STUSPS) %>%
  relocate(State_Name, .before = STATEFP) %>%
  relocate(City, .before = COUNTYFP) %>%
  relocate(AFFGEOID, .before = State_Name) 

#Initial visualization of Pittsburgh's suburbs
ggplot() + 
  geom_sf(data = Pittsburgh_2020, fill = "black", color = "grey") + 
  geom_sf(data = PittsburghCDP, fill = NA, color = "blue") + 
  geom_sf(data = Pittsburgh_metro, fill = NA, color = "red") + 
  theme_void()


References
Hall, M., & Lee, B. (2010). How Diverse Are US Suburbs? Urban Studies, 47(1), 3–28. https://doi.org/10.1177/0042098009346862
Walker, K. (2022). Analyzing US Census Data: Methods, Maps, and Models in R (1st ed.). CRC Press.


