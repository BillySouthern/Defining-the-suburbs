#Unused code 3/29 BS

#To try acquire city names for inner/outer suburbs
#Joining City name
CityNames <- CentralCities_2011 %>%
  select(STATEFP, NAME) %>%
  st_set_geometry(NULL)

InnerOuter_Suburbs_2011 <- InnerOuter_Suburbs_2011 %>% 
  left_join(CityNames, by="STATEFP") %>%
  select(GEOID, Suburb, NAME.y) %>%
  rename(City = NAME.y)