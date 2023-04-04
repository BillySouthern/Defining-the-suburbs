#3/28, initiated by BS
#Goal:  To visualize and plot the social and spatial data 

#Libraries
require(tidyverse)
require(RColorBrewer)


#Bar chart differences
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/TotalPopulation.Rdata")

#Prepping the data
Pop_Totals <- Total_Population %>%
  pivot_longer(
    cols = c(Type, PostCRSuburb, Suburb),
    values_to = "Type"
  ) %>%
  select(!name) %>%
  drop_na(Type) %>%
  group_by(City, Type) %>%
  summarise(Total_2011 = sum(Total_2011, na.rm = T),
            Total_2021 = sum(Total_2021, na.rm = T)) %>%
  mutate(PercentChange = (Total_2021/Total_2011)-1) %>%
  pivot_longer(
    cols = c(Total_2011, Total_2021),
    values_to = "Estimate"
  ) %>%
  rename(Year = "name") %>%
  mutate(Year = str_sub(Year, 7, -1)) %>%
  mutate(Suburb = case_when(Type == "Yes" ~ "Post-Civil Rights",
                            Type == "No" ~ "Pre-Civil Rights",
                            Type == "Inner" ~ "Inner",
                            Type == "Outer" ~ "Outer",
                            Type == "NCDP" ~ "Non-CDP")) %>%
  mutate(Definition = case_when(Type == "Yes" ~ "Age",
                                Type == "No" ~ "Age",
                                Type == "Inner" ~ "Distance",
                                Type == "Outer" ~ "Distance",
                                Type == "NCDP" ~ "Census Designated")) %>%
  mutate(PercentChange=ifelse(row_number()%%2==1,NA,PercentChange))






ggplot(Pop_Totals[Pop_Totals$City == "Charlotte", ], 
       aes(y = Suburb, x = Estimate, alpha = Year, fill = Definition)) + 
  geom_col(width = 0.7, position = "dodge", show.legend=FALSE) +
  geom_text(aes(x = Estimate, y = Suburb, label = scales::percent(PercentChange)), 
            hjust = -0.2, size = 4, color = "black", fontface = "bold", alpha = 1,
            inherit.aes = TRUE, na.rm = T) +
  theme_minimal(base_size = 12.5) + 
  theme(legend.key.size = unit(1, "cm"),
        legend.position = "top",
        strip.text = element_text(
          size = 16, color = "black", face = "bold"),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5, size = 22),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()) +
  labs(title = "Charlotte") +
  scale_x_continuous(labels = scales::comma,
                     expand = c(0, 150000)) +
  scale_fill_brewer(palette = "Dark2") +
  scale_alpha_discrete(range = c(0.45, 1), 
                       guide = "none") +
  guides(fill = guide_legend(byrow = F,
                             label.position = "top")) +
  facet_grid(rows = vars(Definition), scales = "free", switch = "y") 
  
ggsave("CharlotteTotPop.png",
       path = "~/desktop",
       width = 12,
       height = 7,
       units = "in",
       dpi = 500)


--------
#Change in race
  
#Bar chart differences
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/TotalRace.Rdata")

Race_Totals <- Total_Race %>%
  pivot_longer(
    cols = c(Type, PostCRSuburb, Suburb),
    values_to = "Type"
  ) %>%
  select(!name) %>%
  drop_na(Type) %>%
  group_by(City, Type) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  mutate(WhiteChange = (NHWhite_21/NHWhite_11)-1,
         BlackChange = (NHBlack_21/NHBlack_11)-1,
         HispanicChange = (Hispanic_21/Hispanic_11)-1,
         AsianChange = (Asian_21/Asian_11)-1,
         SGChange = (SG_21/SG_11)-1) %>%
  mutate(Suburb = case_when(Type == "Yes" ~ "Post-Civil Rights",
                            Type == "No" ~ "Pre-Civil Rights",
                            Type == "Inner" ~ "Inner",
                            Type == "Outer" ~ "Outer",
                            Type == "NCDP" ~ "Non-CDP")) %>%
  mutate(Definition = case_when(Type == "Yes" ~ "Age",
                                Type == "No" ~ "Age",
                                Type == "Inner" ~ "Distance",
                                Type == "Outer" ~ "Distance",
                                Type == "NCDP" ~ "Census Designated")) %>%
  select(City, Definition, Suburb, WhiteChange, BlackChange, HispanicChange, AsianChange, SGChange)

write.csv(Race_Totals, "/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/RaceTotals.csv", row.names=T)

#Median income changes

load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/MedianIncome.Rdata")

City_Income <- Median_Income %>%
  pivot_longer(
    cols = c(Type, PostCRSuburb, Suburb),
    values_to = "Type") %>% 
  drop_na("Type") %>%
  group_by(City, Type) %>% 
  summarise(Max_11=max(Income_11, na.rm = TRUE), Min_11=min(Income_11, na.rm = TRUE), 
            Median_11=median(Income_11, na.rm = TRUE),
            Max_21=max(Income_21, na.rm = TRUE), Min_21=min(Income_21, na.rm = TRUE), 
            Median_21=median(Income_21, na.rm = TRUE)) %>% 
  pivot_longer(
    cols = starts_with("M"), 
    names_to = "Form", 
    values_to = "Estimate",
    values_drop_na = TRUE) %>%
  mutate(Year = case_when(Form == "Min_11" ~ "2011",
                          Form == "Min_21" ~ "2021",
                          Form == "Median_11" ~ "2011",
                          Form == "Median_21" ~ "2021",
                          Form == "Max_11" ~ "2011",
                          Form == "Max_21" ~ "2021")) 

#Split min max, then join on top of each other

City_Income$Form = str_sub(City_Income$Form, end = -4)

ggplot(data=City_Income[City_Income$City == "Pittsburgh", ], aes(x = Year, y = Estimate, group = Form, color = Type)) +
  geom_line() + geom_point()
