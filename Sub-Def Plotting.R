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
  separate(Form, into = c("Form", "Year"), remove = FALSE, extra = "merge") %>%
  unite("Unique", Type:Form, remove = FALSE) %>%
  mutate(Year = case_when(Year == 11 ~ 2011,
                          Year == 21 ~ 2021)) %>%
  mutate(Definition = case_when(Type == "Yes" ~ "Age",
                                Type == "No" ~ "Age",
                                Type == "Inner" ~ "Distance",
                                Type == "Outer" ~ "Distance",
                                Type == "NCDP" ~ "Census Designated"))


ggplot(data=City_Income[City_Income$City == "Portland", ], aes(x = Year, y = Estimate, group = Unique, color = Definition, alpha = Form)) +
  geom_path(aes(group = Unique, linetype = Type, color = Definition, size = Definition), 
            arrow = arrow(ends = "last", length = unit(0.15, "inches")), 
            show.legend = FALSE) +
  theme_minimal() +
  scale_color_manual(values=c('Distance'='#8da0cb', 'Census Designated'='#fc8d62', "Age" = "#66c2a5")) +
  scale_alpha_manual(name = "",
                     values = c(0.4, 1, 0.4),
                     guide = guide_legend(reverse = TRUE)) +
  scale_size_manual(values = c('Distance'=1, 'Census Designated'= 1.5, "Age" = 1)) +
  scale_linetype_manual(values = c('Inner' = "dashed",'NCDP' = "solid",'No' = "dashed", 
                                  'Yes' = "solid", 'Outer' = "solid"),
                        guide = guide_legend(reverse = TRUE)) +
  scale_x_continuous(breaks=c(2011, 2021), limits = c(2010, 2022)) +
  scale_y_continuous(labels=scales::dollar_format()) +
  facet_grid(. ~ Definition, 
             scales = "free_x") +
  theme(strip.placement = "outside",
        strip.text.x = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size=22),
        legend.position="bottom",
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.x = element_line(size = 0.65),
        panel.grid.minor.x = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.y = element_line(size = 0.2)) +
  guides(alpha = guide_legend(override.aes = list(size = 3.5),
                              label.position = "top")) +
  labs(title = "Portland") 


ggsave("PortlandIncome.png",
       path = "~/desktop",
       width = 10,
       height = 7,
       units = "in",
       dpi = 500)
