#3/28, initiated by BS
#Goal:  To visualize and plot the social and spatial data 

#Libraries
require(tidyverse)
require(RColorBrewer)
require(scales)

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
                                Type == "NCDP" ~ "Census Designated")) %>%
  mutate(Order = case_when(Type == "Yes" ~ "5",
                           Type == "No" ~ "1",
                           Type == "Inner" ~ "4",
                           Type == "Outer" ~ "2",
                           Type == "NCDP" ~ "3")) %>%
  mutate(Order = case_when(Type == "Yes" ~ "4",
                           Type == "No" ~ "1",
                           Type == "Inner" ~ "2",
                           Type == "Outer" ~ "5",
                           Type == "NCDP" ~ "3")) 


ggplot(data=City_Income[City_Income$City == "Charlotte", ], aes(x = Year, y = Estimate, group = Unique, color = Definition, alpha = Form)) +
  geom_path(aes(group = Unique, linetype = Order, color = Definition, size = Definition), 
            arrow = arrow(ends = "last", length = unit(0.15, "inches")), 
            show.legend = F) +
  theme_minimal() +
  scale_color_manual(values=c('Distance'='#8da0cb', 'Census Designated'='#fc8d62', "Age" = "#66c2a5"),
                     guide = "none") +
  scale_alpha_manual(name = "",
                     values = c(0.4, 1, 0.4),
                     guide = "none") +
  scale_size_manual(values = c('Distance'= 1.15, 'Census Designated'= 1.15, "Age" = 1.15),
                    guide = "none") +
  scale_linetype_manual(values = c('1' = "longdash",'3' = "solid",'4' = "longdash", 
                                  '5' = "solid", '2' = "solid"),
                        labels = c("Inner", "Outer", "Non-CDP", "Pre-Civil Rights", "Post-Civil Rights"),
                        name = "",
                        guide = guide_legend(override.aes = list(color = c("#66c2a5", "#66c2a5",
                                                                 "#fc8d62", "#8da0cb", "#8da0cb")),
                                             label.position = "bottom")) +
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
        panel.grid.major.x = element_line(size = 0.65,
                                          color = "grey"),
        panel.grid.minor.x = element_line(size = 0.2),
        panel.grid.major.y = element_line(size = 0.5),
        panel.grid.minor.y = element_line(size = 0.2)) +
  labs(title = "Charlotte") 


ggsave("Charlottencome.png",
       path = "~/desktop",
       width = 10,
       height = 7,
       units = "in",
       dpi = 500)


#Foreign born identity chart

load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/Born.Rdata")

Born_Totals <- Born %>%
  pivot_longer(
    cols = c(Type, PostCRSuburb, Suburb),
    values_to = "Type"
  ) %>%
  drop_na(Type) %>%
  group_by(City, Type) %>% 
  summarize_if(is.numeric,sum,na.rm = TRUE) %>%
  pivot_longer(
    cols = ends_with("1"), 
    names_to = "Form", 
    values_to = "Estimate",
    values_drop_na = TRUE) %>%
  separate(Form, into = c("Form", "Year"), remove = FALSE, extra = "merge") %>%
  mutate(Year = case_when(Year == 11 ~ 2011,
                          Year == 21 ~ 2021)) %>%
  mutate(Definition = case_when(Type == "Yes" ~ "Age",
                                Type == "No" ~ "Age",
                                Type == "Inner" ~ "Distance",
                                Type == "Outer" ~ "Distance",
                                Type == "NCDP" ~ "Census Designated")) %>%
  mutate(Name = case_when(Type == "Yes" ~ "Post CR",
                                Type == "No" ~ "Pre CR",
                                Type == "Inner" ~ "Inner",
                                Type == "Outer" ~ "Outer",
                                Type == "NCDP" ~ "Non-CDP")) %>%
  mutate(Order = case_when(Type == "Yes" ~ "4",
                          Type == "No" ~ "1",
                          Type == "Inner" ~ "2",
                          Type == "Outer" ~ "5",
                          Type == "NCDP" ~ "3")) 


SuburbList <- c(
  '1'="Age\nPre-Civil Rights",
  '2'="Distance\nInner",
  '3'="Census Designated\nNon-CDP",
  '4'="Post-Civil Rights",
  '5'="Outer")

ggplot(Born_Totals[Born_Totals$City == "Portland", ], aes(x = reorder(Year, -Estimate), y = Estimate, fill = Definition, alpha = Form)) + 
  geom_bar(position="fill", stat="identity", width=0.99, ) +
  theme_minimal(base_size = 12.5) + 
  facet_wrap( ~ Order, 
             ncol = 3,
             labeller = as_labeller(SuburbList)) +
  coord_flip() +
  labs(title = "Portland", 
       x = "", 
       y = "") +
  scale_y_continuous(labels = percent,
                     breaks=seq(0, 1, 0.2)) +
  guides(fill=guide_legend(title=NULL)) +
  theme(plot.title = element_text(hjust = 0.5, size=22),
        legend.position="bottom",
        strip.placement = "outside",
        strip.text.x = element_text(size = 18, face = "bold"),
        axis.text.x = element_text(size = 8),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.major.x = element_line(size = 0.5,
                                          color = "grey"),
        panel.grid.minor.x = element_line(size = 0.2,
                                          color = "lightgrey"),
        panel.grid.major.y = element_line(size = 0),
        panel.grid.minor.y = element_line(size = 0)) +
  scale_fill_manual(values=c('Distance'='#8da0cb', 
                             'Census Designated'='#fc8d62', 
                             "Age" = "#66c2a5"),
                    guide = 'none') +
  scale_alpha_manual(name = "",
                     values = c(1, 0.55),
                     guide = guide_legend(override.aes = list(fill = "#fc8d62", "#fc8d62",
                                                              label.position = "top"),
                                          label.position = "bottom",
                                          reverse = TRUE)) +
  guides(fill = "none") 


ggsave("PortlandBorn.png",
       path = "~/desktop",
       width = 10,
       height = 7,
       units = "in",
       dpi = 500)

