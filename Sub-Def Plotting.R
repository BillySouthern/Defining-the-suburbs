#3/28, initiated by BS
#Goal:  To visualize and plot the social and spatial data 

#Libraries
require(tidyverse)

#Bar chart differences
load("/Users/billy/Library/CloudStorage/OneDrive-ThePennsylvaniaStateUniversity/Suburban typologies Paper/Output Data/TotalPopulation.Rdata")

UrbvsSuburb <- PhillyVoterChange %>%
  group_by(Environment) %>%
  st_drop_geometry() %>%
  summarise(Dem_17 = sum(Democratic_17),
            Dem_21 = sum(Democratic_21),
            Repub_17 = sum(Republican_17),
            Repub_21 = sum(Republican_21)) %>%
  filter(! (Environment == "Rural" | Environment == "")) %>%
  mutate(Dem_Change = (Dem_21/Dem_17)-1) %>%
  mutate(Repub_Change = (Repub_21/Repub_17)-1)

UrbvsSuburbpercent <- UrbvsSuburb %>%
  select(Environment, Dem_Change, Repub_Change) %>%
  rename(Dem_21 = Dem_Change,
         Rep_21 = Repub_Change) %>%
  pivot_longer(!Environment) %>%
  rename(Change = name,
         Percent = value) %>%
  mutate(observation = 1:n()) %>%
  mutate(observation = observation*2)

UrbvsSuburb <- UrbvsSuburb %>%
  select(Environment, Dem_17, Dem_21, Repub_17, Repub_21) %>%
  pivot_longer(!Environment) %>%
  rename(Group = name,
         Total = value)%>%
  mutate(observation = 1:n())

UrbvsSuburb <- merge(UrbvsSuburb, UrbvsSuburbpercent, by = c("Environment", "observation"), all.x = T) %>%
  select(-observation, -Change)%>%
  unite("Object", Environment:Group, remove = FALSE) %>%
  separate(Group, into = c("Party", "Year"), sep="_") %>%
  unite("Dodge", Environment:Party, remove = FALSE) 


rm(UrbvsSuburbpercent)

ggplot(UrbvsSuburb, aes(y = Dodge, x = Total, fill=Party, alpha = Year)) + 
  geom_col(width = 0.7, position = "dodge") +
  facet_grid(rows = vars(Environment), scales = "free") +
  geom_text(aes(x = Total, y = Dodge, label = scales::percent(Percent)), 
            hjust = -0.1, size = 4, color = "black", fontface = "bold",
            inherit.aes = TRUE, na.rm = T) +
  theme_minimal(base_size = 12.5) + 
  theme(legend.key.size = unit(1, "cm"),
        legend.position = "top",
        strip.text = element_text(
          size = 16, color = "black", face = "bold"),
        plot.title = 
          element_text(hjust = 0.5)) +
  labs(title = "Party registration change across the urban and suburban", 
       x = "", 
       y = "") +
  scale_x_continuous(labels = ~ number_format(scale = .000001, 
                                              suffix = " million")(abs(.x)),
                     expand = c(0, 150000)) +
  scale_y_discrete(labels=c('Democratic\n2017-2021', 'Republican\n2017-2021')) +
  scale_alpha_discrete(range = c(0.45, 1), 
                       guide = "none") +
  scale_fill_manual(name = "", values = c("#377eb8","#e41a1c"),
                    labels = c("Democratic", "Republican")) +
  guides(fill = guide_legend(byrow = F,
                             label.position = "top")) 
