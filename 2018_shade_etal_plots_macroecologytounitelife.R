library(data.table)
library(tidyverse)
library(e1071)
library(patchwork)

metab <- fread("C:/Users/Userszn/Documents/PhD/TARA_Data/metaB/diatoms/barv2016202_swarm_d1_t100.txt", 
               sep = " ", stringsAsFactors = F)

metab1 <- metab %>% 
  rename(OTU = V1) %>% 
  pivot_longer(`TARA_158_SRF_20-180`:`TARA_078_DCM_20-180`) %>% 
  filter(grepl("SRF", name), value >0) %>% 
  separate(name, into = c("TARA", "St.label", "roba", "roba2"), sep = "_") %>% 
  select(OTU, St.label, value)

##RAD
metab1 %>%
  group_by(St.label) %>%
  summarise(relab = value/sum(value),
            richness = n_distinct(OTU)) %>% 
  arrange(desc(relab)) %>%
  mutate(Rank = seq_len(n()),
         `log10 of % relative abundance` = log10(relab)) %>%
  ungroup() %>% 
  
  ggplot() +
  aes(x = Rank, y = `log10 of % relative abundance`,
      col = richness, group = St.label) +
  geom_line(lwd = 0.5, alpha = 0.25) +
  theme_bw() + 
  ggtitle("Whittaker (RAD) on diatom OTUs",
          subtitle = "metaG, 20-180, SUR")


##occupancy abundance relationship
metab1 %>%
  mutate(tot.st = n_distinct(St.label)) %>% 
  group_by(OTU) %>% 
  mutate(`OTU occupancy` = n_distinct(St.label)/tot.st,
         `Global Mean OTU Abundance` = sum(value)/tot.st,
         `Local Mean OTU Abundance` = sum(value)/`OTU occupancy`) %>% 
  ungroup() %>% 
  select(OTU, St.label, `OTU occupancy`:`Local Mean OTU Abundance`) %>% 
  unique() %>% 
  ggplot() +
  aes(x=`Local Mean OTU Abundance` , y=`OTU occupancy`) +
  geom_point() +
  theme_bw()


##species-area curve : come si fa?

## distance decay similarity: x = distance between stations in meters?
  #in lagrangian? y = Jaccard similarity 

## Rarefaction curve : x = number of sequences, y = OTU richness
  # è uno dei 4 plot locey
metab1 %>% 
  group_by(St.label) %>% 
  summarise(`Number of sequences` = sum(value),
            `OTU richness` = n_distinct(OTU)) %>% 
  ungroup() %>% 
  ggplot() +
  aes(x = `Number of sequences`, y = `OTU richness`) +
  #geom_line(lwd = 0.5, alpha = 0.25) +
  theme_bw() +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

##Elevational richness gradient 
  # non ci interessa
