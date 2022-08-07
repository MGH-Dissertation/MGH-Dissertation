# Load required packages
library(pacman)
p_load(tidyverse, readxl, ggplot2)

# There are five datasets we will use for the analysis:
# (i) Controlv1 = Control group data from DatasetA - model development
# (ii) Controlv2 = Control group data from DatasetB - model validation
# (iii) Lactate = Blood level of lactate data measured for the control group in DatasetA (Controlv1)
# ---- Lactate data is confidential therefore, is not uploaded. 
# (iv) HCT = Haematocrit data measured for the control group in DatasetA (Controlv1)
# (v) ELISA = ELISA data for the control group in DatasetA (Controlv1)

datav1 <- read_excel('Data.xlsx', sheet = 1)
datav2 <- read_excel('Data.xlsx', sheet = 2) # create (ii)
HCT <- read_excel('Data.xlsx', sheet = 3) # create (iii)
ELISA <- read_excel('Data.xlsx', sheet = 4) %>%
  pivot_longer(cols = c('Syndecan-1', 'Elastase', 'IL-10', 'MMP-8', 'TIMP-1', 'G-CSF'),
               names_to = 'ELISA',
               values_to = 'Measurements') %>%
  drop_na(Measurements) # create (iv)

# Create (i) by selecting control group only and remove N/A values
Controlv1 <- filter(datav1, Label == "Control") %>%
  drop_na(Log.odds)

# Create (ii) by removing N/A values
Controlv2 <- datav2 %>%
  drop_na(Log.odds)

# Supplementary Figure S1. Increasing parasite loads after parasite injection
Controlv1 %>% 
  ggplot()+
  geom_jitter(aes(x = Time, y = Parasitaemia, colour = Inoculum))+
  theme_minimal() +
  labs(x = "Days after infection", y ="Parasitaemia (%)", colour="Inoculum")+
  theme(axis.title = element_text(face="bold"), legend.title=element_text(face="bold"))+
  theme(legend.title = element_text(size=12, color = "black", face="bold"),
        legend.justification=c(0,1), 
        legend.position=c(0.05, 0.95),
        legend.background = element_blank(),
        legend.box.background = element_rect(color="black", size=1),
        legend.key = element_blank())
