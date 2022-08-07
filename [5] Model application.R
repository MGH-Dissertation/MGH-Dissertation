# Load HCT and ELISA by '[1] Load dataset.R' file
# Load model3a by '[2] Develop a model of parasitaemia.R' file
# Load required packages
library(pacman)
p_load(tidyverse, readxl, writexl, lme4, ggplot2, ggpubr)

# We will correlate haematocrit, blood level of lactate, and ELISA data from DatasetA with individual mouse parasite growth rate (= estimated individual model slope) 
# Extract estimated parasite growth rate by model3a
slope_control <- ranef(model3a)$Mouse
slope_control <- rownames_to_column(slope_control, "Mouse") # convert rownames to existing column as mouse name was rowname

# Dataframe for correlation analysis 
HCT_Slope_control= full_join(slope_control, HCT, by = c("Mouse")) %>% drop_na() 
ELISA_Slope_control = full_join(slope_control, ELISA, by = c("Mouse")) %>%
  mutate(Measurements = log(Measurements))

# Correlation analysis between HCT and parasite growth rate - Table 3
HCT_Slope_control %>%
  filter(Day == 7) %>%
  ggscatter(y = "HCT", x = "Time", add = "reg.line", 
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
            ylab = "HCT (%)", xlab = "Parasite growth rate")+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        plot.title = element_text(face = "bold"))+
  theme_bw() 

# Correlation analysis between ELISA markers and parasite growth rate - Table 3
ELISA_Slope_control %>%
  filter(Day == 7) %>%
  ggscatter(y = "Measurements", x = "Time", add = "reg.line", 
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
            ylab = "Concentration in log[pg/mL]", xlab = "Parasite growth rate")+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        plot.title = element_text(face = "bold"))+
  theme_bw()+ 
  facet_wrap(~ELISA, scales = "free_y")

############################################################################################################################################
### Do not run - this block of code generates correlations between blood level of lactate and parasite growth rate - Table 3 - data is confidential ###
# Assuming that lactate data was loaded succefully and was converted to the dataset suitable for correlation analysis (Check the section of '# Dataframe for correlation analysis' above)
Lactate_Slope_control %>%
  ggscatter(y = "Lactate.Log", x = "Time", add = "reg.line", 
            conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
            ylab = "Lactate [mmol/L]", xlab = "Parasite growth rate")+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        plot.title = element_text(face = "bold"))+
  theme_bw()+ 
  facet_wrap(~Time)
############################################################################################################################################
