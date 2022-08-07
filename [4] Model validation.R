# Load Controlv1 and Controlv2 by '[1] Load dataset.R' file
# Load required packages
library(pacman)
p_load(tidyverse, lme4, ggplot2, sjPlot)

# Model validation
# Prepare dataset with the first three parasitaemia from both DatasetA and B = early measurement - data is already loaded by '[1] Load dataset.R' file
early_measurement <- full_join(Controlv1, Controlv2, by = c("Mouse", "Time", "Parasitaemia", "Proportion", "Log.odds", "Label", "Inoculum")) %>%
  select_if(~ !any(is.na(.))) %>%
  filter(Time <= 5)

# Prepare dataset with the rest of the data which was not included in the early measurement = future measurement
future_measurement <- full_join(Controlv1, Controlv2, by = c("Mouse", "Time", "Parasitaemia", "Proportion", "Log.odds", "Label", "Inoculum")) %>%
  select_if(~ !any(is.na(.))) %>%
  filter(Time > 5) %>%
  mutate(N.Inoculum = ifelse(Inoculum == "A", log(10^5),
                             ifelse(Inoculum == "B", log(8*10^5),
                                    ifelse(Inoculum == "C", log(10^6), log(10^4))))) %>%
  filter(N.Inoculum == log(10^4))

# Convert a categorical factor, inoculum, to a continuous log-scaled factor 
Controlv3 <- early_measurement %>%
  mutate(N.Inoculum = ifelse(Inoculum == "A", log(10^5),
                             ifelse(Inoculum == "B", log(8*10^5),
                                    ifelse(Inoculum == "C", log(10^6), log(10^4)))))

# Rewrite the model3a with the continuous log-scaled inoculum factor
model3a_continuous <- lmer(Log.odds ~ N.Inoculum*Time + (Time|Mouse), data = Controlv3)

# Create new dataset which can be used to predict parasite load for the subject from DatasetB only (Inoculum = 10^4)
newdata_continuous <- Controlv3 %>% 
  select("Mouse", "N.Inoculum") %>%
  filter(N.Inoculum == log(10^4)) %>%
  crossing(Time = 1:7) 

newdata_continuous_prediction <- newdata_continuous %>%
  mutate(Log.odds = predict(model3a_continuous, newdata_continuous))

# Plot for the early measurements with the predicted line
Figure2 <- Controlv3 %>%
  filter(N.Inoculum == log(10^4)) %>%
  ggplot(aes(x = Time, y = Log.odds, colour = Inoculum))+
  geom_line(data = newdata_continuous_prediction,
            colour = "black",
            linetype = "solid")+
  geom_point()+
  facet_wrap(~Mouse)+
  labs(y="Parasite load", x= "Days after parasite injection", colour = "Inoculum")+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold")) +
  geom_point()

# Add future points - Figure 2
Figure2 +
  geom_point(aes(x = Time, y = Log.odds), 
             shape = 17, size = 2, data = future_measurement)
ggsave("Figure2.png")
