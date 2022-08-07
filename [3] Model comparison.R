# Load Controlv1 by '[1] Load dataset.R' file
# Load model3a by '[2] Develop a model of parasitaemia.R' file
# Load required packages
library(pacman)
p_load(tidyverse, lme4, ggplot2, sjPlot)

# Model comparison
# Compare the linear regression model and model3a (LME model) by plotting observed vs predicted parasite load

# Create new dataset which can be used to predict parasite load 
# Time range is from day 1 to day 7
newdata <- Controlv1 %>% 
  select("Mouse", "Group.for.graph", "Inoculum") %>%
  crossing(Time = 1:7)

# Take predicted parasite load by the linear regression model - the model was developed and loaded by '[2] Develop a model of parasitaemia.R' file
newdata_linear <- newdata %>%
  mutate(Log.odds = predict(linear1, newdata))

# Take predicted parasite load by the LME model3a
newdata_LME <- newdata %>%
  mutate(Log.odds = predict(model3a, newdata))

# Plot observed vs predicted parasite load by each model - Figure 1
Controlv1 %>%
  ggplot(aes(x = Time, y = Log.odds, colour = Inoculum))+
  geom_line(data = newdata_LME,
            colour = "black",
            linetype = "solid")+
  geom_line(data = newdata_linear,
            colour = "black",
            linetype = "dotdash")+
  geom_point()+
  facet_wrap(~Group.for.graph)+
  labs(y="Parasite load", x= "Days after parasite injection", colour = "Inoculum")+
  theme(axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"))
ggsave("Figure1.png")
