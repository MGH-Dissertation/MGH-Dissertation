# Load required packages
library(pacman)
p_load(tidyverse, lme4, ggplot2, sjPlot, glmmTMB)

# Linear regression model #### 
# We developed a linear regression model with time variable and assessed the model fit based on Q-Q plot
linear1 <- lm(Log.odds~Time, data = Controlv1) # Controlv1 is already loaded by '[1] Load dataset.R' file
summary(linear1) # Table 1 
plot(linear1) # Gives you 4 different plots - Second Normal Q-Q plot was used as Supplementary Figure S2

# Create a function that can convert the estimates to original values
InvLogit <- function(X){
  exp(X)/(1+exp(X))
}

coef(linear1)
InvLogit(0.3195592) # slope variable
InvLogit(-2.4348248) # intercept variable

# Linear Mixed Effect (LME) model ####
# A hierarchy of the model was developed using inoculum and time as fixed effects with between-subject variation on slope (Time|Mouse) or intercept (1|Mouse) 
model0 <- lmer(Log.odds ~ (1|Mouse), data = Controlv1)
model1 <- lmer(Log.odds ~ Inoculum + (1|Mouse), data = Controlv1)
model1a <- lmer(Log.odds ~ Time + (1|Mouse), data = Controlv1)
model1b <- lmer(Log.odds ~ Time + (Time|Mouse), data = Controlv1) # a random effect for both the intercept and slope
model2 <- lmer(Log.odds ~ Inoculum + Time + (1|Mouse), data = Controlv1)
model2a <- lmer(Log.odds ~ Inoculum + Time + (Time|Mouse), data = Controlv1) # allow random slopes
model3 <- lmer(Log.odds ~ Inoculum*Time + (1|Mouse), data = Controlv1)
model3a <- lmer(Log.odds ~ Inoculum*Time + (Time|Mouse), data = Controlv1)

# Compare model fitness using AIC and BIC statistics
anova(model0, model1, model1a, model1b, model2, model2a, model3, model3a) # Table 2

# model3a has the lowest AIC and BIC values so we found the predictor estimates from model3a
tab_model(model3a) # Table 3

# Check model assumptions - Supplementary Figure S3
plot_model(model3a, type = "diag", show.values = TRUE)

