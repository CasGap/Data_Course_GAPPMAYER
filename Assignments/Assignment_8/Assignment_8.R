library(tidyverse)
library(easystats)
library(modelr)
library(patchwork)

#1. load the “/Data/mushroom_growth.csv” data set
df <- read.csv("../Data/mushroom_growth.csv")


#2. create several plots exploring relationships between the response and predictors
df %>% 
  ggplot(aes(x=Species,y=GrowthRate)) +
  geom_boxplot()
# P. cornucopiae has a larger range of growth rate and a little bit higher median

df %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate,color=Species,shape=Humidity)) +
  facet_grid(Temperature ~ Light, labeller = label_both) +
  geom_point()
# This is my attempt to show all variables
# From this graph it looks like both species grow more with more light and humidity and moderate nitrogen.
# P. cornucopiae does better in Temperature 20 while P. ostreotus does not change as much with temperature.

filter(df, Species == "P.ostreotus") %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate,color=Humidity)) +
  facet_grid(Temperature ~ Light, labeller = label_both) +
  geom_point()
# From this graph of only P. ostreotus, it looks like when temperature is 25, humidity matters more.
# And temperature increases P. ostreotus growth more than I realized.

filter(df, Species == "P.ostreotus") %>% 
  ggplot(aes(x=Temperature,y=GrowthRate)) +
  facet_grid(Humidity ~ Light, labeller = label_both) +
  geom_point() +
  geom_smooth(method = 'lm')
# P. ostreotus grows best with the higher light exposure, but only likes higher temperatures when humidity is also high.

filter(df, Species == "P.cornucopiae") %>% 
  ggplot(aes(x=Nitrogen,y=GrowthRate,color=Humidity)) +
  facet_grid(Temperature ~ Light, labeller = label_both) +
  geom_point()
# Humidity, for P. cornucopiae matters more when temperature is 20.

filter(df, Species == "P.cornucopiae") %>% 
  ggplot(aes(x=Temperature,y=GrowthRate)) +
  facet_grid(Humidity ~ Light, labeller = label_both) +
  geom_point() +
  geom_smooth(method = 'lm')
# P. cornucopiae grows best with the higher light exposure, but likes lower temperatures when humidity is high (negative correlation between humidity and temperature).


#3. define at least 4 models that explain the dependent variable “GrowthRate”
mod1 <- glm(data = df,
            formula = GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature + Humidity:Temperature)
mod2 <- glm(data = df,
            formula = GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature)
mod3 <- glm(data = df,
            formula = GrowthRate ~ Species * Light * Nitrogen * Humidity * Temperature)
mod4 <- glm(data = df,
            formula = GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature + 
  Species:Light + Light:Nitrogen + Species:Humidity + Light:Humidity + 
  Nitrogen:Humidity + Species:Temperature + Light:Temperature + 
  Humidity:Temperature + Species:Light:Humidity + Light:Nitrogen:Humidity + 
  Species:Light:Temperature + Species:Humidity:Temperature + 
  Light:Humidity:Temperature + Species:Light:Humidity:Temperature)
mod5 <- glm(data = df,
            formula = GrowthRate ~ .^2)
mod6 <- glm(data = df,
            formula = GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature + 
              Species:Light + Species:Humidity + Species:Temperature + 
              Light:Nitrogen + Light:Humidity + Light:Temperature + Humidity:Temperature)


#4. calculate the mean sq. error of each model
# add predictions to new data frame
pred <- df %>%
  spread_predictions(mod1,type='response') %>% 
  spread_predictions(mod2,type='response') %>% 
  spread_predictions(mod3,type='response') %>% 
  spread_predictions(mod4,type='response') %>% 
  spread_predictions(mod5,type='response') %>% 
  spread_predictions(mod6,type='response')
# calculate absolute difference between predicted and actual growth rate
pred <- pred %>% 
  mutate(resid1 = abs(mod1 - GrowthRate)) %>%
  mutate(resid2 = abs(mod2 - GrowthRate)) %>%
  mutate(resid3 = abs(mod3 - GrowthRate)) %>%
  mutate(resid4 = abs(mod4 - GrowthRate)) %>%
  mutate(resid5 = abs(mod5 - GrowthRate)) %>%
  mutate(resid6 = abs(mod6 - GrowthRate))
# mean_err for all 6 models
mean_err1 <- mean(pred$resid1,na.rm = TRUE)
mean_err2 <- mean(pred$resid2,na.rm = TRUE)
mean_err3 <- mean(pred$resid3,na.rm = TRUE)
mean_err4 <- mean(pred$resid4,na.rm = TRUE)
mean_err5 <- mean(pred$resid5,na.rm = TRUE)
mean_err6 <- mean(pred$resid6,na.rm = TRUE)
# This is what we did in class, but I wasn't sure if it is what you meant by mean sq. error
# So I tried this too
sqrt(mean((pred$GrowthRate - pred$mod1)^2))
sqrt(mean((pred$GrowthRate - pred$mod2)^2))
sqrt(mean((pred$GrowthRate - pred$mod3)^2))
sqrt(mean((pred$GrowthRate - pred$mod4)^2))
sqrt(mean((pred$GrowthRate - pred$mod5)^2))
sqrt(mean((pred$GrowthRate - pred$mod6)^2))


#5. select the best model you tried
# About 0.5 difference between mean absolute error of mod3 and mod4 and mean sq. error of mod3 and mod4
compare_performance(mod1,mod2,mod3,mod4,mod5,mod6) %>% plot
mod3$aic
mod4$aic
# overall, mod4 looks the best


#6. add predictions based on new hypothetical values for the independent variables used in your model
df <- add_predictions(df,mod4,type='response')


#7. plot these predictions alongside the real data
plot1 <- filter(df, Species == "P.cornucopiae") %>% 
  ggplot() +
  facet_grid(Temperature ~ Light, labeller = label_both) +
  geom_point(aes(x=Nitrogen,y=GrowthRate,color=Humidity)) +
  geom_line(aes(x=Nitrogen,y=pred,color=Humidity)) +
  labs(title="P. cornucopiae")
plot2 <- filter(df, Species == "P.ostreotus") %>% 
  ggplot() +
  facet_grid(Temperature ~ Light, labeller = label_both) +
  geom_point(aes(x=Nitrogen,y=GrowthRate,color=Humidity)) +
  geom_line(aes(x=Nitrogen,y=pred,color=Humidity)) +
  labs(title="P. ostreotus",caption = "points represent actual values
  lines represent Model 4 predictions")
combined_plot <- plot1 / plot2
combined_plot # extend plots tab vertically to view