library(tidyverse)
library(janitor)

#1. Read in the unicef data
df <- read.csv("unicef-u5mr.csv") %>% 
  clean_names()

#2. Get it into tidy format
dat <- pivot_longer(data = df, 
             cols = -c(country_name,continent,region), 
             names_to = "year",
             values_to = "u5mr",
             names_prefix = "u5mr_")
#fix the year column
library(lubridate)
yr <- as.Date(as.character(dat$year), format = "%Y")
dat$year <- year(yr)

#3. Plot each country’s U5MR over time
dat %>% 
  ggplot(aes(x=year,y=u5mr)) + 
  geom_line(aes(group = country_name)) +
  facet_wrap(~continent) +
  theme_bw()

#4. Save this plot as LASTNAME_Plot_1.png
ggsave("GAPPMAYER_Plot_1.png")

#5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year
dat %>% 
  group_by(continent,year) %>%
  summarize(mean_u5mr = mean(u5mr, na.rm = TRUE)) %>%
  ggplot(aes(x=year,y=mean_u5mr,color=continent)) +
  geom_line() +
  theme_bw()

#6. Save that plot as LASTNAME_Plot_2.png
ggsave("GAPPMAYER_Plot_2.png")

#7. Create three models of U5MR
mod1 <- glm(data = dat,
            formula = u5mr ~ year) # account for only year
mod2 <- glm(data = dat,
            formula = u5mr ~ year + continent) # account for year and continent
mod3 <- glm(data = dat,
            formula = u5mr ~ year * continent) # account for year, continent, and their interaction term

#8. Compare the three models with respect to their performance
mod1$aic
mod2$aic
mod3$aic # mod3 has the lowest AIC value
library(easystats)
compare_performance(mod1,mod2,mod3) %>% plot # mod3 does best at explaining the data in all areas
# From this information, mod3 is the best model.

#9. Plot the 3 models’ predictions
library(modelr)
pred <- dat %>%
  spread_predictions(mod1,type='response') %>% 
  spread_predictions(mod2,type='response') %>% 
  spread_predictions(mod3,type='response') %>% 
  pivot_longer(cols = starts_with("mod"),names_to = "model",values_to = "predicted_u5mr")
pred %>% 
  ggplot(aes(x=year,y=predicted_u5mr,color=continent)) +
  geom_line() +
  facet_wrap(~ model) +
  theme_bw()

#10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020.
E <- data.frame(country_name = "Ecuador",
                year = 2020,
                continent = "Americas")
predicted_value <- predict(mod3,newdata = E)
print(predicted_value)
# The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. How far off was your model prediction???
13 - predicted_value
# Create any model of your choosing that improves upon this “Ecuadorian measure of model correctness.”
improved_mod <- glm(data = dat, formula = u5mr ~ year * continent + country_name)
mod3$aic
improved_mod$aic # improved_mod has lower AIC value
compare_performance(mod3,improved_mod) %>% plot #improved_mod compares better on all model indices
new_predicted_value <- predict(improved_mod,newdata = E)
print(new_predicted_value)
13 - new_predicted_value # improved_mod predicts value of u5mr for Ecuador in 2020 more accurately
