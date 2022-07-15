# The focus of this exercise was to practice feature engineering. This dataset had 12 predictors.
# Collinearity was explored and 4 candidate features were retained.
# Skewness was also dealt with by reflection, log transformation, scaling and centering.
# Finally, I ran a quick logistic regression.
# I look forward to exploring Box-Cox transformations in future datasets.

# libraries

library(tidyverse)
library(janitor)
library(GGally)
library(tidymodels)
library(caret)
library(ggforce)
library(MASS)
library(car)

# data

pumpkin <- pumpkin_seeds %>% 
  rename(Area = ï..Area) %>% 
  mutate(class = "cercevelik",
         Area = as.double(Area)) %>% 
  select(-Class) %>% 
  clean_names() %>% 
  as_tibble()

pumpkin$class[1301:2500] = "urgup sivrisi"

pumpkin <- pumpkin %>% 
  mutate(class = as.factor(class))

pumpkin


# EDA

pumpkin %>% 
  ggplot(aes(fill = class)) + 
  geom_histogram(aes(convex_area)) + 
  facet_wrap(~class)

pumpkin %>% ggplot(aes(fill = class, col = class)) + 
  geom_point(aes(area, eccentricity))

# correlation

ggpairs(pumpkin)
# eccentricity, solidity, extent are skewed

ggcorr(pumpkin, label = TRUE)
corrplot::corrplot(cor(pumpkin[1:12]),
                   method = "shade", 
                   addCoef.col = TRUE,
                   order = "hclust")

# area, equiv_diameter, convex_area, perimeter
# roundness, compactness
# eccentricity, aspect ration
# perimeter, major axis length


findCorrelation(cor(pumpkin[1:12]), 0.65)
modelpump <- pumpkin[,c(6,8,9,10,13)]
modelpump

ggpairs(modelpump)
# solidity, extent are skewed

# reflecting & transforming 
modelpump <- modelpump %>% 
  mutate(solidity = log(1-solidity),
         extent = log(1-extent)) 

modelpump %>% 
  ggpairs()


# Modeling

pump_split <- initial_split(modelpump, 
                            prop = 0.8,
                            strata = class)
testing(pump_split) %>% view


log_reg <- logistic_reg() %>% 
  set_mode("classification") %>% 
  set_engine("glm")

log_rec <- recipe(class ~., data = training(pump_split)) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

lastfit<-workflow() %>% 
  add_model(lin_reg) %>% 
  add_recipe(lin_rec) %>% 
  last_fit(pump_split) 

lastfit %>% collect_metrics


# ROC Curve
lastfit %>% 
  collect_predictions %>% 
  roc_curve(class, .pred_cercevelik) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_path() +
  geom_abline(lty = 3) +
  coord_equal() +
  theme_bw()




