library(tidyverse)
library(vroom)
library(forecast)
library(patchwork)
library(embed)
library(tidymodels)

setwd("~/School/F2023/STAT348/STAT348/KobeBryantShotSelection/")

################################################################################

# shot <- vroom("data.csv")
# samp <- vroom("sample_submission.csv")
# 
# shot$shot_made_flag <- as.factor(shot$shot_made_flag)
# 
# shot$loc_r <- sqrt((shot$loc_x)^2 + (shot$loc_y)^2)
# shot$loc_theta <- atan(shot$loc_y/shot$loc_x)
# shot$loc_theta[is.na(shot$loc_theta)] <- pi/2
# 
# shot$time_remaining <- (shot$minutes_remaining * 60) + shot$seconds_remaining
# 
# shot$season <- sapply(shot$season, function(x){str_extract(x, "(?<=[:punct:])[:digit:]{2}")})
# 
# shot$home <- as.numeric(grepl("vs.", shot$matchup, fixed = TRUE))
# shot$away <- as.numeric(grepl("@", shot$matchup, fixed = TRUE))
# 
# shot$lastminutes <- ifelse(shot$time_remaining <= 180, 1, 0)
# 
# shot$game_num <- as.numeric(shot$game_date)
# 
# shot$first_team <- ifelse((shot$game_num >= 395 & shot$game_num <= 474) |
#                             (shot$game_num >= 494 & shot$game_num <= 575) |
#                             (shot$game_num >= 588 & shot$game_num <= 651) |
#                             (shot$game_num >= 740 & shot$game_num <= 819) |
#                             (shot$game_num >= 827 & shot$game_num <= 903) |
#                             (shot$game_num >= 909 & shot$game_num <= 990) |
#                             (shot$game_num >= 1012 & shot$game_num <= 1093) |
#                             (shot$game_num >= 1117 & shot$game_num <= 1189) |
#                             (shot$game_num >= 1213 & shot$game_num <= 1294) |
#                             (shot$game_num >= 1305 & shot$game_num <= 1362) |
#                             (shot$game_num >= 1375 & shot$game_num <= 1452),
#                           1, 0)
# 
# shot$scoring_leader <- ifelse((shot$game_num >= 740 & shot$game_num <= 819) |
#                                 (shot$game_num >= 827 & shot$game_num <= 903),
#                               1, 0)
# 
# shot$mvp <- ifelse(shot$game_num >= 909 & shot$game_num <= 990, 1, 0)
# 
# shot$finals_mvp <- ifelse((shot$game_num >= 1112 & shot$game_num <= 1116) |
#                             (shot$game_num >= 1206 & shot$game_num <= 1212),
#                           1, 0)
# 
# shot$num_rings <- 0
# shot[shot$game_num >= 311 & shot$game_num <= 394,]$num_rings <- 1
# shot[shot$game_num >= 395 & shot$game_num <= 493,]$num_rings <- 2
# shot[shot$game_num >= 494 & shot$game_num <= 1116,]$num_rings <- 3
# shot[shot$game_num >= 1117 & shot$game_num <= 1212,]$num_rings <- 4
# shot[shot$game_num >= 1213 & shot$game_num <= 1559,]$num_rings <- 5
# 
# shot$postachilles <- ifelse(shot$game_num > 1452, 1, 0)
# 
# test.id <- shot %>% filter(is.na(shot_made_flag)) %>% select(shot_id)
# 
# shot_filtered <- shot %>% select(-c(shot_id, team_id, team_name, matchup, lon, lat, seconds_remaining, minutes_remaining, loc_x, loc_y, game_event_id, game_id, game_date, game_num))
# 
# shotTest <- shot_filtered %>% filter(is.na(shot_made_flag)) %>% select(-c(shot_made_flag))
# shotTrain <- shot_filtered %>% filter(!is.na(shot_made_flag))
# 
# # my_recipe <- recipe(shot_made_flag ~ ., data=shotTrain) %>%
# #   step_mutate(action_type = as.factor(action_type)) %>%
# #   step_mutate(combined_shot_type = as.factor(combined_shot_type)) %>%
# #   step_mutate(period = as.factor(period)) %>%
# #   step_mutate(playoffs = as.factor(playoffs)) %>%
# #   step_mutate(season = as.factor(season)) %>%
# #   step_mutate(shot_type = as.factor(shot_type)) %>%
# #   step_mutate(shot_zone_area = as.factor(shot_zone_area)) %>%
# #   step_mutate(shot_zone_basic = as.factor(shot_zone_basic)) %>%
# #   step_mutate(shot_zone_range = as.factor(shot_zone_range)) %>%
# #   step_mutate(opponent = as.factor(opponent)) %>%
# #   step_mutate(home = as.factor(home)) %>%
# #   step_mutate(away = as.factor(away)) %>%
# #   step_mutate(lastminutes = as.factor(lastminutes)) %>%
# #   step_mutate(num_rings = as.factor(num_rings)) %>%
# #   step_mutate(postachilles = as.factor(postachilles)) %>%
# #   step_mutate(scoring_leader = as.factor(scoring_leader)) %>%
# #   step_mutate(first_team = as.factor(first_team)) %>%
# #   step_mutate(mvp = as.factor(mvp)) %>%
# #   step_mutate(finals_mvp = as.factor(finals_mvp))
# 
# my_recipe <- recipe(shot_made_flag ~ ., data = shotTrain) %>%
#   step_novel(all_nominal_predictors()) %>%
#   step_unknown(all_nominal_predictors()) %>%
#   step_dummy(all_nominal_predictors())
# 
# prep <- prep(my_recipe)
# baked <- bake(prep, new_data = shotTrain)
# baked2 <- bake(prep, new_data = shotTest)

################################################################################
# Loading in the data
kobe <- vroom('data.csv')
samp <- vroom("sample_submission.csv")

### Lat and Long ####

ggplot(data = kobe) +
geom_point(mapping = aes(x = lon, y = lat), color = 'blue')

ggplot(data = kobe) +
  geom_point(mapping = aes(x = loc_x, y = loc_y), color = 'green')

### Feature Engineering ###

## Converting it to polar coordinates

dist <- sqrt((kobe$loc_x/10)^2 + (kobe$loc_y/10)^2)
kobe$shot_distance <- dist

#Creating angle column
loc_x_zero <- kobe$loc_x == 0
kobe['angle'] <- rep(0,nrow(kobe))
kobe$angle[!loc_x_zero] <- atan(kobe$loc_y[!loc_x_zero] / kobe$loc_x[!loc_x_zero])
kobe$angle[loc_x_zero] <- pi / 2

# Create one time variable
kobe$time_remaining = (kobe$minutes_remaining*60)+kobe$seconds_remaining

# Home and Away
kobe$matchup = ifelse(str_detect(kobe$matchup, 'vs.'), 'Home', 'Away')

# Season
kobe['season'] <- substr(str_split_fixed(kobe$season, '-',2)[,2],2,2)

# Game number
kobe$game_num <- as.numeric(kobe$game_date)

# Achilles injury before and after
kobe$postachilles <- ifelse(kobe$game_num > 1452, 1, 0)

# MVP
kobe$mvp <- ifelse(kobe$game_num >= 909 & kobe$game_num <= 990, 1, 0)

### period into a factor
kobe$period <- as.factor(kobe$period)

# delete columns
kobe_f <- kobe %>%
  select(-c('shot_id', 'team_id', 'team_name', 'shot_zone_range', 'lon', 'lat',
            'seconds_remaining', 'minutes_remaining', 'game_event_id',
            'game_id', 'game_date','shot_zone_area',
            'shot_zone_basic', 'loc_x', 'loc_y'))

# Train
train <- kobe_f %>%
  filter(!is.na(shot_made_flag))

# Test
test <- kobe_f %>%
  filter(is.na(shot_made_flag))

# Make the response variable into a factor
train$shot_made_flag <- as.factor(train$shot_made_flag)

recipe <- recipe(shot_made_flag ~ ., data = train) %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors())

test.id <- kobe %>% filter(is.na(shot_made_flag)) %>% select(shot_id)

################################ Naive Bayes ###########################

library(discrim)
library(naivebayes)

## model and workflow

nb_model <- naive_Bayes(Laplace=tune(), smoothness=tune()) %>%
  set_mode("classification") %>%
  set_engine("naivebayes") # install discrim library for the naive bayes eng

nb_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(nb_model)

## Grid of values to tune over
tuning_grid <- grid_regular(Laplace(),
                            smoothness(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(train, v = 5, repeats=1)

## Run the CV
CV_results <- nb_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc)) #Or leave metrics NULL

# Find Best Tuning Parameters1
best_tune_nb <- CV_results %>%
  select_best("roc_auc")

## Finalize the Workflow & fit it
final_wf <- nb_wf %>%
  finalize_workflow(best_tune_nb) %>%
  fit(data=train)


## Predict

predict(final_wf, new_data=test, type="prob")

kobe_predictions_nb <- final_wf %>% predict(new_data=test,
                                            type="prob")

kobe_nb_submit <- as.data.frame(cbind(test.id, as.character(kobe_predictions_nb$.pred_1)))

colnames(kobe_nb_submit) <- c("shot_id", "shot_made_flag")

write_csv(kobe_nb_submit, "kobe_nb_submit.csv")



################################## Random Forest ##############################


library(doParallel)

num_cores <- parallel::detectCores()

cl <- makePSOCKcluster(num_cores)

registerDoParallel(cl)

## Create a workflow with model & recipe

my_mod_kf <- rand_forest(mtry = tune(),
                         min_n=tune(),
                         trees=500) %>%
  set_engine("ranger") %>%
  set_mode("classification")


kobe_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(my_mod_kf)

## Set up grid of tuning values

tuning_grid <- grid_regular(mtry(range = c(1,(ncol(train)-1))),
                            min_n(),
                            levels = 3)

## Set up K-fold CV

folds <- vfold_cv(train, v = 3, repeats=1)

## Find best tuning parameters

CV_results <- kobe_workflow %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc))

best_tune_rf <- CV_results %>%
  select_best("roc_auc")

## Finalize workflow and predict

final_wf <- kobe_workflow %>%
  finalize_workflow(best_tune_rf) %>%
  fit(data=train)

kobe_predictions_rf <- final_wf %>% predict(new_data=test,
                                            type="prob")

kobe_rf_submit <- as.data.frame(cbind(test.id, as.character(kobe_predictions_rf$.pred_1)))

colnames(kobe_rf_submit) <- c("shot_id", "shot_made_flag")

write_csv(kobe_rf_submit, "kobe_rf_submit.csv")

stopCluster(cl)



################################### KNN #######################################



library(doParallel)
library(kknn)

num_cores <- parallel::detectCores()

cl <- makePSOCKcluster(num_cores)

registerDoParallel(cl)

## model and workflow

knn_model <- nearest_neighbor(neighbors=tune()) %>% # set or tune4
  set_mode("classification") %>%
  set_engine("kknn")

knn_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(knn_model)

## Grid of values to tune over
tuning_grid <- grid_regular(neighbors(),
                            levels = 5) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(train, v = 5, repeats=1)

## Run the CV
CV_results <- knn_wf %>%
  tune_grid(resamples=folds,
            grid=tuning_grid,
            metrics=metric_set(roc_auc)) #Or leave metrics NULL

# Find Best Tuning Parameters1
best_tune_knn <- CV_results %>%
  select_best("roc_auc")

## Finalize the Workflow & fit it
final_wf <- knn_wf %>%
  finalize_workflow(best_tune_knn) %>%
  fit(data=train)

## Predict

kobe_predictions_knn <- final_wf %>% predict(new_data=test,
                                               type="prob")

kobe_knn_submit <- as.data.frame(cbind(test.id, as.character(kobe_predictions_knn$.pred_1)))

colnames(kobe_knn_submit) <- c("shot_id", "shot_made_flag")

write_csv(kobe_knn_submit, "kobe_knn_submit.csv")

stopCluster(cl)





























































































