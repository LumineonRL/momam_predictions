library(RSQLite)
library(stringr)
library(magrittr)
library(dplyr)
library(readr)
library(jsonlite)
library(httr)
library(tidyr)
library(tidymodels)
library(fastDummies)

db <- dbConnect(SQLite(), dbname="MOMAM.sqlite")
past_results <- read_csv("momam7/momam7_base_data.csv")

game_list <- dbGetQuery(db, 'SELECT game_id, game FROM game_data;') %>%
  as_tibble()

cumulative_playtime <- dbGetQuery(db, 'SELECT * FROM cumulative_playtime;') %>%
  as_tibble()

past_results %>%
  left_join(game_list)

# Counts how many games are in each group.
# A group is a set of games that either are being played or can be played on the same day.
# For example, if there's a bid war between Ocarina of Time and Majora's Mask,
# they would share the same grouping for that entry.
counts <- past_results %>%
  group_by(grouping) %>%
  count(grouping)

# Splits games with multiple categories and assigns weights to each entry depending
# on their grouping. All of the joins should go here.
games_cat_and_weights <- past_results %>%
  left_join(counts, by = join_by(grouping)) %>%
  mutate(weight = 1/n) %>%
  select(-n) %>%
  left_join(game_list, by = join_by(game)) %>%
  left_join(cumulative_playtime, by = join_by(game_id), multiple = "all") %>%
  dummy_cols(select_columns = c("category"),
             remove_selected_columns = T,
             split = ",") %>%
  pivot_wider(names_from = streamer, values_from = minutes, values_fill = 0,
              names_glue = "cum_playtime_{streamer}")


# Sets the data up to be weighted. Will lose game id with this, so need to have all the data
# ready to go by this point.
pivoted_data <- games_cat_and_weights %>%
  pivot_longer(cols = c(starts_with("category"), starts_with("cum")), # Brings all of the columns together
               names_to = "placeholder") %>%                          # that need to be weighted.
  mutate(weighted_val = value * weight) %>% # Applies the weight
  group_by(MOMAM, grouping, winner, placeholder) %>%
  summarise(weighted_group = sum(weighted_val)) %>%
  pivot_wider(names_from = placeholder, values_from = weighted_group, values_fill = 0) # Puts the data back in their dummy columns


#################################################################################

# This should be a part of the recipe
log_data <- pivoted_data %>%
  mutate(cum_playtime_pie = log(cum_playtime_pie),
         cum_playtime_spike = log(cum_playtime_spike),
         cum_playtime_pie = case_when(cum_playtime_pie == -Inf~0,
                            T~cum_playtime_pie),
         cum_playtime_spike = case_when(cum_playtime_spike == -Inf~0,
                            T~cum_playtime_spike)) %>%
  ungroup() %>%
  dplyr::select(!c(grouping, cum_playtime_NA))


##########################################################################

log_data$winner <- factor(log_data$winner)

nvp

log_data %>%
  step_nzv() %>%
  step_normalize()

log_model <- logistic_reg()

log_fit <- log_model %>%
  fit(winner ~ ., data = log_data)

tidy(log_fit)

tidy(log_fit) %>% 
  dwplot(dot_args = list(size = 2, color = "black"),
         whisker_args = list(color = "black"),
         vline = geom_vline(xintercept = 0, colour = "grey50", linetype = 2))


to_predict <- log_data %>%
  filter(is.na(winner)) %>%
  select(-winner)

View(to_predict)

pivoted_data <- pivoted_data %>%
  filter(!grouping)


model_rec <- recipe(winner ~ ., data = pivoted_data)


filtered_rec <- model_rec %>% 
  step_log(c(cum_playtime_pie, cum_playtime_spike)) %>%
  step_normalize(all_predictors())
  #step_nzv(all_predictors()) %>%
  #step_zv(all_predictors())

filtered_rec <- prep(filtered_rec)
bake(filtered_rec)




filtered_rec
summary(filtered_rec)
tidy(filtered_rec)

stats::predict(log_fit, new_data = to_predict) %>%
  View()
stats::predict(log_fit, new_data = to_predict, type = "prob") %>%
  View()
stats::predict(log_fit, new_data = to_predict, type = "conf_int", level = 0.95) %>%
  View()

?predict

?pivot_longer
games_cat_and_weights %>%
  mutate(weighted_min = minutes*weight) %>%
  group_by(streamer, grouping) %>%
  summarise(game_id, category, weighted_min = sum(weighted_min), ) %>%
  View()


?summarise

# Next need to do some pivoting by category and streamer
# Fix names

games_cat_and_weights %>%
  pivot_wider(names_from = streamer, values_from = minutes, values_fill = 0,
              names_glue = "cum_playtime_{streamer}") %>%
  View()

games_cat_and_weights %>%
  pivot_wider(id_cols = c(grouping, game, streamer), id_expand = T) %>%
  View()

games_cat_and_weights

games_cat_and_weights %>%
  dummy_cols(select_columns = c("category"),
             remove_selected_columns = T) %>%
  View()

games_cat_and_weights %>%
  dummy_cols(select_columns = c("category"),
             remove_selected_columns = T,
             split = ",") %>%
  View()


past_results 


?dummy_extract_names
?dummy_cols

past_results %>%
  mutate(category = str_split(category, ", ")) %>%
  unnest(cols = c(category)) %>%
  left_join(counts, by = join_by(grouping)) %>%
  mutate(weight = 1/n) %>%
  select(-n) %>%
  left_join(game_list) %>%
  left_join(cumulative_playtime, by = join_by(game_id))

past_results
game_list

model_test <- past_results %>%
  left_join(game_list) %>%
  dplyr::select(MOMAM, category, winner) %>%
  dplyr::filter(!is.na(winner))
  
model_rec <- recipe(winner ~ ., data = model_test)
summary(model_rec)

test_dummy <- model_rec %>%
  step_dummy_multi_choice(category) %>%
  prep(training = model_test)
summary(test_dummy)


############################################
predicted_names <- c("Kirby", "MK8", "Ninja Gaiden", "YGOFM",
                     "Ironmon", "Sonic", "All Stars", "Fall Guys",
                     "OoTR", "Hollow Knight", "Mario Party", "BotW",
                     "Monopoly", "Winnie the Pooh", "Spongebob", "Sunshine",
                     "Mario Golf Super Rush", "Mario Golf", "Grab Bag", "SM64", "SMRPG")

pivoted_data$winner <- factor(pivoted_data$winner)

data_complete <- pivoted_data %>%
  dplyr::filter(!is.na(winner) & MOMAM != 7)

data_to_predict <- pivoted_data %>%
  dplyr::filter(!is.na(winner) & MOMAM == 7)


data_recipe <- recipe(winner ~ ., data = data_complete) %>%
  step_rm(grouping) %>%
  step_log(c(cum_playtime_pie, cum_playtime_spike), signed = T) %>%
  step_zv(all_predictors()) %>%
  step_nzv(all_predictors(), freq_cut = 98/2) %>%
  step_lincomb(all_predictors()) %>%
  step_corr(all_predictors()) %>%
  step_normalize(all_predictors())

data_train <- data_recipe %>%
  prep(training = data_complete, retain = TRUE, verbose = TRUE) %>%
  bake(new_data = NULL)

# This is returning all of the data rather than just the training data.
data_test <- data_recipe %>%
  prep(training = data_complete, retain = TRUE, verbose = TRUE) %>%
  bake(new_data = data_to_predict)


logistic_reg_glm_spec <- logistic_reg() %>%
  set_engine('glm')

logistic_reg_glm_spec
log_fit <- parsnip::fit(logistic_reg_glm_spec, data_train)

# This isn't currently working. fit isn't using the workflow from the recipe.
log_wkflow <- workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(logistic_reg_glm_spec)

log_wkflow
tidy(log_fit)

log_fit <- fit(log_wkflow, data_complete)
predict.model_fit(log_fit, data_to, type = "prob")

predict(log_fit, data_to_predict, type = "prob")

rownames(log_fit)
colnames(data_test)

cbind(predicted_names, predict.model_fit(log_fit, data_to_predict, type = "prob"))

predictions <- cbind(predict(log_fit, data_to_predict),
                     predict(log_fit, data_to_predict, type = "prob"),
                     data_to_predict$winner)

accuracy(predictions, truth = `data_to_predict$winner`, estimate = `.pred_class`)$.estimate

#################################################################################
conf_mat(predictions, truth = `data_to_predict$winner`, estimate = `.pred_class`)
classification_metrics <- metric_set(accuracy, mcc, f_meas)
classification_metrics(predictions, truth = `data_to_predict$winner`, estimate = `.pred_class`)

two_class_curve <- roc_curve(predictions, `data_to_predict$winner`, .pred_pie)
roc_auc(predictions, `data_to_predict$winner`, .pred_pie)
autoplot(two_class_curve)


metrics(predictions, `data_to_predict$winner`, `.pred_class`, `.pred_pie`)
#################################################################################
data_to_predict$winner
  
predictions

?conf_mat
##############################################

# SAVE THIS
parsnip_addin()

initial_split(data_to_predict)
