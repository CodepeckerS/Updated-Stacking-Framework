library(tidyverse)
library(discrim)
library(tidymodels)
library(stacks)
library(lubridate)
library(gt)
library(skimr)
library(e1071)
library(lightgbm)
library(treesnip)

load("~/Documents/Uni/UM/Thesis/Data/DAX_prepared.RData") 

##### New Predictors from Jiang et al ####
df <- DAX %>% select(-c(closing_price,gain,loss,response,response_week, growth_week))

options(scipen = 999)  

my_skim <- skim_with(numeric = sfl(Mean = mean,
                                   Median = median,
                                   Standard_Deviation = sd,
                                   Minimum = min,
                                   Maximum = max,
                                   "Skewness" = skewness,
                                   "Kurtosis" = kurtosis),
                     
                     append = FALSE)
num <- my_skim(df) %>% yank("numeric") %>% as_tibble()
write_csv2( num, file = "Univariate_Statistics.csv")


dax_train <- df %>% filter(date < "2021-01-01") %>% dplyr::slice(-c(n():(n()-19))) %>% select(-date)

dax_test <- df %>% filter(date >= "2021-01-01") %>% select(-date)
df <- df %>% select(-date)

folds <- vfold_cv(dax_train, v=10, strata = growth)


# basic recipe used in all models
recip <- recipe(
  growth ~ .,
  data = dax_train
)

# Basic workflow
wflow <-
  workflow() %>%
  add_recipe(recip)

# convenience function
ctrl_grid <- control_stack_grid()

# metric for evaluation
metric <- metric_set(accuracy, roc_auc, f_meas)

set.seed(19)
#### Random Forest ####
rafo_mod <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rafo_wflow <-
  wflow %>%
  add_model(rafo_mod)

rafo_res <- 
  tune_grid(
    object = rafo_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

rafo_best <- rafo_res %>%
  select_best("roc_auc")

rafo_fin <- rafo_wflow %>%
  finalize_workflow(rafo_best)

rafo_fit <-
  rafo_fin %>%
  fit(dax_train)

rafo_pred <- rafo_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  rafo_fit %>% predict(dax_test)) %>% 
  set_names(c("rafo_yes","rafo_no","rafo_class"))

meta <- cbind(growth = dax_test$growth, rafo_pred)

rafo_eval <- meta %>% roc_auc(truth = growth, rafo_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, rafo_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, rafo_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Random Forest")
#### Extremely randomized Trees ####
ert_mod <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
  set_mode("classification") %>%
  set_engine("ranger", splitrule ="extratrees")

ert_wflow <-
  wflow %>%
  add_model(ert_mod)

ert_res <- 
  tune_grid(
    object = ert_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

ert_best <- ert_res %>%
  select_best("roc_auc")

ert_fin <- ert_wflow %>%
  finalize_workflow(ert_best)

ert_fit <-
  ert_fin %>%
  fit(dax_train)

ert_pred <- ert_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  ert_fit %>% predict(dax_test)) %>% 
  set_names(c("ert_yes","ert_no","ert_class"))

meta <- cbind(meta, ert_pred)

ert_eval <- meta %>% roc_auc(truth = growth, ert_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, ert_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, ert_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Extremely Randomized Trees")

all_eval <- bind_rows(rafo_eval,ert_eval)
#### XGBoost ####
xg_mod <- 
  boost_tree(
    mtry = tune(),
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

xg_wflow <-
  wflow %>%
  add_model(xg_mod)

xg_res <- 
  tune_grid(
    object = xg_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

xg_best <- xg_res %>%
  select_best("roc_auc")

xg_fin <- xg_wflow %>%
  finalize_workflow(xg_best)

xg_fit <-
  xg_fin %>%
  fit(dax_train)

xg_pred <- xg_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  xg_fit %>% predict(dax_test)) %>% 
  set_names(c("xg_yes","xg_no","xg_class"))

meta <- cbind(meta, xg_pred)

xg_eval <- meta %>% roc_auc(truth = growth, xg_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, xg_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, xg_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Extreme Gradient Boosting")

all_eval <- bind_rows(all_eval,xg_eval)
#### Light GBM ####
lgbm_mod <- 
  boost_tree(
    mtry = tune(),
    trees = tune(),
    learn_rate = tune(),
    tree_depth = tune()
  ) %>%
  set_mode("classification") %>%
  set_engine("lightgbm")

lgbm_wflow <-
  wflow %>%
  add_model(lgbm_mod)

lgbm_res <- 
  tune_grid(
    object = lgbm_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

lgbm_best <- lgbm_res %>%
  select_best("roc_auc")

lgbm_fin <- lgbm_wflow %>%
  finalize_workflow(lgbm_best)

lgbm_fit <-
  lgbm_fin %>%
  fit(dax_train)

lgbm_pred <- lgbm_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  lgbm_fit %>% predict(dax_test)) %>% 
  set_names(c("lgbm_yes","lgbm_no","lgbm_class"))

meta <- cbind(meta, lgbm_pred)

lgbm_eval <- meta %>% roc_auc(truth = growth, lgbm_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, lgbm_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, lgbm_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Light Gradient Boosting Machine")

all_eval <- bind_rows(all_eval,lgbm_eval)
#### Support Vector Machine ####
svm_mod <-
  svm_rbf(cost = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_recip <- 
  recip %>% 
  step_normalize(all_numeric())

svm_wflow <-
  workflow() %>%
  add_recipe(svm_recip) %>% 
  add_model(svm_mod)

svm_res <-
  tune_grid(
    object = svm_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

svm_best <- svm_res %>%
  select_best("roc_auc")

svm_fin <- svm_wflow %>%
  finalize_workflow(svm_best)

svm_fit <-
  svm_fin %>%
  fit(dax_train)

svm_pred <- svm_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  svm_fit %>% predict(dax_test)) %>% 
  set_names(c("svm_yes","svm_no","svm_class"))

meta <- cbind(meta, svm_pred)

svm_eval <- meta %>% roc_auc(truth = growth, svm_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, svm_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, svm_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Support Vector Machine")

all_eval <- bind_rows(all_eval,svm_eval)
#### k-Nearest Neighbor ####
set.seed(187)
knn_mod <-
  nearest_neighbor(
    mode = "classification", 
    neighbors = tune()
  ) %>%
  set_engine("kknn")

knn_recip <- 
  recip %>% 
  step_normalize(all_numeric())

knn_wflow <-
  workflow() %>%
  add_recipe(knn_recip) %>% 
  add_model(knn_mod)

knn_res <-
  tune_grid(
    object = knn_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

knn_best <- knn_res %>%
  select_best("roc_auc")

knn_fin <- knn_wflow %>%
  finalize_workflow(knn_best)

knn_fit <-
  knn_fin %>%
  fit(dax_train)

knn_pred <- knn_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  knn_fit %>% predict(dax_test)) %>% 
  set_names(c("knn_yes","knn_no","knn_class"))

meta <- cbind(meta, knn_pred)

knn_eval <- meta %>% roc_auc(truth = growth, knn_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, knn_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, knn_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "K Nearest Neighbor")

all_eval <- bind_rows(all_eval,knn_eval)
#### Naive Bayes ####
bayes_mod <-
  naive_Bayes(
    mode = "classification", 
    Laplace = tune()
  ) %>%
  set_engine("naivebayes")

bayes_wflow <-
  wflow %>%
  add_model(bayes_mod)

bayes_res <-
  tune_grid(
    object = bayes_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

bayes_best <- bayes_res %>%
  select_best("roc_auc")

bayes_fin <- bayes_wflow %>%
  finalize_workflow(bayes_best)

bayes_fit <-
  bayes_fin %>%
  fit(dax_train)

bayes_pred <- bayes_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  bayes_fit %>% predict(dax_test)) %>% 
  set_names(c("bayes_yes","bayes_no","bayes_class"))

meta <- cbind(meta, bayes_pred)

bayes_eval <- meta %>% roc_auc(truth = growth, bayes_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, bayes_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, bayes_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Naive Bayes")

all_eval <- bind_rows(all_eval,bayes_eval)
#### Bayesian Additive Regression Trees (BART) ####
bart_mod <- parsnip::bart(trees = tune()) %>% 
  set_engine("dbarts") %>% 
  set_mode("classification")

bart_wflow <- 
  wflow %>% 
  add_model(bart_mod)

bart_res <- 
  tune_grid(
    object = bart_wflow,
    resamples =folds,
    grid = 10,
    control = ctrl_grid
  )

bart_best <- bart_res %>%
  select_best("roc_auc")

bart_fin <- bart_wflow %>%
  finalize_workflow(bart_best)

bart_fit <-
  bart_fin %>%
  fit(dax_train)

bart_pred <- bart_fit %>% predict(dax_test, type= "prob") %>%  bind_cols(
  bart_fit %>% predict(dax_test, type ="class")) %>% 
  set_names(c("bart_yes","bart_no","bart_class")) %>% mutate(bart_class = as_factor(if_else(bart_yes > 0.5, "Yes","No")))

meta <- cbind(meta, bart_pred)

bart_eval <- meta %>% roc_auc(truth = growth, bart_yes) %>% select(.metric, .estimate) %>% 
  bind_rows(meta %>% f_meas(truth = growth, bart_class) %>% select(.metric, .estimate) ) %>% 
  bind_rows(meta %>% accuracy(truth = growth, bart_class) %>% select(.metric, .estimate)) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% mutate(model = "Bayesian Additive Regression Trees")

all_eval <- bind_rows(all_eval,bart_eval)
#### Compare models ####
all_eval %>%  gt(rowname_col = "model") %>% tab_header("Evaluation Metrics of Base Classifier") %>% tab_stubhead("Model") %>% 
  cols_label(model = "Model",
             roc_auc = "AUC",
             f_meas = "F-Score",
             accuracy = "Accuracy") %>% 
  tab_options(table.background.color = "white") %>% 
  opt_table_lines(extent = "none") %>%
  tab_style(
    style = list(
      cell_fill(color = "#001C3D"),
      cell_text(color = "white")
    ),
    locations = cells_stub()) %>%
  tab_style(
    style = list(
      cell_text(weight = "bolder", color = "#001C3D")
    ),
    locations = cells_title()) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#E84E10"),
      cell_text(color = "white", align = "center")
    ),
    locations = cells_column_labels(columns = everything())) %>% 
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_stubhead()) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_stub()
  ) %>% 
  fmt_number(
    columns = c(roc_auc, f_meas, accuracy),
    decimals = 5) %>% 
  opt_row_striping() %>%
  opt_table_font(font = "Roboto") %>% 
  gtsave("Advanced_Stacking_Framework_Base_Models.html", path = "/Users/henryspecht/Documents/Uni/UM/Thesis/Data")

base_plot <-  tibble()
for (i in sequence(((ncol(meta)-1)/3),2,3)){
  n <- colnames(meta[i])
  base_plot <-base_plot %>% bind_rows(meta %>% roc_curve(truth=growth,n) %>% bind_cols(model = str_remove(as.character(colnames(meta[i])),"(_yes).*?") ))
}
base_plot <- base_plot %>% mutate(
  model = if_else(model == "rafo","Random Forest",
                  if_else(model == "ert", "Extremely Randomized Trees",
                          if_else(model=="xg","Extreme Gradient Boosting",
                                  if_else(model == "lgbm","Light Gradient Boosting Machine",
                                          if_else(model == "svm", "Support Vector Machine",
                                                  if_else(model== "knn", "K Nearest Neighbor",
                                                          if_else(model == "bayes","Naive Bayes",
                                                                  if_else(model == "bart", "Bayesian Additive Regression Trees","NA")))))))),
  Model = model
)

theme_thesis <- theme_minimal()+theme(text=element_text(size =4,family="Helvetica"))

base_plot %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, colour = Model)) +
  geom_path(size = 0.2) +
  geom_abline(lty = 3) +
  scale_color_brewer(palette = "Set2") +
  coord_equal() +
  ylab("Sensitivity")+
  xlab("1 - Specificity")+
  theme_thesis
ggsave("Data/Advanced_Stacking_Framework_Base_Models.png", width = 1280, height = 1000, units = "px", dpi = 320, device = "png", limitsize = F)

set.seed(25)
#### Stack from Tree Models ####
mod_stack <-
  # initialize the stack
  stacks() %>%
  # add each of the models
  add_candidates(rafo_res) %>%
  add_candidates(ert_res) %>% 
  add_candidates(xg_res) %>% 
  add_candidates(lgbm_res) %>% 
  add_candidates(svm_res) %>% 
  add_candidates(knn_res) %>% 
  add_candidates(bayes_res) %>%
  add_candidates(bart_res)

mod_stack
autoplot(mod_stack)
mod_stack_bl <- mod_stack %>% blend_predictions() # evaluate candidate models

mod_stack_bl
autoplot(mod_stack_bl) #0.1 penalty

mod_stack_fit <-  mod_stack_bl %>%
  fit_members() # fit non zero stacking coefficients

autoplot(mod_stack_fit, type = "weights")

member_preds <- predict(mod_stack_fit, dax_test, members= TRUE) %>% 
  bind_cols(
    predict(mod_stack_fit, dax_test, type= "prob",member= TRUE)) %>%
  bind_cols(
    .,
    dax_test %>%
      select(growth)
  ) %>%
  select(growth, .pred_class, everything())


mem_auc <-  member_preds %>% accuracy(truth = growth, .pred_class) %>% bind_cols(Model = "Modelstack")
for (i in 1:((ncol(member_preds)-4)/3)+2){
  mem_auc <- mem_auc %>% bind_rows(member_preds %>% accuracy(truth=growth,as_vector(member_preds[i])) %>% bind_cols(Model = str_remove(as.character(colnames(member_preds[i])),".*?(class_)") ))
}

mem_roc <-  member_preds %>% roc_auc(truth = growth, .pred_Yes) %>% bind_cols(Model = "Modelstack")
for (i in 1:((ncol(member_preds)-4)/3)+(((ncol(member_preds)-4)/3)+4)){
  n <- colnames(member_preds[i])
  mem_roc <- mem_roc %>% bind_rows(member_preds %>% roc_auc(truth=growth,n) %>% bind_cols(Model = str_remove(as.character(colnames(member_preds[i])),".*?(No_)") ))
}

mem_f <-  member_preds %>% f_meas(truth = growth, .pred_class) %>% bind_cols(Model = "Modelstack")
for (i in 1:((ncol(member_preds)-4)/3)+2){
  mem_f <- mem_f %>% bind_rows(member_preds %>% f_meas(truth=growth,as_vector(member_preds[i])) %>% bind_cols(Model = str_remove(as.character(colnames(member_preds[i])),".*?(class_)") ))
}

mod_eval <- mem_auc %>% select(.metric, .estimate, Model) %>% 
  bind_rows(mem_roc %>% select(.metric, .estimate, Model) ) %>% 
  bind_rows(mem_f %>% select(.metric, .estimate, Model)) %>% 
  mutate(model = str_remove(Model, ".pred_Yes_*")) %>% select(-Model) %>% 
  pivot_wider(names_from = .metric, values_from = .estimate) %>% 
  mutate(model = ifelse(grepl("rafo_res_1_08", model),"Random Forest (8)", model),
         model = ifelse(grepl("rafo_res_1_01", model),"Random Forest (1)", model),
         model = ifelse(grepl("ert", model),"Extremely Randomized Trees (9)", model),
         model = ifelse(grepl("knn_res_1_2", model),"K Nearest Neighbor (2)", model),
         model = ifelse(grepl("knn_res_1_3", model),"K Nearest Neighbor (3)", model),
         model = ifelse(grepl("xg_res_1_02", model),"Extreme Gradient Boosting (2)", model),
         model = ifelse(grepl("xg_res_1_01", model),"Extreme Gradient Boosting (1)", model),
         model = ifelse(grepl("lgbm", model),"Light Gradient Boosting Machine (9)", model),
         ) %>% 
  select(roc_auc, f_meas,accuracy, model)

mod_eval %>%  gt(rowname_col = "model") %>% tab_header("Evaluation Metrics of Modelstack") %>% tab_stubhead("Model") %>% 
  cols_label(model = "Model",
             roc_auc = "AUC",
             f_meas = "F-Score",
             accuracy = "Accuracy") %>% 
  tab_options(table.background.color = "white") %>% 
  opt_table_lines(extent = "none") %>%
  tab_style(
    style = list(
      cell_fill(color = "#001C3D"),
      cell_text(color = "white")
    ),
    locations = cells_stub()) %>%
  tab_style(
    style = list(
      cell_text(weight = "bolder", color = "#001C3D")
    ),
    locations = cells_title()) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#E84E10"),
      cell_text(color = "white", align = "center")
    ),
    locations = cells_column_labels(columns = everything())) %>% 
  tab_style(
    style = list(
      cell_text(align = "center")
    ),
    locations = cells_stubhead()) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "white",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_stub()
  ) %>% 
  fmt_number(
    columns = c(roc_auc, f_meas, accuracy),
    decimals = 5) %>% 
  opt_row_striping() %>%
  opt_table_font(font = "Roboto") %>% 
  gtsave("Advanced_Stacking_Framework_Modelstack.html", path = "/Users/henryspecht/Documents/Uni/UM/Thesis/Data")

mem_plot <-  member_preds %>% roc_curve(truth = growth, .pred_Yes) %>% bind_cols(Model = "Modelstack")
for (i in 1:((ncol(member_preds)-4)/3)+(((ncol(member_preds)-4)/3)+4)){
  n <- colnames(member_preds[i])
  mem_plot <- mem_plot %>% bind_rows(member_preds %>% roc_curve(truth=growth,n) %>% bind_cols(Model = str_remove(as.character(colnames(member_preds[i])),".*?(No_)") ))
}

mem_plot <- mem_plot %>% mutate(Model = ifelse(grepl("rafo_res_1_08", Model),"Random Forest (8)", Model),
                                Model = ifelse(grepl("rafo_res_1_01", Model),"Random Forest (1)", Model),
                                Model = ifelse(grepl("ert", Model),"Extremely Randomized Trees (9)", Model),
                                Model = ifelse(grepl("knn_res_1_2", Model),"K Nearest Neighbor (2)", Model),
                                Model = ifelse(grepl("knn_res_1_3", Model),"K Nearest Neighbor (3)", Model),
                                Model = ifelse(grepl("xg_res_1_02", Model),"Extreme Gradient Boosting (2)", Model),
                                Model = ifelse(grepl("xg_res_1_01", Model),"Extreme Gradient Boosting (1)", Model),
                                Model = ifelse(grepl("lgbm", Model),"Light Gradient Boosting Machine (9)", Model),
                                order = ifelse(Model == "Modelstack",1,2)
)
mem_plot %>%
  mutate(Model = as.factor(Model),
         Model = fct_reorder(Model,order)) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, colour = Model)) +
  geom_path(size = 0.2) +
  geom_abline(lty = 3) +
  scale_color_brewer(palette = "Set2") +
  coord_equal() +
  ylab("Sensitivity")+
  xlab("1 - Specificity")+
  theme_thesis

ggsave("Data/Advanced_Stacking_Framework_Modelstack.png", width = 1280, height = 1000, units = "px", dpi = 320, device = "png", limitsize = F)


member_preds %>% conf_mat(truth = growth, .pred_class)


#### Comparing all Data Points

fin_preds <- predict(mod_stack_fit, df, members= FALSE) %>% 
  bind_cols(
    predict(mod_stack_fit, df, type= "prob",member= FALSE)) %>%
  bind_cols(
    .,
    df %>%
      select(growth)
  ) %>%
  select(growth, .pred_class, everything())

fin_preds <- cbind(DAX$date,fin_preds)

wrong <- fin_preds %>% filter(growth != .pred_class)

