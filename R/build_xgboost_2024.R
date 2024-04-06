source("helpers/get_data_all.R")
library(tidymodels)
data_all<-get_data_all()

model_data<-data_all|>
  filter(winter_year!=2024)

# write_csv(model_data, "output/dataset_caches/model_data_2024.csv")

set.seed(1234)
split<-initial_split(model_data, strata=year)
train<-training(split)
test<-testing(split)

xgb_spec<-boost_tree(
  trees=1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
)%>%
set_engine("xgboost", objective="count:poisson")%>%
set_mode("regression")

xgb_grid<-grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size=30
)

xgb_wf<-workflow()%>%
  add_formula(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff)%>%
  add_model(xgb_spec)

set.seed(2345)

vb_folds<-vfold_cv(train, strata=year)

vb_folds

doParallel::registerDoParallel()

set.seed(3456)

xgb_res<-tune_grid(
  xgb_wf, 
  resamples = vb_folds, 
  grid = xgb_grid, 
  control = control_grid(save_pred = TRUE)
)


best_rmse <- select_best(xgb_res, "rmse")


final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb

final_res <- last_fit(final_xgb, split)

saveRDS(final_res, 'model/xgboost_2024_v1.rds')
