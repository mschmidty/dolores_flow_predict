library(tidyverse)
library(tidymodels)
library(MetBrewer)

source("R/get_model_data.R")

theme_set(theme_minimal())

outflows<-flow_data|>
  mutate(
    flow_cf = flow*24*60*60,
    year = year(date)
  )|>
  filter(year>1985)|>
  group_by(site_id, year)|>
  mutate(
    cumulative_flow_cf = cumsum(flow_cf),
    total_outflows_cf = sum(flow_cf),
    remaining_outflows_cf = total_outflows_cf-cumulative_flow_cf
  )|>
  ungroup()|>
  mutate(j_day=yday(date))

outflows|>
  filter(site_id=="Dolores")|>
  ggplot(aes(j_day, cumulative_flow_cf, color=as.factor(year)))+
  geom_line(linewidth=1.3)+
  scale_color_manual(values=met.brewer("Renoir", 38))+
  labs(
    title="Cumulative Flows Into McPhee"
  )

outflows|>
  filter(site_id=="Bedrock")|>
  ggplot(aes(j_day, cumulative_flow_cf, color=as.factor(year)))+
  geom_line(linewidth=1.3)+
  scale_color_manual(values=met.brewer("Greek", 38))+
  labs(
    title="Cumulative Flows At Bedrock"
  )

left_to_run<-data_all|>
  left_join(
    outflows|>
      filter(site_id=="Dolores")|>
      select(-site_id, -year),
    by="date"
  )

known_years<-left_to_run|>
  filter(year!=2023)

current_year<-left_to_run|>
  filter(year==2023)

split<-initial_split(known_years, strata=year)
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
  add_formula(remaining_outflows_cf~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+day_to_runoff)%>%
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

known_years%>%
  bind_cols(predict(extract_workflow(final_res), .))%>%
  ggplot(aes(remaining_outflows_cf, .pred, color = index_to_runoff))+
  geom_point(size=4, alpha=1)+
  scale_color_gradientn(
    colors = met.brewer("Isfahan1"),
    guide=guide_colourbar(
      title.position="top",
      barwidth=10,
      barheight=0.5
    )
  )+
  labs(
    title="Left to Runoff",
    subtitle="Comparison of Actual Days of Raftable Releases to Model Prediction",
    x = "Runoff Left Actual",
    y = "Predicted Runoff Left",
    color="Time to Runoff Index"
  )+
  theme(
    legend.position="bottom"
  )

current_year%>%
  bind_cols(predict(extract_workflow(final_res), .))%>%
  ggplot(aes(date, .pred))+
  geom_point()




