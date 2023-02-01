options(
  max.print=100,
  vsc.use_httpgd=TRUE,
  device='quartz'
)

library(snotelr)
library(tidyverse)
library(lubridate)
library(RNRCS)
library(pscl)
library(tidymodels)
library(poissonreg)
library(MetBrewer)
library(extrafont)
loadfonts()

theme_set(theme_minimal(base_family="Inter Medium"))
theme_update(
  plot.title=element_text(family="Inter ExtraBold"),
  legend.position=c(0.8,0.1),
  legend.direction="horizontal",
  plot.margin = margin(1,1,1.5,1.2, "cm")
)

dolores_site_ids<-c(465, 586, 589, 739)

get_snotel_data<-function(site_id){
  snotel_download(site_id = site_id, internal=TRUE)%>%
    as_tibble()
}


all_sntl_data<-lapply(dolores_site_ids, get_snotel_data)%>%
  bind_rows()

sntl_cl<-all_sntl_data%>%
  select(site_id, date, snow_water_equivalent)%>%
  mutate(date=as.Date(date))


## Avg analysis
avg_snow_water_eq<-sntl_cl%>%
  mutate(year=year(date))%>%
  filter(year>1986)%>%
  group_by(date)%>%
  summarize(avg_sn_wtr_eq_all_sites = mean(snow_water_equivalent, na.rm=T))%>%
  ungroup()

all_sites_sntl<-sntl_cl%>%
  filter(year(date)>1986)%>%
  mutate(site_id = paste0("site_id_", site_id))%>%
  pivot_wider(names_from=site_id, values_from=snow_water_equivalent)

bor_data<-grabBOR.data(site_id = "MPHC2000",
                       timescale = 'daily',
                       DayBgn = "1987-01-01",
                       DayEnd = Sys.Date())%>%
  as_tibble()%>%
  mutate(date = as.Date(Date),
         res_volume = as.numeric(`Reservoir Storage Volume (ac_ft) Start of Day Values`))%>%
  select(date, res_volume)

bor_data

# bor_data%>%
#   mutate(year=year(date),date=date%>%as.character%>%str_replace("^\\d{4}","2020")%>%as_date)%>%
#   ggplot(aes(date, res_volume, color=as.factor(year)))+
#   geom_line(linewidth=1.5, alpha=0.75)+
#   scale_color_manual(values=met.brewer("Pillement", 37))+
#   scale_x_date(labels = date_format("%b"))+
#   ggrepel::geom_label_repel(aes(label = as.factor(year)),
#                    nudge_x = 1,
#                    na.rm = TRUE)+
#   theme_minimal()+
#   labs(
#     title="McPhee Dam, Reservoir Volume",
#     subtitle="Source Bureau of Reclemation",
#     y= "Reservoir Storage Volume (ac_ft) Start of Day Values",
#     x = ""
#   )

vars_all<-all_sites_sntl%>%
  left_join(bor_data, by="date")

## Get flow data
url<-paste0("https://waterservices.usgs.gov/nwis/dv/?format=rdb&sites=09169500,%2009166500&startDT=1985-02-01&endDT=", Sys.Date(), "&statCd=00003&siteType=ST&siteStatus=all")


flow_data<-read_tsv(url, skip = 35)%>%
  select(2:5)%>%
  rename(site_id = 1, date = 2, flow=3, code = 4)%>%
  mutate(
    site_id = ifelse(site_id == "09166500", "Dolores", "Bedrock"), 
    flow=as.numeric(flow)
  )%>%
  drop_na()

predicted_variable<-flow_data%>%
  filter(site_id=="Bedrock")%>%
  mutate(
    raftable = ifelse(flow>800, 1, 0),
    year = year(date)
  )%>%
  filter(month(date) %in% c(3:7))%>%
  group_by(year)%>%
  summarize(raftable_release_days = sum(raftable))%>%
  ungroup()

data_all<-vars_all%>%
  mutate(year=year(date))%>%
  left_join(predicted_variable, by="year")%>%
  mutate(total = site_id_465+site_id_586+site_id_589+site_id_739)%>%
  filter(total!=0)%>%
  select(-total)%>%
  mutate(
    yday = yday(date),
    dummy = yday-182,
    day_to_runoff = if_else(dummy<1, dummy+365, dummy),
    index_to_runoff = round(day_to_runoff/30),
    raftable_release_days = ifelse(is.na(raftable_release_days), 0, raftable_release_days)
  )%>%
  select(-yday, -dummy)

data_known<-data_all%>%
  filter(date<"2022-06-01")

this_year<-data_all%>%
  filter(date>"2022-06-01")

lm<-glm(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff, data=data_known, family=poisson)
summary(lm)

data_all%>%
  bind_cols(predicted=predict(lm, data_all))%>%
  ggplot(aes(raftable_release_days, predicted, color=index_to_runoff))+
  geom_point(alpha=0.5)+
  scale_color_gradientn(
    colors = met.brewer("Isfahan1"),
    guide=guide_colourbar(
      title.position="top",
      barwidth=10,
      barheight=0.5
    )
  )+
  labs(
    title="Linear Model Dolores River Predict",
    subtitle="Comparison of Actual Days of Raftable Releases to Model Prediction",
    x = "Raftable Release Days Actual",
    y = "Predicted Raftable Release Days",
    color="Time to Runoff Index"
  )+ggsave("output/linear_model_predict_vs_actual.jpg")


zinf<-zeroinfl(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff, data=data_all, family=poisson)

summary(zinf)

data_all%>%
  bind_cols(predicted=predict(zinf, data_all))%>%
  ggplot(aes(raftable_release_days, predicted, color=index_to_runoff))+
  geom_point()+
  scale_color_gradientn(colors = met.brewer("Isfahan1"))

zinf2<-zeroinfl(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff | site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff, data=data_all, family=poisson)

summary(zinf2)

data_all%>%
  bind_cols(predicted=predict(zinf2, data_all))%>%
  ggplot(aes(raftable_release_days, predicted, color=index_to_runoff))+
  geom_point(alpha=0.5)+
  scale_color_gradientn(colors = met.brewer("Isfahan1"))

data_all%>%
  bind_cols(predicted=predict(zinf, data_all, type="response"))


## Tidymodels Version


set.seed(1234)
split<-initial_split(data_known, strata=year)
train<-training(split)
test<-testing(split)

recipes_obj<-recipe(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff, data=train)%>%
  step_normalize(all_numeric_predictors())

pois_spec<-poisson_reg()%>%
  set_engine("zeroinfl")%>%
  set_mode("regression")

wf <- workflow() %>%
  add_recipe(recipes_obj)

pois_fit <- wf %>%
  add_model(pois_spec) %>%
  fit(data = train)


fit<-pois_fit %>%
  extract_fit_parsnip() 

last_fit<-pois_fit%>%
  last_fit(split)

data_all%>%
  bind_cols(predict(extract_workflow(last_fit), .))%>%
  ggplot(aes(raftable_release_days, .pred, color = index_to_runoff))+
  geom_point(size=4, alpha=0.3)+
  scale_color_gradientn(colors = met.brewer("Isfahan1"))


## XGboost
xgb_spec<-boost_tree(
  trees=1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
)%>%
set_engine("xgboost")%>%
set_mode("regression")

xgb_spec

xgb_grid<-grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size=30
)

xgb_grid

xgb_wf<-workflow()%>%
  add_formula(raftable_release_days~site_id_465+site_id_586+site_id_589+site_id_739+res_volume+index_to_runoff)%>%
  add_model(xgb_spec)

xgb_wf

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

xgb_res

collect_metrics(xgb_res)%>%
  filter(.metric=="rmse") %>%
  select(mean, mtry:sample_size) %>%
  pivot_longer(mtry:sample_size,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  scale_color_manual(values = met.brewer("Isfahan1", 6))+
  labs(title="Parameter tuning results", x = NULL, y = "RMSE")+
  ggsave("output/parameter_tuning_results_xgb.jpg")


best_rmse <- select_best(xgb_res, "rmse")


final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb


library(vip)

final_xgb %>%
  fit(data = train) %>%
  extract_fit_parsnip() %>%
  vip(geom = "point")+
  ggsave("output/feature_importance_xgb.jpg")

final_res <- last_fit(final_xgb, split)

saveRDS(final_res, 'model/xgboost_2023.rds')

collect_metrics(final_res)

data_all%>%
  bind_cols(predict(extract_workflow(final_res), .))%>%
  ggplot(aes(raftable_release_days, .pred, color = index_to_runoff))+
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
    title="XGboost Model Dolores River Predict",
    subtitle="Comparison of Actual Days of Raftable Releases to Model Prediction",
    x = "Raftable Release Days Actual",
    y = "Predicted Raftable Release Days",
    color="Time to Runoff Index"
  )+ggsave("output/xgboost_prediction_actual_to_predicted.jpg")

## Apply this year
this_year%>%
  filter(date>as.Date("2022-11-01"))%>%
  bind_cols(predict(extract_workflow(final_res), .))%>%
  ggplot(aes(date, .pred))+
  geom_smooth()+
  geom_point()+
  labs(
    title="Winter 2022/2023 Dolores River Release Prediction",
    subtitle=paste0("Over Time as of ", Sys.Date()),
    x="",
    y="Predicted Number of Days"
  )+ggsave(paste0("output/predictions_for_this_year_", Sys.Date(), ".jpg"))


