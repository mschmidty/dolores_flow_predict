library(snotelr)
library(tidyverse)
library(lubridate)
library(RNRCS)
library(tidymodels)

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

vars_all 
data_all<-vars_all%>%
  mutate(
    year=year(date),
    winter_year=if_else(month(date)<7, year, year+1)
  )%>%
  left_join(predicted_variable, by=c("winter_year"="year"))%>%
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

set.seed(1234)
split<-initial_split(data_known, strata=year)
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

saveRDS(final_res, 'model/xgboost_2023_v2.rds')
