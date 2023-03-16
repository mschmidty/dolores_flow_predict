library(tidymodels)
library(tidyverse)
library(snotelr)
library(lubridate)
library(RNRCS)
library(brms)

get_data_for_predict<-function(){
  

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

  data_all
}

data_all<-get_data_for_predict()

data_known<-data_all%>%
    filter(date<"2022-06-01")
  
poisson_recipe<-recipe(raftable_release_days~res_volume+site_id_465+site_id_586+site_id_589+site_id_739+index_to_runoff, data=data_known)|>
  step_center(all_predictors())|>
  step_scale(all_predictors())

prepped<-poisson_recipe|>
  prep()

poisson_formula <- bf(raftable_release_days~res_volume+site_id_465+site_id_586+site_id_589+site_id_739+index_to_runoff)

poisson_model <- brm(poisson_formula, data = bake(prep(poisson_recipe), data_known),
                     family = poisson(), prior = c(prior(normal(0, 10), class = Intercept),
                                                   prior(normal(0, 1), class = b)),
                     chains = 4, iter = 1000, warmup = 500, seed = 123)

poisson_model
prediction<-data_known|>
  bind_cols(predict(poisson_model, newdata=bake(prep(poisson_recipe), data_known)))

prediction|>
  filter(Estimate<200)|>
  ggplot(aes(raftable_release_days, Estimate, color=index_to_runoff))+
  geom_point(size=3, alpha=0.5)

prediction|>
  select(date, raftable_release_days, Estimate:Q97.5)|>
  View()
