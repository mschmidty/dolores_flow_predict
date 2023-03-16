library(snotelr)
library(tidyverse)
library(lubridate)
library(RNRCS)
library(tidymodels)
library(MetBrewer)
library(extrafont)
loadfonts(quit=TRUE)

theme_set(theme_minimal(base_family="Inter Medium"))
theme_update(
  plot.title=element_text(family="Inter ExtraBold"),
  legend.position=c(0.8,0.1),
  legend.direction="horizontal",
  plot.margin = margin(1,1,1.5,1.2, "cm")
)

model<-readRDS("model/xgboost_2023_v2.rds")

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

data_known<-data_all%>%
  filter(date<"2022-06-01")

data_known%>%
  bind_cols(predict(extract_workflow(model), .))%>%
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
  )
