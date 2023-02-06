library(xgboost)
library(snotelr)
library(tidyverse)
library(lubridate)
library(RNRCS)
library(tidymodels)
library(MetBrewer)
library(extrafont)
loadfonts(quiet=TRUE)

theme_set(theme_minimal(
  base_family="Chivo Regular",
  base_size=8
))

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

# all_sites_sntl%>%
#   pivot_longer(cols=site_id_465:site_id_739, values_to="sn_w_eq", names_to="site_id")%>%
#   group_by(date)%>%
#   summarize(avg_sn_weq=mean(sn_w_eq, na.rm=T))%>%
#   ungroup()%>%
#   mutate(year=year(date))%>%
#   mutate(year=year(date),date=date%>%as.character%>%str_replace("^\\d{4}","2020")%>%as_date)%>%
#   ggplot(aes(date, avg_sn_weq, color=as.factor(year)))+
#   geom_line(linewidth=1.5, alpha=0.75)+
#   scale_color_manual(values=met.brewer("Pillement", 37))+
#   scale_x_date(labels = date_format("%b"))+
#   theme_minimal()

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

data_all<-vars_all%>%
  mutate(year=year(date))%>%
  mutate(total = site_id_465+site_id_586+site_id_589+site_id_739)%>%
  filter(total!=0)%>%
  select(-total)%>%
  mutate(
    yday = yday(date),
    dummy = yday-170,
    day_to_runoff = if_else(dummy<1, dummy+365, dummy),
    index_to_runoff = round(day_to_runoff/30)
  )%>%
  select(-yday, -dummy)

this_year<-data_all%>%
  filter(date>"2022-06-01")

model<-readRDS("model/xgboost_2023.rds")

predict_this_year<-this_year%>%
  filter(date>=as.Date("2022-12-01"))%>%
  bind_cols(predict(extract_workflow(model), .))

write_csv(predict_this_year, "output/current_predictions.csv")

prediction_plot<-predict_this_year%>%
  ggplot(aes(date, .pred))+
  geom_smooth(color="#27AE60", fill="#D5F5E3")+
  geom_point(size=1.5, color="#27AE60")+
  labs(
    title="Predicted Days of Raftable Runoff on the Dolores River Below McPhee Dam",
    subtitle="Winter 2022/2023 - Data from NRCS SNOTEL, Bureau of Reclamation, and the US Geological Survey.",
    caption="Model: by Mike Schmidt (schmidtynotes.com) | Mastodon: @mschmidty@fosstodon.org",
    y="Predicted Days",
    x="Date of Prediction"
  )+
  theme(
    plot.background=element_rect(fill="#FFFFFF", color="#FFFFFF"),
    plot.title=element_text(family="Chivo Black", hjust=0.5, vjust=3, size=13),
    plot.subtitle=element_text(size=6, color="#555555", hjust=0.5, vjust=5),
    plot.caption=element_text(size=6, color="#555555", vjust=-13),
    plot.margin=unit(c(30,30,30,30), 'pt'),
    axis.title=element_text(size=6, family="Chivo Bold"),
    axis.title.x=element_text(vjust=-2),
    axis.title.y=element_text(angle=90, vjust=-0.5),
    axis.text=element_text(color="#888888"),
    panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="#FFFFFF", color="#FFFFFF")
  )
  
ggsave("output/current_prediction.jpg", plot=prediction_plot, dpi=300, height=1500, width=2300, units="px")
