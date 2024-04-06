library(tidymodels)
library(tidyverse)
library(MetBrewer)
library(ggtext)

options(
  max.print=100,
  vsc.use_httpgd=TRUE,
  device='quartz'
)

source("helpers/get_data_all.R")
all_data<-get_data_all()
model_data<-all_data

write_csv(all_data, "output/dataset_caches/all_data_2024.csv")

model_data<-read_csv("output/dataset_caches/model_data_2024.csv")
model<-readRDS("model/xgboost_2024_v1.rds")

model_data%>%
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
  )+
  theme(
    legend.position="top"
  )

data_all<-read_csv("output/dataset_caches/all_data_2024.csv")

current_year<-all_data|>
  filter(winter_year==2024)


current_year_pred<-current_year%>%
  bind_cols(predict(extract_workflow(model), .))|>
  ggplot(aes(date, .pred))+
  geom_point(size=1.5, color="#27AE60")+
  labs(
    title="Predicted Days of Raftable Runoff on the Dolores <br>River Below McPhee Dam",
    subtitle=paste0("Winter 2023/2024 | Model Run On: <span style='color:#27AE60; font-weight:bold;'>", format(Sys.Date(), format='%B %d, %Y'), "</span> | Data from NRCS SNOTEL, Bureau of Reclamation, and the US Geological Survey."),
    caption="Model: by Mike Schmidt (schmidtynotes.com) | Mastodon: @mschmidty@fosstodon.org",
    y="Predicted Days",
    x="Date of Prediction"
  )+
  theme(
    plot.background=element_rect(fill="#FFFFFF", color="#FFFFFF"),
    plot.title=element_markdown(family="Chivo Black", hjust=0.5, vjust=3, size=13, lineheight=1.2),
    plot.subtitle=element_markdown(size=6, color="#555555", hjust=0.5, vjust=5),
    plot.caption=element_text(size=6, color="#555555", vjust=-13),
    plot.margin=unit(c(30,30,30,30), 'pt'),
    axis.title=element_text(size=6, family="Chivo Bold"),
    axis.title.x=element_text(vjust=-2),
    axis.title.y=element_text(angle=90, vjust=-0.5),
    axis.text=element_text(color="#888888"),
    panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="#FFFFFF", color="#FFFFFF"),
    legend.position="top"
  )

ggsave("output/prediction_2024.jpg", plot=current_year_pred, height=1500,width=2100, units="px", dpi=300)
