library(tidyverse) ## For all the data cleaning work.
library(lubridate)
library(fable)
library(RNRCS)
library(here)

## Get current snotel site data for four sites in Dolores River Drainage

dolores_site_ids<-c(465, 586, 589, 739)

get_snotl_data<-function(site_id){
  grabNRCS.data(network = "SNTL",
                site_id = site_id,
                timescale = "daily",
                DayBgn = 01-01-1986,
                DayEnd = Sys.Date()
  )%>%
    as_tibble()%>%
    mutate(site_id_num = site_id)
}

all_sntl_data<-lapply(dolores_site_ids, get_snotl_data)%>%
  bind_rows()


## Subset data and convert to tsibble (Time Series Tibble)
se_site<-all_sntl_data%>%
  select(Date, Snow.Depth..in..Start.of.Day.Values, Snow.Water.Equivalent..in..Start.of.Day.Values, site_id_num)%>%
  mutate(
    date = as.Date(Date)
  )%>%
  rename(snow_depth = 2, snow_water_eq=3)

avg_sn<-se_site%>%
  filter(year(Date)>1987)%>%
  group_by(Date)%>%
  summarize(avg_sn_eq =  mean(snow_water_eq, rm.na = T))%>%
  ungroup()%>%
  mutate(Date = as.Date(Date))

se_site_year<-se_site%>%
  group_by(year(date), site_id_num)%>%
  summarize(max_se = max(snow_water_eq, na.rm = T))%>%
  ungroup()%>%
  rename(year=1)

avg_snwater_eq<-se_site_year%>%
  filter(year>1986)%>%
  group_by(year)%>%
  summarize(avg_snow_water_e = mean(max_se))%>%
  ungroup()

avg_sn_ts<-avg_sn%>%
  as_tsibble(index = Date)

## Load model

fit<-avg_sn_ts%>%
  model(
    arima = ARIMA(avg_sn_eq~fourier("year", K = 12))
  )

## Make prediction and get the max data range.

pred_sn_pk<-fit%>%
  forecast(h = "6 months")%>%
  filter(avg_sn_eq == max(avg_sn_eq))%>%
  mutate(ci_90 = hilo(.distribution, 90))%>%
  mutate(pred_lower = ci_90$.lower, pred_upper = ci_90$.upper)%>%
  as_tibble()%>%
  rename(pred_avg_sn_eq = avg_sn_eq)%>%
  select(Date, pred_avg_sn_eq, pred_lower, pred_upper)%>%
  pivot_longer(-Date, names_to = "estimated_eq", values_to = "avg_snow_water_e")%>%
  mutate(year = year(Date))%>%
  select(-Date)


## Pull BOR data for the last year.

bor_data<-grabBOR.data(site_id = "MPHC2000",
                       timescale = 'daily',
                       DayBgn = Sys.Date()-months(6),
                       DayEnd = Sys.Date())%>%
  as_tibble()%>%
  mutate(date = as.Date(Date),
         res_volume = as.numeric(`Reservoir Storage Volume (ac_ft) Start of Day Values`))%>%
  select(date, res_volume)

res_vol<-bor_data%>%
  filter(month(date) %in% c(01, 02, 03))%>%
  group_by(year(date))%>%
  summarize(min_vol = min(res_volume, na.rm = T))%>%
  ungroup()%>%
  rename(year = 1)

var_df<-avg_snwater_eq%>%
  full_join(res_vol)%>%
  arrange(year)

pred_sn_pk_1<-pred_sn_pk%>%
  mutate(min_vol = var_df%>%
           filter(year == year(Sys.Date()))%>%
           pull(min_vol))

test_data<-var_df%>%
  filter(year==year(Sys.Date()))%>%
  mutate(estimated_eq = "current")%>%
  bind_rows(pred_sn_pk_1)

# model_lm<-readRDS(here::here("output/models/lm_model.rds"))
#
#
# model_lm%>%
#   augment(newdata = test_data, type.predict = "response")%>%
#   select(-year)%>%
#   write_csv(here::here("output/csv/prediction.csv"))

rf_model<-readRDS(here::here("model/rf_model.rds"))

test_data%>%
  mutate(prediction = stats::predict(rf_model, .),
         date = Sys.Date())%>%
  write_csv(here::here("predictions/predictions.csv"), append = TRUE)


