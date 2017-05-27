library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)

### Clean up Global Environment
cat("\014")
rm(list = ls())
gc()

options(java.parameters = "-Xmx8g" )
setwd("E:/Data Science/Zillow")

## Load data
properties <- fread('./Data/properties_2016.csv')
transactions <- fread('./Data/train_2016.csv')

## Renaming columns
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

transactions <- transactions %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

properties <- properties %>% 
  mutate(tax_delinquency = ifelse(tax_delinquency=="Y",1,0),
         flag_fireplace = ifelse(flag_fireplace=="Y",1,0),
         flag_tub = ifelse(flag_tub=="Y",1,0))

## Outcome

transactions %>% 
  ggplot(aes(x=logerror)) + 
  geom_histogram(bins=400, fill="dodgerblue")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(-0.5,0.5))

qplot(x = seq_len(length(transactions$logerror)), transactions$logerror[order(transactions$logerror, decreasing = F)], 
      xlab = "index", ylab = "log-error", colour = "dodgerblue")

transactions <- transactions %>% mutate(abs_logerror = abs(logerror))
transactions %>% 
  ggplot(aes(x=abs_logerror)) + 
  geom_histogram(bins=400, fill="dodgerblue")+
  theme_bw()+theme(axis.title = element_text(size=16),axis.text = element_text(size=14))+
  ylab("Count")+coord_cartesian(x=c(0,0.5))

## Missing values
missing_values <- properties %>% summarize_each(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_percent")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_percent),y=missing_percent)) +
  geom_bar(stat="identity",fill="dodgerblue")+
  coord_flip()+theme_bw()

## Select features with less 70% NA values
good_features <- filter(missing_values, missing_percent < 0.70)


vars <- good_features$feature[str_detect(good_features$feature,'num_')]

## Areas where Zestimate predicts well or not
transactions <- transactions %>% mutate(percentile = cut(abs_logerror,quantile(abs_logerror, probs=c(0, 0.1, 0.25, 0.75, 0.9, 1),names = FALSE),
                                                         include.lowest = TRUE,labels=FALSE))

tmp1 <- transactions %>% 
  filter(percentile == 1) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp2 <- transactions %>% 
  filter(percentile == 5) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")
tmp3 <- transactions %>% 
  filter(percentile == 3) %>% 
  sample_n(5000) %>% 
  left_join(properties, by="id_parcel")

tmp1 <- tmp1 %>% mutate(type="best_fit")
tmp2 <- tmp2 %>% mutate(type="worst_fit")
tmp3 <- tmp3 %>% mutate(type="typical_fit")


tmp <- bind_rows(tmp1,tmp2,tmp3)
tmp <- tmp %>% mutate(type = factor(type,levels = c("worst_fit", "typical_fit", "best_fit")))

## Distribution
col_pal <- "Set2"

tmp %>% ggplot(aes(x=latitude, fill=type, color=type)) + 
  geom_density(alpha=0.1, size=1.2) + theme_bw() + 
  scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

tmp %>% ggplot(aes(x=num_bathroom, fill=type, color=type)) + 
  geom_density(alpha=0.1, size=1.2) + theme_bw() + 
  scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

tmp %>% ggplot(aes(x=num_bedroom, fill=type, color=type)) + 
  geom_density(alpha=0.1, size=1.2) + theme_bw() + 
  scale_fill_brewer(palette=col_pal)+scale_color_brewer(palette=col_pal)

## On a map!!
lat <- range(properties$latitude/1e06,na.rm=T)
lon <- range(properties$longitude/1e06,na.rm=T)

df <- tmp %>% 
  select(id_parcel,longitude,latitude, type) %>% 
  mutate(lon=longitude/1e6,lat=latitude/1e6) %>% 
  select(id_parcel,lat,lon, type) %>% 
  left_join(transactions,by="id_parcel")

pal <- colorFactor(
  palette = col_pal,
  domain = df$type)

leaflet(tmp) %>% 
  addTiles() %>% 
  fitBounds(lon[1],lat[1],lon[2],lat[2]) %>% 
  addCircles(lng = ~lon, lat = ~lat, popup = ~ type, color = ~ pal(type))
