library(ggmap)
library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(stringr)
library(leaflet)

# CSV 파일 로드
df <- read.csv(file = 'data/node_gangwon_합본.csv', fileEncoding = 'UTF-8-BOM')

leaflet(df) %>%
  addTiles() %>%
  addCircleMarkers(
    ~경도, ~위도,
    color = NA,
    fillColor = ifelse(df$구분 == '바이오가스화시설', 'green',
                       ifelse(df$구분 == '하수처리시설', 'blue', 'red')),
    radius = ifelse(df$구분 == '바이오가스화시설', 10, 5),
    fillOpacity = 0.6
  )
