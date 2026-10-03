library(dplyr)
library(ggplot2)
library(geojsonsf)
library(sf)
library('ggrepel')
library('colorspace')

# 저장소 루트에서 실행

energy <- read.csv(file = 'data/한국환경공단_폐자원에너지 기타 정보_20231231.csv', fileEncoding = 'cp949')

energy_city <- energy %>%
  mutate(시도 = ifelse(시도 == "강원", "강원도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "경기", "경기도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "경남", "경상남도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "경북", "경상북도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "광주", "광주광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "대전", "대전광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "대구", "대구광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "부산", "부산광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "서울", "서울특별시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "세종", "세종특별자치시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "울산", "울산광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "인천", "인천광역시", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "전남", "전라남도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "전북", "전라북도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "제주", "제주특별자치도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "충남", "충청남도", 시도)) %>% 
  mutate(시도 = ifelse(시도 == "충북", "충청북도", 시도)) 

bio <- energy_city %>%
  filter(시설구분 == "바이오가스화시설")

bio <- bio %>%
  select(반입생산구분, 시도, 폐기물구분, 단위, 년)

bio_income_ani <- bio %>%
  filter(폐기물구분 == "가축분뇨")

bio_income <- bio %>%
  filter(폐기물구분 == "분뇨")

bio_gas <- bio %>%
  filter(폐기물구분 == "생산량(A+B)")

bio_food <- bio %>%
  filter(폐기물구분 == "음식물류폐기물")

bio_fw <- bio %>%
  filter(폐기물구분 == "음폐수")

bio_elec <- bio %>%
  filter(폐기물구분 == "전기생산량")


### 지도
KOR_SIDO <- geojson_sf('data/KOR_SIDO.json')

map <- KOR_SIDO
map$시도 <- paste(map$CTP_KOR_NM)

##가축 분뇨 반입량
map_income_ani <- map %>% merge(bio_income_ani, by = "시도", all.x = TRUE)

map_income_ani <- map_income_ani %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_income_ani %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "가축 분뇨 반입량",
    palette = "YlOrRd",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2)
  )

##분뇨 반입량
map_income <- map %>% merge(bio_income, by = "시도", all.x = TRUE)

map_income <- map_income %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_income %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "분뇨 반입량",
    palette = "Reds",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2)
  )

##바이오 가스 생산량
map_gas <- map %>% merge(bio_gas, by = "시도", all.x = TRUE)

map_gas <- map_gas %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_gas %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "바이오 가스 생산량",
    palette = "YlGnBu",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2),
    plot.title = element_text(hjust = 0.5)  # 제목 중앙 정렬
  ) +
  ggtitle("2023년 전국 바이오가스 생산량")


##음쓰 반입량
map_food <- map %>% merge(bio_food, by = "시도", all.x = TRUE)

map_food <- map_food %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_food %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "음식물류 폐기물 반입량",
    palette = "OrRd",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2)
  )

##음폐수 반입량
map_fw <- map %>% merge(bio_fw, by = "시도", all.x = TRUE)

map_fw <- map_fw %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_fw %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "음폐수 반입량",
    palette = "Oranges",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2)
  )

##전기 생산량
map_elec <- map %>% merge(bio_elec, by = "시도", all.x = TRUE)

map_elec <- map_elec %>%
  mutate(년 = ifelse(년 == 0, NA, 년))

map_elec %>%
  ggplot(aes(fill = 년)) +
  geom_sf() +
  coord_sf(datum = NA) + 
  scale_fill_continuous_sequential(
    name = "바이오 전기에너지 생산량",
    palette = "Blues",
    rev = T,
    labels = scales::comma,
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.title.align = 0.5,
    legend.text.align = 1.0,
    legend.position = c(0.85, 0.2)
  )

bio_gas %>%
  ggplot(aes(x = 시도, y = 년)) +
  geom_col(fill = "#56B4E9", width = 0.6, alpha = 0.9) +
  scale_y_continuous(expand = c(0, 0),
                     breaks = c(0, 2e7, 4e7, 6e7, 8e7),
                     labels = c("0", "20", "40", "60", "80"),
                     name = "바이오 가스 생산량") +
  xlab("") +
  theme_minimal() +
  theme(
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )


#############################################
geo <- read.csv(file = 'data/지역선정.csv')

library(ggplot2)
library(dplyr)
library(tidyr)

# 데이터프레임에서 필요한 조건을 만족하는 데이터 추출
subset_data <- geo %>%
  filter(시도 == "강원" & 시군구 == "횡성군") %>%
  select(폐기물.종류, X2019년발생량, X2020년발생량, X2021년발생량, X2022년발생량) %>%
  pivot_longer(cols = starts_with("X"), names_to = "년도", values_to = "발생량")

# 그래프 그리기
ggplot(subset_data, aes(x = 년도, y = 발생량, color = 폐기물.종류, group = 폐기물.종류)) +
  geom_line() +
  labs(title = "강원 횡성의 폐기물 종류별 발생량 변화") +
  scale_color_brewer(palette = "Set3") +
  theme_minimal()





total_data <- geo %>%
  select(시도, starts_with("X")) %>%
  group_by(시도) %>%
  summarise(across(starts_with("X"), sum, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("X"), names_to = "년도", values_to = "발생량")

# 그래프 그리기
ggplot(total_data, aes(x = 년도, y = 발생량, color = 시도, group = 시도)) +
  geom_line() +
  labs(title = "시도별 년도별 폐기물 발생량", color = "시도") +
  scale_color_brewer(palette = "Set3") +
  theme_minimal()
