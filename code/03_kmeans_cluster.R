# 1) 영동·영서 구분 파일 만들기
# 필요한 패키지 로드
library(dplyr)
library(readr)

# CSV 파일 로드
file_path <- 'data/node_gangwon_합본.csv'
data <- read_csv(file_path, locale = locale(encoding = "UTF-8"))

# 영동 지방에 해당하는 시군 목록
영동_지방 <- c('고성군', '속초시', '양양군', '강릉시', '동해시', '삼척시', '태백시')

# '지역' 열을 생성하여 영동과 영서로 구분
data <- data %>%
  mutate(지역 = if_else(시군 %in% 영동_지방, '영동', '영서'))

# 결과를 새로운 CSV 파일로 저장, CP949 인코딩 사용
new_file_path <- 'data/node_gangwon_분류.csv'
write.csv(data, new_file_path, row.names = FALSE, fileEncoding = "CP949")

# 2) 영서 시설 k-means 군집
library(dplyr)
library(ggmap)
library(mapview)
library(plotly)
library(ggplot2)

# 필요한 라이브러리 불러오기
library(dplyr)
library(ggplot2)
library(sf)
library(mapview)

# CSV 파일로부터 데이터 불러오기
df <- read.csv("data/node_gangwon_분류.csv", fileEncoding = 'cp949')

# 데이터 확인
head(df)

# 특정 지역(영서) 데이터 필터링
df_filtered <- filter(df, 지역 == '영서')

# 클러스터링에 사용할 데이터 선택 및 정규화
clustering_data <- select(df_filtered, 경도, 위도)
clustering_data <- as.data.frame(lapply(clustering_data, scale))

# k-means 클러스터링 실행
set.seed(123)  # 재현 가능성을 위한 시드 설정
kmeans_result <- kmeans(clustering_data, centers = 4, iter.max = 1000)

# 클러스터 결과를 원본 데이터에 추가
df_filtered$cluster <- as.factor(kmeans_result$cluster)

# 데이터를 sf 객체로 변환
df_sf <- st_as_sf(df_filtered, coords = c("경도", "위도"), crs = 4326)

# mapview를 사용하여 클러스터링 결과를 지도 위에 시각화
map <- mapview(df_sf, zcol = "cluster")
map

# 클러스터별 고유 번호 추가
df_filtered$cluster_id <- as.numeric(as.factor(df_filtered$cluster))

# 데이터프레임을 CSV 파일로 저장
write.csv(df_filtered, "data/updated_gangwon_data.csv", row.names = FALSE, fileEncoding = "cp949")
