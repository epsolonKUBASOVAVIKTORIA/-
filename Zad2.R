#постройте картосхему плотности деревьев (штук на км2) родов Дуб и Ива с диаметром более 15см
# Кубасова В.А.
# Установите необходимые пакеты (если они еще не установлены)
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("tidyr")

library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

# Очистим полностью память 
rm(list=ls())

# Проверка рабочей директории
getwd()

# Рабочая директория
setwd("C:/Modlnf/Zad2")

# Считаем данные в переменные
greendb = read.csv("greendb.csv")
map = sf::read_sf("moscow.geojson")

# График с заливкой
ggplot(map) + geom_sf(aes(fill = NAME)) + theme(legend.position = "none")

# Фильтруем данные для Липы и Ели с диаметрами ствола более 15 см
spec = greendb$species_ru
genus = stringr::str_split(spec, pattern = " ", simplify = TRUE)[, 1]
data = greendb %>% mutate(Genus = genus)

# Фильтруем данные и создаем новую переменную Genus
filtered_data <- data %>%
  mutate(Genus = stringr::str_split(species_ru, pattern = " ", simplify = TRUE)[, 1]) %>%  # Создаем Genus
  filter(d_trunk_m > 0.15, Genus %in% c("Дуб", "Ива")) %>%  # Фильтруем по диаметру и родам
  group_by(adm_region, Genus) %>%  # Группируем данные по регионам и родам
  summarise(count = n(), .groups = "drop")  # Считаем количество деревьев и сбрасываем 

map=map %>% mutate(adm_region=NAME)
map <- map %>%
  mutate(area_km2 = st_area(geometry) / 1e6)  # Преобразование площади в км²

filtered_data=pivot_wider(filtered_data, names_from = Genus, values_from = count)

map=left_join(map,filtered_data , by="adm_region")

ggplot(map)+
  geom_sf(aes(fill=`Дуб`))+theme()

ggplot(map)+
  geom_sf(aes(fill=`Ива`))+theme()
