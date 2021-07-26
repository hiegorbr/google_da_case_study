## Primeiramente vamos importar as bibliotecas que serão necessárias para a nossa análise.
## A tidyverse é a biblioteca geral que engloba a ggplot2 e a readr que nos possibilita trazer os dados e plotar
## A lubridate nos ajuda a trabalhar mais facilmente com datas

library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

## Código realizado para importar os dados para o RStudio

getwd()
setwd("/Users/hieeg/Documents/Cursos/Google Data Analyst/Curso 8 - Estudos de caso/Estudo de caso 1 - Cyclistic/Dados originais")
mai_2020 <- read.csv("cyclistic_tripdata_052020.csv")
jun_2020 <- read.csv("cyclistic_tripdata_062020.csv")
jul_2020 <- read.csv("cyclistic_tripdata_072020.csv")
ago_2020 <- read.csv("cyclistic_tripdata_082020.csv")
set_2020 <- read.csv("cyclistic_tripdata_092020.csv")
out_2020 <- read.csv("cyclistic_tripdata_102020.csv")
nov_2020 <- read.csv("cyclistic_tripdata_112020.csv")
dez_2020 <- read.csv("cyclistic_tripdata_122020.csv")
jan_2021 <- read.csv("cyclistic_tripdata_012021.csv")
fev_2021 <- read.csv("cyclistic_tripdata_022021.csv")
mar_2021 <- read.csv("cyclistic_tripdata_032021.csv")
abr_2021 <- read.csv("cyclistic_tripdata_042021.csv")

## Verificar incongruências nos nomes das colunas

colnames(mai_2020)
colnames(jun_2020)
colnames(jul_2020)
colnames(ago_2020)
colnames(set_2020)
colnames(out_2020)
colnames(nov_2020)
colnames(dez_2020)
colnames(jan_2021)
colnames(fev_2021)
colnames(mar_2021)
colnames(abr_2021)

## Verificados os nomes, buscamos incongruências nos tipos

str(mai_2020)
str(jun_2020)
str(jul_2020)
str(ago_2020)
str(set_2020)
str(out_2020)
str(nov_2020)
str(dez_2020)
str(jan_2021)
str(fev_2021)
str(mar_2021)
str(abr_2021)

## Alterando o tipo das variáveis que apresentaram incompatibilidade

mai_2020 <- mutate(mai_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

jun_2020 <- mutate(jun_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

jul_2020 <- mutate(jul_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

ago_2020 <- mutate(ago_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

set_2020 <- mutate(set_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

out_2020 <- mutate(out_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

nov_2020 <- mutate(nov_2020, 
                   start_station_id = as.character(start_station_id), 
                   end_station_id = as.character(end_station_id),
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

dez_2020 <- mutate(dez_2020,
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

jan_2021 <- mutate(jan_2021,
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

fev_2021 <- mutate(fev_2021,
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

mar_2021 <- mutate(mar_2021,
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

abr_2021 <- mutate(abr_2021,
                   started_at = as_datetime(started_at),
                   ended_at = as_datetime(ended_at))

## Agora que foi verificada a compatibilidade das tabelas, vamos unir todas em uma única tabela

all_trips <- bind_rows(mai_2020, 
                       jun_2020, 
                       jul_2020, 
                       ago_2020, 
                       set_2020, 
                       out_2020, 
                       nov_2020, 
                       dez_2020, 
                       jan_2021, 
                       fev_2021, 
                       mar_2021, 
                       abr_2021)

## Com todas as tabelas juntas em uma só, vamos retirar as colunas que não interessam para a nossa análise

all_trips <- all_trips %>% 
  select(-c(start_station_id,
            end_station_id,
            start_lat, 
            start_lng, 
            end_lat, 
            end_lng))


## Adicionaremos novas colunas para nos ajudar na análise
## A primeira coluna adicionada será ride_length, que nos fornecerá o tempo de cada corrida

all_trips <- mutate(all_trips, 
                    ride_length = ended_at - started_at)



## Adicionaremos algumas divisões de tempo para que possamos analisar os dados em diferentes níveis

all_trips$date        <- as.Date(all_trips$started_at)
all_trips$day         <- format(as.Date(all_trips$date), "%d")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$month       <- format(as.Date(all_trips$date), "%m")
all_trips$year        <- format(as.Date(all_trips$date), "%y")


## Removeremos os dados em que a duração da corrida é negativa
## Como estamos removendo dados, criaremos uma nova tabela

all_trips_v2 <- all_trips[!(all_trips$ride_length<0),]


## Análises

## 1. Ver a quantidade de corridas dos tipos de usuário

all_trips_v2 %>%
  group_by(member_casual) %>%
  summarize(number_of_rides = n()) %>%
  arrange(member_casual)

## 2. Ver a média de tempo de corrida entre casual e membro

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# 3. Ver a média de tempo de corrida entre casual e membro e com o dia da semana

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, 
                                    levels=c("domingo", 
                                             "segunda-feira", 
                                             "terça-feira", 
                                             "quarta-feira", 
                                             "quinta-feira", 
                                             "sexta-feira", 
                                             "sábado"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)


## 4. Ver o comportamento dos usuários de acordo com o dia da semana

all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%            # agrupa os dados a partir dos membros e dias da semana
  summarize(number_of_rides = n(),                    # calcula a quantidade de corridas
            avg_duration = mean(ride_length)) %>%     # e a média de duração
  arrange(member_casual, day_of_week)                 # ordenas as linhas pelos valores das colunas


## 5. Ver o comportamento dos usuários de acordo com o mês

v1 <- all_trips_v2 %>% 
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(month, member_casual) 


## 6. Ver o comportamento dos usuários de acordo com o tipo de bicicleta

all_trips_v2 %>% 
  group_by(rideable_type, member_casual) %>%
  summarize(number_of_rides = n()) %>%
  arrange(rideable_type, member_casual)


## 7. Ver qual é a estação mais comum de início de corrida

all_trips_v2 %>% 
  group_by(start_station_name, member_casual) %>%
  summarize(number_of_rides = n()) %>%
  arrange(desc(number_of_rides), member_casual)


## 8. Ver qual é a estação mais comum de fim de corrida

all_trips_v2 %>% 
  group_by(end_station_name, member_casual) %>%
  summarize(number_of_rides = n()) %>%
  arrange(desc(number_of_rides), member_casual)


## Compartilhar
## Vamos fazer uma data viz para comparar melhor o comportamento dos 2 tipos de usuários

## 1. Utilização por semana

options(scipen = 100)   #comando para desativar a notação científica
all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Quantidade de corridas por dia da semana",
       subtitle = "Comparação entre usuários casuais e membros anuais",
       x = "Dia da semana",
       y = "Quantidade de corridas") +
  theme_minimal() +
  scale_fill_discrete(name="",
                      labels=c("Usuário casual", "Membro anual")) +
  theme(legend.position="right")

 

## 2. Duração de uso por semana

all_trips_v2 %>% 
  group_by(member_casual, day_of_week) %>%
  summarize(number_of_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Média de duração das corridas por dia da semana",
       subtitle = "Comparação entre usuários casuais e membros anuais",
       x = "Dia da semana",
       y = "Duração das corridas") +
  theme_minimal() +
  scale_fill_discrete(name="",
                      labels=c("Usuário casual", "Membro anual")) +
  theme(legend.position="right")


## 3. Utilização por mês

all_trips_v2 %>% 
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Quantidade de corridas por mês",
       subtitle = "Comparação entre usuários casuais e membros anuais",
       x = "Mês",
       y = "Quantidade de corridas") +
  theme_minimal() +
  scale_fill_discrete(name="",
                      labels=c("Usuário casual", "Membro anual")) +
  theme(legend.position="right")


## 4. Duração de uso por mês

all_trips_v2 %>% 
  group_by(member_casual, month) %>%
  summarize(number_of_rides = n(),
            avg_duration = mean(ride_length)) %>%
  arrange(member_casual, month) %>%
  ggplot(aes(x = month, y = avg_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Média de duração das corridas por mês",
       subtitle = "Comparação entre usuários casuais e membros anuais",
       x = "Mês",
       y = "Duração das corridas") +
  theme_minimal() +
  scale_fill_discrete(name="",
                      labels=c("Usuário casual", "Membro anual")) +
  theme(legend.position="right")


## 5. Tipo de bicicleta por usuário

all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>%
  summarize(number_of_rides = n(),) %>%
  arrange(member_casual, rideable_type) %>%
  ggplot(aes(x = rideable_type, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") + 
  labs(title = "Tipo de bicicleta utilizado por usuário", 
       subtitle = "Comparação entre usuários casuais e membros anuais",
       x = "Tipo da biclicleta",
       y = "Quantidade de corridas") +
  theme_minimal() +
  scale_fill_discrete(name="",
                      labels=c("Usuário casual", "Membro anual")) +
  theme(legend.position="right")