install.packages("nycflights13")
library("nycflights13")
vuelos <- nycflights13::flights
vuelos
library(nycflights13)
library(lubridate)
install.packages("lubridate")
library(lubridate)
library(tidyverse)
# Vuelos tarde
retraso <- vuelos[which(vuelos$arr_delay > 60),]
dim(retraso)
a <- vuelos$arr_delay[vuelos$arr_delay > 60]
dim(a)
b <- filter(vuelos, arr_delay > 60)
dim(b)

# San Francisco (aeropuertos SFO y OAK)
sanfran <- vuelos[vuelos$dest == "SFO" | vuelos$dest == "OAK",]
sanfran
filter(vuelos, dest == "SFO" | dest == "OAK")

# United American (UA) o por American Airlines (AA)
filter(vuelos, carrier == "UA" | carrier == "AA")
vuelos_op <- vuelos[vuelos$carrier == "UA" | vuelos$carrier == "AA",]
vuelos_op

# Abril, Mayo y Junio salieron
vuelos[vuelos$month == c(4,5,6),]
vuelos[vuelos$month == 4 | vuelos$month == 5 | vuelos$month == 6,]
filter(vuelos, month == 4 | month == 5 | month == 6)

# llegaron más de una hora tarde. salieron con menos de una hora de retraso
re <- filter(vuelos, dep_delay < 60, arr_delay > 60)
dim(re)

# salieron con más de una hora de retraso. llegar con menos de 30 minutos 
sa <- filter(vuelos, dep_delay > 60, arr_delay < 30)
dim(sa)
vuelos_hora2 <- vuelos[which(vuelos$dep_delay > 60 & vuelos$arr_delay < 30),]
vuelos_hora2

# vuelos nocturno doce y 7
vuelo_nocturno <- vuelos[which(vuelos$hour >=0 & vuelos$hour <=7),]
vuelo_nocturno
noc <- filter(vuelos, hour >= 0, hour <= 7)
dim(noc)
dim(vuelo_nocturno)

#¿Cuántos vuelos tienen un valor desconocido de dep_time?
dep_desconocido <- vuelos[is.na(vuelos$dep_time),]
dim(dep_desconocido)
table(is.na(vuelos$dep_time))
print("Hay 8255 vuelos con valor desconocido de dep_time")

#¿Qué variables del dataset contienen valores desconocidos?
table(is.na(vuelos))
apply(is.na(vuelos), 2, summary)


# Ordena los vuelos de flights para encontrar los vuelos más retrasados en la salida. ¿Quévuelos fueron los que salieron los primeros antes de lo previsto?
order(vuelos)
??arrange
arrange(vuelos, desc(dep_delay))
?desc
arrange(vuelos, dep_delay)

# más rapidos
arrange(vuelos, desc(distance/air_time))

# trayectos más largos
arrange(vuelos, desc(distance))

# trayectos más cortos
arrange(vuelos, distance)

#cambiar horas a minutos
?mutate
mutate(vuelos, dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100))
dep_timemin <- (vuelos$dep_time %/% 100 * 60 + vuelos$dep_time %% 100)
dep_timemin
vuelos_min <-mutate(vuelos, sched_dep_time_min = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440, dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440)

#  Compara los valores de dep_time, sched_dep_time y dep_delay
vuelos_min$comparacion1 <- vuelos_min$sched_dep_time + vuelos_min$dep_delay
vuelos_min$comparacion2 <- vuelos_min$sched_dep_time_min + vuelos_min$dep_delay

 # algún patrón del número de vuelos que se cancelan cada día.
vueloscancelados <-  vuelos %>%
  mutate(cancelado = (is.na(arr_delay) | is.na(arr_delay))) %>%
  group_by(year, month, day) %>%
  summarise(num_cancelado = sum(cancelado), num_vuelo = n(),)

library(ggplot2)
ggplot(vueloscancelados) +
  geom_line(mapping = aes(x = num_vuelo, y = num_cancelado, col=num_vuelo, )) 

# cuelos cancelados y retraso
proporcion_retraso <- vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(proporcion_cancelados = mean(cancelados),media_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()
proporcion_retraso

ggplot(proporcion_retraso) +
  geom_line(aes(x = media_dep_delay, y = proporcion_cancelados, col=proporcion_cancelados))

#cancelados con retraso promedio por aeropuertos
LGA <- filter(vuelos, origin=="LGA")

Prop_retraso_cancel_LGA <-  LGA %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

ggplot(Prop_retraso_cancel_LGA) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col= prop_cancelados))

# compañia peores retrasos
vuelos %>%
  group_by(carrier) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(dep_delay))
vuelos %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))

#hora volar sin retrasos
vuelos %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)

#dia sin retrasos
make_dtime <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
vuelos_dt <- vuelos %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_dtime(year, month, day, dep_time),
    arr_time = make_dtime(year, month, day, arr_time),
    sched_dep_time = make_dtime(year, month, day, sched_dep_time),
    sched_arr_time = make_dtime(year, month, day, sched_arr_time)) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
vuelos_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)

vuelos_dt %>%
  mutate(wday = wday(dep_time, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarize(ave_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% 
  ggplot(aes(x = wday, y = ave_dep_delay)) + 
  geom_bar(stat = "identity", col = "red")

# cada destino minutos de retraso

retraso_totvuelos <- vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  summarise(arr_delay = sum(arr_delay))
retraso_totvuelos

# proporcion retrasos por destino

vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest, origin, carrier, flight) %>%
  summarise(arr_delay = sum(arr_delay)) %>%
  group_by(dest) %>%
  mutate(
    arr_delay_prop = arr_delay / sum(arr_delay)
  ) %>%
  arrange(dest, desc(arr_delay_prop)) %>%
  select(carrier, flight, origin, dest, arr_delay_prop) 

# retrasos segun hora del dia
vuelos_dt %>%
  mutate(sched_dep_hour = hour(sched_dep_time)) %>%
  group_by(sched_dep_hour) %>%
  summarise(dep_delay = mean(dep_delay)) %>%
  ggplot(aes(y = dep_delay, x = sched_dep_hour)) +
  geom_point() +
  geom_smooth()

sessionInfo()
