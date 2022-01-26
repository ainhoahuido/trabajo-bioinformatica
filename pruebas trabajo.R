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
