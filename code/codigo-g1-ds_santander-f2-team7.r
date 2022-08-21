install.packages("ggplot2")
install.packages("dplyr")

library(fbRanks)
library(dplyr)
library(ggplot2)

# Si fbRanks no esta disponible en tu version de R:
# Descargalo del CRAN https://cran.r-project.org/src/contrib/Archive/fbRanks/fbRanks_2.0.tar.gz
# Instala igraphs: install.packages("igraph")
# install.packages(path_absoluta_del_archivo, repos = NULL, type = "source")

# Otra opción de instalación de fbRanks:
# install.packages("remotes")
# library(remotes)
# install_github("cran/fbRanks")

# Colocar el directorio de trabajo según corresponda
# setwd("C:/Users/hvasq/OneDrive/CursoBeduDataScience/Phase2/R/Data/Proyecto")
# Directorio de trabajo si se usa RStudio Cloud con el repositorio del proyecto

setwd("/cloud/project/Data")



# Descarga de archivos
# https://www.football-data.co.uk/spainm.php

# Creamos una lista con números del 10-19 que nos ayudará más tarde a completar las URL con los datasets y nombrar los archivos descargados
lista_numeros <- as.list(10:19)

# Creación de una lista que contiene las URL de los datasets
lista_urls_datasets <- lapply(lista_numeros, function(x){paste("https://www.football-data.co.uk/mmz4281/", x, x+1, "/SP1.csv", sep="")})

# Descarga de los archivos datasets como archivos .csv
for(i in 1:length(lista_urls_datasets)){
    nombre_archivo <- paste("SP1-", lista_numeros[[i]], lista_numeros[[i]]+1, ".csv", sep = "")

    # Verificamos que los archivos .csv no existan para descargarlos
    if(!file.exists(nombre_archivo)){
        download.file(url = lista_urls_datasets[[i]], destfile = nombre_archivo, mode = "wb")
    }
}
# Lectura de los datos

lista_dataframes <- lapply(list.files(pattern = ".csv"), read.csv)

# Procesamiento de los datos

lista_dataframes_s <- lapply(lista_dataframes[1:9], select, Date:FTAG, BbMx.2.5:BbAv.2.5.1)

#La siguiente línea de código es equivalente a :
#d1920S <- select(lista_dataframes[[10]], Date:FTAG, Max.2.5:Avg.2.5.1, -Time)
#lista_dataframes_s <- append(lista_dataframes_s, list(d1920S))

lista_dataframes_s <- append(lista_dataframes_s, list(select(lista_dataframes[[10]], Date:FTAG, Max.2.5:Avg.2.5.1, -Time)))

# Cambiando formato de las fechas

# Los dataframes 1-8 tendrán formato de fecha %d/%m/%y y el formato para los dataframes 9-10 será %d/%m/%Y
lista_dataframes_s <- append(lapply(lista_dataframes_s[1:8], mutate,  Date = as.Date(Date, format = "%d/%m/%y")), 
                           lapply(lista_dataframes_s[9:10], mutate,  Date = as.Date(Date, format = "%d/%m/%Y")))


# Renombrando nombre de las columnas
# Separando df 1-9 en df_1_9 y df 10 en df_10 para manipular columnas por separado

df_1_9 <- bind_rows(lista_dataframes_s[1:9])
df_10 <- lista_dataframes_s[[10]]

df_1_9 <- rename(df_1_9,  Max.2.5.O = BbMx.2.5, 
                 Avg.2.5.O = BbAv.2.5, 
                 Max.2.5.U = BbMx.2.5.1,
                 Avg.2.5.U = BbAv.2.5.1)

df_10 <- rename(df_10,  Max.2.5.O = Max.2.5, 
                 Avg.2.5.O = Avg.2.5, 
                 Max.2.5.U = Max.2.5.1,
                 Avg.2.5.U = Avg.2.5.1)

# Ordenando columnas
df_1_9 <- select(df_1_9, colnames(df_10))

#Uniendo df 1-9 con df 10
data <- bind_rows(df_1_9,df_10)

#Renombrando columnas
data <- rename(data, date = Date, home.team = HomeTeam, home.score = FTHG, away.team = AwayTeam, away.score = FTAG)

# Ordenamos columnas
data <- select(data, date, home.team, home.score, away.team, away.score:Avg.2.5.U)

# Eliminamos objetos para reducir el consumo de memoria
rm(lista_numeros); rm(lista_urls_datasets); rm(lista_dataframes); rm(lista_dataframes_s); rm(df_1_9); rm(df_10)

# Data frames de partidos y equipos

md <- data %>% select(date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)

# Conjuntos iniciales de entrenamiento y de prueba

f <- scores$date # Fechas de partidos
fu <- unique(f) # Fechas sin repetición
Ym <- format(fu, "%Y-%m") # Meses y años
Ym <- unique(Ym) # Meses y años sin repetir
places <- which(Ym[15]==format(scores$date, "%Y-%m")) # Consideramos partidos de 15 meses para comenzar a ajustar el modelo
ffe <- scores$date[max(places)] # Fecha final conjunto de entrenamiento

# Consideraremos partidos de 15 meses para comenzar a ajustar el modelo. Así, nuestro primer conjunto de entrenamiento consiste de datos de partidos hasta el `r ffe` 

train <- scores %>% filter(date <= ffe)
test <- scores %>% filter(date > ffe)

head(train, n = 1); tail(train, n = 1)
head(test, n = 1); tail(test, n = 1)

# Primer ajuste del modelo

traindate <- unique(train$date)
testdate <- unique(test$date)

ranks <- rank.teams(scores = scores, teams = teams, 
                    min.date = traindate[1], 
                    max.date = traindate[length(traindate)])

# Primera predicción

pred <- predict(ranks, date = testdate[1])

phs <- pred$scores$pred.home.score # predicted home score
pas <- pred$scores$pred.away.score # predicted away score
pht <- pred$scores$home.team # home team in predictions
pat <- pred$scores$away.team # away team in predictions

# Continuar ajustando y prediciendo

phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL
for(i in 1:(length(unique(scores$date))-170)){
  ranks <- rank.teams(scores = scores, teams = teams, 
                      min.date = unique(scores$date)[i], 
                      max.date = unique(scores$date)[i+170-1], 
                      silent = TRUE,
                      time.weight.eta = 0.0005)
  pred <- predict(ranks, date = unique(scores$date)[i+170],
                  silent = TRUE)
  
  phs <- c(phs, pred$scores$pred.home.score) # predicted home score
  pas <- c(pas, pred$scores$pred.away.score) # predicted away score
  pht <- c(pht, pred$scores$home.team) # home team in predictions
  pat <- c(pat, pred$scores$away.team) # away team in predictions
}

# Eliminamos NA's

buenos <- !(is.na(phs) | is.na(pas)) # 
phs <- phs[buenos] # predicted home score
pas <- pas[buenos] # predicted away score
pht <- pht[buenos] # home team in predictions
pat <- pat[buenos] # away team in predictions
momio <- data %>% filter(date >= unique(scores$date)[171]) # momios conjunto de prueba
momio <- momio[buenos,]
mean(pht == momio$home.team); mean(pat == momio$away.team)
mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)
mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)
hs <- momio$home.score
as <- momio$away.score

# Probabilidades condicionales

mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
# probabilidad condicional estimada de ganar en over 2.5
mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
# probabilidad condicional estimada de ganar en under 2.5

# Juegos con momios máximos

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Max.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Max.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Max.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Max.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + # se modifica el color y ancho de la linea 
  geom_line(color="light blue",lwd = 2) + # se modifica el color y el ancho de los puntos
  geom_point(color="red",lwd=1) +
  labs(x= "Número de juego",
       y = "Capital",
       title = "Realizando una secuencia de juegos con momios máximos") +
  theme_gray()+ # se establece el tema de líneas blancas con fondo gris
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas  
p

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 3) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
    if((hs[j] + as[j]) > 2.5) cap <- cap + 1000*(momio$Avg.2.5.O[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
  
  if(((phs[j] + pas[j]) < 2.1) & (0.58/(momio$Avg.2.5.U[j]^-1) > 1)){
    if((hs[j] + as[j]) < 2.5) cap <- cap + 1000*(momio$Avg.2.5.U[j]-1)
    else cap <- cap - 1000
    g <- c(g, cap)
  }
}

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + 
  geom_line(color="black",lwd = 2) + # se modifica el color y ancho de la linea 
  geom_point(color="green",lwd=1) + # se modifica el color y el ancho de los puntos
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos con momios promedio") +
  theme_bw()+ # se establece el tema de lineas negras con fondo claro
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p
