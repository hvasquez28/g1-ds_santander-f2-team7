install.packages("ggplot2")
install.packages("dplyr")

library(fbRanks)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(FactoMineR)
library(BasketballAnalyzeR)


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

setwd("/cloud/project/Data/MatchData")

md <- data %>% select(date:away.score)
write.csv(md, "match.data.csv", row.names = FALSE)
df <- create.fbRanks.dataframes(scores.file = "match.data.csv")
teams <- df$teams; scores <- df$scores

head(teams, n = 2L); dim(teams); head(scores, n = 2L); dim(scores)


## -------------------------------------------------------------------------
## En esta sección aportamos codigo nuevo de un analisis de datos distinto


ca <- c() #Casa / Away;
tw <- c() #Team that Won
tl <- c() #Team that lost
dif_goles <- c();avgU <- NULL;avgO<- NULL; w<- NULL;l <- NULL;e<- NULL 
for (i in 1:3800) {
  if(data$home.score[i] > data$away.score[i])
  {
    tw[i] = data$home.team[i]
    tl[i] = data$away.team[i]
    w[i] = data$home.score[i] 
    l[i] = data$away.score[i]
    dif_goles[i] = data$home.score[i] - data$away.score[i]
    ca[i] = 1
    avgO[i] = data$Avg.2.5.O[i]; avgU[i] = data$Avg.2.5.U[i]
    e[i] = 0
  }
  else if(scores$home.score[i] < scores$away.score[i])
  {
    tl[i] = scores$home.team[i]
    tw[i] = scores$away.team[i]
    w[i] = scores$away.score[i]
    l[i] = scores$home.score[i]
    dif_goles[i] =  scores$away.score[i]-scores$home.score[i]
    ca[i] = 0
    avgO[i] = data$Avg.2.5.O[i]; avgU[i] = data$Avg.2.5.U[i]
    e[i] = 0
  }
  else if(scores$home.score[i] == scores$away.score[i])
  {
    ca[i] = 0
    e[i] = 1
    dif_goles[i] = 0
  }
}
winner <- data.frame(team=tw,w,avgO,avgU) #Datos de partidos ganados
losser <- data.frame(team=tl,l,avgO,avgU) #Datos de partidos perdidos
winner <- winner %>% drop_na(team)
losser <- losser %>% drop_na(team)
winner <- winner %>% group_by(team) %>% summarise(avg_gw = mean(w),avgUw = mean(avgU), avgOw=mean(avgO), g = n())
losser <- losser %>% group_by(team) %>% summarise(avg_gl = mean(l),avgUl = mean(avgU), avgOl=mean(avgO), p = n())
dim(losser)#2884

#Se hara un cluster analisis para determinar las caracteristicas que conforman
#a los equipos ganadores, tanto como perdedores.

wins <- data.frame(winner[,-c(1)])
wkc <- kclustering(wins)
loses <- data.frame(losser[,-c(1)])
lkc <- kclustering(loses)

#Graficos para determinar la cantidad de clusters a usar
plot(wkc)
plot(lkc)

#En base a las graficas se toma la decision de hacerlo por 6 clusters
#La linea azul presente en el grafico, es el rendimiento general de todos los equipos.
#La roja indica que tan bien/mal lo hace un grupo.
lkc2 <- kclustering(loses,k=6,labels = losser$team)
plot(lkc2,profiles =T)

wkc2 <- kclustering(wins,k=6,labels = winner$team)
plot(wkc2, profiles =T)

#De estos graficos se observa que tanto el barcelona como el real madrid
#Son los que presentan diferecias mas remarcadas.
lhc <- hclustering(loses,k=6,labels = losser$team)
plot(lhc,
     rect = T,
     colored.branches = T)

whc <- hclustering(wins,k=6,labels = winner$team)
plot(whc,
     rect = T,
     colored.branches = T)

#Se unen los datos para determinar a los mejores equipos.
wls <- inner_join(winner,losser, by ="team")
wls <- mutate(wls, win_rate = g/(p+g))
wls %>% arrange(desc(win_rate))
wls2 <- data.frame(wls[,-c(1)])
wlkc <- kclustering(wls2)
plot(wlkc)

#Con 6 clusters se observa que los clusters mas explosivos estan conformados por
#El numero 1 y el numero 3, donde p (perdidos) es menor al promedio,
#g(ganados) es mayor al promedio asi como win rate.
#En base a este analisis se puede determinar, quienes pueden ganar el campeonato
#La siguiente temporada.
wlkc2 <- kclustering(wls2,k=6,labels = wls$team)
plot(wlkc2,profiles =T)

#Aqui podemos observar de forma mas clara quienes conforman cada cluster, donde
#aquellos grupos que se unen mas pronto presentan mas similitudes que aquellos
#que se integran mas tarde.
#Por ejemplo, en este caso, el cluster 5 es el ultimo en agruparse
#Mientras que el cluster 4 y 3 se unen primero.
wlhc <- hclustering(wls2,k=6,labels = wls$team)
plot(wlhc,
     rect = T,
     colored.branches = T)

logic <- data.frame(ca,dif_goles,e)
logic2 <- subset(logic, e!= 1)
home_away <- glm(ca ~ dif_goles, data=logic, family = "binomial") #Ca #Dif en goles. Logistic Model
summary(home_away) 

#El modelo mejora considerablemente cuando eliminamos los empates.
#Entre menor sea el AIC, NULL Deviance y Residual Dev, mejor el modelo
#AIC 3775 #Sin Empates
#NULL 3809
#Res 3771

#AIC 4547 #Con Empates
#Null 5259
#Res 4543

ggplot(logic, aes(x=dif_goles,y=ca))+
  geom_point(alpha=.2)+
  geom_smooth(method = "glm", method.args=list(family="binomial"))+
  labs(
    title="R. Logistica ~ Con Empates",
    x = "Dif En Goles",
    y = "Probabilidad de ganar en Casa"
  )

ggplot(logic2, aes(x=dif_goles,y=ca))+
  geom_point(alpha=.2)+
  geom_smooth(method = "glm", method.args=list(family="binomial"))+
  labs(
    title="R. Logistica ~ Sin Empates",
    x = "Dif En Goles",
    y = "Probabilidad de ganar en Casa"
  )

## --------------------------------------------------------------------------



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


### -------------------------------------------------------------------------
# Se cambió esta sección del codigo, por un codigo propio para hacer uso 
# de contraste de hipotesis

#Por los histogramas de nuestros datos
#Sabemos que estos no siguen una distribucion normal
#Por otro lado, al calcular el AIC dentro del modelo de ranks
#Nos percatamos que este aumenta conforme la poblacion lo hace
#Esto por la misma naturaleza del criterio de AIC
#De igual forma, tambien aumenta la desviacion, pero esta de forma menos explosiva

#Por lo que la obtencion de nuestro mejor modelo sera basara en la probabilidad
#condicional estimada de ganar en over 2.5
# Y la probabilidad condicional estimada de ganar en bajo 2.5 goles

#Por otro lado, podemos observar del modelo de regresion logistica basada en la
#Dif de goles, que el cambio mas notorio ocurre cuando un equipo lleva una ventaja
#superior a dos goles
#El modelo inicial itera 1066 veces la funcion team.ranks con intervalos de j 
#Fechas unicas.

#170 ~ 13%, 618 ~ 50%, 309 ~ 25%, 927 ~ 75%
#for (j in 170:927) # Para Estimar el mejor modelo
#{
  phs <- NULL; pas <- NULL; pht <- NULL; pat <- NULL; j <- 325; pu_25 <- NULL; po_25 <- NULL
  for(i in 1:(length(unique(scores$date))-j)){
    ranks <- rank.teams(scores = scores, teams = teams, 
                        min.date = unique(scores$date)[i], 
                        max.date = unique(scores$date)[i+j-1], 
                        silent = TRUE,
                        time.weight.eta = 0.0005)
    pred <- predict(ranks, date = unique(scores$date)[i+j],
                    silent = TRUE)
    
    phs <- c(phs, pred$scores$pred.home.score) # predicted home score
    pas <- c(pas, pred$scores$pred.away.score) # predicted away score
    pht <- c(pht, pred$scores$home.team) # home team in predictions
    pat <- c(pat, pred$scores$away.team) # away team in predictions
  }
  
  buenos <- !(is.na(phs) | is.na(pas)) # 
  phs <- phs[buenos] # predicted home score
  pas <- pas[buenos] # predicted away score
  pht <- pht[buenos] # home team in predictions
  pat <- pat[buenos] # away team in predictions
  momio <- data %>% filter(date >= unique(scores$date)[j+1]) # momios conjunto de prueba
  momio <- momio[buenos,]
  mean(pht == momio$home.team); mean(pat == momio$away.team)
  mean(phs + pas > 2.5 & momio$home.score + momio$away.score > 2.5)#.32  #.31  #.31
  mean(phs + pas < 2.5 & momio$home.score + momio$away.score < 2.5)#.24  #.25  #.26
  hs <- momio$home.score
  as <- momio$away.score
  
  # Probabilidades condicionales
  
  mean(phs + pas > 3) # proporción de partidos con más de tres goles según el modelo
  mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
  # probabilidad condicional estimada de ganar en over 2.5
  #po_25[j-169] <- mean(phs + pas > 3 & hs + as > 2.5)/mean(phs + pas > 3) 
  
  mean(phs + pas < 2.1) # proporción de partidos con menos de 2.1 goles según el modelo
  mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
  # probabilidad condicional estimada de ganar en under 2.5
  #pu_25[j-169] <- mean(phs + pas < 2.1 & hs + as < 2.5)/mean(phs + pas < 2.1) 
#}

# probabilidad condicional estimada de ganar en over 2.5
s <- seq(0,3,.1)
y<- NULL; y2 <- c()
for (i in 1:31) {
  y[i] <-  mean(phs + pas > 3 & hs + as > s[i])/mean(phs + pas > 3)
  y2[i] <- mean(phs + pas < 2.1 & hs + as < s[i])/mean(phs + pas < 2.1)
}
length(s)
plot(s,y)

#Al graficar las proporciones es claro observar que las probabilidad aumentan
#De forma considerable en todos los niveles, es decir, cuadno la probabilidad
#de ganar con 1 gol, 2 y 3 de ventaja. 
#Dadas las limitaciones de mi ordenador, el mejor modelo al que se ha llegado
#Es cuando tomamos el 24%-26% de fechas unicas. 

#H.o. prop. de ganar con 2.5 goles de ventaja con 325 fechas unicas es 
#menor o igual que con 170

#H.A. Prop de ganar con 2.5 goles de ventaja con 325 fechas unicas es mayor que con 
#170 unicas. 

#Probabilidades Condicionales Estimadas
#.661 > 2.5  #325
#.594 < 2.5  #325

#.64  > 2.5 #170
#.58  < 2.5 #170 

hist(as + hs)
hist(pas + phs)


#Al graficar el histograma de la suma de los goles reales (Por asi decirlo)
#Nos damos cuenta que la cantidad de partidos donde hubo mas de 4 goles
#Es mucho mayor que aquella de los partidos predecidos, en base a esto
#Se cambia el parametro a 4 para obtener mejores resultados.
#Si se disminuye el valor condicional de la suma real de los goles anotados
#Se obtienen mejores ganancias al igual que menores perdidas.
# (phs[j] + pas[j]) > 3) y (hs[j] + as[j]) > 2.5) 
# <<<< (phs[j] + pas[j]) > 4) y (hs[j] + as[j]) > 2.5) #Las perdidas disminuyen
# <<<<  (phs[j] + pas[j]) > 4) y (hs[j] + as[j]) > 1.5) #

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
tail(g)
# Escenario con momios máximos

g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos ~ Momios Maximos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p

# Escenario con momios promedio

cap <- 50000; g <- NULL

for(j in 1:length(phs)){
  if(((phs[j] + pas[j]) > 4) & (0.64/(momio$Avg.2.5.O[j]^-1) > 1)){
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
tail(g) #58170


g <- data.frame(Num_Ap = 1:length(g), Capital = g)
p <- ggplot(g, aes(x=Num_Ap, y=Capital)) + geom_line( color="purple") + geom_point() +
  labs(x = "Número de juego", 
       y = "Capital",
       title = "Realizando una secuencia de juegos") +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1),
        axis.text.y = element_text(face = "bold", color="blue" , size = 10, angle = 25, hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 
p
