## app.R ##

## Dashboard de proyecto final de R del equipo 7

#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinythemes")
#install.packages("dplyr")

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)

#Esta parte es el análogo al ui.R
ui <- 
  
  fluidPage(
    
    dashboardPage(
      
      dashboardHeader(title = "Predicción de resultados", titleWidth = 300),
      title = "Proyecto de R del Equipo 7",
      skin = "green",
      
      dashboardSidebar(
        
        sidebarMenu(
          menuItem("Gráficos de barras", tabName = "Goles", icon = icon("chart-column")),
          menuItem("Goles local - visitante", tabName = "img", icon = icon("soccer-ball-o")),
          menuItem("Data Table", tabName = "data_table", icon = icon("table")),
          menuItem("Factores de ganancia", tabName = "img2", icon = icon("glyphicon glyphicon-stats", lib = "glyphicon")),
          menuItem("Hallazgos por Clustering", tabName = "clus", icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon")),
          menuItem("Hallazgos adicionales", tabName = "adi", icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon")) 
        )
        
      ),
      
      dashboardBody(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        tabItems(
          
          # Gráficas de barras, donde en el eje de las X se muestren los goles de local y visitante,
          tabItem(tabName = "Goles",
                  fluidRow(
                    titlePanel("Goles a favor y en contra por equipo"), 
                    selectInput("x", "Seleccione el valor de X",
                                choices = c("Local" = "home.score", "Visitante" = "away.score")),
                    
                    
                    plotOutput("plot1", height = 450, width = 750)
                  )
          ),
          
          # Imágenes goles local - visitante
          tabItem(tabName = "img",
                  fluidRow(
                    titlePanel(h3("Probabilidad de goles en casa y visitante")),
                    selectInput("img", "Seleccione la gráfica",
                                choices = c("Prob. Marg. Local", "Prob. Marg. Visitante", "Prob. Conjunta")),
                    
                    conditionalPanel(condition = "input.img == 'Prob. Marg. Local' ", 
                                     img( src = "ProbMarginalLocal.png", 
                                          height = 450, width = 450)
                    ),
                    conditionalPanel(condition = "input.img == 'Prob. Marg. Visitante' ", 
                                     img( src = "ProbMarginalVisitante.png", 
                                          height = 450, width = 450)
                    ),
                    conditionalPanel(condition = "input.img == 'Prob. Conjunta' ", 
                                     img( src = "ProbConjunta.png", 
                                          height = 450, width = 450)
                    )
                    
                  )
          ),
          
          #data table del fichero match.data.csv
          tabItem(tabName = "data_table",
                  fluidRow(        
                    titlePanel(h3("Data Table")),
                    dataTableOutput ("data_table")
                  )
          ),
          
          #  imágenes de las gráficas de los factores de ganancia promedio y máximo
          tabItem(tabName = "img2",
                  fluidRow(
                    titlePanel(h3("Gráficas de dispersión para la correlación de las variables")),
                    selectInput("img2", "Seleccione la gráfica",
                                choices = c("Factores de ganancia promedio", "Factores de ganancia maximo")),
                    
                    conditionalPanel(condition = "input.img2 == 'Factores de ganancia promedio' ", 
                                     img( src = "MomiosPromedio.png", 
                                          height = 450, width = 450)
                    ),
                    conditionalPanel(condition = "input.img2 == 'Factores de ganancia maximo' ", 
                                     img( src = "MomiosMaximo.png", 
                                          height = 450, width = 450)
                    )
                    
                  )
          ),
          
          #  Prueba de contraste de Hipotesis
          tabItem(tabName = "clus",
                  fluidRow(
                    titlePanel(h2("Hallazgos adicionales usando algoritmos de Clustering")),
                    titlePanel(h3("Graficos para determinar la cantidad de clusters a usar")),
                    img( src = "DetClusterWineers.jpg", 
                         height = 350, width = 450),
                    p("Determinante de clusters de equipos ganadores"),
                    p(" "),
                    img( src = "DetClusterLosers.jpg", 
                         height = 350, width = 450),
                    p("Determinante de clusters de equipos perdedores"),
                    titlePanel(h4("En base a las gráficas anteriores se toma la decisión de hacerlo con 6 clusters")),
                    titlePanel(h4("En las siguientes gráficas se usa este numero de clusters.")),
                    
                    p("Los clusters van difiriendo en cada una de las siguientes gráficas"), 
                    p("La linea azul presente en el grafico, es el rendimiento general de todos los equipos."), 
                    p("La roja indica que tan bien/mal lo hace un grupo."),
                    p("Si la lineas rojas se salen del circulo azul, esto quiere decir que los equipos conformados por el cluster, lo hacen mejor que el promedio"),
                    p("Mientras que aquellas que se mantienen dentro del promedio(circulo azul), presentan un menor rendimiento "),
                    img( src = "ClusterLosersKmeans.jpg", 
                         height = 350, width = 450),
                    p("Caracteristicas de los equipos perdedores"),
                    img( src = "ClusterWineersKmeans.jpg", 
                         height = 350, width = 450),
                    p("Caracteristicas de los equipos Ganadores"),
                    img( src = "ClusterJeraquicoLosers.jpg", 
                         height = 350, width = 450),
                    p("Cluster Jerarquico de equipos Perdedores"),
                    img( src = "ClusterJeraquicoWinners.jpg", 
                         height = 350, width = 450),
                    p("Cluster Jerarquico de equipos Ganadores"),
                 
                    titlePanel(h4("Se unen los datos para determinar a los mejores equipos.")),
                    img( src = "DetClusterCombinado.jpg", 
                         height = 350, width = 450),
                    p("Determinante de Clusters de equipos combinados"),
                    p("Con 6 clusters se observa que los clusters mas explosivos estan conformados por 
                    el numero 6 y el numero 3, donde p (perdidos) es menor al promedio,
                    g(ganados) es mayor al promedio asi como win rate."),
                    
                    
                    img( src = "AnalisisClusterConjunto.jpg", 
                         height = 350, width = 450),
                    p("Caracteristicas Determinantes de ambos equipos"),
                    img( src = "ClusterJeraquicoCombinado.jpg", 
                         height = 350, width = 450),
                    p("Cluster Jerarquico de equipos Combinados"),
                    p("De estos graficos se observa que tanto el Barcelona como el Real Madrid 
                    son los que presentan diferencias más remarcadas."),
                    titlePanel(h4("En base a este análisis se puede determinar, 
                    quienes pueden ganar el campeonato la siguiente temporada."))
                    
                  )
          ),
        
          #  Hallazgos adicionales que hicimos con los datos
          tabItem(tabName = "adi",
                  fluidRow(
                    titlePanel(h3("Hallazgos y gráficas adicionales")),
                    p("Se toma la decision de crear un modelo de regresion logistica basado en la diferencia de goles 
                      necesaria para que un equipo como local gane. (El complemento es la probabilidad de que el equipo
                      gane como visitante)"),
                    p("Se toman en cuenta los empates, y posteriormente, se eliminan aquellos partidos 
                      para obtener un mejor modelo. "),
                    img( src = "RegLogisticaConEmpates.jpg", 
                         height = 350, width = 450),
                    p("Gráfica de Regresión Logistica Con Empates"),
                    img( src = "RegLogisticaSinEmpates.jpg", 
                         height = 350, width = 450),
                    p("Gráfica de Regresión Logistica Sin Empates"),
                    p("El modelo mejora considerablemente cuando eliminamos los empates."),
                    p("AIC 3775  #Sin Empates"),
                    p("NULL 3809"),
                    p("Res 3771"),
                    p("AIC 4547  #Con Empates"),
                    p("Null 5259"),
                    p("Res  4543"),
                    
                    titlePanel(h4("Gráficas de Momios resultantes")),
                    img( src = "NuevoMomiosMax.jpg", 
                         height = 350, width = 450),
                    p("Gráfica de Momios Máximos"),
                    img( src = "NuevoMomiosProm.jpg", 
                         height = 350, width = 450),
                    p("Gráfica de Momios Promedios"),
                    
                    
                    p("Al graficar las proporciones es claro observar que las probabilidad aumentan
    de forma considerable en todos los niveles, es decir, cuando la probabilidad
    de ganar con 1 gol, 2 y 3 de ventaja. 
    Dadas las limitaciones del ordenador, el mejor modelo al que se ha llegado
    Es cuando tomamos el 24%-26% de fechas unicas."), 
     
    p("H.o. prop. de ganar con 2.5 goles de ventaja con 325 fechas unicas es 
    menor o igual que con 170"),
    
    p("H.A. Prop de ganar con 2.5 goles de ventaja con 325 fechas unicas es mayor que con 
    170 unicas."),
    
    p("Probabilidades Condicionales Estimadas"),
    p("#.661 > 2.5  #325"),
    p("#.594 < 2.5  #325"),
    
    p("#.64  > 2.5 #170"),
    p("#.58  < 2.5 #170 "),
    
    p("hist(as + hs) #El histograma muestra que hay mas partidos(reales) con mas de 4 goles"),
    p("hist(pas + phs)#Que partidos predecidos (con mas de 4 goles).")
                  )
          )
 
      )
    )
  )
  )

#De aquí en adelante es la parte que corresponde al server

server <- function(input, output) {
  library(ggplot2)
  
  #gráficas de barras de 
  output$plot1 <- renderPlot({
   
    macthdataURL<-"https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv"
    data <-  read.csv(macthdataURL, header = T)
    
    data <- mutate(data, FTR = ifelse(home.score > away.score, "H", ifelse(home.score < away.score, "A", "D")))
    
    x <- data[,input$x]
    
    
    data %>% ggplot(aes(x, fill = FTR)) + # se hace un pipeline para data con el operador %>%
      geom_bar() + #Gráfica de barras
      facet_wrap("away.team") + #se establecen niveles de colores de acuerdo a away.team
      labs(x =  ifelse(input$x == "home.score", "Local", "Visitante"), y = "Goles") + 
      ylim(0,50)
    
    
  })
  

  #Data Table
  macthdataURL<-"https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2021/main/Sesion-08/Postwork/match.data.csv"
  data <-  read.csv(macthdataURL, header = T)
  
  output$data_table <- renderDataTable( {data}, 
                                        options = list(lengthMenu =  c(10,20,25,40,50),
                                                       pageLength = 10)
  )
  
}


shinyApp(ui, server)