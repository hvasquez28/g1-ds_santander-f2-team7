## app.R ##

## Dashboard de proyecto final de R del equipo 7

library(shiny)
library(shinydashboard)
#install.packages("shinythemes")
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
          menuItem("Prueba de Hipotesis", tabName = "hip", icon = icon("glyphicon glyphicon-question-sign", lib = "glyphicon")) 
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
          
          #  Prueba de Hipotesis y algo nuevo que hicimos con los datos
          tabItem(tabName = "hip",
                  fluidRow(
                    titlePanel(h3("Prueba de Hipotesis y ... ")),
                   
                    
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
    
    
    data %>% ggplot(aes(x, fill = FTR)) + 
      geom_bar() + 
      facet_wrap("away.team") +
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