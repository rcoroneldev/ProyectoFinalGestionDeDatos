# ===================================
# ===================================
# Proyecto Final Empresa Telefonica

# Conexion a Amazon web services
#install.packages('DBI')
#install.packages('RMySQL')
#install.packages("VIM")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot")
#install.packages("ggplot2")


library(DBI)
library(RMySQL)
library(VIM,warn.conflicts = FALSE)
#library(dplyr)
library(tidyr,warn.conflicts = FALSE)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
#library(ggplot)


id_host = "finalproject1.cy77ex0y7q5u.us-east-1.rds.amazonaws.com"
conectionToMySQL <- dbConnect(RMySQL::MySQL(),
                              dbname = "",
                              host = id_host,
                              user = "usuario_master",
                              password = rstudioapi::askForPassword("Database password"),
                              Port     = 3306)

# Mostrar las bases de datos existentes
dbGetQuery(conectionToMySQL, 'show databases')

# Usar la base de datos creada telefonicadb
nameDatabase <- dbGetQuery(conectionToMySQL, 'use telefonicadb')

# Consultas usando SQL
dfTelefonica <- dbGetQuery(conectionToMySQL,
                           'SELECT * FROM Telefonica')
dfTelefonica


# Exploracion y visualizacion de datos (Estadistica Descriptiva)

#1. Gr?fico de Sectores Circulares para el ESTADO del cliente

# Usando ggplot2
fi <- prop.table(table(dfTelefonica$ESTADO))
df2 <- as.data.frame(fi)
# Barras simples
pie <- ggplot(df2, aes(x="", y=Freq, fill=Var1)) + 
  geom_bar(stat="identity", width=1)
# Conversi?n a "pie" (coordenadas polares)
pie <- pie + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5))
# Cambio de escala de colores
pie <- pie + scale_fill_manual(values=c("#55DDE0", "#F6AE2D", "#F26419", "#999999"))
# Remover y a?adir etiquetas/t?tulo
pie <- pie + labs(x = NULL, y = "% de clientes", fill = "Estado de la atencion",
                 title = "Distribucion del estado de las atenciones")
# Limpiar el formato
pie <- pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, 
                                                               color = "#666666")) 

pie
  
#2. Gr?fico de barras para el nivel academico de los clientes
# Usando ggplot2 (se carga al cargar tidyverse)

barChart <- ggplot(data = dfTelefonica, aes(x = NIVEL_ACADEMICO, y = ..count.., fill = NIVEL_ACADEMICO)) +
  geom_bar() +
  labs(title = "Distribucion del nivel academico de los clientes") +
  theme_bw() +
  theme(legend.position = "right")

#3 
# Tabla de Frecuencia para la modalidad de pago
frec <- table(dfTelefonica$MODALIDAD_PAGO)                      # Frecuencia
prop <- prop.table(table(dfTelefonica$MODALIDAD_PAGO))*100      # Porcentaje
#modalidadDePago <- t(rbind(frec, prop))
#modalidadDePago

barchart2 <- barplot(prop, main="Distribucion de la modalidad de pago de los clientes", 
                     xlab="Modalidad de Pago", col = 2:4,
                     ylab="Porcentaje de Clientes")




#install.packages("tidyverse")

library(shiny)
library(tidyverse)

# Leer datos
df <- read.csv("Telefonica.csv",sep = ';',header = TRUE)
datos <- na.omit(df)
head(df, 10)    # Primeros 10 datos

# Tabla de Frecuencia para la edad
#frec <- table(df$MODALIDAD_PAGO)                      # Frecuencia
#prop <- prop.table(table(df$MODALIDAD_PAGO))*100      # Porcentaje
#modalidadPago <- t(rbind(frec, prop))
#modalidadPago

#barchart2 <- barplot(prop, main="Distribucion de la modalidad de pago de los clientes", 
 #       xlab="Modalidad de Pago", col = 2:4,
  #     ylab="Porcentaje de Clientes")

# Dsshboard Shiny


# vector
stage <- c("PETICION CANCELADA", "TERMINADA")
department <- c("PIURA","LIMA","LAMBAYEQUE","AYACUCHO","PASCO","ICA","UCAYALI","ANCASH","CAJAMARCA")
province <- c("PIURA","CHICLAYO","SECHURA","LIMA","NASCA","OXAPAMAPA","CORONEL PORTILLO")
state <- c("SAN JUAN DE LURIGANCHO","YANAHUANCA","POMAHUACA","LOS OLIVOS","LA MOLINA","SURQUILLO","LA VICTORIA")
documentType <- c("DNI","RUC","CE")
nameProduct <- c("CABLE","PAQUETE ESTANDAR DIGITAL","MOVISTAR TV SATELITAL ESTANDAR")
canalVenta <- c("ONLINE WEB","TIENDA FISICA")
modoVenta <- c("NEGOCIO APP","WEB VENTA")
nivelAcademico <- c("SECUNDARIA","SECUNDARIA INCOMPLETA","PRIMARIA INCOMPLETA","SUPERIOR")
modalidadPago <- c("FINANCIADO",'CONTADO')


ui <- fluidPage(

  tabsetPanel(
    tabPanel("General",
             h2("Bienvenido al proyecto de datos de la Empresa Telefonica "),
             p("A continuacion se muestra la presentacion de la empresa "),
             #HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/T1-k7VYwsHg" 
             #     frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
             
             HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/OsDjOsA559o" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
    
    tabPanel("Importar Datos", 
             #fileInput("file", "Ingreso de archivo", buttonLabel = "Cargar Datos"),
             
             # App title ----
             titlePanel("Subir Archivo"),
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Escoger CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Checkbox if file has header ----
                 h5("Seleccionar header"),
                 checkboxInput("header", "Header", TRUE),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select separator ----
                 radioButtons("sep", "Seleccionar Separador",
                              choices = c(Coma = ",",
                                          PuntoYcoma  = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 # Input: Select quotes ----
                 radioButtons("quote", "Seleccionar Comillas",
                              choices = c(None = "",
                                          "Comillas dobles" = '"',
                                          "Comillas Simple" = "'"),
                              selected = '"'),
                 
                 # Horizontal line ----
                 tags$hr(),
                 
                 # Input: Select number of rows to display ----
                 radioButtons("disp", "Numeros de filas",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head")
                 
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Data file ----
                 tableOutput("contents")
                 
               )
               
             )
             
       
             
    ),
    
  
    tabPanel("Formulario de Registro",
             # Título de la aplicación
             titlePanel("Lista de Clientes Registrados"),
             
            sidebarLayout(  
              sidebarPanel(
             h2("Registro de Nuevos Clientes"),
             p("En esta sección se puede ingresar nuevas transacciones realizadas por los clientes."),
             numericInput("code", "Codigo del cliente", value=13791761),
             selectInput("_stage", "Estado", stage),
             dateInput("_date",label = "Fecha de ingreso"),
             selectInput("_documentType", "Tipo de documento", documentType),
             numericInput("_document", "Documento", value=135003176),
             dateRangeInput("_daterange", "Fecha de Alta y Fecha de Baja:",
                            start = "2021-01-01",
                            end   = "2021-12-31"),
             selectInput("_department", "Departamento", department),
             selectInput("_province", "Provincia", province),
             selectInput("_state", "Estado", state),
             selectInput("_product", "Producto", nameProduct),
             selectInput("_canalVenta", "Canal de Venta", canalVenta),
             selectInput("_modoVenta", "Modo de Venta", modoVenta),
             selectInput("_nivelacademico", "Nivel Academico", nivelAcademico),
             selectInput("_modalidadPago", "Modalidad de Pago", modalidadPago) ),
            
             mainPanel(
               dataTableOutput("tdinamica")
             )
            ) 
             
             
             ),
    tabPanel("Analisis Descriptivo",
             # Fila 1
             fluidRow(
               column(4,selectInput("_modalidadPago", "Modalidad de Pago", modalidadPago)),
               column(4,dateRangeInput("_daterangeD", "Fecha de Alta y Fecha de Baja:",start = "2021-01-01",end   = "2021-12-31")),
               #column(4,"Polígono de frecuencia",numericInput("ancho", label = "Tamaño intervalo", value = 0.1, step = 0.1),
               column(4,sliderInput("_ageSlider", "Edad:",min = 18, max = 90, value = 40)
               )
             ),
             # Fila 2
             fluidRow(
               #column(12, plotOutput("pie"))
               column(12, plotOutput("chart1"))
               #column(4, plotOutput("pie")),
               #column(3, verbatimTextOutput("ttest"))
             ),
             # Fila 3
             fluidRow(
               column(12, plotOutput("chart2"))
             )  ,
             fluidRow(
                column(12, plotOutput("pie"))
             )
             
             
             
             
             
             
             ),
    
    tabPanel("Analisis de Outliers"),
    tabPanel("Analisis de Datos Perdidos")
    
  )
  
)






server <- function(input, output, session) {
   # output pie chart
  
  output$pie <- renderPlot({
    
    # Gráfico pie chart
    pie
  }, res = 96)
  
  output$chart1 <- renderPlot({
    
    # Gráfico pie chart 1
    barChart
  }, res = 96)
  
  output$chart2 <- renderPlot({
    
    # Gráfico pie chart 2
    barchart2
  }, res = 96)
  
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  # Mostrar dataTable 
  
  #output$tdinamica <- renderDataTable(mtcars, options = list(pageLength = 5))
  output$tdinamica <- renderDataTable(dfTelefonica, options = list(pageLength = 5))
  
}


shinyApp(ui, server)