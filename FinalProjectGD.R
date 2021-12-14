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
#install.packages("shiny")
library(DBI)
library(RMySQL)
library(VIM,warn.conflicts = FALSE)
#library(dplyr)
library(tidyr,warn.conflicts = FALSE)

library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
#library(shiny)


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



#------------------------------------------------------------------------------
#2. Gráfico de barras para el nivel academico de los clientes
#------------------------------------------------------------------------------

# Tabla de Frecuencia para el nivel académico
frec <- table(dfTelefonica$NIVEL_ACADEMICO)                      # Frecuencia
prop <- prop.table(table(dfTelefonica$NIVEL_ACADEMICO))*100      # Porcentaje

nivelAcademico <- t(rbind(frec, prop))
nivelAcademico

library(tidyverse)      # Para poder usar %>%
table(dfTelefonica$NIVEL_ACADEMICO)
prop.table(table(dfTelefonica$NIVEL_ACADEMICO)) %>% round(digits = 2)

# Nota: para obtener un formato similar a SAS o SPSS se puede usar gmodels
library(gmodels)
CrossTable(dfTelefonica$NIVEL_ACADEMICO, format="SAS")
CrossTable(dfTelefonica$NIVEL_ACADEMICO, format="SPSS")

#Gráfico usando ggplot2 (se carga al cargar tidyverse)
ggplot(data = dfTelefonica, aes(x = NIVEL_ACADEMICO, y = ..count.., fill = NIVEL_ACADEMICO)) +
  geom_bar() +
  labs(title = "Distribución del nivel académico de los clientes") +
  theme_bw() +
  theme(legend.position = "right")


#------------------------------------------------------------------------------
#3. Gráfico de Sectores Circulares para la tipología unificada
#------------------------------------------------------------------------------

frec <- table(dfTelefonica$TIPOLOGIA_UNIFICADA)                      # Frecuencia
prop <- prop.table(table(dfTelefonica$TIPOLOGIA_UNIFICADA))*100      # Porcentaje

tipologiaUnificada <- t(rbind(frec, prop))
tipologiaUnificada

# Usando ggplot2
fi <- prop.table(table(dfTelefonica$TIPOLOGIA_UNIFICADA))
df2 <- as.data.frame(fi)
# Barras simples
pie <- ggplot(df2, aes(x="", y=frec, fill=Var1)) + 
  geom_bar(stat="identity", width=1)
# Conversión a "pie" (coordenadas polares)
pie <- pie + coord_polar("y", start=0) + 
  geom_text(aes(label = paste0(round(Freq*100), "%")),
            position = position_stack(vjust = 0.5))
# Remover y añadir etiquetas/título
pie <- pie + labs(x = NULL, y = "% de clientes", fill = "Tipología de la atención",
                  title = "Distribución de las tipologías de atenciones")
# Limpiar el formato
pie <- pie + theme_classic() + theme(axis.line = element_blank(),
                                     axis.text = element_blank(),
                                     axis.ticks = element_blank(),
                                     plot.title = element_text(hjust = 0.5, 
                                                               color = "#666666")) 
pie


#------------------------------------------------------------------------------
#4. Distribución del tipo de producto y el subproducto
#------------------------------------------------------------------------------

#Tabla de contingencia
tabla1 <- table(dfTelefonica$TIPO_PRODUCTO, dfTelefonica$SUB_PRODUCTO)
tabla1

tabla2 <- prop.table(tabla1, margin=1)
tabla2

frec <- table(dfTelefonica$TIPO_PRODUCTO)                      # Frecuencia
prop <- prop.table(table(dfTelefonica$TIPO_PRODUCTO))*100      # Porcentaje
tipoProducto <- t(rbind(frec, prop))
tipoProducto

frec <- table(dfTelefonica$SUB_PRODUCTO)                      # Frecuencia
prop <- prop.table(table(dfTelefonica$SUB_PRODUCTO))*100      # Porcentaje
subProducto <- t(rbind(frec, prop))
subProducto

# Usando ggplot2
ggplot(data = dfTelefonica, aes(x = TIPO_PRODUCTO, y = ..count.., fill = SUB_PRODUCTO)) +
  geom_bar(position = "fill") +
  labs(y = "Prop. de clientes", title = "Distribución del tipo de producto vs el subproducto") +
  theme_bw() +
  theme(legend.position = "bottom")
 
#------------------------------------------------------------------------------
#5. Distribución de frecuencias para el monto pagado
#------------------------------------------------------------------------------

# Tabla de frecuencia (usando la regla de Sturges)
factorx <- factor(cut(dfTelefonica$monto_producto, breaks=nclass.Sturges(dfTelefonica$monto_producto),right=TRUE))
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("monto_producto","ni")
xout

# Histograma y polígono de frecuencia
h1 <- hist(dfTelefonica$monto_producto, breaks = "Sturges",
           xlab="Monto",
           ylab="Número de clientes")


# Analisis de datos perdidos(Imputacion de los datos)
# ====================================================

# Cantidad de filas
ndatos <- nrow(dfTelefonica)
ndatos

dfTelefonicaParte <- dfTelefonica[,c('TIPO_DOCUMENTO','DOCUMENTO','UBIGEO','UBIGEO_RENIEC','EDAD','SUB_PRODUCTO','monto_producto','TIPOLOGIA_UNIFICADA')]

# Mostrar qué columnas tienen valores perdidos
cidx_perd <- which(colSums(is.na(dfTelefonicaParte))!=0)
cidx_perd

# Cantidad de valores perdidos en las columnas
nperdidos <- colSums(is.na(dfTelefonicaParte[,cidx_perd]))
nperdidos

# Porcentaje de valores perdidos en las columnas
pperdidos <- 100*nperdidos/ndatos
pperdidos

# Gráfico de agregación: "aggregation plot"
agreg <- aggr(dfTelefonicaParte, numbers=TRUE)
agreg

# Ejemplo de visualización diferente: ordenado según valores faltantes
aggr(dfTelefonicaParte, numbers=TRUE, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

vRiesgo <- c("R1","R2","RT")
dfTelefonicaParte[ , vRiesgo] <- NA

vImputar <- c("SP")
dfTelefonicaParte[ , vImputar] <- NA

dfTelefonicaParte$R1[dfTelefonicaParte$UBIGEO != dfTelefonicaParte$UBIGEO_RENIEC & dfTelefonicaParte$TIPO_DOCUMENTO != 'CE' ] <- 1
dfTelefonicaParte$R1[dfTelefonicaParte$UBIGEO == dfTelefonicaParte$UBIGEO_RENIEC] <- 0
dfTelefonicaParte$R1[is.na(dfTelefonicaParte$R1)] <- 0

dfTelefonicaParte$R2[dfTelefonicaParte$EDAD >= 70] <- 1
dfTelefonicaParte$R2[is.na(dfTelefonicaParte$R2)] <- 0

dfTelefonicaParte$RT[is.na(dfTelefonicaParte$RT)] <- dfTelefonicaParte$R1+dfTelefonicaParte$R2

dfTelefonicaParteImputado = dfTelefonicaParte

dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA[is.na(dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA) & dfTelefonicaParteImputado$RT == 1] <- 'MALA VENTA'
dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA[is.na(dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA) & dfTelefonicaParteImputado$RT > 1] <- 'FRAUDE POR SUSCRIPCION'
dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA[is.na(dfTelefonicaParteImputado$TIPOLOGIA_UNIFICADA) & dfTelefonicaParteImputado$RT == 0] <- 'FALSO POSITIVO'

Mode <- function (x, na.rm) {
  xtab <- table(x)
  xmode <- names(which(xtab == max(xtab)))
  if (length(xmode) > 1) xmode <- ">1 mode"
  return(xmode)
}

dfTelefonicaParteImputado[is.na(dfTelefonicaParteImputado[,"SUB_PRODUCTO"]),"SUB_PRODUCTO"] <- Mode(dfTelefonicaParteImputado[,"SUB_PRODUCTO"], na.rm = TRUE)
dfTelefonicaParteImputado$monto_producto[is.na(dfTelefonicaParteImputado$monto_producto) & dfTelefonicaParteImputado$SUB_PRODUCTO == "PAQUETE ESTANDAR DIGITAL"] <- 79
dfTelefonicaParteImputado$monto_producto[is.na(dfTelefonicaParteImputado$monto_producto) & dfTelefonicaParteImputado$SUB_PRODUCTO == 'MOVISTAR TV SATELITAL ESTANDAR'] <- 89
dfTelefonicaParteImputado$monto_producto[is.na(dfTelefonicaParteImputado$monto_producto) & dfTelefonicaParteImputado$SUB_PRODUCTO == 'PAQUETE HOGAR DIGITAL'] <- 69

#eliminando columnas que se agregaron para el procesamiento
dfTelefonicaParteImputado <- dfTelefonicaParteImputado[,c('TIPO_DOCUMENTO','DOCUMENTO','UBIGEO','UBIGEO_RENIEC','EDAD','SUB_PRODUCTO','monto_producto','TIPOLOGIA_UNIFICADA')]


# Imputamos la edad con la media
avg_edad <- mean(dfTelefonicaParteImputado$EDAD, na.rm = TRUE)
avg_edad <- round(avg_edad, 0)
dfTelefonicaParteImputado$EDAD[is.na(dfTelefonicaParteImputado$EDAD)] <- avg_edad

dfTelefonicaParteImputado

# Analisis de Valores Atípicos 
# ========================================

#Separamos solo las columnas numéricas

dfTelefonicaNumerico = dfTelefonicaParteImputado[c('EDAD','monto_producto')]

#Por simplicidad omitimos valores perdidos

dfTelefonicaOutliers <- na.omit(dfTelefonicaNumerico)

#Análisis multivariado : boxplot

par(mfrow = c(1, 3))
plot(dfTelefonicaOutliers)
boxplot(dfTelefonicaOutliers[, 1], main = "Valores de Edad")
boxplot(dfTelefonicaOutliers[, 2], main = "Valores de Monto")
par(mfrow = c(1, 1))

#Análisis multivariado : barplot

par(mfrow = c(1, 3))
plot(dfTelefonicaOutliers)
barplot(dfTelefonicaOutliers[, 1], main = "Valores de Edad")
barplot(dfTelefonicaOutliers[, 2], main = "Valores de Monto")
par(mfrow = c(1, 1))

# Distancia de Mahalanobis cuadrada

mu <- colMeans(dfTelefonicaOutliers)
S <- cov(dfTelefonicaOutliers)
dm2 <- mahalanobis(dfTelefonicaOutliers, mu, S)
barplot(dm2, main= "Mahalanobis")

#Valores ordenados por distancia de Mahalanobis

dm2[order(dm2, decreasing = TRUE)]
idx_max <- which.max(dm2) #indice del valor máximo

#Distribución Chi-cuadrado: punto corte

p <- 0.975
dof <- ncol(dfTelefonicaOutliers)
k <- qchisq(p, dof)
idx_outliers <- which(dm2 > k)
idx_outliers

#Grafica de ojiva (probabilidad acumulada)

plot(sort(dm2), ppoints(nrow(dfTelefonicaOutliers)), xlab = "DM al cuadrado ordenada", ylab = "prob acum")
abline(v = qchisq(p, dof), col = "red")

#Grafica cuantil-cuantil (QQ - plot)
x <- qchisq(ppoints(nrow(dfTelefonicaOutliers)), dof) #valores de chi-cuadrado
y <- dm2 #distancia de Mahalanobis
qqplot(x, y, main = expression("Q-Q plot para" ~ {chi^2}), xlab = "Cuantiles", ylab = "Dist Mahalanobis cuadrada")
abline(a = 0, b = 1, col = "red")

#Exclusion de ouliers
dfTelefonicaClean <- dfTelefonicaParteImputado[-idx_outliers, ]
dfTelefonicaClean

library(shiny)
library(tidyverse)

# Leer datos
df <- read.csv("Telefonica.csv",sep = ';',header = TRUE)
datos <- na.omit(df)
head(df, 10)    # Primeros 10 datos


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
               column(4,selectInput("_idmodalidadPago", "Modalidad de Pago", modalidadPago)),
               column(4,dateRangeInput("_daterangeD", "Fecha de Alta y Fecha de Baja:",start = "2021-01-01",end   = "2021-12-31")),
               #column(4,"Polígono de frecuencia",numericInput("ancho", label = "Tamaño intervalo", value = 0.1, step = 0.1),
               column(4,sliderInput("_ageSlider", "Edad:",min = 18, max = 90, value = 40))
             ),
             # Fila 2
             fluidRow(
               #column(12, plotOutput("pie"))
               column(12, plotOutput("chart1"))
               #column(4, plotOutput("pie")),
               #column(3, verbatimTextOutput("ttest"))
             ),
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
  
  #output$chart2 <- renderPlot({
    
    # Gráfico pie chart 2
    #barchart2
    #barChart2
  #}, res = 96)
  
  
  
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