library(tidyverse)
library(shiny)
library(plotly)
library(bslib)
library(thematic)
library(extrafont)
library(ggtext)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)
library(extrafont)
library(ggtext)
library(reshape2)
library(wordcloud)
library(RColorBrewer)
library(listviewer)
library(shinydashboard)
library(htmltools)
library(shinyWidgets)
library(ggwordcloud)
library(stringr)
library(DT)
library(bsicons)




# Abrimos el archivo de noticias

noticias <- read.csv("Noticias 30 Junio 2024 compilado.csv", 
                     stringsAsFactors = FALSE, 
                     encoding = 'latin1',
                     sep=";") %>%  tbl_df()

# Fecha en formato Date

noticias <- noticias %>% mutate(fecha=as.Date(fecha, "%Y-%m-%d"))

noticias$fecha1 <- strftime(noticias$fecha, "%d-%m-%Y")

noticias <- noticias %>% filter(diario!="" & texto!="")

noticias <- noticias %>% filter(diario=="Ultima Hora" | diario=="ABC Color" |
                                diario=="La Nación" | diario=="5 días")

noticias <- noticias %>% filter(fecha>="2023-02-01" & fecha<="2024-06-30")

# Pasamos a minúscula todo el texto

noticias$texto <- tolower(noticias$texto)


# Creamos la variable marca


noticias <- noticias %>% mutate(marca=0) %>% 
  mutate(marca=as.factor(marca))

noticias$titulo <- ifelse(noticias$enlace=="sin enlace", noticias$titulo, paste0("<a href=\"", noticias$enlace, "\">", noticias$titulo, "</a>"))


# Descarga del léxico en formato csv y abrirlo

afinn <- read.csv("lexico_afinn.en.es revisado.csv", sep=";",
                  stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()



# Combinación entre la base de datos y Afinn

noticias_afinn <- 
  noticias %>%
  mutate(texto1=texto) %>% 
  unnest_tokens(input = "texto", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa"))




# Eliminar stop words

noticias_afinn <-  noticias_afinn %>% filter(Palabra!="no", Palabra!="sí",
                                             Palabra!="gracias", Palabra!="tipo",
                                             Palabra!="serio", Palabra!="sol",
                                             Palabra!="llegar", Palabra!="sentido",
                                             Palabra!="solo", Palabra!="mayor",
                                             Palabra!="pasado", Palabra!="superior",
                                             Palabra!="apoyo", Palabra!="acuerdo",
                                             Palabra!="capacidad", Palabra!="tiempo",
                                             Palabra!="puesto")

noticias_afinn <- noticias_afinn %>% 
  mutate(eliminar=ifelse(sum(str_count(noticias_afinn$texto1, "honor colorado"))>=1 & noticias_afinn$Palabra=="honor", 1, 0)) %>% 
  mutate(eliminar=ifelse(sum(str_count(noticias_afinn$texto1, "fondo para la excelencia"))>=1 & noticias_afinn$Palabra=="excelencia", 1, eliminar)) %>% 
  mutate(eliminar=ifelse(sum(str_count(noticias_afinn$texto1, "esperanza martínez"))>=1 & noticias_afinn$Palabra=="esperanza", 1, eliminar)) %>% 
  filter(eliminar!=1)





# Extracción de fechas


noticias_min_date = as.Date(min(noticias$fecha))
noticias_current_date = as.Date(max(noticias$fecha))


# Temas

# Creamos variales por temas

trans <- c('transformación educativa', 'transformacion educativa',
           'Transformación educativa', 'Proyecto de transformación educativa')


trans <- str_c(trans, collapse="|")

ue <- c('unión europea')
ue <- str_c(ue, collapse="|")

convenio <- c('convenio')
convenio <- str_c(convenio, collapse = "|")

fonacide <- c('fonacide')
fonacide <- str_c(fonacide, collapse="|")

corrupcion <- c("corrupción", "denuncia", "robo")
corrupcion <- str_c(corrupcion, collapse="|")

docente <- c("gremio", "gremios", "salario docente", "remuneración docente", "formación docente")
docente <- str_c(docente, collapse = "|")

infraestructura <- c("infraestructura", "derrumbe", "colapso",
                     "mueble", "techo", "baño", "letrina")

infraestructura <- str_c(infraestructura, collapse = "|")

aprendizaje <- c("aprendizaje", "Pruebas estandarizadas")
aprendizaje <- str_c(aprendizaje, collapse = "|")


violencia <- c("Abuso", "abuso", "violencia", "asesinato", "gresca", "golpear")
violencia<- str_c(violencia, collapse = "|")


analfabetismo <- c("analfabetismo", "analfabeto" )
analfabetismo <- str_c(analfabetismo, collapse = "|")

becas <- c("becas", "beca")
becas <- str_c(becas, collapse = "|")


financiamiento <- c("financiamiento", "presupuesto")
financiamiento <- str_c(financiamiento, collapse = "|")

# Calculamos la cantidad de veces que las palabras son utilizadas en cada noticia

noticias <- noticias %>% 
  mutate(fonacide=str_count(texto, fonacide),
         trans=str_count(texto, trans),
         ue=str_count(texto, ue),
         convenio=str_count(texto, convenio),
         corrupcion=str_count(texto, corrupcion),
         docente=str_count(texto, docente),
         infraestructura=str_count(texto, infraestructura),
         aprendizaje=str_count(texto, aprendizaje),
         violencia=str_count(texto, violencia),
         analfabetismo=str_count(texto, analfabetismo),
         becas=str_count(texto, becas),
         financiamiento=str_count(texto, financiamiento))

# Marcamos con una variable dicotómica si la palabra es mencionada en cada noticia

noticias <- noticias %>% 
  mutate(fonacide=ifelse(fonacide>=1, 1, 0),
         trans=ifelse(trans>=1, 1, 0),
         trans=ifelse(ue>=1 & convenio>=1, 1, trans),
         ue=ifelse(ue>=1, 1, 0),
         corrupcion=ifelse(corrupcion>=1, 1, 0),
         docente=ifelse(docente>=1, 1, 0),
         infraestructura=ifelse(infraestructura>=1, 1, 0),
         aprendizaje=ifelse(aprendizaje>=1, 1, 0),
         violencia=ifelse(violencia>=1, 1, 0),
         analfabetismo=ifelse(analfabetismo>=1, 1, 0),
         becas=ifelse(becas>=1, 1, 0),
         financiamiento=ifelse(financiamiento>=1, 1, 0))

# Creamos una base del ranking de noticias por mes

temas <- noticias %>% 
  mutate(time_floor = lubridate::floor_date(fecha, 'month')) %>% 
  group_by(time_floor, diario) %>% 
  summarise(fonacide=sum(fonacide),
            trans=sum(trans),
            ue=sum(ue),
            corrupcion=sum(corrupcion),
            docente=sum(docente),
            infraestructura=sum(infraestructura),
            aprendizaje=sum(aprendizaje),
            violencia=sum(violencia),
            analfabetismo=sum(analfabetismo),
            becas=sum(becas),
            financiamiento=sum(financiamiento)) %>%
  select(time_floor, diario, fonacide, trans, corrupcion, docente, infraestructura, aprendizaje, violencia, analfabetismo, becas, financiamiento) %>%
  pivot_longer(cols = c(fonacide, trans, corrupcion, docente, infraestructura, aprendizaje, violencia, analfabetismo, becas, financiamiento),
               names_to="temas",
               values_to = "cantidad") %>% 
  filter(time_floor>="2022-09-01") 

levels(temas$temas)[levels(temas$temas)=="Trans"] <- "Transformación" 
levels(temas$temas)[levels(temas$temas)=="Corrupcion"] <- "Corrupción" 



##------------------------ SHINY ---------------------------------------

theme <- bs_theme(version = 5,
                  bootswatch = "minty",
                  bg = "#FDFEFE", fg = "black", primary = "#3BBBE2",
                  secondary="#3BBBE2",
                  base_font = font_google("Montserrat", local=TRUE),
                  code_font = font_google("Montserrat", local=TRUE)
)



ui <- fluidPage(
  theme = theme,
      fluidRow(
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #A6A6A6}")),
        column(5, searchInput("clave",
                              "Buscá acá una palabra o frase clave:",
                              placeholder = "Escribe aquí", 
                              btnSearch = icon("search"), 
                              btnReset = icon("remove"),
                              width = "100%")), 
        
        column(3, pickerInput("diario", "Seleccioná los diarios:",   
                                                         choices = (unique(noticias$diario)), 
                                                         options = list(`actions-box` = TRUE, `none-selected-text` = "¡Realice una selección!"),
                                                         multiple = TRUE,
                                                         selected = c("Ultima Hora", "ABC Color", "La Nación", "5 días"))),
        
        
        column(4, dateRangeInput("Fecha",
                              "Seleccioná un rango de fecha:", width = "100%",
                              start = as.Date(noticias_min_date,"%d-%m-%Y"),
                              end = as.Date(noticias_current_date,"%d-%m-%Y"),
                              min = "2022-09-01",
                              max=as.Date(noticias_current_date,"%d-%m-%Y"),
                              language = "es", separator = "-",
                              format = "dd-mm-yyyy"))
        
      ),
      mainPanel(width = 12,
        tabsetPanel(
          tabPanel("General",
                   fluidRow(
                     column(width=3, 
                            value_box(height=125,
                                      title = "Cantidad de noticias",
                            value = textOutput("conteo"),
                            showcase = bs_icon("newspaper")),
                            value_box(height=125,
                                      title = "Porcentaje del total",
                                      value = textOutput("porcentaje"),
                                      showcase = bs_icon("percent")),),
                     column(width=9, card(full_screen=TRUE,
                                          max_height=300,
                                          card_body(
                                            
                                          plotlyOutput("mmm", width = "100%")),
                                          card_body(max_height=40,
                                                    fill = FALSE,
                                                    p(class = "fs-10", "Al escribir una palabra clave en
                         el buscador se marcara la cantidad de artículos en los que se mencionan dicha palabra en cada diario.
                           "))
                                          )),
                     column(
                       width = 12, box(width=12, solidHeader = TRUE,
                       DT::dataTableOutput("table")
                     ))
            )),
          tabPanel("Frecuencia",
                   fluidRow(
                     column(width=12, card(
                       full_screen = TRUE,
                       card_body(max_height = 400, 
                                 plotlyOutput("p", width = "100%")),
                       card_body(
                         fill = FALSE,
                         p(class = "text-muted", "En este gráfico interactivo, cada barra representa la cantidad 
                           de artículos por día referentes a educación publicados en medios de prensa impresos 
                           monitoreados. Al escribir una palabra clave en el buscador, se identificará la cantidad 
                           de noticias que contienen dicha referencia en barras de color distinta. Los espacios en 
                           blanco corresponden a días en los que no se identifican publicaciones relacionadas a 
                           educación en los medios seleccionados."
                       )))),
                     column(width = 4, card(
                       full_screen = TRUE,
                       card_body(
                         fill = FALSE,
                         max_height = 350,
                         card_title("Frecuencia por día y mes"),
                         p(class = "text-muted", "En esta sección se puede
                           observar dos gráficas, la primera nos muestra la cantidad
                           de artículos referentes a educación por día, y la segunda gráfica 
                           por mes.
                           A través de los controles de arriba se puede filtrar los diarios que se
                           quieren escoger,
                           cambiar el periodo de tiempo,
                           o escribir una palabra o frase clave,
                           al hacer esto se marcaran la cantidad de artículos en los que aparecen dicha palabra en color azul.
                           En el caso del gráfico por día, los espacios en blanco
                  corresponden a días en los que no hubo publicaciones en los diarios seleccionados.")))),
                     column(width=8, card(
                       full_screen = TRUE,
                       card_body(
                         max_height = 300,
                       plotlyOutput("b", width = "100%")),
                       card_body(
                         fill = FALSE,
                         p(class = "text-muted", "En este gráfico interactivo cada barra reprensenta
                         la cantidad de artículos educativos por mes. Al escribir una palabra clave en
                         el buscador se marcara la cantidad de artículos en los que se mencionan dicha palabra en cada mes.
                           ")))))),
          tabPanel("Sentimientos", fluidRow(
            column(width=6, card(
              full_screen = TRUE,
              card_body(
                max_height = 300, plotlyOutput("m", width = "100%")),
              card_body(
                fill = FALSE,
                p(class = "text-muted", "En este gráfico interactivo cada barra representa
                la proporción  palabras según su enfoque positivo y negativo."
                )))),
            column(width=6, card(
              full_screen = TRUE,
              card_body(
                max_height = 300,
                plotlyOutput("q", width = "100%")),
              card_body(
                fill = FALSE,
                p(class = "text-muted", "En este gráfico interactivo cada barra representa
                la proporción  palabras según su enfoque positivo y negativo por diario."
                )))),
            column(width=12,
                   card(
                     full_screen = TRUE,
                     card_body(
                       max_height = 400,
                       plotlyOutput("u", width = "100%")),
                     card_body(
                       fill = FALSE,
                       p(class = "text-muted", "En este gráfico interactivo cada barra representa
                la cantidad de veces que cada una de las palabras se repiten. Aparecen las 10 palabras
                    positivas más repetidas, y las 10 palabras negativas más repetidas para cada selección."
                       ))))
          )),
    tabPanel("Sobre el monitoreo",
             column(width = 12, card(
               full_screen = TRUE,
               card_body(
                 fill = FALSE,
                 card_title("Fundamentación"),
                 p(class = "text-muted", tags$div(class="header", checked=NA,
                                                  tags$p("El abordaje de la educación en los medios es 
                                                         de suma importancia, ya que puede tener efectos en la representación que 
                                                         la sociedad desarrolla sobre sí misma y sobre su sistema educativo, impulsarla 
                                                         hacia procesos constructivos, destructivos, o el statu quo.
                                                         Teniendo en cuenta que la ciudadanía en general utiliza como primera fuente de información 
                                                         a los medios de comunicación, el valor de los mismos en la construcción de la opinión pública 
                                                         sobre educación, no es menor."),
                                                  tags$b("El monitoreo noticias de educación"),
                                                  tags$p("Monitoreo educación en medios propone analizar la frecuencia, 
                                                         periodicidad de temas, enfoque de noticias sobre educación, con el fin de generar información 
                                                         sobre el tratamiento informativo de noticias educativas en medios de prensa escrita y promover 
                                                         debates y reflexiones sobre la incidencia de éstas en la construcción opinión pública de los actores."),
                                                  tags$b("Objetivo General"),
                                                  tags$p("Monitorear el tratamiento informativo sobre noticias educativas para generar debate sobre la contribución 
                                                         de los medios como promotores de conocimiento, de formación de la opinión pública, fortalecer la capacidad de 
                                                         demanda de una educación de mayor calidad.")
                 )
                   )),
               card_body(
                 fill = FALSE,
                 card_title("Metodología"),
                 p(class = "text-muted", tags$div(class="header", checked=NA,
                                                  tags$p("El monitoreo de noticias de educación utiliza herramientas de Minería de texto para desarrollar análisis de 
                                                  frecuencia de noticias educativas y análisis de sentimientos. La Minería de texto es el proceso de transformar textos 
                                                  no estructurados a textos estructurados de manera a encontrar patrones e información de relevancia. En específico, se 
                                                  trabaja con el diccionario afinn, que fue elaborado por Finn Årup Nielsen entre 2009 y 2011. 
                                                  Este diccionario contiene casi 2500 palabras, que son valoradas manualmente con un número entero entre -5 (negativo) y +5 
                                                  (positivo). Mediante esta escala es posible realizar análisis de sentimientos, que consiste en clasificar las palabras 
                                                  agrupando las positivas y negativas para luego contar la cantidad de cada una de estas y poder obtener la proporción de palabras positivas y negativas en cada noticia.
                                                         Para poder llegar a este análisis, se pasan por varias etapas:."),
                                                  tags$b("Primera etapa: Carga de artículos"),
                                                  tags$p("Se elabora una matriz de carga de noticias de educación en donde se establecen criterios y campos a cargar utilizando diarios impresos. 
                                                         En este monitoreo se trabajan con los diarios ABC Color, Última Hora, La Nación y 5 Días/El independiente. Como mínimo, la matriz de carga 
                                                         debe contener una columna que identifique la fecha de la noticia, otra que identifique el diario en el que se encontró la noticia, otra 
                                                         que recoja el título de la noticia, una columna que contenga el cuerpo de la noticia (es de este campo de donde se extrae el texto para el 
                                                         análisis de sentimiento) y otra con el enlace a la noticia en digital."),
                                                  tags$b("Segunda etapa: Limpieza de matriz"),
                                                  tags$p("En esta etapa, se eliminan las ‘stop words’ o palabras que aparecen con frecuencia en una lengua pero que, por lo general, no tienen mucho 
                                                         significado ni contribuyen al contexto de una frase o documento. Algunos ejemplos son el, la, los, las, un, una, unos, unas, y, o, de, del, al, 
                                                         con, por, para, que, si, no, es, son, fue, ha, en, a, ante, desde, hasta, sobre, entre.También se pasa todo el texto a minúsculas para evitar que 
                                                         una misma palabra que comienza con mayúsculas se contabilice como una palabra diferente a la misma palabra en minúsculas."),
                                                  tags$b("Tercera etapa: Minería de texto"),
                                                  tags$p("Utilizando el lenguaje de programación R y los paquetes estadísticos tidyverse, tidytext, shiny, y otros se procesa la matriz de carga.")
                 )
                 ))
               ))),
    
    )))
      


server <- function(input, output, session) {
  
  
    
  datos = reactive({
    a=subset(noticias, diario %in% input$diario)
    return(a)
    
  })
  
  sentimientos = reactive({
    b=subset(noticias_afinn, diario %in% input$diario)
    return(b)
    
  })
  
  
  temas = reactive({
    c=subset(temas, diario %in% input$diario)
    return(c)
  })
  
  
  output$p <- renderPlotly({
    req(datos())
    
    Sys.setlocale("LC_TIME","Spanish")
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    
    p <- datos() %>% 
      mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2]) %>%
      group_by(fecha, fecha1, marca) %>% 
      summarize(conteo=n()) %>% 
      mutate(fecha_label = format(fecha, format = "%d-%Y", locale = "es_ES")) %>%
      ggplot(aes(fecha, conteo, fill=marca, text = paste('<br>Fecha: ', fecha1,
                                                 '<br>Cantidad: ', conteo)))+
      geom_col()+
      scale_x_date(labels = date_format("%m-%Y"), date_breaks="1 month",
                   expand = c(0, 0))+
      labs(title="Cantidad de noticias educativas por día",
           subtitle = "<b><span style='color:#3BBBE2;font-size:15pt'>Noticias educativas</span></b>",
           x=NULL, y="Cantidad")+
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c("#3BBBE2", "#002147"))+
      scale_colour_manual(values = c("#FFFFFF", "#FFFFFF"))+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title.position = 'plot',
            plot.title = element_markdown(family="Montserrat", size=20),
            plot.subtitle = element_markdown(family="Montserrat", size=15, hjust = 0.5),
            legend.position = "none",
            legend.text=element_text(family="Montserrat", size=9),
            axis.text.y = element_text(family="Montserrat", size=10, colour="black"),
            axis.text.x = element_text(family="Montserrat", size=12, face="bold", colour="black"),
            axis.title.y = element_text(family="Montserrat", size=11, colour="black"),
            axis.title.x = element_text(family="Montserrat", size=7, colour="black"),
            plot.margin = margin(10, 10, 0, 10))
    
    
    
    ggplotly(p, tooltip = c("text"))
    
  })
  
  
  output$b <- renderPlotly({
    req(datos())
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    
    b <- datos() %>% 
      mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2]) %>%
      group_by(mes = lubridate::floor_date(fecha, 'month'), marca) %>% 
      summarize(conteo=n()) %>% 
      ggplot(aes(mes, conteo, fill=marca, text = paste(
        '<br>Cantidad: ', conteo)))+
      geom_col()+
      labs(title="Cantidad de noticias educativas por mes",
           x=NULL, y="Cantidad")+
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_date(labels = date_format("%m-%Y"), date_breaks="1 month",
                   expand = c(0, 0))+
      theme_minimal()+
      scale_fill_manual(values = c("#3BBBE2", "#002147"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title.position = 'plot',
            plot.title = element_markdown(family="Montserrat", size=10),
            plot.subtitle = element_markdown(family="Montserrat", size=15, hjust = 0.5),
            legend.position = "none",
            legend.text=element_text(family="Montserrat", size=9),
            axis.text.y = element_text(family="Montserrat", size=10, colour="black"),
            axis.text.x = element_text(family="Montserrat", size=5, colour="black"),
            axis.title.y = element_text(family="Montserrat", size=11, colour="black"),
            axis.title.x = element_text(family="Montserrat", size=7, colour="black"),
            plot.margin = margin(10, 10, 0, 10))
    
    
    
    ggplotly(b, tooltip = c("text"))
    
  })
  
  
  output$mmm <- renderPlotly({
    req(datos())
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    
  mmm <- datos() %>% 
      mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2]) %>%
      group_by(diario, marca) %>% 
      summarize(conteo=n()) %>% 
      ggplot(aes(reorder(diario, FUN=mean, conteo), conteo, fill=marca, text = paste(
        '<br>Cantidad: ', conteo)))+
      geom_col()+
      labs(title="Cantidad de noticias educativas por diario",
           x=NULL, y="Cantidad")+
      scale_y_continuous(expand = c(0, 0)) +
      theme_minimal()+
      scale_fill_manual(values = c("#3BBBE2", "#002147"))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            plot.title.position = 'plot',
            plot.title = element_markdown(family="Montserrat", size=10),
            plot.subtitle = element_markdown(family="Montserrat", size=15, hjust = 0.5),
            legend.position = "none",
            legend.text=element_text(family="Montserrat", size=9),
            axis.text.y = element_text(family="Montserrat", size=10, colour="black"),
            axis.text.x = element_text(family="Montserrat", size=9, face="bold", colour="black"),
            axis.title.y = element_text(family="Montserrat", size=11, colour="black"),
            axis.title.x = element_text(family="Montserrat", size=7, colour="black"),
            plot.margin = margin(10, 10, 0, 10))
    
    
    
    ggplotly(mmm, tooltip = c("text"))
    
  })
  
  
  
  
  
  
  output$m <- renderPlotly({
    req(datos())
    
    Sys.setlocale("LC_TIME","Spanish")
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
  m <- sentimientos() %>% 
    mutate(marca=str_count(texto1, tolower(input$clave))) %>% 
    mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
    filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
             marca==1) %>%
    group_by(Tipo) %>%
    summarize(conteo=n()) %>%
    mutate(Proporcion = conteo / sum(conteo)) %>%
    ggplot() +
    aes(Tipo, Proporcion, fill = forcats::fct_rev(Tipo), text = paste('<br>Proporción: ', paste0(round(Proporcion*100, 0), "%"))) +
    geom_col() +
    labs(title="Enfoque <span style='color:#3BBBE2;font-size:20pt'>positivo</span> vs <span style='color:#EE4655;font-size:20pt'>negativo</span>",
         y="Proporción",
         x=NULL)+
    scale_fill_discrete(name="Enfoque")+
    scale_y_continuous(labels = percent_format(), expand = c(0,0)) +
    theme_minimal()+
    theme(legend.position = "none",
          panel.grid = element_blank(),
          plot.title.position = 'plot',
          plot.margin = margin(10, 10, 0, 10),
          plot.title = element_text(family="Montserrat", size=20),
          plot.subtitle = element_text(family="Montserrat", size=10),
          axis.text.x = element_text(family="Montserrat", size=12, face="bold"),
          axis.text.y = element_text(family="Montserrat", size=10),
          axis.title.x = element_text(family="Montserrat"),
          axis.title.y = element_text(family="Montserrat"))+
    scale_fill_manual(values = c("#3BBBE2", "#EE4655"))
    
    
    ggplotly(m, tooltip=c("text"))
    
  })

  
  output$q <- renderPlotly({
    req(datos())
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    q <- sentimientos() %>% 
      mutate(marca=str_count(texto1, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>%
      group_by(diario) %>%
      count(Tipo) %>%
      mutate(Proporcion = n / sum(n)) %>%
      ggplot() +
      aes(diario, Proporcion, fill = forcats::fct_rev(Tipo), text = paste(
                                                                         '<br>Proporción: ', paste0(round(Proporcion*100, 0), "%"))) +
      geom_col() +
      labs(title="Enfoque <span style='color:#3BBBE2;font-size:20pt'>positivo</span> vs <span style='color:#EE4655;font-size:20pt'>negativo</span> por diario",
           y="Proporción",
           x=NULL)+
      scale_fill_discrete(name="Enfoque")+
      scale_y_continuous(labels = percent_format(), expand = c(0,0)) +
      theme_minimal()+
      theme(legend.position = "none",
            panel.grid = element_blank(),
            plot.title.position = 'plot',
            plot.margin = margin(10, 10, 0, 10),
            plot.title = element_text(family="Montserrat", size=20),
            axis.text.x = element_text(family="Montserrat", size=12, face="bold"),
            axis.text.y = element_text(family="Montserrat", size=10),
            axis.title.x = element_text(family="Montserrat"),
            axis.title.y = element_text(family="Montserrat"))+
      scale_fill_manual(values = c("#3BBBE2", "#EE4655"))
    
    
    ggplotly(q, tooltip=c("text"))
    
  })
  
  
  
  output$u <- renderPlotly({
    req(datos())
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    u <- sentimientos() %>% 
      mutate(marca=str_count(texto1, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>% 
      group_by(Tipo, Palabra) %>%
      summarize(conteo=n()) %>% 
      arrange(desc(conteo)) %>% 
      slice_head(n=10) %>%
      ungroup() %>% 
      mutate(Palabra=reorder_within(Palabra, conteo, Tipo)) %>% 
      ggplot() + 
      geom_col(aes(Palabra, conteo, fill=Tipo, text = paste(
                                                            '<br>Cantidad: ', conteo))) +
      labs(title="Top 10 de palabras <span style='color:#3BBBE2;font-size:20pt'>positivas</span> y <span style='color:#EE4655;font-size:20pt'>negativas</span>",
           x=NULL,
           y=NULL)+
      theme_minimal()+
      theme(legend.position = "none",
            panel.grid = element_blank(),
            plot.title.position = 'plot',
            plot.margin = margin(0, 0, 0, 0),
            plot.title = element_markdown(family="Montserrat", size=20),
            plot.subtitle = element_text(family="Montserrat", size=10),
            axis.text.x = element_text(family="Montserrat", size=14, face="bold"),
            axis.text.y = element_text(family="Montserrat", size=10),
            axis.title.x = element_text(family="Montserrat"),
            axis.title.y = element_text(family="Montserrat"))+
      scale_size_area(max_size = 15)+
      scale_fill_manual(values = c("#EE4655", "#3BBBE2"))+
      coord_flip()+
      scale_x_reordered() +
      scale_y_continuous(expand = c(0, 0))

    
    
    ggplotly(u, tooltip=c("text"))
    
  })
  
  output$table <- DT::renderDataTable({
    
    
    basesita <- datos() %>% mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
                                                                   marca==1) %>% 
      mutate(Fecha=fecha) %>% 
      mutate(Diario=diario) %>% 
      mutate(Título=titulo) %>% 
      select(Fecha, Diario, Título)
    
    datatable(basesita, escape = FALSE,
              caption = htmltools::tags$caption(
                style = 'caption-side: top; text-align: left;', ('Lista de noticias seleccionadas')
              ),
                  options = list(paging = TRUE, searching = FALSE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
                    
                  ),
                  rownames = FALSE, selection = "single"
    )
    
  })
  
  observeEvent(input$table_rows_selected, {
    
    basesita <- datos() %>% mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>% 
      select(fecha, diario, enlace)
    
  })
  
  output$conteo <- renderText({
    {baseconteo <- datos() %>% mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>% summarize(conteo=n()) 
    conteo <- as.numeric(baseconteo$conteo)
    conteo}
    
  })

  
  output$porcentaje <- renderText({
    {baseconteo <- datos() %>% mutate(marca=str_count(texto, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>% summarize(conteo=n()) 
    conteo <- as.numeric(baseconteo$conteo)
    totalidad <- datos() %>% summarize(suma=n())
    todo <- as.numeric(totalidad$suma)
    porcentaje <- conteo/todo
    percent(porcentaje)}
    
  })
  

  output$dos <- renderPlotly({
    req(datos())
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    
    dos <- sentimientos() %>% 
      mutate(marca=str_count(texto1, tolower(input$clave))) %>% 
      mutate(marca=as.factor(ifelse(marca>=1, 1, 0))) %>% 
      filter(titulo!="no hay noticia" & fecha>=input$Fecha[1] & fecha<=input$Fecha[2] &
               marca==1) %>% 
      group_by(Tipo) %>%
      summarize(conteo=n()) %>% 
      ungroup() %>% 
      ggplot() + 
      geom_bar(aes(Tipo, fill=Tipo, text = paste(
        '<br>Cantidad: ', conteo))) +
      labs(title="Cantidad de noticias <span style='color:#3BBBE2;font-size:20pt'>positivas</span> y <span style='color:#EE4655;font-size:20pt'>negativas</span>",
           x=NULL,
           y=NULL)+
      theme_minimal()+
      theme(legend.position = "none",
            panel.grid = element_blank(),
            plot.title.position = 'plot',
            plot.margin = margin(0, 0, 0, 0),
            plot.title = element_markdown(family="Montserrat", size=20),
            plot.subtitle = element_text(family="Montserrat", size=10),
            axis.text.x = element_text(family="Montserrat", size=14, face="bold"),
            axis.text.y = element_text(family="Montserrat", size=10),
            axis.title.x = element_text(family="Montserrat"),
            axis.title.y = element_text(family="Montserrat"))+
      scale_fill_manual(values = c("#EE4655", "#3BBBE2"))+
      scale_y_continuous(expand = c(0, 0))
    
    
    
    ggplotly(dos, tooltip=c("text"))
    
  })
  
  
  output$temas_mes <- renderPlotly({
    req(temas())
    
    Sys.setlocale("LC_TIME","Spanish")
    
    if (is.null(input$clave)){
      return(NULL)      
    }
    
    
    temas_mes <- temas %>%
      group_by(time_floor) %>% 
      arrange(time_floor, temas, cantidad) %>% 
      group_by(time_floor) %>% 
      mutate(rank = (row_number(cantidad))) %>% 
      ungroup() %>% 
      mutate(temas=as.factor(str_to_title(temas))) %>% 
      mutate(rank2=rank(-rank)) %>% 
      ungroup() %>% 
      filter(rank>5) %>% 
      mutate(mes=format(time_floor, "%b-%Y")) %>% 
      ggplot(aes(time_floor, rank2, fill=temas))+
      geom_raster()+
      geom_text(aes(label=temas), color="white", size=4, family="Montserrat", fontface="bold")+
      labs(x=NULL,
           title="Top 5 de temas tratados en las noticias educativas")+
      scale_x_date(labels = date_format("%b-%Y"), date_breaks="1 month",
                   expand = c(0, 0), position = "top")+
      scale_y_reverse(breaks = seq(1, 10, by=1))+
      scale_fill_manual(values=c("#2B455E", "#8F3287", "#37B7C1", "#EC6E41", "#346899", "#FFD45E", "#3BBBE2", "#EE4655", "#A6A6A6", "#37B7C1", "#EC6E41"))+
      theme_bw()+
      facet_grid(~factor(mes, levels=c("sep.-2022", "oct.-2022", "nov.-2022", "dic.-2022", "ene.-2023", "feb.-2023", "mar.-2023", "abr.-2023", "may.-2023", "jun.-2023", "jul.-2023", "ago.-2023")),  scales = "free")+
      theme(panel.grid = element_blank(),
            plot.title = element_text(family="Montserrat", size=35),
            axis.title.y = element_blank(),
            axis.text.y=element_text(family="Montserrat", size=40, face="bold"),
            axis.text.x= element_blank(),
            legend.position = "none",
            panel.margin.x=unit(0,"lines"),
            strip.text.x = element_text(family = "Montserrat", size=20, face="bold"),
            strip.background = element_rect(colour="black",
                                            fill="white"))
    
    ggplotly(temas_mes) 
    
    
  })
  
}



thematic::thematic_shiny() 
shinyApp(ui, server)