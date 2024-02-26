##########################################################################
# MASTER_WEB_SCRAPING
# Descripción: Realizar un barrido en la web de plataformas inmobiliarias
# Autor: AC ANALYTICS
# Fecha: 21/09/2022
# Modificado por: Arce-Castaño
# Fecha de modificación: 23/09/2022
#####


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(data.table)
library(rvest)
library(xml2)
library(XML)
library(stringr)
library(selectr)
library(jsonlite)
library(tidyverse)
library(tidyr)                  
library(dplyr) 
library(readr)
library(lubridate)
library(openxlsx)
library(plyr)
library(leaflet)






ui = dashboardPage(
  title= 'Web Scraping',
  header = dashboardHeader(
    title = 'Menu'
  ),
  
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Scraping", tabName="Scraping", icon=icon("fas fa-diagnoses")),
      selectInput("pagina","PAGINA WEB",choices=c("MERCADO LIBRE"='MERCADOLIBRE')),
      textInput("URL","URL"),
      actionButton("correr","Reproducir",icon=icon("far fa-play-circle")),
      actionButton("restaurar","Restaurar",icon=icon("fas fa-broom")),
      actionButton("eliminar","Eliminar Fila",icon=icon("fas fa-trash")),
      downloadButton('DESCARGAR_DATA','Descargar')
    )
  ),
  
  body = dashboardBody(
    
    tabItems(
      tabItem(
        tabName="Scraping",
        fluidRow(
          DT::DTOutput("DataBase") %>% withSpinner(color="#0dc5c1"),
          tags$script("$(document).on('click', '#DataBase button', function(){
                        Shiny.setInputValue('lastClickId', this.id);
                        Shiny.setInputValue('lastClick', Math.random()) 
                      });")
        ),
        fluidRow(
          leafletOutput('mapa')
        )
      )
    )
    
  )
)

server = function(input, output, session) {
  
  
  ### MERCADOLIBRE ----
  
  Base <- reactiveValues()
  
  # url <- 'https://listado.mercadolibre.com.co/inmuebles/apartamentos/venta/valle-del-cauca/cali/santa-rita-o-santa-teresa-o-santa-teresita/'
  #
  # LINK = 'https://casa.mercadolibre.com.co/MCO-1392376721-venta-de-casa-campestre-en-el-mameyal-un-oasis-natural-con-diseno-excepcional-_JM#position=1&search_layout=grid&type=item&tracking_id=44ed74fc-abd5-4fb1-a91c-639c8aab5d1a'
  # LINK = 'https://casa.mercadolibre.com.co/MCO-1159966452-venta-casa-en-normandia-_JM#position=3&search_layout=grid&type=item&tracking_id=44ed74fc-abd5-4fb1-a91c-639c8aab5d1a'
  # 
  # UrlPg <- LINK %>% as.character() %>% read_html()
  # UrlPg %>% html_node('img') #%>% html_attr('src') #%>% coorde()
  
  
  
  ## FUNCION
  Scrap.ML <- function(url=NULL, write=FALSE){
    tryCatch({
      if(is.null(url)) return()
      WEBS <- NULL
      url_list <- url
      resultados <- url %>% read_html() %>% html_nodes('span.ui-search-search-result__quantity-results') %>% html_text()
      showNotification(resultados, type = "message")
      tryCatch({
        showNotification(paste('Iniciando Proceso...'), type = "message")
        while(length(url_list)){
          URL <- url_list %>% as.character() %>% read_html()
          url_list <- URL %>% html_nodes(".andes-pagination__button--next a") %>% html_attr("href")
          url <- rbind(url, url_list)
        }
      },error=function(con){
        return()
      },finally={
        url
      })
      
      # funcion sin tilde 
      SinTilde <- function(x){
        data <- toupper(x) %>% str_replace_all("Á","A") %>% str_replace_all("É","E") %>% str_replace_all("Í","I") %>% str_replace_all("Ó","O") %>% str_replace_all("Ú","U") %>% str_replace_all("Ü","U") %>% str_replace_all("Ñ","N") %>% stringr::str_wrap()
        return(data)
      }
      
      # funcion coordenada
      coorde <- function(x){
        if(is.null(x)) return()
        lat_ini <- data.frame(str_locate_all(x, 'center='))$end %>% as.numeric()
        lng_fin <- data.frame(str_locate_all(x, '&zoom='))$start %>% as.numeric()
        coorde <- x %>% str_sub(lat_ini+1, lng_fin-1) %>% str_replace_all('%2C',',')
        return(coorde)
      }
      
      K <- length(url)
      withProgress(message='Preparando paginas...', value = 1/K,{
        for(enlace in url){
          tryCatch({
            url_enlace <- enlace %>% as.character() %>% read_html()
            WEB <- url_enlace %>% html_nodes('a.ui-search-link__title-card') %>% html_attr('href') %>% data.frame()
            WEBS <- rbind(WEBS, WEB)
          },error=function(con){ 
            url_enlace<-NULL;WEB<-NULL
            showNotification(paste('Error:',con), type = "error")
          })
          incProgress(message=paste0('Descargando Data...'), amount=1/K)
        }
      })
      
      BASE <- NULL ; TABLA <- NULL
      PAGINAS <- unique(WEBS[,1])
      N <- length(PAGINAS)
      
      withProgress(message='Descargando Data...', value = 1/N,{
        
        print('Procesando: ')
        for(LINK in PAGINAS){
          tryCatch({
            #
            cat(paste0(paste0(round((which(PAGINAS == LINK)/length(PAGINAS))*100, 1), '%'), sep=" "))
            #
            UrlPg <- LINK %>% as.character() %>% read_html()
            TIPO_NEGOCIO <- UrlPg %>% html_node('span.ui-pdp-subtitle') %>% html_text() %>% str_wrap()
            TITULO <- UrlPg %>% html_node('h1') %>% html_text() %>% str_wrap()
            FECHA_PUBLICACION <- UrlPg %>% html_node('p.ui-pdp-header__bottom-subtitle') %>% html_text() %>% str_remove_all('Publicado hace') %>% str_wrap()
            AREA <- UrlPg %>% html_node('span.ui-pdp-size--SMALL') %>% html_text() %>% str_remove_all('totales') %>% str_wrap()
            VALOR <- UrlPg %>% html_node('span.andes-money-amount__fraction') %>% html_text() %>% str_wrap()
            UBICACION <- UrlPg %>% html_node('.ui-vip-location__subtitle p') %>% html_text() %>% str_wrap()
            COORDENADA <- UrlPg %>% html_node('#ui-vip-location__map > div > img') %>% html_attr('src') %>% coorde()
            DESCRIPCION <- UrlPg %>% html_node('p.ui-pdp-description__content') %>% html_text() %>% str_wrap() %>% SinTilde()
            #
            ROW <- c(LINK, TIPO_NEGOCIO, TITULO, FECHA_PUBLICACION, AREA, VALOR, UBICACION, COORDENADA, DESCRIPCION) %>% t() %>% data.frame()
            BASE <- rbind(BASE, ROW)
            #
            # TABLA (EXTRACCION DE ATRIBUTOS)
            tab <- UrlPg %>% html_node('table')
            if(is.na(tab)) next
            tab <- tab %>% html_table(fill=TRUE, dec=",") %>% data.frame()
            tab$LINK <- LINK
            tab$X1 <- tab$X1 %>% SinTilde() %>% str_replace_all(' ','_')
            TABLA <- rbind(TABLA, tab)
            incProgress(message=paste0('Descargando Data...'), amount=1/N)
            #
          },error=function(con){ 
            showNotification(paste('Error:',con), type="error")
            print(paste('Error:',con))
          },finally={
            LINK<-NULL; UrlPg<-NULL; tab<-NULL; ROW<-NULL
            TIPO_NEGOCIO<-NULL; TITULO<-NULL; FECHA_PUBLICACION<-NULL; AREA<-NULL; VALOR<-NULL; UBICACION<-NULL; COORDENADA<-NULL; DESCRIPCION<-NULL
          })
        }
      })
      #
      # unique(TABLA$X1)
      showNotification(paste('Proceso Finalizado!'), type = "message")
      #
      colnames(BASE) <- c('LINK', 'TIPO_NEGOCIO', 'TITULO', 'FECHA_PUBLICACION', 'AREA', 'VALOR', 'UBICACION', 'COORDENADA', 'DESCRIPCION')
      #
      LISTA_LINK <- unique(TABLA$LINK)
      #
      tryCatch({
        for(i_link in LISTA_LINK){
          dato <- TABLA %>% dplyr::filter(LINK %in% i_link)
          VAR_HEAD <- unique(dato$X1)
          for(var in VAR_HEAD){
            filtro <- dato %>% dplyr::filter(X1 %in% var) %>% unique()
            BASE[BASE$LINK == i_link, var] <- filtro[filtro$LINK == i_link, 'X2']
          }
        }
      },error=function(err){
        print(err)
      })
      #
      DATABASE <- BASE
      #
      # if(write==TRUE) openxlsx::write.xlsx(DATABASE,'DataBase_ML.xlsx', overwrite=TRUE)
      #
      return(data.frame(DATABASE))
      #
    },error=function(con){
      print(con)
      return(list(showNotification(paste('Algo salio mal!'), type="warning"), DATABASE<-NULL))
    })
  }
  
  
  # RUN
  observeEvent(input$correr,{
    tryCatch({ 
      if(input$URL=="") return(showNotification(sprintf('Campo URL requerido!'), type="warning"))
      if(input$pagina=="MERCADOLIBRE"){
        if(input$URL%like%'mercadolibre'!=TRUE) return(showNotification(sprintf('URL incorrecto!'), type="error"))
        #
        Base$Data <- Scrap.ML(as.character(input$URL))
      }
    },error=function(con){
      return(list(showNotification(paste('ERROR!'), type="warning"), Base$Data <- NULL))
    })
  })
  
  
  ### BASE DE DATOS ----
  
  output$DataBase <- DT::renderDataTable({
    if(is.null(Base$Data)) return(NULL)
    Data <- Base$Data
    Data$LINK <-paste0('<a href=',Data$LINK,' target="_blank">Sitio Web</a>')
    Data %>% DT::datatable(
      rownames=FALSE, escape=FALSE, selection='single', extensions=c('Scroller'), 
      options = list(scrollY=400, scrollX=TRUE, dom='tri', deferRender=TRUE, scroller=TRUE
      ))
  })
  
  observeEvent(input$restaurar,{
    shinyjs::reset('URL')
    Base$Data <- NULL
  })
  
  observeEvent(input$eliminar,{
    tryCatch({
      if(is.null(input$DataBase_rows_selected)) return()
      
      row_to_del <- as.numeric(input$DataBase_rows_selected)
      Base$Data <- Base$Data[-row_to_del,]
      
    },error=function(cond){ 
      print(paste('eliminar fila',cond))
    })
  })
  
  output$DESCARGAR_DATA <- downloadHandler(
    filename = paste0(input$pagina,"_Scrap_",Sys.Date(),".xlsx"),
    content = function(file){
      write.xlsx(Base$Data, file)
    })
  
  ### MAPA
  
  observe({
    if(is.null(Base$Data)) return()
    tryCatch({
      data <- Base$Data
      NewCoord <- stringr::str_split_fixed(data$COORDENADA, ",", n=Inf) %>% data.frame()
      colnames(NewCoord) <- c('LATITUD','LONGITUD')
      NewCoord$LINK <- data$LINK
      NewCoord$LATITUD <- NewCoord$LATITUD %>% stringr::str_replace_all(",",".") %>% as.numeric()
      NewCoord$LONGITUD <- NewCoord$LONGITUD %>% stringr::str_replace_all(",",".") %>% as.numeric()
      NewCoord$STREETVIEW <- paste0('<a href=https://www.google.com/maps?layer=c&cbll=',NewCoord$LATITUD,',',NewCoord$LONGITUD,' target="_blank">STREET VIEW</a>')
      NewCoord$TABLA <- paste0(
        '<table class="default">
    ',ifelse(!is.na(NewCoord$LINK),paste0('<tr><th colspan="2" style="text-align:center;"><a href=',NewCoord$LINK,' target="_blank">Ver Anuncio</a></th></tr>'),''),'
    ',ifelse(!is.na(NewCoord$LATITUD),paste0('<tr><th colspan="2" style="text-align:center;"><a href=https://www.google.com/maps?layer=c&cbll=',NewCoord$LATITUD,',',NewCoord$LONGITUD,' target="_blank">Street View</a></th></tr>'),''),'
    </table>')
      
      COOR <- NewCoord
      leafletProxy("mapa") %>%
        setView(lng=median(COOR$LONGITUD),lat=median(COOR$LATITUD), 12) %>%
        addCircleMarkers(lng=COOR$LONGITUD, lat=COOR$LATITUD, popup=COOR$TABLA)
      
    },error=function(con){
      return(showNotification(paste('No hay coordenadas!'), type="warning"))
    })
  })
  
  
  output$mapa <- renderLeaflet({
    if(is.null(Base$Data)) return()
    leaflet() %>% setView(-76.5216547, 3.4359304, zoom = 11) %>% 
      addProviderTiles(providers$OpenStreetMap) 
  })
  
  
  
  
  
  
}

shinyApp(ui, server)
