# packages
library(shiny)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(classInt)
library(pals)
library(leafgl)
library(shinyWidgets)
library(terra)
library(raster)
library(mapview)
library(leaflet)
library(leafem)
library(collapsibleTree)
library(shinycssloaders)
library(bs4Dash)
library(fresh)
library(ggthemes)
library(shinyjs)
library(ggiraph)
library(markdown)
library(htmlwidgets)
library(htmltools) 
library(waiter)
library(tictoc)
library(starsTileServer)
library(stars)
library(callr)

source("show-hide.R")

Sys.setlocale("LC_ALL", "es_ES.UTF8")

# data global
load("./data/legend_data.RData")
ts <- as_date(as_datetime(time(rast("./data/horas_sol_esp.tiff"))))

    # anom rel
    anomrel <- read_stars("./data/horas_sol_esp_anomrel.tiff", proxy = T)
    names(anomrel) <- "sun"
    anomrel <- st_set_dimensions(anomrel, 3, ts, names = "time")
    
    
    colFun_anomrel <- function(x, alpha = 0.8) {
      
      col_reds <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7')
      col_reds <- colorRampPalette(col_reds)
      
      col_bues <- colorRampPalette(c('#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
      
      col3 <- c(rev(col_bues(7)), rev(col_reds(9)))
      
      col3 <- paste0(col3, as.character(as.raw(as.numeric(alpha)*255)))
      
      cutp <- c(0.17,0.5, 0.70, 0.80, 0.90, 0.95, 0.97,
                1.03, 1.05, 1.08, 1.10, 1.15, 1.20, 1.30, 1.40, 1.5, 3.3)-1
      
      tmp <- cut(x, cutp*100)
      
      return(as.character(factor(tmp, levels(tmp), col3)))
      
    }
    
    
    rp_anomrel <- r_bg(args = list(grid = anomrel, colFun = colFun_anomrel), function(grid, colFun) {
      starsTileServer::starsTileServer$new(grid, colFun)$run(port = 4001, docs = TRUE)
    })


    # observado
    
    obs <- read_stars("./data/horas_sol_esp.tiff", proxy = T)
    names(obs) <- "sun"
    obs <- st_set_dimensions(obs, 3, ts, names = "time")
    
    colFun_obs <- function(x, alpha = 0.8) {
      
      col <- plasma(15)
      col <- paste0(col, as.character(as.raw(as.numeric(alpha)*255)))
      at <- as.numeric(leg$lab)
      at[15] <- 450
      at <- c(0, at)
      
      tmp <- cut(x, at)
      
      return(as.character(factor(tmp, levels(tmp), col)))
      
    }
    
    rp_obs <- r_bg(args = list(grid = obs, colFun = colFun_obs), function(grid, colFun) {
      starsTileServer::starsTileServer$new(grid, colFun)$run(port = 4002, docs = TRUE)
    })

  
  # anomalia abs

    anom <- read_stars("./data/horas_sol_esp_anom.tiff", proxy = T)
    names(anom) <- "sun"
    anmom <- st_set_dimensions(anom, 3, ts, names = "time")
    
    colFun_anom <- function(x, alpha = 0.8) {
      
      
      col_reds <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7')
      col_reds <- colorRampPalette(col_reds)
      
      col_bues <- colorRampPalette(c('#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
      
      col3 <- c(rev(col_bues(7)), rev(col_reds(6)))
      col3 <- paste0(col3, as.character(as.raw(as.numeric(alpha)*255)))
      
      cutp <- c(-170, -75, -40, -25, -15, -10, -5, 5, 10, 15, 25, 40, 75, 170)
      
      tmp <- cut(x, cutp)
      
      return(as.character(factor(tmp, levels(tmp), col3)))
      
      
    }
    
    
    rp_anom <- r_bg(args = list(grid = anom, colFun = colFun_anom), function(grid, colFun) {
      starsTileServer::starsTileServer$new(grid, colFun)$run(port = 4003, docs = TRUE)
    })





# ui 
## theme colors
fic_theme <- create_theme(
  bs4dash_sidebar_light(
    bg = "#01AAC9",
    color = "white"),
  bs4dash_status(primary = "#252525"),
  bs4dash_vars(
    navbar_light_color = "black",
    navbar_light_active_color = "#bec5cb",
    navbar_light_hover_color = "#bec5cb"
  )
)

# body 
bodyTag <-   dashboardBody(
  use_theme(fic_theme),
  useShinyjs(),
  tags$head(
    tags$style(
      "
      .sidebar-toggle{ position: absolute;left: 0; }
        #map {
          height: calc(100vh - 57px) !important;
        
        }

        .card:nth-of-type(1) {
          margin-top: 30px;
          position: absolute;
          z-index: 1;
          opacity: 0.9;
          width: 280px;
          
        }

        #burger, #burger:hover {
          font-size: medium;
          background-color: transparent;
          border-color: transparent;
          margin-left: 8px;
          margin-top: 10px;         
        }

        #burger:focus {
          outline: 0;
        }
        
        
        
        "
    )
  ),
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
    includeScript("gomap.js"),
  ),
  tabItems(
    tabItem("map_home",
            fluidRow(
              addSpinner(leafletOutput("map"), spin = "circle", color = "#01AAC9"),
              
              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                            width = 400, height = "auto", cursor = "default",
                            sliderInput("slidertime", "Fecha", value = max(ts), 
                                        min = min(ts),
                                        max = max(ts),
                                        timeFormat="%b %Y",
                                        width = 400
                            ),
                                    selectInput("data_type", "", 
                                        c("Observada", "Anomalía absoluta", "Anomalía relativa")),
                            actionButton("hide", "",
                                         icon = icon("chart-simple"),
                                         width = '45px',
                                         style = "padding:0px;"),
                            girafeOutput("ggi", height = "100%", width = "100%")
                            
                            
              )),
    ),
    tabItem("info",
            fluidRow(
              
              includeMarkdown("info.md")
              
            )
    )
  )
)

# leaflet 100%
bodyTag$children[[1]]$attribs$style <- "padding: 0px 0px !important" 

# footer sidebar
footer_sidebar <- HTML(paste0(
  "<br><br><br><br><br><br><br><br><br>",
  "<table style='margin-left:auto; margin-right:auto;'>",
  "<tr>",
  "<td style='padding: 5px;'><a href='mailto:info@ficlima.org ' target='_blank'><i class='fa-solid fa-envelope'></i></a></td>",
  "<td style='padding: 5px;'><a href='https://twitter.com/FIClima' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
  "<td style='padding: 5px;'><a href='https://www.instagram.com/ficlima1992/' target='_blank'><i class='fab fa-instagram fa-lg'></i></a></td>",
  "</tr>",
  "</table>",
  "<br>",
  "<center><small><p>Copyright &copy; 2023 <a href='https://www.ficlima.org/'>FIClima</a><br>Todos los derechos reservados.</p></small></center>"))

#final ui
ui <- dashboardPage(
  
  header = dashboardHeader(div("EXPLORADOR DE LA INSOLACIÓN"), 
                           div(a(target="_blank", href = 'https://www.ficlima.org/',
                                 img(src = 'logo_fic_org.png', title = "FIClima", height = "40px")),
                               style = "padding-top:5px; padding-bottom:5px;position: relative; display: inline-block;float:right;padding-left:10px;"
                           )),
  sidebar = dashboardSidebar(skin = "light", collapsed = T,
                             expandOnHover = F,   minified = FALSE,
                             sidebarMenu(
                               id = "sidebarMenu",
                               menuItem("Home", tabName = "map_home", icon = icon("home"), selected=TRUE),
                               menuItem("Info", tabName = "info", icon = icon("thumbtack"))
                             ),
                             footer_sidebar),
  body = bodyTag,
  dark = NULL,
  freshTheme = TRUE
)

######

# server side

server <- function(input, output, session) {
  
 
  
  url <- reactive({

    port <- switch(input$data_type, "Observada"=2, "Anomalía absoluta"=3, "Anomalía relativa"=1)
    sprintf(
      "http://127.0.0.1:400%d/map/sun/{z}/{x}/{y}?&time=%s&alpha=0.8",
      port,
      strftime(round_date(ymd(input$slidertime), "month"), tz = "UTC", format = "%Y-%m-%d")
    )
  }) %>% debounce(100)
  
  # color palette by data input 
  colpal  <- reactive({
    
    if(input$data_type == "Anomalía relativa"){
      
      col_reds <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7')
      col_reds <- colorRampPalette(col_reds)
      
      col_bues <- colorRampPalette(c('#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
      
      col3 <- c(rev(col_bues(7)), rev(col_reds(9)))
      
      cutp <- c(0.17,0.5, 0.70, 0.80, 0.90, 0.95, 0.97,
                1.03, 1.05, 1.08, 1.10, 1.15, 1.20, 1.30, 1.40, 1.5, 3.3)-1
      
      lab_temp <- scales::number(cutp[-1]*100, suffix = "%")
      lab_temp[c(1, 16)] <- c("< -50%", "> 50%")

      tmp <- list(lab =  lab_temp, col = col3, cut = cutp * 100, colcut = col3,
                  title = "Anomalía", layer = "Anomalía")
      return(tmp)
    }
    
    if(input$data_type == "Observada"){
      col <- plasma(15)
      at <- as.numeric(leg$lab)
      at[15] <- 450
      at <- c(0, at)
      lab_temp <- scales::number(parse_number(rev(leg$lab)), suffix = "h")
      lab_temp[c(1, 15)] <- c("> 361h", "< 117h")
      tmp <- list(lab =   lab_temp, col = rev(col), colcut = col, title = "Horas de sol", cut = at)
      return(tmp)
    }
    
    if(input$data_type == "Anomalía absoluta"){
      
      col_reds <- c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7')
      col_reds <- colorRampPalette(col_reds)
      
      col_bues <- colorRampPalette(c('#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
      
      col3 <- c(rev(col_bues(7)), rev(col_reds(6)))
      
      cutp <- c(-170, -75, -40, -25, -15, -10, -5, 5, 10, 15, 25, 40, 75, 170)
      
      lab_temp <- scales::number(cutp[-1], suffix = "h")
      lab_temp[c(1, 13)] <- c("< -75h", "> 75h")

      tmp <- list(lab =  lab_temp, col = col3, cut = cutp, colcut = col3,
                  title = "Anomalía")
      return(tmp)
    }
    
    
  })
  
  
  # data filter 
  
  toggle("ggi") # plot area hidden
  
  # chart event  
  observeEvent(input$hide, {
    toggle("ggi", anim = T, animType = "slide")
    if(is.null(input$map_click)) info("¡Elige un punto en el mapa!")
  })
  
  # mapa base leaflet 
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      addProviderTiles(provider = providers$CartoDB.Positron, group = "Carto Positron") %>%
      setView(lng = -3, lat = 39, zoom = 6) %>%
      garnishMap(addScaleBar, addMouseCoordinates,
                 position = "bottomleft") %>%
      addLayersControl(overlayGroups = "Valor",
                       position = "bottomright") 
    
  })
  
  
  # plotly graph
  observe({
    
    
    output$ggi <- renderGirafe({
      req(input$map_click)
      
      p <- st_point(c(input$map_click[[2]], input$map_click[[1]])) %>% 
        st_sfc(crs = 4328) %>% st_sf() 
      
      r <- rast("./data/horas_sol_esp.tiff") 
      norm <- rast("./data/horas_sol_normal.tiff")
      
      normal_df <- tibble(mo = month(as_date(as_datetime(time(norm)))), sunhr_normal = round(unlist(t(extract(norm, p))[-1,])))
      
      df <- tibble(date = as_date(as_datetime((time(r)))), suhr_obs = round(unlist(t(extract(r, p))[-1,])))
      
      df <- complete(df, date = seq(ymd("1983-01-01"), make_date(year(today()), 12, 1), "month"))
      
      df <- mutate(df, mo = month(date), yr = year(date)) %>% left_join(normal_df)
      lonlat <- str_c(round(input$map_click[[2]], 4),"º Lon\n", round(input$map_click[[1]], 4), "º Lat")
      
      g <- filter(df, yr == year(ymd(input$slidertime))) %>%
        ggplot(aes(date, suhr_obs)) + 
        geom_col_interactive(aes(y= sunhr_normal, tooltip = str_c(str_to_upper(month(date, label = T, abbr = F)),"\n", "Normal 1983-2010: ", sunhr_normal),
                                 fill = "promedio"), 
                             width = 20,
                             hover_nearest = TRUE) +
        geom_col_interactive(width = 5, alpha = .8,
                             aes(tooltip = str_c(str_to_upper(month(date, label = T, abbr = F)),"\n", "Observado: ", suhr_obs),
                                 fill = "observado",
                                 width = 10),
                             hover_nearest = TRUE) +
        scale_x_date(date_labels = "%b", date_breaks = "month") +
        scale_fill_manual(breaks=c('observado', 'promedio'),
                          values=c('observado'="#fd8d3c", "promedio"="#525252")) +
        labs(x = NULL, y = "Horas de sol (h)", subtitle = lonlat,
             title = year(ymd(input$slidertime)),
             caption = "© FIClima", fill = NULL) +
        theme_economist(base_family = "Lato") +
        theme(axis.title.y = element_text(margin = margin(r = 5), size = 18),
              plot.title = element_text(size = 30),
              plot.subtitle = element_text(hjust = .25, vjust = 11.3),
              axis.text = element_text(size = 14),
              plot.caption = element_text(margin = margin(t = 10, b = 5),
                                          hjust = 1),
              legend.text = element_text(size = 10),
              legend.position = "bottom",
              legend.justification = 1)
      
      tooltip_css <- "font-size: 10px;color: white;"
      
      girafe(ggobj  = g, pointsize = 12,
             options = list(
               opts_tooltip(opacity = .8, use_fill = TRUE, zindex = 1500,
                            css = tooltip_css)
             ))
      
    })
  })
  
  observe({
    if(!is.null(input$map_click) & input$hide==1) {
      
      p <- st_point(c(input$map_click[[2]], input$map_click[[1]])) %>% 
        st_sfc(crs = 4326) %>% st_sf()
      
      leafletProxy("map", session) %>%
        clearMarkers() %>%
        addMarkers(data = p,
                   icon = list(
                     iconUrl = 'https://raw.githubusercontent.com/FortAwesome/Font-Awesome/6.x/svgs/solid/bullseye.svg',
                     iconSize = c(15, 15)
                   )) 
    }
  })
  
  
  observeEvent(input$hide != 0, {
    
    leafletProxy("map", session) %>%
      clearMarkers() 
    
  })
  
  # leaflet raster 
  observe({

    dy <- which(ts == round_date(ymd(input$slidertime), "month"))

    leafletProxy("map", session) %>%
      clearImages() %>%
      clearControls() %>%
      clearGroup("sun") %>%
      addTiles(url(),
               group = "sun",
               options = tileOptions(useCache = TRUE, crossOrigin = TRUE)
      ) %>%
      #addImageQuery(brick(data_sel()) %>% subset(dy), 
       #             type="mousemove", 
        #            layerId = "Valor", 
         #           project = T,
          #          digits = 0,
          #          prefix = "",
           #         position = "bottomright") %>%
    addLegend(colors = colpal()$col, labels = colpal()$lab,
              title = "",  labFormat = labelFormat(digits = 0),
              position = "bottomright") 


    
    
  })
  
  
}

shinyApp(ui, server)
