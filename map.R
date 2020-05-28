# Map of Trafford councillors #

# Source: Trafford Council
# URL: https://democratic.trafford.gov.uk/mgMemberIndex.aspx
# Licence: OGL v3.0

library(tidyverse)
library(sf) 
library(leaflet)
library(leaflet.extras) 
library(htmlwidgets) 
library(htmltools)

# read list of Trafford's councillors
df <- read_csv("councillors.csv")

# create HTML popup
councillors <- df %>% 
  mutate(popup = paste0("
  <figure>
  <img src='", Image, "' alt='", Name, "'>
  <figcaption><a href='", Page, "' target='_blank'>", Name, "</a><br />", Party, "<br /> 
  <a href='mailto:", Email, "'>", Email,"</a><br />", Telephone, "</figcaption>
  </figure>")) %>%
  select(Ward, popup) %>% 
  group_by(Ward) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(names_from = id, values_from = popup) %>% 
  mutate(`3` = replace_na(`3`, "")) %>% 
  unite(popup, 2:4, sep = "", remove = TRUE) %>% 
  mutate(popup = paste0("<h3 style='text-align: center;'>", Ward, "</h3>", popup))

# retrieve ward codes from ONS Open Geography Portal
lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv") %>% 
  filter(LAD19NM == "Trafford") %>% 
  pull(WD19CD)

# retrieve ward vector boundaries
wards <- st_read(paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD_DEC_2019_UK_BGC/FeatureServer/0/query?where=", 
                        URLencode(paste0("wd19cd IN (", paste(shQuote(lookup), collapse = ", "), ")")), 
                        "&outFields=*&outSR=4326&f=geojson")) %>% 
  select(area_code = WD19CD, area_name = WD19NM, lon = LONG, lat = LAT)

# join councillor information to ward boundaries
sf <- left_join(wards, councillors, by = c("area_name" = "Ward")) 

# build map
map <- leaflet(height = "100%", width = "100%") %>% 
  setView(-2.35533522781156, 53.419025498197, zoom = 12) %>% 
  addTiles(urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png", 
           attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors &copy; <a href="https://carto.com/attributions">CARTO</a> | <a href="https://www.ons.gov.uk/methodology/geography/licences">Contains OS data © Crown copyright and database right (2020)</a> | Source: <a href="https://democratic.trafford.gov.uk/mgMemberIndex.aspx?FN=WARD&VW=TABLE&PIC=1" target="_blank">Trafford Council</a>',
           options = tileOptions(minZoom = 12, maxZoom = 17)) %>%
  addPolygons(data = sf, fillColor = "#CCCCCC", weight = 0.8, opacity = 1, color = "#212121",
              popup = ~popup,
              highlight = highlightOptions(color = "#046DC3", weight = 3, bringToFront = TRUE)) %>% 
  addLabelOnlyMarkers(data = sf, lng = ~lon, lat = ~lat, label = ~as.character(area_name), 
                      labelOptions = labelOptions(noHide = T, textOnly = T, direction = "left",
                        style = list("color" = "#212121",
                                     "font-size" = "14px",
                                     "text-shadow" = "-1px -1px #FFFFFF, 1px -1px #FFFFFF, -1px 1px #FFFFFF, 1px 1px #FFFFFF"))) %>%
  addFullscreenControl() %>% 
  addEasyButton(
    easyButton(
      position = "topleft",
      icon = "fa-crosshairs",
      title = "Locate Me",
      onClick = JS(c("function(btn,  map){map.locate({setView:true,enableHighAccuracy: true })}"))
    )) %>% 
  addControl(paste0("<h1>Trafford councillors by ward</h1>"), position = 'topright',  className = "map-title") %>% 
  onRender(paste0("function(el, x) {$('head').append(","\'<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\'",");}"))

# add CSS
browsable(
  tagList(list(
    tags$head(
      tags$style(
        "@import url('https://fonts.googleapis.com/css?family=Open+Sans%7CRoboto');
        html, body {height: 100%; margin: 0; font-family: 'Open Sans', sans-serif;}
        .leaflet-popup {position: absolute; text-align: center;}
        .leaflet-popup-content {margin-top: 5px;
                                margin-left: 0px;
                                min-width: 100 px !important;
                                max-height: 300px;
                                overflow: auto;}
        .leaflet-control.map-title {background-color: transparent;
                                    color: #000000;
                                    font-size: 22px;}
        .map-title h1 {color: #707070; font-family: 'Roboto', sans-serif; font-size: 1em; padding: 0; margin: 0; text-shadow: -1px -1px #FFFFFF, 1px -1px #FFFFFF, -1px 1px #FFFFFF, 1px 1px #FFFFFF}"
      )
    )
  ),
  map
  )
)

