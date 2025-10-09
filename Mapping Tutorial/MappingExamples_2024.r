## Cool R Mapping Examples
##		
##	Created by David Keellings
##  and adapted from Geomcomputation with R by Lovelace, Nowosad, & Muenchow 2023

####  AL Study Area Map  ####

## Load required libraries
pkgs <- c("sf","tmap","tmaptools","grid","terra","rmapshaper")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
library(ggplot2)
library(sf)
library(tmap)
library(tmaptools)
library(grid)
#library(raster)
library(terra)
#library(maptools)
library(rmapshaper)

## Read in Regions Shapefile
climregs <- st_read("/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Data/seclimatedivisions/Study area clim divs dissolve.shp")
st_transform(climregs, 4326)
## Select climate regions in AL from shapefile attributes
alregs <- climregs[climregs$ST_ABBRV=="AL",]

## Read in shapefile of US
usa <- st_read("/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Data/cb_2016_us_state_20m/cb_2016_us_state_20m.shp")
st_transform(usa, 4326)

## Read in raster DEM of AL
alDEM <- rast("/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Data/DEM_SE_US/AL_dem_projected.tif")
## Reproject AL DEM to match shapefiles 
#st_transform(alDEM, 4326)
## Write out re-projected AL DEM
#writeRaster(alDEM, overwrite=TRUE, filename="/Volumes/KINGSTON/R Class/Example Data/DEM_SE_US/Al_dem_projected.tif")

## Subset all states that are not called Hawaii, Alaska, PR
US_cont <- usa %>% subset(!NAME %in% c("Alaska","Hawaii","Puerto Rico")) %>% ms_simplify() 
## Create new attribute for if state name equals AL, red, otherwise, white
US_cont$isAL <- ifelse(US_cont$NAME=="Alabama", "red", "white")

## Create a tm package map object
m_al <- tm_shape(alDEM) + tm_raster("AL_dem_projected.tif", palette="BuGn", style="fixed", breaks=c(0,50,100,200,400,Inf), interval.closure="left", title="Elevation (m)") + 
		tm_shape(alregs, is.master=TRUE) + tm_polygons("Identifier", title = "", showNA = TRUE, alpha=0, border.col = "black", border.alpha = 1, legend.show=FALSE) +
		tm_layout(frame=TRUE, inner.margins = c(0.08, 0.035, 0.01, 0.07), legend.show=TRUE, legend.position=c(0.79, 0.85), legend.title.size=1) +
		tm_text("Identifier", shadow=TRUE, bg.color="white") + tm_grid(projection=4326, labels.size = .6, lwd=0, labels.inside.frame = FALSE)

## Create US CONUS map object with AL in red
m_usa <- tm_shape(US_cont) + tm_borders(lwd=1, col = "black", alpha = .5) + tm_fill("isAL")

## Create a view port object (inset mapping area)
vp_usa <- viewport(x = 0.65, y = 0.15, width = 0.5, height = 0.5)

## Open png graphics device
png(file = "/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Output/StudyMap_2024.png", width = 3200, height = 4500, units = 'px', res = 600)
par(bg="white")

## Switch on mapping mode
tmap_mode("plot")
## Call map object
m_al
## Print US map in viewport
print(m_usa, vp = vp_usa)

dev.off()

####  Animated Global Pop. Map  ####

## Load required libraries
install.packages(c("spData", "gifski"))
library(sf)
library(dplyr)
library(spData)
library(tmap)
library(gifski)

## Filter out the Antarctic
world2 <- filter(world, continent != "Antarctica")

## Create a tm package map object
urb_anim <- tm_shape(world2) + tm_polygons() + 
  tm_shape(urban_agglomerations, title = "Population (m)") + 
  tm_symbols(size = "population_millions", col = "red", alpha = 0.5, title.col = "Population (m)", title.size = "Population (m)") + 
  tm_layout() + tm_facets(by = "year", nrow = 1, ncol = 1, free.coords = FALSE)

## Create animation of tm object, 
tmap_animation(urb_anim, filename = "/Volumes/KINGSTON UF/KINGSTON COPY/R Class/Mapping Tutorial/Example Output/urb_anim.gif", delay = 25)


#### Let's try some interactive mapping ####
## First set tm mode to "view" for interactive maps
tmap_mode("view")
## Join world country polygons with world coffee production data
world_coffee = left_join(world, coffee_data, by = "name_long")
## Create facets for displaying alongside each other
facets = c("coffee_production_2016", "coffee_production_2017")
## Create tm object with faceted maps
tm_shape(world_coffee) + tm_polygons(facets) + 
  tm_facets(nrow = 1, sync = TRUE)
## Turn off view mode and return to regular map plotting mode
tmap_mode("plot")

## Load mapview package - a quick and simple way to create interactive maps
install.packages("mapview")

library(mapview)

## Quick example
mapview(nz)

## More involved map with multiple layers
trails |>
  st_transform(st_crs(franconia)) |>
  st_intersection(franconia) |>
  st_collection_extract("LINESTRING") |>
  mapview(color = "red", lwd = 3, layer.name = "trails") +
  mapview(franconia, zcol = "district") +
  breweries

## Another way to map interactively
library(leaflet)

pal = colorNumeric("RdYlBu", domain = cycle_hire$nbikes)

leaflet(data = cycle_hire) |> 
  addProviderTiles(providers$CartoDB.Positron) |>
  addCircles(col = ~pal(nbikes), opacity = 0.9) |> 
  addPolygons(data = lnd, fill = FALSE) |> 
  addLegend(pal = pal, values = ~nbikes) |> 
  setView(lng = -0.1, 51.5, zoom = 12) |> 
  addMiniMap()

## Combine leaflet mapping with shiny for interactive web applications

library(shiny)
## You may run into issue with fastmap (dependency of shiny) fix is below:
#options(repos = c(
#  +     PPM = "https://packagemanager.rstudio.com/all/latest", 
#  +     CRAN = "https://cloud.r-project.org"
#  + ))
#install.packages("fastmap")

library(leaflet)
library(spData)

## Create user interface - widgets including sliders
ui = fluidPage(
  sliderInput(inputId = "life", "Life expectancy", 49, 84, value = 80),
  leafletOutput(outputId = "map")
)
## Server is a function that takes the ui input and generates an output map
server = function(input, output) {
  output$map = renderLeaflet({
    leaflet() |> 
      addMapPane("base", zIndex=400) |>
      addMapPane("life", zIndex=420) |>
      addPolygons(data = world[world$lifeExp < input$life, ], options = pathOptions(pane = "life"), opacity = 0.2, weight = 1) |>
        addTiles(options = pathOptions(pane = "base"))    
    })
}
## Shiny puts the ui and server together so that every time the user provides input
## the R code runs in the background to create a new output map
shinyApp(ui, server)

## Complex Super Zip Example
https://shiny.posit.co/r/gallery/interactive-visualizations/superzip-example/
