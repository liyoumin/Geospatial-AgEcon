## Alfalfa, SPI mapping
##	Created by YOUMIN LI
## Load required libraries
pkgs <- c("sf","tmap","tmaptools","grid","terra","rmapshaper")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
library(ggplot2)
library(sf); library(dplyr)
library(tmap)
library(tmaptools)
library(grid)
#library(raster)
library(terra)
#library(maptools)
library(rmapshaper)
library(ggrepel)
save.image("my_workspace.RData")
load("my_workspace.RData")

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

### average spei
spei_avg <- spei_df %>%
  filter(Year >= 2005, Year <= 2025) %>%
  group_by(State) %>%
  summarize(SPEI_avg = mean(SPEI, na.rm = TRUE)) %>%
  ungroup()

options(tigris_use_cache = TRUE)

# Load CONUS states only
states_sf <- states(cb = TRUE) %>%
  st_as_sf() %>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","AS","VI","MP"))
map_ready <- states_sf %>%
  left_join(spei_avg, by = c("NAME" = "State"))

tmap_mode("plot")
tm_shape(map_ready) +
  tm_polygons(
    col = "SPEI_avg",
    palette = "RdBu",
    style = "cont",
    textNA = "No Data"
  ) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    main.title = "State-level Average SPEI (2005–2025)"
  )

### scatter plot
# Change from 2017 to 2019 for each state
df <- alf_spei %>%
  sf::st_drop_geometry() 

changes_17_19 <- df %>%
  filter(year %in% c(2017, 2019)) %>%
  group_by(STUSPS, year) %>%
  summarise(
    Export_Value = sum(Export_Value, na.rm = TRUE),
    SPEI         = mean(SPEI, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = year,
    values_from = c(Export_Value, SPEI),
    names_glue  = "{.value}_{year}"
  ) %>%
  mutate(
    d_export = Export_Value_2019 - Export_Value_2017,  # Δ Export_Value
    d_spei   = SPEI_2019        - SPEI_2017           # Δ SPEI
  )

## 2. Long-run average SPEI (2005–2025) for color --------------------------------
spei_avg <- df %>%
  filter(year >= 2005, year <= 2025) %>%  # adjust if you want a shorter window
  group_by(STUSPS) %>%
  summarise(
    spei_avg_0525 = mean(SPEI, na.rm = TRUE),
    .groups = "drop"
  )
plot_df <- changes_17_19 %>%
  left_join(spei_avg, by = "STUSPS")
## 3. Scatter plot --------------------------------------------------------------
ggplot(plot_df, aes(x = d_spei, y = d_export)) +
  # quadrant lines: four “planes”
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.5) +
  # y = -x dashed line (net decrease above, net increase below)
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  # points colored by long-run average SPEI
  geom_point(aes(color = spei_avg_0525), size = 3) +
  # state abbreviations
  geom_text_repel(aes(label = STUSPS),
                  size = 3,
                  show.legend = FALSE) +
  # color: negative avg SPEI (dry) = red, positive (wet) = blue
  scale_color_gradient2(
    name = "Avg SPEI\n(2005–2025)",
    low = "red", mid = "white", high = "blue",
    midpoint = 0
  ) +
  labs(
    x = "Change in SPEI (2019 vs 2017)",
    y = "Change in Export Value (2019 vs 2017)",
    title = "Change in Export Value vs SPEI (2017–2019)",
    subtitle = "Color = Average SPEI (2005–2025): red = drier, blue = wetter"
  ) +
  theme_bw()

### export value=========================================
state_geo <- alf_spei %>%
  select(STUSPS, geometry) %>%
  distinct()
avg_export <- alf_spei %>%
  st_drop_geometry() %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(STUSPS) %>%
  summarise(avg_export = mean(Export_Value, na.rm = TRUE))

alf_map <- state_geo %>%
  left_join(avg_export, by = "STUSPS") %>%
  st_as_sf()

tm_shape(alf_map) +
  tm_polygons(
    col = "avg_export",
    palette = "Greens",
    style = "fixed",
    breaks = c(0, 10, 30, 50, 200, 1000, 5000, 10000, Inf),
    labels = c("0 – 10", "10 – 30", "30 – 50", "50 – 200", "200 - 1000", "1000 - 5000", "5000 - 10000", "10000+"),
    colorNA = "white",
    textNA = "No Data",
    title = "Avg Export Value\n(USD, 2005–2025)"
  ) +
  tm_borders(col = "black", lwd = 0.6) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.text.size = 0.8,
    legend.title.size = 1.0,
    frame = FALSE,
    main.title = "Average Export Value by State (2005–2025)",
    main.title.size = 1.5
  )

ggplot(alf_map) +
  geom_sf(aes(fill = avg_export), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "A", 
    name = "Avg Export\n(2005–2025)",
    direction = -1
  ) +
  labs(
    title = "Average Export Value by State (2005–2025)",
    caption = "Source: USDA_GATS"
  ) +
  theme_minimal()

### average acres
avg_acres <- df %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(STUSPS) %>%
  summarise(
    avg_acres = if (all(is.na(ALFALFA_HAYLAGE_ACRES))) NA_real_
    else mean(ALFALFA_HAYLAGE_ACRES, na.rm = TRUE)
  )

map_acres <- states_sf %>%
  left_join(avg_acres, by = "STUSPS")

tmap_mode("plot")
tm_shape(map_acres) +
  tm_polygons("avg_acres",
              palette = "YlGn",
              style = "quantile",
              title = "Avg Acres (2005–2025)") +
  tm_borders(lwd = 0.4) +
  tm_text("STUSPS", size = 0.6) +
  tm_layout(main.title = "Average Acres by State",
            legend.outside = TRUE)

### ave irrigation
avg_irr <- df %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(STUSPS) %>%
  summarise(
    avg_irr = ifelse(
      all(is.na(irr_ton)), 
      NA_real_, 
      mean(irr_ton, na.rm = TRUE)
    )
  )

map_irr <- states_sf %>%
  left_join(avg_irr, by = "STUSPS")

tm_shape(map_irr) +
  tm_polygons("avg_irr",
              palette = "YlGn",
              style = "quantile",
              title = "Avg Irrigation Alfalfa Production (2005–2025)") +
  tm_borders(lwd = 0.4) +
  tm_text("STUSPS", size = 0.6) +
  tm_layout(main.title = "Average Irrigation Alfalfa Production",
            legend.outside = TRUE)

### scatter plot
scatter_df <- df %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(STUSPS) %>%
  summarise(
    avg_prod  = mean(alf_yield_tonacre, na.rm = TRUE),
    avg_irrig = mean(irr_water_acrefeet, na.rm = TRUE),
    avg_spei  = mean(SPEI, na.rm = TRUE)
  )
ggplot(scatter_df, aes(x = avg_prod, y = avg_irrig)) +
  geom_point(aes(color = avg_spei), size = 3) +
  geom_text_repel(aes(label = STUSPS), size = 3) +
  scale_color_gradient2(
    name = "Avg SPEI\n(2005–2025)",
    low = "red", mid = "white", high = "blue",
    midpoint = 0
  ) +
  labs(
    x = "Average Production",
    y = "Average Irrigation",
    title = "Irrigation vs Production (2005–2025)",
    subtitle = "Point color = Average SPEI (Dry = Red, Wet = Blue)"
  ) +
  theme_bw()

##. export rate
avg_exp_rate <- df %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(STUSPS) %>%
  summarise(
    avg_exp_rate  = mean(exp_rate, na.rm = TRUE) * 100
  )

map_rates<- states_sf %>%
  left_join(avg_exp_rate, by = "STUSPS")

tm_shape(map_rates) +
  tm_polygons(
    "avg_exp_rate",
    palette = "Blue",
    style = "cont",
    title = "Avg Export Rate (%)"
  ) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

### value changes
df <- df %>%
  mutate(prod_ton_num = readr::parse_number(as.character(alf_prod_ton)))

prod_change <- df %>%
  filter(year %in% c(2018, 2023)) %>%
  group_by(STUSPS, year) %>%
  summarise(
    prod_val = sum(prod_ton_num, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = prod_val,
    names_glue = "prod_{year}"
  ) %>%
  mutate(
    d_prod = prod_2023 - prod_2018,
    d_prod_pct = (d_prod / prod_2018) * 100
  )

map_change <- states_sf %>%
  left_join(prod_change, by = "STUSPS")

tm_shape(map_change) +
  tm_polygons(
    "d_prod_pct",
    palette = "-RdBu",
    style = "pretty",
    title = "Change in Production Value Percentage (2023 - 2018)"
  ) +
  tm_borders(col = "grey40", lwd = 0.6) +
  tm_text("STUSPS", size = 0.6) +
  tm_layout(
    main.title = "Alfalfa Production Value Change, 2018–2023",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

####=====================  Animated Global Pop. Map  ####----------------------------------------------------------------
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
# https://shiny.posit.co/r/gallery/interactive-visualizations/superzip-example/
