#### state SPI
###SPI_state
spi <- rast("/Users/macpro/Desktop/Youmin-phd/geospatical/Project/data/NCLIMGRID_DAILY-spi-12mo.tif")
spi_state <- read.csv("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/SPI.csv", row.names = NULL)
spi_county <- read.csv("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/SPI-county.csv", row.names = NULL) 
### reset date
library(lubridate)
spi_state <- spi_state %>%
  mutate(
    # remove the "d_" prefix
    DATE_clean = sub("d_", "", DATE),
    # parse as date
    DATE_clean = ymd(DATE_clean),
    # format to mm/dd/yyyy
    DATE_formatted = format(DATE_clean, "%Y/%m")
  )

spi_county <- spi_county %>%
  mutate(
    # remove the "d_" prefix
    DATE_clean = sub("d_", "", DATE),
    # parse as date
    DATE_clean = ymd(DATE_clean),
    # format to mm/dd/yyyy
    DATE_formatted = format(DATE_clean, "%Y/%m")
  )

# matching # Join
spi_state <- spi_state %>%
  mutate(
    State = str_to_lower(str_trim(X0)),           # clean state names
    Date = parse_date_time(DATE_formatted, 
                           orders = c("Y-m-d", "Y/m/d", "Ym", "Y/m", "Y-m")), 
    Year = year(Date)
  )

spi_d3_yearly <- spi_state %>%
  group_by(State, Year) %>%
  summarise(
    D3_months = sum(D3, na.rm = TRUE),         # how many D3 months per year
    D3_share  = mean(D3, na.rm = TRUE) * 100   # % of months in D3
  ) %>%
  ungroup()

map_spi2018 <- USstate_con %>%
  left_join(filter(spi_d3_yearly, Year == 2023), by = "State")

#### mapping
tmap_mode("plot")
tm_shape(map_spi2018) +
  tm_polygons(
    fill = "D3_months",
    palette = "-RdYlBu",
    title = "Extreme Drought (D3) Index"
  ) +
  tm_borders(col = "gray40") +
  tm_layout(
    title = "State-level SPI D3 – 2023",
    legend.outside = TRUE,
    frame = FALSE
  )


spi_d3_yearly <- spi_d3_yearly %>%
  mutate(Year = as.factor(Year))



