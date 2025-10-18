#### US dairy
##map of cow inventory heads each stat in 2025
cow <- read.csv("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/USDA-NASS-cattle_cows_milk-inventory_head-2025.csv")
cow_clean <- cow %>%
  mutate(
    State = str_to_lower(str_trim(state_name)),
    Value = as.numeric(gsub(",", "", Value))
  ) %>%
  filter(!is.na(Value))   # drop non-numeric rows

USstate_cow <- USstate_con %>%
  left_join(cow_clean, by = "State")

tmap_mode("plot")
tm_shape(USstate_cow) +
  tm_polygons(
    fill = "Value",
    fill.scale = tm_scale_intervals(values = "YlGn", style = "quantile"),
    fill.legend = tm_legend(title = "Value"),
    fill.free = FALSE
  ) +
  tm_shape(usa) +
  tm_borders(col = "black", lwd = 0.4) +
  tm_title("U.S. cow heads in each state") +
  tm_layout(legend.outside = TRUE)

##map of alfalfa each stat
alf_acre <- read.csv("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/state_alfafa.csv")

# matching # Join
alf_clean <- alf_acre %>%
  filter(Year %in% c(2018, 2023)) %>%
  mutate(
    State = str_to_lower(str_trim(State)),
    Value = str_replace_all(Value, ",", ""),         # remove commas
    Value = ifelse(Value %in% c("(D)", "(Z)", "(H)", "(L)", "(X)", "NA", ""), NA, Value),
    Value = as.numeric(Value)
  ) %>%
  filter(!is.na(Value))   # drop non-numeric rows
alf_acres <- alf_clean %>%
  filter(str_detect(Data.Item, "ACRES HARVESTED")) %>%
  mutate(
    Domain.Category = str_trim(Domain.Category),
    Domain.Category = ifelse(Domain.Category == "", "NOT SPECIFIED", Domain.Category)
  )

alf_acres_sum <- alf_acres %>%
  group_by(State, Year, Domain.Category) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

ggplot(alf_acres_sum, aes(x = reorder(State, Value), y = Value, fill = Domain.Category)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Year, ncol = 1) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Alfalfa Acres Harvested by Domain Category and State",
    y = "Acres Harvested",
    x = "State",
    fill = "Domain Category"
  ) +
  theme_minimal(base_size = 12)

USstate_alf <- USstate_con %>%
  left_join(alf_acres_sum, by = "State")

USstate_alf <- USstate_alf %>%
  mutate(
    facet_group = paste0(Year, " | ", Domain.Category)
  )

tmap_mode("plot")
tm_shape(USstate_alf) +
  tm_polygons(
    fill = "Value",
    fill.scale = tm_scale_intervals(values = "YlGn", style = "quantile"),
    fill.legend = tm_legend(title = "Value"),
    fill.free = FALSE
  ) +
  tm_facets(by = "facet_group") +
  tm_shape(usa) +
  tm_borders(col = "black", lwd = 0.4) +
  tm_title("U.S. Alfalfa Acres Harvested by Domain Category and Year") +
  tm_layout(legend.outside = TRUE)

USstate_alf <- USstate_alf %>%
  group_by(State, Domain.Category, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

alf_change <- USstate_alf %>%
  select(State, Domain.Category, Year, Value) %>%
  pivot_wider(names_from = Year, values_from = Value) %>%
  mutate(
    Change = `2023` - `2018`,
    PctChange = 100 * (`2023` - `2018`) / `2018`
  )

alf_change %>%
  group_by(Domain.Category) %>%
  summarise(
    mean_change = mean(PctChange, na.rm = TRUE),
    median_change = median(PctChange, na.rm = TRUE),
    states_covered = sum(!is.na(PctChange))
  )

tm_shape(alf_change) +
  tm_polygons(
    fill = "PctChange",
    fill.scale = tm_scale_continuous(values = "-RdYlGn", midpoint = 0),
    fill.legend = tm_legend(title = "% Change (2023−2018)")
  ) +
  tm_shape(USstate_con) +
  ##tm_facets(by = "Domain.Category") +
  tm_borders(col = "black") +
  tm_title("Alfalfa Irrigation Area Changes in U.S. (2018–2023)") +
  tm_layout(legend.outside = TRUE)

alf_change %>%
  group_by(Domain.Category) %>%
  top_n(5, wt = PctChange) %>%
  arrange(Domain.Category, desc(PctChange))

ggplot(alf_change, aes(x = reorder(State, Change), y = Change, fill = Domain.Category)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Change in Irrigated Alfalfa Area by State (2018–2023)",
    x = "State",
    y = "Change in Acres",
    fill = "Domain Category"
  ) +
  theme_minimal(base_size = 13)
ggplot(alf_change, aes(x = reorder(State, Change), y = Change, fill = Domain.Category)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Domain.Category, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Change in Alfalfa Area by Domain Category, 2018–2023",
    x = "State",
    y = "Change (Acres)"
  ) +
  theme_bw()

tm_shape(alf_change) +
  tm_polygons("PctChange", 
              palette = "RdYlGn",
              style = "quantile",
              title = "% Change 2018–2023") +
  tm_layout(title = "Irrigated Alfalfa Area Change by State (2018–2023)")

alf_map_irrig <- alf_change %>% filter(Domain.Category == "NOT SPECIFIED")
tm_shape(alf_map_irrig) +
  tm_polygons("PctChange", palette = "-RdYlGn", title = "% Change (2023–2018)") +
  tm_borders(col = "black") +
  tm_layout(title = "Alfalfa Proudtion Value Change (2018–2023)", legend.outside = TRUE)
