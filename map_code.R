europe <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(continent == "Europe")

resistance_data <- data.frame(
  country = c("Italy", "Slovenia", "Poland", "Germany", "France", "Portugal"),
  resistance_rate = c(3.2, 2.9, 1.3, 0.8, 0.6, 0.4)
)
europe <- europe %>%
  left_join(resistance_data, by = c("admin" = "country"))

ggplot(data = europe) +
  geom_sf(aes(fill = resistance_rate)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", 
                      name = "Resistance Rate (%)") +
  labs(title = "Resistance Rates in European Countries",
       subtitle = "Resistance rates (%) for different countries") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ggplot(data = europe) +
  geom_sf(aes(fill = resistance_rate)) +
  geom_sf_text(aes(label = resistance_rate), color = "black", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white", 
                      name = "Resistance Rate (%)") +
  labs(title = "Resistance Rates in European Countries",
       subtitle = "Resistance rates (%) for different countries") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

tm_shape(europe) +
  tm_fill("resistance_rate", 
          title = "Resistance Rates (%)",
          palette = "Blues", 
          colorNA = "grey90", 
          textNA = "No Data") +
  tm_borders() +
  tm_text("resistance_rate", size = 0.5) +
  tm_layout(title = "Resistance Rates in European Countries",
            legend.position = c("right", "bottom"))


world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Bhutan
bhutan <- world[world$admin == "Bhutan", ]

ggplot(data = bhutan) +
  geom_sf(fill = "lightgreen", color = "darkgreen") +
  labs(title = "Map of Bhutan") +
  theme_minimal() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        panel.grid = element_blank())
