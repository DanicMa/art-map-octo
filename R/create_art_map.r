#' Using Open Streetmaps API obtain information about Salamanca and plot a map
#'

#' @export
#' @import ggplot2 osmdata sf
#' @examples create_art_map()

create_art_map <- function() {
  city <- "Praha"
  country <- "Czechia"
  dpi <- 200
  
  # obtain coordinates for ggplot
  bb <- getbb(paste(city, country, sep=", "))

  # obtain the dataset
  print("Downloading information")
  streets <- bb %>%
    opq() %>%
    add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary")) %>%
    osmdata_sf()

  small_streets <- bb %>%
    opq() %>%
    add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "pedestrian", "footway", "track","path")) %>%
    osmdata_sf()

  water <- 
    opq(bbox = bb) %>%
    add_osm_feature(key = 'water', value = c("river")) %>%
    osmdata_sf()
  
  waterway <- 
    opq(bbox = bb) %>%
    add_osm_feature(key = 'waterway', value = c("river", "riverbank")) %>%
    osmdata_sf()

  # create map
  print("Creating map")
  final_map <- ggplot() +
          #ggtitle(city") +
          geom_sf(data = water$osm_polygons, fill = "blue", colour = NA) +
          geom_sf(data = waterway$osm_polygons, fill = "blue", colour = NA) +
          geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = "white", size = .5, alpha = .8) +
          geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = "grey", size = .3, alpha = .8) +
          coord_sf(xlim = c(min(bb[1,]), max(bb[1,])), ylim = c(min(bb[2,]), max(bb[2,])), expand = FALSE) +
          theme_void() +
          theme(
          axis.ticks = element_blank(),
          plot.background = element_rect(fill="black"),
          plot.margin=unit(c(2.4,2.4,2.4,2.4), "cm")
          )

  print("Saving map")
  # save map
  ggsave(final_map, 
        filename = paste("plots/", gsub(" ", "_", city), ".png", sep = ""),
        scale = 1, 
        width = 36, 
        height = 24, 
        units = "in",
        dpi = dpi)

  print("All Done!!")
}
