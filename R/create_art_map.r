#' Using Open Streetmaps API obtain information about Salamanca and plot a map
#'

#' @export
#' @import ggplot2 osmdata sf
#' @examples create_art_map()

create_art_map <- function() {
  library(osmdata)
  library(ggplot2)
  library(sf)
  
  city <- "Rostock"
  country <- "Germany"
  dpi <- 200
  color_primary <- "white"
  color_secondary <- "grey"
  color_river <- "blue"
  color_sea <- color_river
  color_coast <- color_sea
  
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
    add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "pedestrian")) %>%
    #add_osm_feature(key = "highway", value = c("residential", "living_street", "unclassified", "service", "pedestrian", "footway", "track","path")) %>%
    osmdata_sf()

  water <- 
    opq(bbox = bb) %>%
    add_osm_feature(key = 'water', value = c("river")) %>%
    osmdata_sf()
  
  waterway <- 
    opq(bbox = bb) %>%
    add_osm_feature(key = 'waterway', value = c("river", "riverbank")) %>%
    osmdata_sf()

  coastline <- opq(bbox = bb) %>% 
    add_osm_feature(key = 'natural', value = 'coastline') %>% 
    osmdata_sf()

  # create map
  print("Creating map")
  final_map <- ggplot() +
          #ggtitle(city") +
          geom_sf(data = water$osm_polygons, fill = color_river, colour = NA) +
          geom_sf(data = waterway$osm_polygons, fill = color_river, colour = NA) +
          geom_sf(data = streets$osm_lines, inherit.aes = FALSE, color = color_primary, size = .5, alpha = .8) +
          geom_sf(data = small_streets$osm_lines, inherit.aes = FALSE, color = color_secondary, size = .3, alpha = .8) +
          geom_sf(data = coastline$osm_lines, inherit.aes = FALSE, color = color_coast, size = .2, alpha = .8) +
          geom_sf(data = coastline$osm_polygons, fill = color_coast, colour = NA) +
          coord_sf(xlim = c(min(bb[1,]), max(bb[1,])), ylim = c(min(bb[2,]), max(bb[2,])), expand = FALSE) +
          theme_void() +
          theme(
          axis.ticks = element_blank(),
          #plot.margin=unit(c(0,2.4,2.4,2.4), "cm"),
          plot.background = element_rect(fill="black")
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
