# LOAD MAGRITTR -----------------------------------------------------------

library(magrittr)



# PULL FONT FROM GOOGLE FONTS ---------------------------------------------

sysfonts::font_add_google(name = "Ubuntu", family = "ubuntu")
showtext::showtext_auto()



# PULL DATA FROM NPD AND SAVE ---------------------------------------------

# format today's date for including in filenames
current_day_compact <-
  lubridate::today() %>%
  stringr::str_remove_all(pattern = "-")

# format today's date for plot text
current_day_slashes <-
  lubridate::today() %>%
  format(format = "%d/%m/%Y")

# pull file from web
download.file(url = "https://factpages.npd.no/downloads/shape/fldArea.zip",
              destfile = paste0("field_shapefile_", current_day_compact, ".zip"))



# UINZIP AND READ IN SHAPEFILE --------------------------------------------

# unzip folder
unzip(zipfile = paste0("field_shapefile_", current_day_compact, ".zip"),
      exdir = paste0("field_shapefile_", current_day_compact))

# find shapefile in unzipped folder
shp_path <-
  list.files(path = paste0("field_shapefile_", current_day_compact),
             pattern = ".shp$",
             full.names = TRUE)

# read in shapefile
df_shp <-
  sf::st_read(shp_path)



# DO SOME CLEANING --------------------------------------------------------

# nice up field names
df_shp <-
  df_shp %>%
  dplyr::mutate(fieldName = stringr::str_to_title(fieldName))

# transform coordinate system into an equal area projection for the area
df_shp <-
  df_shp %>%
  sf::st_transform(crs = 3035)

# calculate field areas
df_shp <-
  df_shp %>%
  dplyr::mutate(field_area = sf::st_area(geometry))

# create factor to order fields by area
df_shp <-
  df_shp %>%
  dplyr::mutate(fieldName = forcats::as_factor(fieldName)) %>%
  dplyr::mutate(fieldName = forcats::fct_reorder(fieldName,
                                                 .x = field_area,
                                                 .desc = TRUE)) %>%
  dplyr::arrange(field_area)



# CENTRE ALL POLYGONS ON THE SAME POINT -----------------------------------

# calculate bounding boxes for each field polygon
df_shp <-
  df_shp %>%
  dplyr::mutate(field_bbox = purrr::map(geometry, sf::st_bbox))

# measure half widths and heights of bounding boxes for each polygon
df_shp <-
  df_shp %>%
  dplyr::rowwise() %>%
  dplyr::mutate(xmin = unlist(field_bbox)$xmin,
                ymin = unlist(field_bbox)$ymin,
                xmax = unlist(field_bbox)$xmax,
                ymax = unlist(field_bbox)$ymax) %>%
  dplyr::mutate(xhalf = (xmax - xmin)/2,
                yhalf = (ymax - ymin)/2)

# translate polygons
df_shp <-
  df_shp %>%
  dplyr::mutate(geometry = geometry - c(xmin+xhalf, ymin+yhalf)) %>% # center (bottom left then shift up and across by half)
  sf::st_set_crs(3035)



# MAKE GREY BACKGROUND CIRCLE ---------------------------------------------

# make circles for each field polygon's bounding box, calculate areas, and get the largest
background_circle <-
  # create new bounding boxes from each translated polygon
  df_shp %>%
  dplyr::mutate(field_bbox_new = purrr::map(geometry, sf::st_bbox)) %>%
  dplyr::pull(field_bbox_new) %>%
  purrr::map(sf::st_as_sfc) %>%
  do.call(rbind, .) %>%
  sf::st_as_sfc(crs = 3035) %>%
  # calculate circles on each bounding box
  geos::geos_minimum_bounding_circle() %>%
  sf::st_as_sf() %>%
  dplyr::mutate(circle_area = sf::st_area(geometry)) %>%
  # return just the largest circle (the one we want to use for plotting)
  dplyr::filter(circle_area == max(circle_area))



# CALCULATE SMALLEST AND LARGEST FIELD AREAS IN SQ.KM ---------------------

area_min <-
  (min(df_shp$field_area) / 1e6) %>%
  units::drop_units() %>%
  round(digits = 1)

area_max <-
  (max(df_shp$field_area) / 1e6) %>%
  units::drop_units() %>%
  round(digits = 1)



# MAKE PLOT ---------------------------------------------------------------

plot_map <-
  df_shp %>%
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = background_circle,
                   colour = NA,
                   fill = "#005864", # greeny colour from npd.no
                   alpha = 0.2,
                   show.legend = FALSE) +
  ggplot2::geom_sf(colour = NA,
                   fill = "#C93311", # orangey colour from npd.no
                   alpha = 1,
                   show.legend = TRUE) +
  ggplot2::coord_sf(datum = NA) +
  ggplot2::facet_wrap(dplyr::vars(fieldName),
                      ncol = 11,
                      strip.position = "top") +
  ggplot2::labs(title = "Oil & gas fields on the Norwegian Continental Shelf by area",
                subtitle = paste0("125 fields, from the ", area_max, " sq.km Troll to the ", area_min, " sq.km Islay"),
                caption = bquote("Data: https://factpages.npd.no ("*.(current_day_slashes)*"), Projection & area calculations: EPSG:3035, Colours from: https://www.npd.no, Author: Sam Fielding, Licence: CC BY-SA --- "*bold("Made with R"))) +
  ggplot2::theme_bw() +
  ggplot2::theme(text = ggplot2::element_text(family = "ubuntu",
                                              color = "#4e4d47",
                                              size = 50 * 4.5),
                 
                 plot.margin = ggplot2::margin(t = 7, r = 0, b = 4, l = 0,
                                               unit = "mm"),
                 plot.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 plot.title = ggplot2::element_text(hjust = 0.5),
                 plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                                       margin = ggplot2::margin(t = 1, r = 0, b = 5, l = 0,
                                                                                unit = "mm")),
                 plot.title.position = "plot",
                 
                 plot.caption = ggplot2::element_text(hjust = 0.5,
                                                      margin = ggplot2::margin(t = 4, r = 0, b = 0, l = 0,
                                                                               unit = "mm"),
                                                      colour = "#939184",
                                                      size = 20 * 4.5),
                 plot.caption.position = "plot",
                 
                 panel.border = ggplot2::element_blank(),
                 panel.background = ggplot2::element_rect(fill = "#f5f5f2", color = NA),
                 
                 axis.title.y = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 
                 strip.text.x = ggplot2::element_text(hjust = 0.5,
                                                      color = "#616365",
                                                      size = 22 * 4.5),
                 strip.background = ggplot2::element_rect(fill = NA,
                                                          colour = NA))



# SAVE PLOT AS PNG --------------------------------------------------------

ggplot2::ggsave(filename = paste0("ncs-fields-by-area", ".png"),
                plot = plot_map,
                width = 310,
                height = 430,
                units = "mm",
                dpi = 1200)
