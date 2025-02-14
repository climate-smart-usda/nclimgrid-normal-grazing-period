library(tidyverse)
library(arrow)
library(magrittr)

setwd("~/git/mt-climate-office/usdm-archive/")
source("R/usdm_layout.R")
source("R/update_usdm.R")
source("R/update_usdm_video.R")
setwd("~/git/mt-climate-office/nclimgrid-normal-grazing-period/")

oconus <-
  sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_counties_rotated_simple.parquet")

county_ngp <-
  list(
  sf::read_sf("nclimgrid-normal-grazing-period.arrow"),
  sf::read_sf("~/git/mt-climate-office/era5-normal-grazing-period/ak-era5-normal-grazing-period.arrow")
) %>%
  dplyr::bind_rows() %>%
  dplyr::group_by(COUNTY_FIPS = FIPS) %>%
  dplyr::summarise(`Number of Days in Grazing Period` = max(`Number of Days in Grazing Period`, na.rm = TRUE)) %>%
  dplyr::left_join(oconus, .)

p <-
  ggplot(sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_rotated_simple.parquet")) +
  geom_sf(data = sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_rotated_simple.parquet"),
          fill = "gray80",
          color = NA,
          show.legend = FALSE) +
  geom_sf(aes(fill = `Number of Days in Grazing Period`),
          data = county_ngp,
          color = "white",
          linewidth = NA,
          show.legend = T) +
  geom_sf(data = sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_states_rotated_simple.parquet"),
          color = "white",
          alpha = 0,
          show.legend = FALSE,
          linewidth = 0.2) +
  scale_fill_viridis_c(
    name = "Days in Normal\nGrazing Period",
    guide = guide_colorbar(direction = "vertical",
                         title.position = "top",
                         override.aes = list(linewidth = 0.5))
  ) +
  usdm_layout(attribution = "Normal grazing periods are calculated according to NAP-190 guidance using the\nnClimGrid (CONUS) and ERA5-Land (AK) reanalysis data.\nNo data yet available for Hawaii or Puerto Rico.\nMap courtesy of the Montana Climate Office.")

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

grid::grid.draw(gt) %>%
  ggsave(plot = .,
         filename = "nclimgrid-normal-grazing-period.png",
         device = ragg::agg_png,
         width = 10,
         height = 5.14,
         # height = 6.86,
         bg = "white",
         dpi = 600)

ngp_2022 <-
"~/git/mt-climate-office/fsa-normal-grazing-period/fsa-normal-grazing-period.csv" %>%
  readr::read_csv() %>%
  dplyr::group_by(COUNTY_FIPS = FSA_CODE) %>%
  dplyr::summarise(`Grazing Period Start Date` = min(`Grazing Period Start Date`, na.rm = TRUE),
                   `Grazing Period End Date` = max(`Grazing Period End Date`, na.rm = TRUE)) %>%
  dplyr::mutate(`Number of Days in Grazing Period` = as.integer(`Grazing Period End Date` - `Grazing Period Start Date`),
                `Number of Days in Grazing Period` = ifelse(`Number of Days in Grazing Period` > 365, 365, `Number of Days in Grazing Period`)) %>%
  dplyr::left_join(oconus, .)

p <-
  ggplot(sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_rotated_simple.parquet")) +
  geom_sf(data = sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_rotated_simple.parquet"),
          fill = "gray80",
          color = NA,
          show.legend = FALSE) +
  geom_sf(aes(fill = `Number of Days in Grazing Period`),
          data = ngp_2022,
          color = "white",
          linewidth = NA,
          show.legend = T) +
  geom_sf(data = sf::read_sf("~/git/mt-climate-office/usdm-archive/usdm-archive/oconus/oconus_states_rotated_simple.parquet"),
          color = "white",
          alpha = 0,
          show.legend = FALSE,
          linewidth = 0.2) +
  scale_fill_viridis_c(
    name = "2022 Days in Normal\nGrazing Period",
    guide = guide_colorbar(direction = "vertical",
                           title.position = "top",
                           override.aes = list(linewidth = 0.5))
  ) +
  usdm_layout(attribution = "Normal grazing periods are the maximum official 2022 grazing period per county.\n\nNo data yet available for Hawaii or Puerto Rico.\nMap courtesy of the Montana Climate Office.")

gt <- ggplot_gtable(ggplot_build(p))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

grid::grid.draw(gt) %>%
  ggsave(plot = .,
         filename = "nclimgrid-normal-grazing-period-2022.png",
         device = ragg::agg_png,
         width = 10,
         height = 5.14,
         # height = 6.86,
         bg = "white",
         dpi = 600)


