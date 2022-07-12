#' calc_distance
#'
#'Calculate the distance of mangrove patches from threats.
#' @param x must be a vector file representing a threat. The formula will calculate the distance between it and mangrove centroids
#'
#' @return spatial object form sf package
#' @export
#'
#'@importFrom rlang .data
#'
#'
#' @examples
#' \dontrun{
#' thr <- lapply(FileList, sf::st_as_sf)
#' mangrove <- sf::st_as_sf(mangrove_layer)
#' centroid <- sf::st_point_on_surface(mangrove)
#' lapply(thr, calc_distance)
#'  }
#'
calc_distance <- function(x) {


      distance <- as.data.frame(
            geosphere::dist2Line(
                  sf::as_Spatial(.data$centroid),
                  sf::as_Spatial(x),
                  distfun = geosphere::distGeo)) |>
            dplyr::mutate(Threat = x |>
                                dplyr::pull(.data$Attribute) |>
                                unique(),
                          Threat = paste0("threat_", .data$Threat),
                          ID = 1:nrow(.data$centroid))
      distance <- sf::st_as_sf(distance,
                               coords = c("lon", "lat"),
                               crs = 4326)}
