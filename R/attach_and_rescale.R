#' Attach and Rescale
#' This formula is a wrapper for the final portion of the Index Calculation. It takes calculated distances, binds it with
#' mangroves layer, calculates a mean of the threats and finally rescale the index in a value from 0 to 1.
#'
#' @param x is the distance calculated form the `calc_distance.R` function.
#'
#' @return spatial object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#'
#' \dontrun{
#'
#' attach_and_rescale(distance)
#' }
#'
attach_and_rescale <- function(x) {

      distance <- do.call(rbind, distance) |>
            as.data.frame() |>
            dplyr::select(-.data$geometry) |>
            tidyr::pivot_wider(id_cols = .data$ID, names_from = .data$Threat, values_from = distance)

      mangrove <- cbind(mangrove, distance)

      mangrove$MTI <- rowMeans(dplyr::select(as.data.frame(mangrove), dplyr::starts_with("threat_")), na.rm = TRUE)

      mangrove$MTI <- 1 - scales::rescale(mangrove$MTI)

      mangrove
}

