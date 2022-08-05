##mangrove threat index=group
##matrixr=name
##Layer=vector
##Threats=optional multiple vector
##Attribute=string
##Output=output vector
##load_vector_using_rgdal


>      thr <- lapply(Threats, sf::st_as_sf)
>
>      mangrove <- sf::st_as_sf(Layer)
>      centroid <- sf::st_point_on_surface(mangrove)
>
>      distance <- lapply(thr, function(x) {
>
>            distance <- as.data.frame(
>                  geosphere::dist2Line(
>                        sf::as_Spatial(centroid),
>                        sf::as_Spatial(x),
>                        distfun = geosphere::distGeo)) |>
>                  dplyr::mutate(Threat = x |> dplyr::pull(Attribute) |> unique(),
>                                Threat = paste0("threat_", Threat),
>                                ID = 1:nrow(centroid))
>
>
>            distance <- sf::st_as_sf(distance,
>                                     coords = c("lon", "lat"),
>                                     crs = 4326)})
>      distance <- do.call(rbind, distance) |> as.data.frame() |> dplyr::select(-geometry) |> tidyr::pivot_wider(id_cols = ID, names_from = Threat, values_from = distance)
>      mangrove <- cbind(mangrove, distance)
>      mangrove$MTI <- rowMeans(dplyr::select(as.data.frame(mangrove), dplyr::starts_with("threat_")), na.rm = TRUE)
>      mangrove$MTI_scaled <- 1-scales::rescale(mangrove$MTI)
>
>      Output <- sf::as_Spatial(mangrove)
