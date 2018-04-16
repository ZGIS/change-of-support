#' Generate data
#'
#' This function can be used to generate randomised test data.
#'
#' @param layers Desired number of output layers.
#'
#' @return A list of \code{sf} polygons.
#'
#' @export
generate_data <- function (layers = 10)
{
    n_pts <- 500
    eps <- 0.05
    change_rate <- 0.05

    xy <- matrix (runif (2 * n_pts), ncol = 2)
    n_change <- as.integer (change_rate * n_pts)
    dat <- vector (mode = "list", length = layers)

    for (i in seq_len (layers))
    {
        xy [sample (1:n_pts, n_change), ] <- runif (2 * n_change, min = -0.1,
                                                    max = 0.1)
        dat [[i]] <- generate_data_layer (xy, eps)
        generate_data_layer (xy, eps)
    }
    dat
}

generate_data_layer <- function (xy, eps)
{
    clust <- dbscan::optics (xy, eps = eps, minPts = 3)
    optics_cluster <- dbscan::extractDBSCAN (clust, eps)
    c_ids <- optics_cluster$cluster
    layer <- data.frame (xy, c_ids)
    names (layer) <- c ("x", "y", "cluster")
    unique_clusters <- unique (sort (layer$cluster))
    unique_clusters <- unique_clusters [unique_clusters != 0]

    for (i in seq_along (unique_clusters))
    {
        size <- sum (layer$cluster == unique_clusters [i], na.rm = TRUE)
        if (size < 3)
            layer <- layer [layer$cluster != unique_clusters [i], ]
    }
    unique_clusters <- unique (sort (layer$cluster))
    unique_clusters <- unique_clusters [unique_clusters != 0]

    sfc <- list ("POLYGON", length (unique_clusters))
    coord_list <- vector (mode = "list", length = length (unique_clusters))
    for (i in seq_along (unique_clusters))
    {
        c_id <- unique_clusters [i]
        coords <- as.matrix (layer [layer$cluster == c_id, 1:2])
        hull <- coords [chull (coords), ]
        coords <- rbind (hull, hull [1, ])

        sfc [[i]] <- sf::st_polygon (list (coords))
    }
    sf::st_sfc (sfc, crs = 4326)
}
