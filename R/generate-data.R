#' Generate data
#'
#' This function can be used to generate randomised test data.
#'
#' @param layers Desired number of output layers.
#' @param n_objects Desired number of objects per layer.
#' @param change_rate The rate of randomness introduced to each new layer.
#'
#' @return A list of \code{sf} polygons.
#'
#' @export
generate_data <- function (layers = 50, n_objects = 20, change_rate = 0.05)
{
    if (layers < 1)
        stop ("layers has to be >= 1")
    if (n_objects < 2)
        stop ("n_objects has to be > 1")
    if (change_rate < 0 || change_rate > 1)
        stop ("change_rate has to be between 0 and 1.")

    n_pts <- n_objects * max (layers, 20)

    xy <- matrix (runif (2 * n_pts), ncol = 2)
    n_change <- as.integer (change_rate * n_pts)
    dat <- vector (mode = "list", length = layers)

    for (i in seq_len (layers))
    {
        xy_rand <- xy
        change_ids <- sample (1:n_pts, n_change)
        change_coords <- xy [change_ids, ]
        add_to_coords <- runif (2 * n_change, min = -0.1, max = 0.1)
        change_coords <- change_coords + add_to_coords
        change_coords [change_coords > 1] <- 1
        change_coords [change_coords < 0] <- 0
        xy_rand [change_ids, ] <- change_coords

        dat [[i]] <- generate_data_layer (xy_rand, n_objects)
    }
    dat
}

generate_data_layer <- function (xy, n_clusters)
{
    h_cluster <- hclust (dist (xy))
    c_ids <- cutree (h_cluster, n_clusters)
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
