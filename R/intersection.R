#' Intersections
#'
#' Determine which objects in one layer intersect objects of another layer.
#'
#' @param l1 \code{sfc} polygon layer at time 1.
#' @param l2 \code{sfc} polygon layer at time 2.
#'
#' @return A \code{sfc} polygon layer.
#'
#' @export
intersections <- function (l1, l2)
{
    rel <- suppressMessages (sf::st_relate (l1, l2, pattern = "2********"))
    maxintersections <- max (sapply (rel, length))
    rel_matrix <- matrix (nrow = nrow (rel), ncol = maxintersections)

    for (i in seq_along (rel))
    {
        insert <- rep (NA, maxintersections)
        r_i <- rel [[i]]
        for (j in seq_along (r_i))
            insert [j] <- r_i [j]
        rel_matrix [i, ] <- insert
    }

    rel_matrix <- data.frame (rel_matrix)
    cnames <- paste0 ("intersection", 1:ncol (rel_matrix))
    colnames (rel_matrix) <- cnames
    intersections <- data.frame (l1, rel_matrix)

    return (intersections)
}
