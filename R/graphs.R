#' Graphs
#'
#' Calculate the graphs connecting objects across time.
#'
#' @param ts_dat time series of \code{sf} objects.
#'
#' @return An \code{igraph} object representing all objects and their
#' connections.
#'
#' @export
get_graph <- function (ts_dat)
{
    graph_from <- vector ("list", length (ts_dat) - 1)
    graph_to <- vector ("list", length (ts_dat) - 1)

    name <- vector ("list", length (ts_dat))
    area <- vector ("list", length (ts_dat))
    perimeter <- vector ("list", length (ts_dat))
    compactness <- vector ("list", length (ts_dat))
    orientation <- vector ("list", length (ts_dat))

    for (i in seq_len (length (ts_dat)))
    {
        l1 <- ts_dat [[i]]

        area_i <- as.numeric (getArea (l1))
        perimeter_i <- as.numeric (getPerimeter (l1))
        compactness_i <- suppressWarnings (as.numeric (getCompactness (l1)))
        orientation_i <- sapply (l1, getOrientation)
        name_i <- paste (as.character (i), seq_along (l1), sep = ".")

        area [[i]] <- rep (area_i, fromvertices)
        perimeter [[i]] <- rep (perimeter_i, fromvertices)
        compactness [[i]] <- rep (compactness_i, fromvertices)
        orientation [[i]] <- rep (orientation_i, fromvertices)
        name [[i]] <- rep (name_i, fromvertices)
    }

    for (i in seq_len (length (ts_dat) - 1))
    {
        l1 <- ts_dat [[i]]
        l2 <- ts_dat [[i + 1]]

        rel <- intersections (l1, l2)
        intersection_cols <- which (grepl ("intersection[0-9]", colnames (rel)))
        ints <- as.matrix (rel [, intersection_cols])
        fromvertices <- apply (ifelse (is.na (ints), 0, 1), 1, sum)
        from <- rep (as.integer (names (fromvertices)), fromvertices)

        to <- as.vector (t (ints))
        to <- to [!is.na (to)]
        from <- paste (as.character (i), sep = ".", from)
        to <- paste (as.character (i + 1), sep = ".", to)
        graph_from [[i]] <- from
        graph_to [[i]] <- to
    }

    vert <- data.frame (name = unlist (name),
                        area = unlist (area),
                        perimeter = unlist (perimeter),
                        compactness = unlist (compactness),
                        orientation = unlist (orientation))
    vert <- unique (vert)
    relations <- data.frame(from = unlist (graph_from), to = unlist (graph_to))
    g <- graph.data.frame(relations, directed = TRUE, vertices = vert)
    return (g)
}
