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
    overlap <- vector ("list", length (ts_dat) - 1)

    name <- vector ("list", length (ts_dat))
    area <- vector ("list", length (ts_dat))
    all_areas <- vector ("list", length (ts_dat))
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

        area [[i]] <- area_i
        perimeter [[i]] <- perimeter_i
        compactness [[i]] <- compactness_i
        orientation [[i]] <- orientation_i
        name [[i]] <- name_i
    }

    for (i in seq_len (length (ts_dat) - 1))
    {
        l1 <- ts_dat [[i]]
        l2 <- ts_dat [[i + 1]]

        rel <- intersections (l1, l2)
        intersection_cols <- which (grepl ("intersection[0-9]", colnames (rel)))
        ints <- as.matrix (rel [, intersection_cols])
        if (ncol (ints) > 1)
        {
            fromvertices <- apply (ifelse (is.na (ints), 0, 1), 1, sum)
            from <- rep (as.integer (names (fromvertices)), fromvertices)
            all_areas [[i]] <- rep (area [[i]], fromvertices)
        } else
        {
            from <- as.vector (ints)
        }

        to <- as.vector (t (ints))
        to <- to [!is.na (to)]
        from_name <- paste (as.character (i), sep = ".", from)
        to_name <- paste (as.character (i + 1), sep = ".", to)

        area_overlap <- as.numeric (getArea (st_intersection (l1, l2)))
        area_overlap <- area_overlap [area_overlap > 0]
        overlap [[i]] <- area_overlap / all_areas [[i]]
        graph_from [[i]] <- from_name
        graph_to [[i]] <- to_name
    }

    vert <- data.frame (name = unlist (name),
                        area = unlist (area),
                        perimeter = unlist (perimeter),
                        compactness = unlist (compactness),
                        orientation = unlist (orientation))
    vert <- unique (vert)
    relations <- data.frame(from = unlist (graph_from), to = unlist (graph_to),
                            overlap = unlist (overlap))
    g <- graph.data.frame (relations, directed = TRUE, vertices = vert)
    return (g)
}

#' Qualitative change of support
#'
#' Calculate the qualitative change of suppot that connected nodes in a graph
#' undergo. The support can be one of the following: stable, split, merge,
#' anihilation, creation.
#'
#' @param dat \code{igraph} object representing all objects and layers.
#'
#' @return Map of qualitative COS
#'
#' @export
qualitative_cos <- function (dat)
{
}
