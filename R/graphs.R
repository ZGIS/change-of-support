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

    relations <- data.frame(from = unlist (graph_from), to = unlist (graph_to))
    g <- graph.data.frame(relations, directed=TRUE)
    return (g)
}
