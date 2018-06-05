#' get area of polygon
#'
#' This function calculates the area of a \code{sf} polygon.
#'
#' @param polygon \code{sf} polygon
#'
#' @return area of the \code{sf} polygon in units of coordinate system
#'
#' @export
getArea <- function(polygon)
{
  return (sf::st_area(polygon))
}

#' get perimeter of polygon
#'
#' This function calculates the perimeter of a \code{sf} polygon.
#'
#' @param polygon \code{sf} polygon
#'
#' @return perimeter of the \code{sf} polygon in units of coordinate system
#'
#' @export
getPerimeter <- function(polygon)
{
  return (sf::st_length(sf::st_cast(polygon, "MULTILINESTRING")))
}

#' get compactness of polygon
#'
#' This function calculates the compactness of a \code{sf} polygon.
#'
#' @param polygon \code{sf} polygon
#'
#' @return compactness of the \code{sf} polygon in units of coordinate system
#'
#' @export
getCompactness <- function(polygon)
{
  perimeter <- getPerimeter(polygon)
  area <- getArea(polygon)
  return ((2*sqrt(pi*area))/(perimeter))

}

#' get orientation of polygon
#'
#' This function calculates the orientation of a \code{sf} polygon.
#'
#' @param polygon \code{sf} polygon
#'
#' @return orientation of the \code{sf} polygon in degree
#'
#' @export
getOrientation <- function(polygon)
{
    coords <- MBR (polygon [[1]])
    l1 <- getLength (coords [1,], coords [2, ])
    l2 <- getLength (coords [2,], coords [3, ])
    max_index <- as.integer (ifelse (l1 > l2, 1, 2))
    orientation <- getAngle (coords [max_index, ], coords [max_index + 1, ])
    return (orientation)
}

getLength <- function (c1, c2)
{
    a <- abs (c1 [1] - c2 [1])
    b <- abs (c1 [1] - c2 [1])
    return (sqrt (a * a + b * b))
}

getAngle <- function(c1, c2)
{
    dot.prod <- c1 %*% c2
    norm.x <- norm(c1, type="2")
    norm.y <- norm(c2, type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
}

MBR <- function(points)
{
    points <- unique (points)
    a <- alphahull::ashape(points, alpha=1000)      # One way to get a convex hull...
    e <- a$edges[, 5:6] - a$edges[, 3:4]            # Edge directions
    norms <- apply(e, 1, function(x) sqrt(x %*% x)) # Edge lengths
    v <- diag(1/norms) %*% e                        # Unit edge directions
    w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges

    # Find the MBR
    vertices <- (points) [a$alpha.extremes, 1:2]    # Convex hull vertices
    minmax <- function(x) c(min(x), max(x))         # Computes min and max
    x <- apply(vertices %*% t(v), 2, minmax)        # Extremes along edges
    y <- apply(vertices %*% t(w), 2, minmax)        # Extremes normal to edges
    areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
    k <- which.min(areas)                           # Index of the best edge (smallest area)

    # Form a rectangle from the extremes of the best edge
    cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}
