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
    coords <- polygon [[1]]
    lengths <- rep (0, dim (coords) [1] - 1)
    for (i in seq_len (dim (coords) [1] - 1))
    {
        c1 <- coords [i, ]
        c2 <- coords [i + 1, ]
        lengths [i] <- getLength (c1, c2)
    }
    max_index <- which (lengths == max (lengths))
    orientation <- getAngle (coords [max_index, ], coords [max_index + 1, ])
    return (orientation)
}

getLength <- function (c1, c2)
{
    a <- abs (c1 [1] - c2 [1])
    b <- abs (c1 [1] - c2 [1])
    return (sqrt (a * a + b * b))
}

getAngle <- function(c1, c2){
    dot.prod <- c1 %*% c2
    norm.x <- norm(c1, type="2")
    norm.y <- norm(c2, type="2")
    theta <- acos(dot.prod / (norm.x * norm.y))
    as.numeric(theta)
}
