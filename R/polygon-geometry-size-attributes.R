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

}
