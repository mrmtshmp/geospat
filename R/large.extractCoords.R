#' Extract coord matrix from SpatialPolygonDataframe and return the centroid of each block.
#'
#' @importFrom geosphere centroid
#'
#' @param large.sp.df a SpatialPolygonDataframe-class object.
#'
#' @export


# Extract coord matrix from SpatialPolygonDataframe@polygon (list-object).

extractCoords <- function(polygons)
  {
  results <- list()
  for(i in 1:length(polygons@Polygons))
  {
    results[[i]] <- polygons@Polygons[[i]]@coords
  }
  results <- Reduce(rbind, results)
  results
}

# Find a centroid.

cntrd <- function(mat, var.long, var.lat) {
  data.frame(geosphere::centroid(mat[,c(var.long, var.lat)]))
  }

# Main

large.extractCoords <- function(large.sp.df){
  large.polygons <- large.sp.df@polygons
  res <- ldply(
    large.polygons,
    function(polygons){
      res.coord <- extractCoords(polygons) %>%
        as.matrix()
      dimnames(res.coord)[[2]] <- c("long","lat")
      res.cnt_coord <-
        cntrd(
          res.coord,
          var.long = "long",
          var.lat = "lat"
          )
      return(res.cnt_coord)
    }
  )
}
