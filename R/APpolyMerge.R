#' Merge polygons
#'
#' Take a List of SpatialPolygonsDataFrame objects and merge them. This function automates the process of
#' assigning new polygon IDs, which is usually the issue that prevents merging.
#'
#' @param polyList List. a List of SpatialPolygonsDataFrame objects
#' @param newID logical. If TRUE, the polygon IDs in \code{polyList} will be replaced to prevent duplicate IDs.
#'
#' @importFrom methods slot
#'
#' @return A merged SpatialPolygonsDataFrame


APpolyMerge<- function(polyList, newID = FALSE){

  # Check that input is valid
  if(!all(unique(sapply(polyList, class)) == "SpatialPolygonsDataFrame")){
    stop("'polyList' must be a List of SpatialPolygonsDataFrame objects")
  }

  # Get existing IDs
  inIDs <- do.call(c, lapply(polyList, function(poly) sapply(poly@polygons, slot, "ID")))

  # Assign new IDs if needed
  if(any(duplicated(inIDs))){

    if(newID){

      l <- length(polyList[[1]])
      polyList[[1]] <- sp::spChFIDs(polyList[[1]], as.character(1:l))

      if(length(polyList) > 1){

        for(i in 2:length(polyList)){

          polyList[[i]] <- sp::spChFIDs(polyList[[i]], as.character((l + 1):(l + length(polyList[[i]]))))
          l <- l + length(polyList[[i]])
        }
      }

    }else{

      stop("'polyList' contains polygons with duplicate IDs. Use the 'newID' argument to assign new unique IDs")

    }
  }

  # Merge polygons
  if(length(polyList) == 1){

    return(polyList)

  }else{

    outPoly <- maptools::spRbind(polyList[[1]], polyList[[2]])

    if(length(polyList) > 2){
      for(i in 3:length(polyList)){

        outPoly <- maptools::spRbind(outPoly, polyList[[i]])

      }
    }

    return(outPoly)
  }
}
