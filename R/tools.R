#' Add two numbers
#'
#' @param x first number
#' @param y second number
#'
#' @return the result of addition
#' @export
#'
#' @examples
add <- function(x, y=0){
  x + y + z
}

#' Get Raster Layer from sdmpredictors
#'
#' @param lyr
#' @param tif
#' @param crs
#' @param dir_sdm_cache
#' @param res
#' @param extent_crop
#' @param method
#' @param redo_tif
#' @param fill_na
#' @param fill_window
#'
#' @return raster object after loading from sdmpredictors, projecting and cropping to study area,
#' and writing raster. The function simply loads the existing raster if the tif already exists,
#' unless redo_tif = TRUE, in which case it is recreated with the arguments given.
#' @export
#'
#' @examples
lyr_to_tif <- function(lyr, tif, crs, dir_sdm_cache, res=10000, extent_crop=NULL, method="ngb", redo_tif=FALSE, fill_na=TRUE, fill_window=3){

  message("lyr_to_tif() messages...")

  if (!file.exists(tif) | redo_tif){
    #browser()

    message("  loading layer from sdmpredictors and projecting")
    r <- sdmpredictors::load_layers(lyr, equalarea = F, datadir=dir_sdm_cache)
    r <- raster::projectRaster(r, crs=sp::CRS(crs), res=10000, method="ngb")

    if (!is.null(extent_crop)){
      message("  cropping raster to extent")
      r <- raster::crop(r, extent_crop)
    }

    if (fill_na){
      message("  filling in missing (NA) values with focal window")
      # raster - Fill the gaps using nearest neighbors
      #   https://gis.stackexchange.com/questions/181011/fill-the-gaps-using-nearest-neighbors
      fill.na <- function(x, i=5) {
        if( is.na(x)[i] ) {
          return( round(mean(x, na.rm=TRUE),0) )
        } else {
          return( round(x[i],0) )
        }
      }
      r <- focal(
        r, w = matrix(1,fill_window,fill_window), fun = fill.na, pad=T, na.rm=F)
    }

    message("  writing raster to tif")
    raster::writeRaster(r, tif, overwrite=T)
  } else {
    message("  tif found, so reading")
    r <- raster::raster(tif)
  }

  r
}

#' Basic raster plot
#'
#' @param r
#' @param title
#' @param ncolors
#' @param color_palette
#'
#' @return
#' @export
#'
#' @examples
plot_raster <- function(r, title, ncolors = 1000, color_palette = c("#5E85B8","#EDF0C0","#C13127")){

  my_colors = colorRampPalette(color_palette)
  plot(r, col=my_colors(ncolors), axes=FALSE, box=FALSE)
  #plot(inventorycoords, add=TRUE)
  title(cex.sub = 1.25, sub = title)
}


