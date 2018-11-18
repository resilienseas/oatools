#' Get Raster Layer from sdmpredictors
#'
#' @param lyr layer
#' @param tif tif of raster
#' @param crs coordinate reference system
#' @param dir_sdm_cache local cache directory for fast loading if already calculated
#' @param res resolution (default: 10000 m)
#' @param extent_crop extent to crop (default: NULL)
#' @param method method of interpolation with `raster::projectRaster()` (default: "ngb")
#' @param redo_tif boolean (True or False) whether to refetch layer, ie of other parameters change
#' @param fill_na boolean (True or False) whether to fill in NAs
#' @param fill_window number of neighboring cells to fill in with `raster::focal()` (default: 3)
#'
#' @return raster object after loading from sdmpredictors, projecting and cropping to study area,
#' and writing raster. The function simply loads the existing raster if the tif already exists,
#' unless redo_tif = TRUE, in which case it is recreated with the arguments given.
#' @export
#'
#' @examples
lyr_to_tif <- function(
  lyr, tif, crs, dir_sdm_cache,
  res=10000, extent_crop=NULL, method="ngb",
  redo_tif=FALSE, fill_na=TRUE, fill_window=3){

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

      if (fill_window %% 2 == 0) stop("The fill_window must be an odd number.")
      fill.na <- function(x) {
        i <- (fill_window * floor(fill_window / 2))  + ceiling(fill_window / 2)
        if( is.na(x)[i] ) {
          # browser() # uncomment to stop execution and inspect values
          return( round(mean(x, na.rm=TRUE),0) )
        } else {
          return( x[i] )
        }
      }

      r <- focal(
        r, w = matrix(1, fill_window, fill_window), fun = fill.na, pad=T, na.rm=F)
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
#' @param r raster
#' @param title title of plot
#' @param ncolors number of colors (default: 1000)
#' @param color_palette color palette (default: c("#5E85B8","#EDF0C0","#C13127"))
#'
#' @return Function does not return any value, but plot is generated with the default output device.
#' @export
#'
#' @examples
plot_raster <- function(r, title, ncolors = 1000, color_palette = c("#5E85B8","#EDF0C0","#C13127")){

  my_colors = colorRampPalette(color_palette)
  plot(r, col=my_colors(ncolors), axes=FALSE, box=FALSE)
  #plot(inventorycoords, add=TRUE)
  title(cex.sub = 1.25, main = title)
}



