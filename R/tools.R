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

#' Find gaps in ocean acidification monitoring
#'
#' @param df data frame subsetted to OA rows
#' @param fld_lon name of field with longitude in df
#' @param fld_lat name of field with latitude in df
#' @param r_sst_mean sea surface temperature range raster
#' @param r_sst_range sea surface temperature mean
#' @param r_do_mean dissolved oxygen mean raster
#' @param r_do_range dissolved oxygen range raster
#' @param distanceweight weighting factor for distance. default value of 10
#' @param temporalweight weighting factor for temporal as compared to spatial. default value of 10^-11.
#'
#' @return raster of final gap analysis
#' @export
#'
#' @examples
#find_gaps <- function(df, r_sst_mean, r_sst_range, r_do_mean, r_do_range,
#                      fld_lon="Longitude", fld_lat="Latitude",
#                      distanceweight = 10^-11, temporalweight = 10){

#  df_coords  <- cbind.data.frame(df[[fld_lon]], df[[fld_lat]])

  # remove duplicate locations
#  deduped.coords <- unique(df_coords)

  # create spatial points objects
#  pts <- SpatialPoints(deduped.coords, CRS("+proj=longlat +ellps=WGS84"))
#  pts <- spTransform(pts, CRS('+init=EPSG:6414')) # TODO: add projection as argument

#  vor <-voronoi(pts)

#  r_vor <- rasterize(vor, r_sst_mean, "id")
  #plot(vor)

#  make_r_var <- function(r, r_nofill, fld){
    # make_r_var(r=r_sst_mean, fld="SST"|"SSTrange")

#    df_r_pts <- raster::extract(r, pts, method='simple', df=TRUE)
#    colnames(df_r_pts)<-c("id", fld)

    # substitute polygon id for monitoring site sea surface temerature of that polygon
#    r_vor_pts_sstmean <- subs(r_vor, df_r_pts, by="id", which=fld)

    # extract sst and do mean and range value for each monitoring site cell
#    df_pts_r <- raster::extract(r, pts, method='simple', df=TRUE)

    # rename column names of sitesstrange
#    colnames(df_pts_r) <- c("id", fld)

    # substitute polygon id for monitoring site sea surface temerature of that polygon
#    r_vor_pts <- subs(r_vor, df_pts_r, by="id", which=fld, subsWithNA=FALSE)

    # normalize
#    r_nofill  <- r_nofill/maxValue(r_nofill)
#    r_vor_pts <- r_vor_pts/maxValue(r_nofill) # TODO: check b/c df?

    # calculate differences between each cell and the closest monitoring site
#    r_dif <- abs(r_nofill - r_vor_pts)

    #...
#    r_dif
#  }

#  r_dif_sst_mean  <- make_r_var(r_sst_mean, r_sst_mean_nofill, "SST")
#  r_dif_sst_range <- make_r_var(r_sst_range, r_sst_range_nofill, "SSTrange")
#  r_dif_do_mean   <- make_r_var(r_do_mean, r_do_mean_nofill, "DO")
#  r_dif_do_range  <- make_r_var(r_do_range, r_do_range_nofill, "DOrange")

  # r_vor     = vorraster
  # pts       = carbcompletecoords
  # r_vor_pts = carbcompletepolygonsst
  # df_pts    = carbcompletesitesstrange
  # r_nofill  = r_sst_mean_nofill
  # carbcompletesstmeandiff
  # r_dissimilarity = carbcompletedissimilarity
  # r_distance      = carbcompletedistance
  # r_gap           = carbcompletegap
  # r_gap_severe    = carbcompleteseveregaps
  # r_gap_high      = carbcompletehighprioritygaps
  # r_gap_low       = carbcompletelowprioritygaps
  # r_gap_final     = carbcompletefinalgaps

  # create oceanographic dissimilarity layer
#  r_dissimilarity<- sqrt((r_dif_sst_mean^2+r_dif_do_mean^2)+temporalweight*(r_dif_sst_range^2+r_dif_do_range^2))

#  r_distance <- distanceFromPoints(r_dissimilarity, pts) * distanceweight

#  r_gap <- setValues(r_distance, sqrt((getValues(r_distance)^2+(getValues(r_dissimilarity)^2))))

#  r_gap_severe <- setValues(r_distance, sqrt((getValues(r_distance)^2+(getValues(r_dissimilarity)^2)))) > quantile(r_gap, (.999))
#  r_gap_high   <- setValues(r_distance, sqrt((getValues(r_distance)^2+(getValues(r_dissimilarity)^2)))) > quantile(r_gap, (.99))
#  r_gap_low    <- setValues(r_distance, sqrt((getValues(r_distance)^2+(getValues(r_dissimilarity)^2)))) > quantile(r_gap, (.75))
#  r_gap_final  <- r_gap_severe + r_gap_low + r_gap_high

#  r_gap_final
#}
