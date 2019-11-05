#' Make histogram of diatance to pharmacy and each pharmacy's scores.
#'
#' @import sp
#' @import sf
#' @import ggplot2
#' @importFrom raster shapefile
#' @importFrom plyr ldply
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom plyr .
#' @importFrom plyr ddply
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom leaflet colorQuantile
#'
#'
#' @param fn.RData.loc_score = "../Data/test_HmMs_2035.RData",
#' @param rbPal = NULL,
#' @param vec.prob_q = NULL,
#' @param dir.Data = "../Data",
#' @param weight.var_name = "score.merged_PTD_2035",
#' @param fn.Shape.GovRegion A character vector of (a) file name(s) (with ".shp") as the background.
#' @param fn.ShapeP.SchoolRegion = "/190626/A32-16_30_GML/shape/A32P-16_30.shp",
#' @param fn.Shape.SchoolRegion = "/190706/A32-13_30/shp/A32-13_30.shp",
#' @param prefix.pdf_output = "location_scor"
#'
#' @export


plot.wSDG <- function(
  fn.RData.loc_score = "../Data/test_HmMs_2050.RData",
  rbPal = NULL,
  vec.prob_q = NULL,
  dir.Data = "../Data",
  weight.var_name = "score.merged_PTD_2050",
  fn.Shape.GovRegion = c(
    '/190706/N03-190101_30_GML/N03-19_30_190101.shp',
    '/190706/N03-190101_24_GML/N03-19_24_190101.shp',
    '/190706/N03-190101_27_GML/N03-19_27_190101.shp',
    '/190706/N03-190101_29_GML/N03-19_29_190101.shp'
    ),
  fn.ShapeP.SchoolRegion = "/190626/A32-16_30_GML/shape/A32P-16_30.shp",
  fn.Shape.SchoolRegion = "/190706/A32-13_30/shp/A32-13_30.shp",

  prefix.pdf_output = "location_score.only_pref_30.weight"
){

  # Data Loading -----------------------------------------------------------------

  load(
    file = fn.RData.loc_score
  )

  long.df.res.distm.rank_1 <-
    test[[1]]
  long.long.df.res.distm.rank_1.merge_mesh_on_pharm <-
    test[[2]]
  rank.restrict <-
    test[[3]]


  Shape.SchoolRegion <- shapefile(
    sprintf(
      "%s/%s", dir.Data,
      fn.Shape.SchoolRegion
    )
  )


  print('Read (a) shape file(s) (goverment region)')

  for(i in 1:length(fn.Shape.GovRegion)){
    Shape_i.GovRegion <- shapefile(
      sprintf(
        "%s/%s", dir.Data,
        fn.Shape.GovRegion[i]
      )
    )
    if(i==1){
      Shape.GovRegion <- Shape_i.GovRegion
    }else{
      Shape.GovRegion <- Shape.GovRegion %>%
        rbind(Shape_i.GovRegion)
    }
  }




  # Shape files (school region) -------------------------------------------------------------------


  ShapeP.SchoolRegion <- shapefile(
    sprintf(
      "%s/%s", dir.Data,
      fn.ShapeP.SchoolRegion
    )
  )

  # print("Loading shape (a) file(s).")
  # for(i in 1:length(fn.mesh.popEst)){
  #   i.Shape.mesh.pop_Est <-
  #     rgdal::readOGR(
  #       sprintf(
  #         "%s",
  #         fn.mesh.popEst[i]
  #       )
  #     )
  #   if(i==1){
  #     Shape.mesh.pop_Est <- i.Shape.mesh.pop_Est
  #   }else{
  #     Shape.mesh.pop_Est <- rbind(
  #       Shape.mesh.pop_Est,
  #       i.Shape.mesh.pop_Est
  #     )
  #   }
  # }

  fn.SpatialPointDataFrame  =
    '../Output/DataForMap.Wakayama_v01.RData'

  load(
    file = fn.SpatialPointDataFrame
  )




  # Plot scores on atlas. ---------------------------------------------------

  sptsDataframe_data <-
    sptsDataframe@data %>%
    dplyr::mutate(
      Ph.ID =
        sprintf(
          "%s_%s",
          ID.pref, ID
        )
    ) %>%
    dplyr::filter(ID.pref==30) %>%
    dplyr::left_join(
      long.long.df.res.distm.rank_1.merge_mesh_on_pharm
    )


  test.sptsDataframe <- sptsDataframe

  test.sptsDataframe@data <-
    sptsDataframe_data %>%
    dplyr::filter(
      weight == weight.var_name
    )


  if(is.null(rbPal)){
    if(is.null(vec.prob_q)){vec.prob_q <- c(0.0, 0.3, 0.8, 0.9,1.0)}
    rbPal <-
      leaflet::colorQuantile(
        palette = "RdYlBu",
        domain = long.long.df.res.distm.rank_1.merge_mesh_on_pharm$val,
        probs = vec.prob_q,
        reverse = TRUE
      )
  }

  long.long.df.res.distm.rank_1.merge_mesh_on_pharm$Col <-
    rbPal(
      long.long.df.res.distm.rank_1.merge_mesh_on_pharm$val
    )

  test.sptsDataframe@data$Col <-
    rbPal(
      test.sptsDataframe@data$val
    )

  Col.uni <-
    rbPal(
      quantile(long.long.df.res.distm.rank_1.merge_mesh_on_pharm$val, vec.prob_q)
    ) %>%
    unique()

  print(
    sprintf(
      "Output File is: %s",
      sprintf(
        "%s_%s.rank_%s.algscore_%s.pdf",
        prefix.pdf_output,
        unique(test.sptsDataframe@data$weight),
        rank.restrict,
        alg.score
        )
      )
    )
  pdf(
    sprintf(
      "%s_%s.rank_%s.algscore_%s.pdf",
      prefix.pdf_output,
      unique(test.sptsDataframe@data$weight),
      rank.restrict,
      alg.score
    ),
    width = 20,
    height = 20
  )
  print(test.sptsDataframe@data$Col)
  plot(Shape.GovRegion, col='white', lwd=0.05)
  plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
  graphics::points(
    spts_CommuCareCentr,
    col='black',
    pch='@',
    cex=0.5
  )
  points(
    test.sptsDataframe,
    col=
      test.sptsDataframe@data$Col,
    pch=2,
    cex=0.4
  )
  for(i in 1:length(Col.uni)){
    plot(Shape.GovRegion, col='white', lwd=0.05)
    plot(Shape.SchoolRegion, col='ivory', lwd=0.05, add=TRUE)
    graphics::points(
      spts_CommuCareCentr,
      col='black',
      pch='@',
      cex=0.5
    )
    graphics::points(
      test.sptsDataframe[test.sptsDataframe@data$Col==Col.uni[i],],
      col=
        test.sptsDataframe@data$Col[test.sptsDataframe@data$Col==Col.uni[i]],
      pch=2,
      cex=1
    )
  }

  # Histogram ---------------------------------------------------------------

  ggdata <-
    long.long.df.res.distm.rank_1.merge_mesh_on_pharm %>%
    mutate(
      ID.pref = gsub('(.+)_(.+)', '\\1', Ph.ID)
    ) %>%
    ggplot(
      aes(x=val)
    )

  plot(
    ggdata +
      geom_histogram(aes(fill=Col)) +
      scale_fill_identity(guide = "legend") +
      scale_x_continuous(trans = 'log10') +
      facet_grid(ID.pref~., scales = 'free_y') +
      theme_bw()
  )
  dev.off()

}

