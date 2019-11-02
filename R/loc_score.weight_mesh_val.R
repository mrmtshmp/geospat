#' Location score weighting by values on meshes.
#'
#' @import sp
#' @import sf
#' @importFrom geosphere centroid
#' @importFrom geosphere distm
#' @importFrom rgdal readOGR
#' @importFrom plyr ldply
#' @importFrom dplyr select
#' @importFrom tidyr gather
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @param fn.SpatialPointDataFrame RData file of SpatialPolygonDataFrame
#' @param crs_default projection.
#' @param rank.restrict Rank restriction. Default=5.
#' @param select_col.mesh_pop Column selector for mesh data (SpatialPolygonDataFrame@data$...)
#' @param fn.mesh.popEst Filename of mesh data.
#' @param fn.Wakayama_Ph Filename (.csv) of locations.
#' @param fn.RData.output_df.res.distm  A character vector.
#' @param fn.RData.output_long.df.res.distm  A character vector.
#' @param fn.RData.output_long.df.res.distm.with_score  A character vector.
#'
#' @export

# dir.Sub    = "./Sub"
# fn.Sub_require_libraries = "require_libraries.R"
# source(sprintf("%s/%s", dir.Sub, fn.Sub_require_libraries))

loc_score.weight_mesh_val <- function(

  fn.SpatialPointDataFrame  = '../Output/DataForMap.Wakayama_v01.RData',

  crs_default = '+init=epsg:4612',

  # Settings for analysis ---------------------------------------------------

  rank.restrict = 5,
  select_col.mesh_pop = c(
    "PTC_2020", "PTD_2020", "PTE_2020"
  ),

  # Input

  fn.mesh.popEst = c(
    "../Data/500m_mesh_suikei_2018_shape_24/500m_mesh_2018_24.shp",
    "../Data/500m_mesh_suikei_2018_shape_27/500m_mesh_2018_27.shp",
    "../Data/500m_mesh_suikei_2018_shape_29/500m_mesh_2018_29.shp",
    "../Data/500m_mesh_suikei_2018_shape_30/500m_mesh_2018_30.shp"
  ),

  fn.Wakayama_Ph = "Wakayama_MasterAnaData.csv",

  fn.RData.output_df.res.distm = NULL,
  fn.RData.output_long.df.res.distm = NULL,
  fn.RData.output_long.df.res.distm.with_score = NULL
  ){

  ANS <- readline(
    "This programme will take very long runtime. Would you stop it? [Yes/No]"
    )
  if (ANS != "No") stop('Proccess was stopped by user.')

  # Load data ---------------------------------------------------------------

  load(
    file = fn.SpatialPointDataFrame
  )

  print("Loading (a) shape file(s).")
  for(i in 1:length(fn.mesh.popEst)){
    i.Shape.mesh.pop_Est <-
      rgdal::readOGR(
        sprintf(
          "%s",
          fn.mesh.popEst[i]
        )
      )
    if(i==1){
      Shape.mesh.pop_Est <- i.Shape.mesh.pop_Est
    }else{
      Shape.mesh.pop_Est <- rbind(
        Shape.mesh.pop_Est,
        i.Shape.mesh.pop_Est
      )
    }
  }

  centroids <-
    large.extractCoords(
      Shape.mesh.pop_Est
    )

  ADS <- Shape.mesh.pop_Est@data
  ADS <- cbind(centroids, ADS)

  ADS.spdf <-
    SpatialPointsDataFrame(
      coords = data.frame(ADS$lon,ADS$lat),
      data = ADS,
      proj4string = CRS(crs_default)
    )

  rownames(sptsDataframe@data) <-
    make.names(
      sprintf(
        "_%s_%s.%s",
        round(sptsDataframe@coords[,1],2),
        round(sptsDataframe@coords[,2],2),
        1:nrow(sptsDataframe@coords)
      )
    )

  print("Running geosphere::distm() for large dataset.")
  mat.res.distm <- geosphere::distm(
    x = ADS.spdf,
    y = sptsDataframe,
    fun = distGeo
  )

  df.res.distm <-
    as.data.frame(
      mat.res.distm
    )

  colnames(df.res.distm) <-
    rownames(
      sptsDataframe@coords
    )
  rownames(df.res.distm) <-
    Shape.mesh.pop_Est@data$MESH_ID


  weight.ID <-
    Shape.mesh.pop_Est@data %>%
    dplyr::select(
      MESH_ID,
      eval(select_col.mesh_pop)
    )

  rm(Shape.mesh.pop_Est)

  ## *CAUTION Running this pipe takes long time.

  print("*CAUTION Running this pipe takes long time.")
  long.df.res.distm <-
    df.res.distm %>%
    rownames_to_column(
      "MESH_ID"
    ) %>%
    gather(
      Ph.ID, dist,
      -MESH_ID
    ) %>%
    arrange(
      MESH_ID,
      dist
    ) %>%
    ddply(
      .(MESH_ID),
      function(MESH){
        res <- rank(MESH$dist)
        MESH$rank <- res
        return(MESH)
      },
      .progress = "text"
    )
  ## ## ## ## ## ## ## ## ##

  print("FIN_*CAUTION Running this pipe takes long time.")

  rm(df.res.distm)

  if(!is.null(fn.RData.output_df.res.distm)){
    save(
      long.df.res.distm,
      file = fn.RData.output_df.res.distm
    )
    }


  # dist.sum ----------------------------------------------------------------

  long.df.res.distm.rank_1 <-
    long.df.res.distm[
      long.df.res.distm$rank <= rank.restrict,
      ] %>%
    left_join(
      weight.ID,
      by = "MESH_ID"
    )

  rm(long.df.res.distm)

  if(!is.null(fn.RData.output_long.df.res.distm)){
    save(
      long.df.res.distm.rank_1,
      file = fn.RData.output_long.df.res.distm
    )
    }

  long.df.res.distm.rank_1.score_col <-
    long.df.res.distm.rank_1 %>%
    ddply(
      .(MESH_ID),
      function(MESH){
        score <- sum(MESH$dist**2)/MESH$dist**2
        return(data.frame(score))
      }
    )

  long.df.res.distm.rank_1 <-
    cbind(
      long.df.res.distm.rank_1,
      long.df.res.distm.rank_1.score_col %>%
        dplyr::select(score)
    )

  if(!is.null(fn.RData.output_long.df.res.distm.with_score)){
    save(
      long.df.res.distm.rank_1,
      file = fn.RData.output_long.df.res.distm.with_score
    )
    }

  # Merge score on each Pharmacy (weighted by population).

  long.df.res.distm.rank_1.merge_mesh_on_pharm <-
    long.df.res.distm.rank_1 %>%
    mutate(
      DUMMY = 1
    ) %>%
    ddply(
      .(Ph.ID),
      function(Ph){
        score.merged <- apply(
          X = matrix(c("DUMMY",select_col.mesh_pop),nrow = 1),
          MARGIN = 2,
          function(col){
            score.merged <- sum(Ph[,col] * Ph[,"score"])
            res <- assign(
              x = sprintf("score.merged_%s",col),
              score.merged
            )
            return(res)
          }
        )
        return(score.merged)
      }
    )
  colnames(long.df.res.distm.rank_1.merge_mesh_on_pharm) <-
    c("Ph.ID", "DUMYY",sprintf("score.merged_%s",select_col.mesh_pop))

  long.long.df.res.distm.rank_1.merge_mesh_on_pharm <-
    long.df.res.distm.rank_1.merge_mesh_on_pharm %>%
    gather(
      weight, val, -Ph.ID
    )

  return(
    list(
      long.df.res.distm.rank_1,
      long.long.df.res.distm.rank_1.merge_mesh_on_pharm,
      rank.restrict
    )
  )
}

