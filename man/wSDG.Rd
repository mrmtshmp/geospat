% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loc_score.weight_mesh_val.R
\name{wSDG}
\alias{wSDG}
\title{Location score weighting by values on meshes.}
\usage{

  wSDG(fn.SpatialPointDataFrame = "../Output/DataForMap.Wakayama_v01.RData",
  crs_default = "+init=epsg:4612", rank.restrict = 5,
  alg.score = "HmMs", select_col.mesh_pop = c("PTC_2020", "PTD_2020",
  "PTE_2020"),
  fn.mesh.popEst = c("../Data/500m_mesh_suikei_2018_shape_24/500m_mesh_2018_24.shp",
  "../Data/500m_mesh_suikei_2018_shape_27/500m_mesh_2018_27.shp",
  "../Data/500m_mesh_suikei_2018_shape_29/500m_mesh_2018_29.shp",
  "../Data/500m_mesh_suikei_2018_shape_30/500m_mesh_2018_30.shp"),
  fn.LonLat_and_data.facilities = "Wakayama_MasterAnaData.csv",
  fn.RData.output_df.res.distm = NULL,
  fn.RData.output_long.df.res.distm = NULL,
  fn.RData.output_long.df.res.distm.with_score = NULL)
}
\arguments{
\item{fn.SpatialPointDataFrame}{RData file of SpatialPolygonDataFrame}

\item{crs_default}{projection.}

\item{rank.restrict}{Rank restriction. Default=5.}

\item{alg.score}{Selector of scoring algorithm ("HmMs", "nwSS" or any function-class object).}

\item{select_col.mesh_pop}{Column selector for mesh data (SpatialPolygonDataFrame@data$...)}

\item{fn.mesh.popEst}{Filename of mesh data.}

\item{fn.LonLat_and_data.facilities}{Filename (.csv) of locations.}

\item{fn.RData.output_df.res.distm}{A character vector.}

\item{fn.RData.output_long.df.res.distm}{A character vector.}

\item{fn.RData.output_long.df.res.distm.with_score}{A character vector.}
}
\description{
Location score weighting by values on meshes.
}
