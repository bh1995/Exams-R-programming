# Problem 2

cluster <- setRefClass("cluster", 
    fields = list(
      clusters = "list",
      n_clusters = "numeric",
      n_points = "numeric",
      Lp_distance = "numeric"
    ),
    
    methods = list(
      initialize = function(p)
      {
        stopifnot(class(p)=="numeric")
        clusters <<- list(Clusters = as.character(), Clu_center = as.character(), n_points = as.numeric(), observations = as.numeric(), belong = as.character(), distance = as.numeric(), unique_ID = as.character())
        n_clusters <<- n_clusters
        n_points <<- n_points
        i <- 0
        j <- 0
        Lp_distance <<- abs((clusters$observation[i] - clusters$observation[j]))^p
      }, 
      
      create_cluster = function(cluster_center)
      {
        stopifnot(class(cluster_center)=="numeric")
        clusters$Clusters <<- append(clusters$Clusters, cluster_center)
        n_clusters <<- n_clusters + 1
        
      }
    )
)

build_data_object <- function(p)
{
  b <- cluster$new(p)
}