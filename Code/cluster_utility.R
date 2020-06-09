

calc_coherence <- function(seurat_object) {
  
  nr_of_clusters     <- length(levels(seurat_object@active.ident[1]))
  cluster_table      <- data.frame()
  weighted_coherence <- 0
  for (i in 0:(nr_of_clusters - 1)) {
    cluster  <- table(labels[WhichCells(seurat_object, idents = i), "ident"]) %>% 
      as.data.frame() %>%
      column_to_rownames(var = "Var1") %>%
      t() %>%
      as.data.frame() 
    sc                    <- sum(cluster)
    mc                    <- max(cluster)

    cluster$Class         <- names(cluster[which(cluster == max(cluster))])
    cluster$Cluster       <- i
    cluster$Coherence     <- mc/sc
    cluster$Count         <- sc
    cluster               <- select(cluster, "Class", "Cluster", "Coherence", "Count", everything())
    cluster               <- select(cluster, "Class", "Cluster", "Coherence", "Count")
    cluster_table         <- rbind(cluster_table, cluster)
    weighted_coherence    <- weighted_coherence + cluster$Coherence * cluster$Count
  }
  weighted_coherence      <- weighted_coherence / sum( cluster_table[, "Count"])
  rownames(cluster_table) <- c(0: (dim(cluster_table)[1] - 1))
  cluster_table           <- cluster_table %>% arrange(Class)
  return ( list(cluster_table      = cluster_table, 
                weighted_coherence = weighted_coherence))
}


find_best_resolution <- function(seurat_object) {
  
  coherence_table <- data.frame()
  
  for (cluster_resolution in seq(0.05, 1, by = 0.05)) {
    seurat_object   <- FindClusters(seurat_object, resolution = cluster_resolution)
    ret             <- calc_coherence(seurat_object)
    row             <- data.frame(resolution = cluster_resolution, coherence = ret$weighted_coherence) 
    coherence_table <- rbind(coherence_table, row)
  }
  max_coherence     <- max(coherence_table[,"coherence"])
  
  max_row           <- which(coherence_table$coherence == max_coherence)[1]
  best_resolution   <- coherence_table[max_row, "resolution"]
  
  for (cluster_resolution in seq(best_resolution - 0.04, best_resolution + 0.04, by = 0.01)) {
    seurat_object   <- FindClusters(seurat_object, resolution = cluster_resolution)
    ret             <- calc_coherence(seurat_object)
    row             <- data.frame(resolution = cluster_resolution, coherence = ret$weighted_coherence) 
    coherence_table <- rbind(coherence_table, row)
  }
  max_coherence     <- max(coherence_table[,"coherence"])
  
  max_row           <- which(coherence_table$coherence == max_coherence)[1]
  best_resolution   <- coherence_table[max_row, "resolution"]
  
  return (list(best_resolution = best_resolution, 
               coherence_table = coherence_table))
}
