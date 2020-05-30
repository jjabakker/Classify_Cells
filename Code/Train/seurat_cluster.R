'
######################################################################################################################

######################################################################################################################

'

## Run PCA, Umap, tSNE

# Note that this is not the PCA that the the train and predict is done with. It is only done to get some clustering insights

run_seurat_cluster <- function (seurat_object) {
  all.genes     <- rownames(seurat_object)
  seurat_object <- ScaleData(seurat_object, features = all.genes)
  seurat_object <- RunPCA(seurat_object, features = VariableFeatures(seurat_object))
  seurat_object <- RunTSNE(seurat_object)
  seurat_object <- RunUMAP(seurat_object, dims = 1:15)
  
  
  # First run FindNeighbors to compute a snn graph
  # If there is no pca information, this fails 
  # Then find the clusters with FindClusters
  # The resolution parameter determines how many clusters you get
  # A smaller number is less clusters
  
  seurat_object  <- FindNeighbors(seurat_object, dims = 1:15)     
  
  #############################################################################################################
  sprintf("Check cluster resolution. Current value is %2.1f", cluster_resolution)
  #############################################################################################################
  
  seurat_object  <- FindClusters(seurat_object, resolution = cluster_resolution)
  head(x = Idents(seurat_object), 8)
  
  
  nr_of_clusters <- length(levels(seurat_object@active.ident[1]))
  cluster_names  <- list()
  cluster_info   <- list()
  
  for (i in 0:(nr_of_clusters - 1)) {
    t  <- table(labels[WhichCells(seurat_object, idents = i),"ident"])
    st <- sum(t)
    mt <- max(t)
    cluster_info[[i+1]] <- t
    
    if (i == 0) {
      print_cluster = as.data.frame(t) %>% column_to_rownames("Var1") 
      colnames(print_cluster) <- i
      print_cluster <- t(print_cluster)
    } else {
      pc = as.data.frame(t) %>% column_to_rownames("Var1")
      colnames(pc) <- i
      pc <- t(pc)
      print_cluster = rbind(print_cluster, pc)
    }
    
    if (mt/st > 0.5) {
      # The cluster is quite uniform
      name               <- names(which(t == mt))
      cluster_names[i+1] <- name
    } else {
      cluster_names[i+1] <- "uncertain"
    }
  }
  
  cat(sprintf("\n\n Clusters have been formed. The coherence of the clusters is shown.\n")) 
  print(cluster_info)
  cat(sprintf("\n\n"))
  
  # This only makes sense when the coherence is really high.....  
  # names(cluster_names) <- levels(seurat_object)
  # seurat_object        <- RenameIdents(seurat_object, unlist(cluster_names))
  
  return (seurat_object)
}
