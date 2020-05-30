'
######################################################################################################################

######################################################################################################################
'

read_config <- function (config_path, dataset_name) {

  
  ####################################################################################
  # If there is a configuration file read it, otherwise use defaults
  ####################################################################################
  
  config <- file.exists(file.path(config_path, paste0(dataset_name, "_config.csv")))
  
  if (config) {
    config_file_name <- file.path(config_path, paste0(dataset_name, "_config.csv"))
    config_data      <- read.csv(config_file_name, 
                                 header = TRUE,
                                 row.names = 1,
                                 stringsAsFactors = FALSE)
  }
  
  # Ignore cells if they belong to very small classes, i.e.if there are only a few cells per class
  min_class_size     <- ifelse(config, as.integer(config_data["min_class_size", "Value"]),     10)
  
  # Less than 10 really gives a problem
  min_class_size     <- min(min_class_size, 10)
  
  # Parameter to determine the cluster resolution. 
  # Usually between 0.1 and 2. A higher number will generate more clusters
  cluster_resolution <- ifelse(config, as.numeric(config_data["cluster_resolution", "Value"]), 0.5)
  
  # Parameters to set boundaries on the number if genes and transcripts per cell 
  minCount           <- ifelse(config, as.integer(config_data["minCount", "Value"]),           0)
  maxCount           <- ifelse(config, as.integer(config_data["maxCount", "Value"]),          -1)
  minFeature         <- ifelse(config, as.integer(config_data["minFeature", "Value"]),         0)
  maxFeature         <- ifelse(config, as.integer(config_data["maxFeature", "Value"]),        -1)
  
  ####################################################################################
  # Make sure there is a config_data list
  ####################################################################################
  
  if (!config) {
    config_data <- data.frame(Parameter  = c("min_class_size",
                                             "cluster_resolution",
                                             "minCount",
                                             "maxCount",
                                             "minFeature",
                                             "maxFeature"),
                               Value  = c(min_class_size,
                                          cluster_resolution,
                                          minCount,
                                          maxCount,
                                          minFeature,
                                          maxFeature)
                              )
    config_data <- column_to_rownames(config_data, var = "Parameter")
  }
  
  ####################################################################################
  # Read back the variables that will be used
  ####################################################################################
  
  if (config) {
    cat(sprintf("Parameters were read from configuration file %s.\n", config_file_name))
  } else {
    cat(sprintf("Default parameters were used.\n"))
  }
  
  if (min_class_size == -1) {
    cat(sprintf("No limit is applied to minimum class size.\n"))
  } else {
    cat(sprintf("Cells belonging to a class smaller than %d will be ignored.\n", min_class_size))
  }
  
  cat(sprintf("The minimum number of transcripts per cell is set to: %d.\n", minCount))
  cat(sprintf("The maximum number of transcripts per cell is set to: %d.\n", maxCount))
  cat(sprintf("The minimum number of genes per cell is set to      : %d.\n", minFeature))
  cat(sprintf("The maximum number of genes per cell is set to      : %d.\n", maxFeature))
  cat(sprintf("\n\n"))
  
  
  return(config_data )
}


process_config <- function (min_class_size, minCount, maxCount, minFeature, maxFeature, data, labels) {
  
 
  ####################################################################################
  # If a class limit is set, remove cells of a class that is too small
  ####################################################################################
  
  if (min_class_size != -1) {
    
    removed_classes     <- !(table(labels$ident) > min_class_size)
    cells_to_keep       <- !(is.element(labels[,1], names(removed_classes)[removed_classes]))
    
    ori                 <- dim(data)[1]
    data                <- data[cells_to_keep,]
    labels              <- labels[cells_to_keep,]
    labels$ident        <- factor(labels$ident)
    
    rm(removed_classes, cells_to_keep)
    
    # Show composition
    cat(sprintf("\n\nDataset after removing small classes.\n"))
    cat(sprintf("Removed %d cells from small classes \n\n", ori - dim(data)[1] )) 
    cat(sprintf("There are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
    for (i in 1:length(table(labels$ident))) {
      cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
    }
    cat(sprintf("\n"))
  }
  
  
  ####################################################################################
  # Remove cells that have too few or many transcripts or features
  ####################################################################################
  
  # Compile the metadata from the count table
  
  metaData     <-  data.frame(
    nCount_RNA   = rowSums(data),
    nFeature_RNA = (dim(data)[2] - rowCounts(as.matrix(data), value = 0))
  )
  
  p1 <- ggplot(metaData, aes(x=1, y = nFeature_RNA)) + 
    geom_violin(aes(color="red"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4)  
  p2 <- ggplot(metaData, aes(x=1, y = nCount_RNA)) + 
    geom_violin(aes(color="red"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4)
  grid.arrange(p1, p2, ncol=2)
  
  p1 <- ggplot(metaData, aes(x=1, y = nFeature_RNA)) + 
    geom_violin(aes(color="green"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4) +
    geom_hline(yintercept = minFeature, col = 'red')
  if (maxFeature != -1) {
    p1 <- p1 + geom_hline(yintercept = maxFeature, col = 'red')
  }
  p2 <- ggplot(metaData, aes(x=1, y = nCount_RNA)) + 
    geom_violin(aes(color="green"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4) +
    geom_hline(yintercept = minCount, col = 'red')  
  if (maxCount != -1) {
    p2 <- p2 + geom_hline(yintercept = maxCount, col = 'red')
  }
  grid.arrange(p1, p2, ncol=2)
  
  p1 <- ggplot(metaData, aes(x=1, y = nFeature_RNA)) + 
    geom_violin(aes(color="green"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4) +
    geom_hline(yintercept = minFeature, col = 'red')
  if (maxFeature != -1) {
    p1 <- p1 + geom_hline(yintercept = maxFeature, col = 'red')
  }
  p2 <- ggplot(metaData, aes(x=1, y = nCount_RNA)) + 
    geom_violin(aes(color="green"), show.legend = FALSE) + 
    geom_jitter(height = 0, width = 0.4) +
    geom_hline(yintercept = minCount, col = 'red')  
  if (maxCount != -1) {
    p2 <- p2 + geom_hline(yintercept = maxCount, col = 'red')
  }
  grid.arrange(p1, p2, ncol=2)
  
  # Now select the cells to keep and which ones to drop
  cellIndex <- metaData$nCount_RNA   > minCount & 
    metaData$nFeature_RNA > minFeature
  if (maxCount != -1) {
    cellIndex <- cellIndex & (metaData$nCount_RNA   < maxCount)
  }
  if (maxFeature != -1) {
    cellIndex <- cellIndex & (metaData$nFeature_RNA < maxFeature)
  }
  
  ggplot(metaData, aes(x = nCount_RNA, y = nFeature_RNA)) + 
    geom_point(data = metaData[!cellIndex,], colour="red") + 
    geom_point(data = metaData[cellIndex,], colour="black") + 
    labs(title = "No Title")
  
  data    <- data[cellIndex,]
  labels  <- as.data.frame(labels[cellIndex,])
  
  # Show composition
  cat(sprintf("\n\nData set after correction for invalid cells\n"))
  cat(sprintf("Removed %d cells\n\n", sum(cellIndex == FALSE))) 
  cat(sprintf("\nThere are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
  for (i in 1:length(table(labels$ident))) {
    cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
  }
  
  ####################################################################################
  # Remove genes that are 0 for all cells
  ####################################################################################
  
  genes_to_keep <- (colSums(data) != 0)
  dim(data)
  ori           <- dim(data)[2]
  data          <- data[, genes_to_keep]
  dim(data) 
  
  # Show composition
  cat(sprintf("\n\nCorrection for genes that do not occur in any cell\n"))
  cat(sprintf("Removed %d 'all zero' genes\n", ori - dim(data)[2])) 
  cat(sprintf("\nThere are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
  for (i in 1:length(table(labels$ident))) {
    cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
  }
  
  
  return(list (data = data, labels = labels, config_data = config_data))
}
