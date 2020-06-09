
####################################################################################################################
# Single model
####################################################################################################################

save_variability_model <- function (path, 
                               model_dataset_name, 
                               learning_method, 
                               preproc_method, 
                               model, 
                               preproc_model,
                               top_genes,
                               features_limit,
                               trainRowNumbers) {
  model_info <- list(model           = model,
                     preproc_model   = preproc_model,
                     top_genes       = top_genes,
                     features_limit  = features_limit,
                     trainRowNumbers = trainRowNumbers)
  
  save(model_info, 
       file = file.path(path, paste0("VariabilityModels/model_", model_dataset_name, "_", learning_method, "_", preproc_method, ".rData")))
}


load_variability_model <- function(rdata_path, model_name, method_name, preproc_method) {
  
  m_name <- file.path(rdata_path, "VariabilityModels", paste0("model_", model_name, "_", method_name, "_", preproc_method, ".rData"))
  if (file.exists(m_name)) {
    load(file = m_name)
  } else {
    err <- sprintf("Model file %s could not be read", m_name)  
    stop(err)
  }
  return ( list(model_information = model_info))
}


####################################################################################################################
# Cluster model
####################################################################################################################

save_cluster_model <- function (path, 
                                model_dataset_name, 
                                learning_method, 
                                preproc_method, 
                                model, 
                                preproc_model,
                                top_genes,
                                features_limit,
                                marker_list,
                                cluster_table,
                                weighted_coherence,
                                trainRowNumbers) {
  model_info <- list(model              = model,
                     preproc_model      = preproc_model,
                     trainRowNumbers    = trainRowNumbers,
                     top_genes          = top_genes,
                     features_limit     = features_limit,
                     marker_list        = marker_list,
                     weighted_coherence = weighted_coherence,
                     cluster_table      = cluster_table)
  
  save(model_info, 
       file = file.path(path, paste0("ClusterModels/model_", model_dataset_name, "_c_", learning_method, "_", preproc_method, ".rData")))
}



load_cluster_model <- function(rdata_path, model_name, method_name, preproc_method) {
  
  m_name <- file.path(rdata_path, "ClusterModels", paste0("model_", model_name, "_c_", method_name, "_", preproc_method, ".rData"))
  if (file.exists(m_name)) {
    load(file = m_name)
  } else {
    err <- sprintf("Model file %s could not be read", m_name)  
    stop(err)
  }
  return ( list(model_information = model_info))
}



####################################################################################################################
# Report Data
####################################################################################################################

save_report_data <- function(report_out,
                             path,
                             file_name) {
  save(report_out,
       file = file.path(path, paste0("ReportData/report_", file_name, ".rData")))
  
}


load_report_data <- function(path,
                             model_dataset_name) {
  load(file.path(path, paste0("ReportData/report_", model_dataset_name, ".rData")))
  return (report_out)
}



####################################################################################################################
# Single Data
####################################################################################################################


save_single_data <- function(rdata_path, dataset_name, data, labels) {
  
  data_information = list(
    data            = data,
    labels          = labels,
    dataset_name    = dataset_name)

  save(data_information,
       file = file.path(rdata_path, "SingleData", paste0(dataset_name, "_dataset.rData")))
}
    
load_single_data <- function(rdata_path, dataset_name) {

  d_name <- file.path(rdata_path, "SingleData", paste0(dataset_name, "_dataset.rData"))
  if (file.exists(d_name)) {
    load(file = d_name)
  } else {
    err <- sprintf("Model file %s could not be read", d_name)
    stop(err)
  }
  return ( list (data   = data_information$data,
                 labels = data_information$labels))

}



'
######################################################################################################################
The function reads in a data and labels fil in csv format.

Cell and feauture names are brought in a standard format:
   All lowercase
   Blanks, "_" and "-" are replaced  by "."
   Trailing chromosome info is removed.

The script expects a variable dataset_name that holds the dataset that needs to be read

The script returns two dataframes:
   data
   labels (with one column named "ident", and a second colum cell_id (equal to the rownames)

Both data frames hold as row name the cell id. 
######################################################################################################################
'

read_csv_dataset <- function (data_path, dataset_name) {
  
  # Read the data and labels and format names
  data      <- read.csv(file.path(data_path, dataset_name, "Data.csv"), 
                        stringsAsFactors = FALSE, 
                        row.names = 1)
  labels    <- read.csv(file.path(data_path, dataset_name, "Labels.csv"), 
                        stringsAsFactors = FALSE)
  
  # Convert gene names to lower case
  colnames(data) <- apply(as.matrix(colnames(data)), 1, function(x) str_to_lower( x))
  
  # Strip out the chromosome part - useful in case the names of the gene is of the type 'genename__chr12' 
  colnames(data) <- apply(as.matrix(colnames(data)), 1, function(x) str_split(x, "__chr")[[1]][1])
  
  # There is a risk that you introduce duplicates though. Check for that and correct
  # It happens in Muraro where you have genes witb the same name on the X and Y chromosome
  
  dup            <- duplicated(colnames(data))
  dup_index      <- which (dup == TRUE)
  for (d in dup_index) {
    colnames(data)[d] <- paste0(colnames(data)[d], ".problem")
  }
  
  # Make sure there are no underscores or dashes in cell names
  rownames(data) = apply(as.matrix(rownames(data)), 1, function(x) str_replace_all( x, "[-_]", "."))
  
  # Make sure that cell names are lower case
  rownames(data)      <- str_to_lower(rownames(data))
  
  # Make sure there are no blanks or underscores in gene names
  colnames(data)      <- apply(as.matrix(colnames(data)), 1, function(x) str_replace_all( x, "[-_]", "."))
  
  # Make sure there are no blanks or dashes in class names
  labels              <- apply(labels, 2, function(x) str_replace_all( x, "[ -]", "."))
  labels              <- as.matrix(labels)
  
  # Give the labels the cell names
  rownames(labels)    <- rownames(data)
  
  # Make sure you leave data frames and name the first column of labels
  labels              <- as.data.frame(labels)
  colnames(labels)[1] <- "ident"
  data                <- as.data.frame(data)
  
  # Add a second column to labels to avoid R's weird behaviours with one column dataframes
  labels$cell_id      <- rownames(labels)
  
  # Show results
  cat(sprintf("Information on dataset %s\n", dataset_name))
  cat(sprintf("There are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
  for (i in 1:length(table(labels$ident))) {
    cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
  }
  cat(sprintf("\n"))
  
  return (list(data   = data, 
               labels = labels))
}








