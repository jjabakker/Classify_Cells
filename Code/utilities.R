
####################################################################################################################
# Single model
####################################################################################################################

save_single_model <- function (path, 
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
       file = file.path(path, paste0("SingleModels/model_", model_dataset_name, "_", learning_method, "_", preproc_method, ".rData")))
}


load_single_model <- function(rdata_path, model_name, method_name, preproc_method) {
  
  m_name <- file.path(rdata_path, "SingleModels", paste0("model_", model_name, "_", method_name, "_", preproc_method, ".rData"))
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
       file = file.path(path, paste0("SingleClusterModels/model_", model_dataset_name, "_c_", learning_method, "_", preproc_method, ".rData")))
}



load_cluster_model <- function(rdata_path, model_name, method_name, preproc_method) {
  
  m_name <- file.path(rdata_path, "SingleClusterModels", paste0("model_", model_name, "_c_", method_name, "_", preproc_method, ".rData"))
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










