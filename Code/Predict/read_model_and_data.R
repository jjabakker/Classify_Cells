'
######################################################################################################################

The script reads:

  - the models that were generated with the "model_name" dataset. 
  - the preproc_model that was applied in generating the model
  - the count table from the dataset specified by the "dataset_name"
  - the labels from the dataset specified by the "dataset_name"
  
In the current setup there will up to five models available.   
Note that this script is only suitable for datasets that come with label classification 

######################################################################################################################

'

read_model_and_data <- function(rdata_path, model_name, preproc_method) {
  
  m_name <- file.path(rdata_path, paste0(model_name, "_models_", preproc_method, ".rData"))
  if (file.exists(m_name)) {
    load(file = m_name)
  } else {
    err <- sprintf("Model file %s could not be read", m_name)  
    stop(err)
  }
  
  d_name <- file.path(rdata_path, paste0(dataset_name, "_data_and_labels.rData"))
  if (file.exists(d_name)) {
    load(file = d_name)
  } else {
    err <- sprintf("Data file %s could not be read", d_name)  
    stop(err)
  }
  
  cat(sprintf("Predicting dataset %s with model %s.\n", d_name, m_name))

  return ( list (model = model_information, data = data_information))
  
}