'
######################################################################################################################

The script reads:

  - the models that were generated with the "model_name" dataset. 
  - the preproc_model that wa applied in generating the model
  - the count table from the dataset specified by the "dataseset_name"
  - the labels from the dataset specified by the "dataseset_name"
  
In the current setup there will up to five models available.   
Note that this script is onkly suitable for datasets that come with label classification 

######################################################################################################################

'

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

# From the model file define the following
preproc_model   <- model_information[["preproc_model"]]
model_svmRadial <- model_information[["model_svmRadial"]]
model_svmLinear <- model_information[["model_svmLinear"]]
model_rf        <- model_information[["model_rf"]]
model_gbm       <- model_information[["model_gbm"]]
model_glmnet    <- model_information[["model_glmnet"]]
features_limit  <- model_information[["features_limit"]]
PCA_Threshold   <- model_information[["PCA_Threshold"]]
top_genes       <- model_information[["top_genes"]]

# From the data file define the following
data            <- data_information[["data"]]
labels          <- data_information[["labels"]]
trainRowNumbers <- data_information[["trainRowNumbers"]]

cat(sprintf("Predicting dataset %s with model %s.\n", d_name, m_name))

