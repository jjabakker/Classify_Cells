save_model <- function (path, 
                        model_dataset_name, 
                        learning_method, 
                        preproc_method, 
                        model, 
                        preproc_model) {
  
  save(model, preproc_model, 
       file = file.path(path, paste0("SingleModels/model_", model_dataset_name, "_", learning_method, "_", preproc_method, ".rData")))
}

load_model <- function (path, 
                        model_dataset_name, 
                        learning_method, 
                        preproc_method) {
  
  load(file = file.path(path, paste0("SingleModels/model_", model_dataset_name, "_", learning_method, "_", preproc_method, ".rData")))
  return ( list(model = model, precproc_model = preproc_model))

}

save_report_data <- function(report_out,
                             path,
                             model_dataset_name) {
  save(report_out,
       file = file.path(path, paste0("ReportData/report_", model_dataset_name, ".rData")))
  
}


load_report_data <- function(path,
                             model_dataset_name) {
  load(file.path(path, paste0("ReportData/report_", model_dataset_name, ".rData")))
  return (report_out)
}
