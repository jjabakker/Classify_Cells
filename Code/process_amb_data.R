'
######################################################################################################################
The script does some preprocessing, then trains a GBM model on AMB data and then predicts against the test set.
######################################################################################################################
'
preproc_method <- "pca"

source("Code/predict_model_with_labels.R")

labels$ident        <- labels[ , classification_category] 
labels              <- select(labels, ident, everything())
labels$ident        <- factor(labels$ident)

# Remove small classes
min_class_size      <- 10
removed_classes     <- !(table(labels$ident) > min_class_size)
nr_removed_classes  <- length(removed_classes[removed_classes== TRUE])
cells_to_keep       <- !(is.element(labels[,1], names(removed_classes)[removed_classes]))
ori                 <- dim(data)[1] 
data                <- data[cells_to_keep,]
labels              <- labels[cells_to_keep,]
labels$ident        <- factor(labels$ident)
rm(removed_classes, cells_to_keep)

# Show composition
cat(sprintf("Information on dataset after removing small classes\n"))
cat(sprintf("Removed %d cells from %d small classes\n\n", ori - dim(data)[1] ,nr_removed_classes)) 

cat(sprintf("There are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
for (i in 1:length(table(labels$ident))) {
  cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
}
cat(sprintf("\n"))

# Remove genes that are 0 for all cells
genes_to_keep <- (colSums(data) != 0)
cat(sprintf("\n"))
ori           <- dim(data)[2]
data          <- data[ , genes_to_keep]

# Show composition
cat(sprintf("Information on processed dataset %s\n", dataset_name))
cat(sprintf("Removed %d all-zero genes\n\n",ori - dim(data)[2])) 
cat(sprintf("There are %d cells and %d features.\n\n", dim(data)[1], dim(data)[2]))
for (i in 1:length(table(labels$ident))) {
  cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
}

model_gbm    <- NULL
gbm_accuracy <- 0
method       <- "gbm"  

# Limit the data set to the number of genes specified in 'features_limit' (1000 has been found to be a good choice)
genes_to_keep      <- intersect(top_genes[1:features_limit], colnames(data))
ldata              <- as.data.frame(data[ , genes_to_keep])
ldata              <- cbind.data.frame(labels, ldata)
ldata              <- ldata %>% select(-one_of("Class", "Subclass", "cluster"))
trainRowNumbers    <- createDataPartition(ldata$ident, p = 0.8, list = FALSE)

train_data <- ldata[trainRowNumbers,]
test_data  <- ldata[-trainRowNumbers,]

# Remove genes that are 0 for all cells, you have to do it again, because the split in test and train data  may have introduced 0 genes again!!!
td                      <- train_data[,-1]
genes_to_keep           <- (colSums(td) != 0)
td                      <- td[ , genes_to_keep]
train_data              <- cbind(train_data[,1], td)
colnames(train_data)[1] <- "ident"
# end fix

preproc_model <- preProcess(train_data, 
                            method  = caret_preprocess_method,
                            thresh  = PCA_Threshold,
                            verbose = TRUE)

pp_train_data <- predict(preproc_model, train_data)

model_gbm     <- train(ident ~ ., 
                       data      = pp_train_data, 
                       method    = method,
                       trControl = trainControl("cv", 
                                                number = 5, 
                                                classProbs = TRUE,
                                                verboseIter = TRUE)) 

###########################################################################
# Predict and Evaluate
###########################################################################

ret            <- predict_model_with_labels(method         = method,
                                            preproc_method = preproc_method,
                                            model          = model_gbm,
                                            preproc_model  = preproc_model,
                                            model_name     = dataset_name,
                                            dataset_name   = dataset_name,
                                            data           = test_data, 
                                            labels         = test_data[,1:2], 
                                            report_out     = report_out)

report_out        <- ret$report_out
class_summary     <- ret$predicted_classes
predicted_classes <- ret$predicted_classes
p1                <- ret$p1
p2                <- ret$p2
                      
cm               <- GetConfusionMatrix(test_data[,1],
                                       predicted_classes)
                                       
accuracy         <- cm[["overall"]][["Accuracy"]]


if (class(cm[["byClass"]]) == "numeric") {
  medianF1     <- median(cm[["byClass"]]["F1"], na.rm = TRUE)
} else {
  medianF1     <- median(cm[["byClass"]][,"F1"], na.rm = TRUE)
}


print(class_summary)

