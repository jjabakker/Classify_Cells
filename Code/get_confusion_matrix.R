######################################################################################################################
# Get the confusion matrix
######################################################################################################################

# Before calling confusionMatrix add missing classes to make sure bot Predicted and Regference have the same classes 

GetConfusionMatrix <- function(PredictedClass, ReferenceClass) {
  
  levelsP  <- levels(PredictedClass)
  levelsR  <- levels(ReferenceClass)
  add_to_R <- setdiff(levelsP, levelsR)
  add_to_P <- setdiff(levelsR, levelsP)
  
  levels(PredictedClass) <- c(levels(PredictedClass), add_to_P)
  levels(ReferenceClass) <- c(levels(ReferenceClass), add_to_R)
  
  cm <- confusionMatrix(PredictedClass,
                        ReferenceClass,
                        mode = "everything",
                        dnn  = c("Predicted", "Reference"))
  return(cm)
}