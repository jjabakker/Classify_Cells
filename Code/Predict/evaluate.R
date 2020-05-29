'
######################################################################################################################

######################################################################################################################

'


min_prob_value = 0.6
min_prob_ratio = 4

# Overrule the classification if the probability is too low
prob_max   <- rowMaxs(as.matrix(probability))
prob_ratio <- apply(probability,
                    1,
                    function (x) {
                      sp = sort(x, decreasing = TRUE)
                      return (sp[1]/sp[2])
                    })
reliable                     <- prob_max > min_prob_value  | prob_ratio > min_prob_ratio
corrected_classes            <- predicted_classes
levels(corrected_classes)    <- c(levels(predicted_classes), "Unassigned")
corrected_classes[!reliable] <- "Unassigned"
correct                      <-  as.character(predicted_classes) == as.character(labels[,1])

# Get the prediction result nicely together
Predicted  <- cbind("ReferenceClass" = labels[,1], 
                    "PredictedClass" = predicted_classes, 
                    "Reliable"       = reliable,
                    "CorrectedClass" = corrected_classes, 
                    "Correct"        = correct,
                    "Max"            = prob_max, 
                    "Ratio"          = prob_ratio,
                    probability)

assigned_cells    <- Predicted[Predicted$CorrectedClass != "Unassigned",]

predicted_count   <- dim(probability)[1]
assigned_count    <- sum(reliable)
unassigned_count  <- predicted_count - assigned_count 
correct_predicted <- sum(as.character(Predicted$PredictedClass) == as.character(Predicted$ReferenceClass))
correct_assigned  <- sum(as.character(assigned_cells$PredictedClass) == as.character(assigned_cells$ReferenceClass))

correct_predicted_percentage <- (correct_predicted / predicted_count) * 100
correct_assigned_percentage  <- (correct_assigned / assigned_count) * 100
unassigned_percentage        <- unassigned_count / predicted_count * 100

acinar_overall = mean(Predicted[Predicted$PredictedClass == "acinar","Max"])

class_summary <- data.frame(
  average_prob = numeric(),
  prob_correct = numeric(),
  prob_incorrect = numeric(),
  prob_reliable = numeric(),
  prob_unreliable = numeric(),
  nr_correct = numeric(),
  nr_incorrect = numeric(),
  nr_reliabe = numeric(),
  nr_unreliabe = numeric())

for (class in colnames(probability)) {
  new_rec = data.frame(
    average_prob = mean(Predicted[which((Predicted$PredictedClass == class)), "Max"]),
    prob_correct = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Correct == TRUE)), "Max"]),
    prob_incorrect = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Correct == FALSE)), "Max"]),
    prob_reliable = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == TRUE)), "Max"]),
    prob_unreliable = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == FALSE)), "Max"]),
      
    nr_correct = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Correct == TRUE)), ]),
    nr_incorrect = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Correct == FALSE)), ]),
    nr_reliabe = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == TRUE)), ]),
    nr_unreliabe = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == FALSE)), ]))
  rownames(new_rec) <- class
  if ((new_rec$nr_correct + new_rec$nr_incorrect) != 0) {
    class_summary <- rbind(class_summary, new_rec)
  }
}

colnames(class_summary) = c('Average Prob', 
                            'Prob (Correct)',
                            'Prob (incorrect)',
                            'Prob (Reliable)',
                            'Prob (Unreliable)',
                            'Nr  (Correct)',
                            'Nr  (Incorrect)',
                            'Nr  (Reliabe)',
                            'Nr (Unreliabe)')
class_summaries[[paste0(method,"-",dataset_name,  "-", model_name)]] = class_summary

# Plot the results 

CorrectlyPredicted   <- Predicted[Predicted$Correct == TRUE,]
IncorrectlyPredicted <- Predicted[Predicted$Correct == FALSE,]

p1 <- ggplot() + 
  geom_jitter(data = CorrectlyPredicted, 
              mapping = aes(x = PredictedClass, y = Max),
              width = 0.3,
              height = 0,
              alpha = 0.7,
              size  = 0.6,
              color = 'green') +
  geom_jitter(data = IncorrectlyPredicted, 
              mapping = aes(x = PredictedClass, y = Max),
              width = 0.3,
              height = 0,
              alpha = 0.7,
              size  = 0.6,
              color = 'red') +
  labs(x = "Predicted") +
  labs(y = "Probability of prediction") +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1)) 

p2 <- ggplot() + 
  geom_jitter(data = CorrectlyPredicted, 
              mapping = aes(x = ReferenceClass, y = Max),
              width = 0.3,
              height = 0,
              alpha = 0.7,
              size  = 0.6,
              color = 'green') +
  geom_jitter(data = IncorrectlyPredicted, 
              mapping = aes(x = ReferenceClass, y = Max),
              width = 0.3,
              height = 0,
              alpha = 0.7, 
              size  = 0.6,
              color = 'red') +
  labs(x = "Reference") +
  labs(y = "Probability of prediction") +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1)) 

title_string <- sprintf("Dataset '%s' predicted with model data '%s', using method '%s' (%d features, PCA threshold of %2.1f)", 
                        dataset_name, model_name, method, features_limit, PCA_Threshold)
grid.arrange(p1, p2, ncol=2, top = title_string)




######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
# New addition: careful!
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################



######################################################################################################################
# Get the confusion matrix
######################################################################################################################

# Before callingconfusionMatrix add missing classes to make sure bot Predicted and Regference have the same classes 

levelsP  <- levels(Predicted$PredictedClass)
levelsR  <- levels(Predicted$ReferenceClass)
add_to_R <- setdiff(levelsP, levelsR)
add_to_P <- setdiff(levelsR, levelsP)

levels(Predicted$PredictedClass) <- c(levels(Predicted$PredictedClass), add_to_P)
levels(Predicted$ReferenceClass) <- c(levels(Predicted$ReferenceClass), add_to_R)

cm <- confusionMatrix(Predicted$PredictedClass,
                      Predicted$ReferenceClass,
                      mode = "everything",
                      dnn = c("Predicted", "Reference"))

cat(sprintf("\n\n\n\n"))
cat(sprintf("*************************************************************************************************************\n"))
cat(sprintf("Confusion Matrix for %s, model %s, and dataset %s\n", method, model_name, dataset_name))
cat(sprintf("*************************************************************************************************************\n\n"))
print(cm)


######################################################################################################################
# Calculate median F1
######################################################################################################################

F1_1         <- cm[["byClass"]][,c("Precision","Recall")]
F1_2         <- F1_1[complete.cases(F1_1),]
if  (class(F1_2) == "numeric") {
  F1_3         <- 2 * (F1_2["Precision"] * F1_2["Recall"]) / (F1_2["Precision"] + F1_2["Recall"])
} else {
  F1_3         <- 2 * (F1_2[ , "Precision"] * F1_2[ , "Recall"]) / (F1_2[ ,"Precision"] + F1_2[ ,"Recall"])
}
medianF1     <- median(F1_3, na.rm = TRUE)
meanF1       <- mean(F1_3, na.rm = TRUE)




######################################################################################################################
# Fill the report-out table
######################################################################################################################

Pred1        <- Predicted[ ,c("Reliable", "Correct")]
Accuracy     <- dim(Pred1[which(Pred1$Correct == TRUE),])[1] / dim(Pred1)[1]

Pred2        <- Predicted[Predicted$Reliable == TRUE, c("Reliable", "Correct")]
CorrAccuracy <- dim(Pred2[which(Pred2$Correct == TRUE),])[1]  / dim(Pred2)[1]
new          <- data.frame(Method       = method,
                           ModelData    = model_name,
                           TestData     = dataset_name,
                           Accuracy     = Accuracy,
                           CorrAccuracy = CorrAccuracy,
                           Confidence   = mean(Predicted$Max),
                           medianF1     = medianF1,
                           meanF1       = meanF1) 

report_out   <- rbind(report_out, new)
