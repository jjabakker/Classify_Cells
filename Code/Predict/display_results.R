'
######################################################################################################################

######################################################################################################################

'

prob_max          <- rowMaxs(as.matrix(probability))
probability       <- cbind(probability, TotalProb = rowSums(probability))
probability

min_prob_value    <- 0.6

reliable                     <- prob_max > min_prob_value
corrected_classes            <- predicted_classes
levels(corrected_classes)    <- c(levels(predicted_classes), "Unassigned")
corrected_classes[!reliable] <- "Unassigned"
correct                      <- as.character(predicted_classes) == as.character(labels[,1])


######################################################################################################################
# Get the prediction result nicely together in the 'Predicted' table
######################################################################################################################

Predicted  <- cbind("PredictedClass" = predicted_classes, 
                    "Reliable"       = reliable,
                    "CorrectedClass" = corrected_classes, 
                    "Max"            = prob_max, 
                    probability)

plot_result <- ggplot() + 
  geom_jitter(data    = Predicted[Predicted$Reliable == TRUE,], 
              mapping = aes(x = PredictedClass, y = Max),
              width   = 0.2,
              height  = 0,
              alpha   = 0.8,
              size    = 0.7,
              color   = 'black') +
  geom_jitter(data    = Predicted[Predicted$Reliable == FALSE,], 
              mapping = aes(x = PredictedClass, y = Max),
              width   = 0.2,
              height  = 0,
              alpha   = 0.6,
              size    = 0.6,
              color   = 'grey') +
  labs(x = "Predicted") +
  labs(y = "Probability of prediction") +
  scale_y_continuous(limits = c(0, 1.0), breaks = seq(0, 1.0, 0.1)) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust  = 1)) 


######################################################################################################################
# Store information in the class summary table
######################################################################################################################

class_summary <- data.frame(
  average_prob    = numeric(),
  prob_reliable   = numeric(),
  prob_unreliable = numeric(),
  nr_reliabe      = numeric(),
  nr_unreliabe    = numeric())

for (class in colnames(probability)) {
  if (!is_empty(Predicted[which((Predicted$PredictedClass == class)), "Max"])) {
    new_rec = data.frame(
      average_prob    = mean(Predicted[which((Predicted$PredictedClass == class)), "Max"],),
      prob_reliable   = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == TRUE)), "Max"]),
      prob_unreliable = mean(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == FALSE)), "Max"]),
      
      nr_reliable     = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == TRUE)), ]),
      nr_unreliabe    = nrow(Predicted[which((Predicted$PredictedClass == class) & (Predicted$Reliable == FALSE)), ]))
    rownames(new_rec) <- class
    if ((new_rec$nr_reliable + new_rec$nr_unreliabe) != 0) {
      class_summary <- rbind(class_summary, new_rec)
    }
  }
}
colnames(class_summary) = c('Average Prob', 
                            'Prob (Reliable)',
                            'Prob (Unreliable)',
                            'Nr (Reliabe)',
                            'Nr (Unreliabe)')
class_summaries[[paste0(method, "-", dataset_name,  "-", model_name)]] = class_summary
