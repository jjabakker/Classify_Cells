'
######################################################################################################################
The script combines two datesets, (data1, labels1) and (data2, labels2), into one combine dataset (data, labels)
The rows are simpy added.
The columns are merged.
The merging is likely to produce NA values, which are replaced by numeric 0s
######################################################################################################################
'

# Scale the data before combining or not 
SCALE_CENTER_PREPROC <- FALSE            

logger.info("Info: Started combining data from dataset %s and dataset %s", dataset_name, dataset_name2)
tic(msg = "", quiet = TRUE)

if (SCALE_CENTER_PREPROC)  {
  preproc_model <- preProcess(data1,
                              method  = c('scale', 'center'),
                              verbose = TRUE)
  data1         <- predict(preproc_model, data1)
  
  preproc_model <- preProcess(data2,
                              method  = c('scale', 'center'),
                              verbose = TRUE)
  data2         <- predict(preproc_model, data2)
}

# For the data tables, bring the rownames in column 'name' and make it the first column
# This is needed for temporary saving the rownames  
data1$name        <- rownames(data1)
data1             <- select(data1, name, everything())
data2$name        <- rownames(data2)
data2             <- select(data2, name, everything())

# rbindlist is doing the work
data              <- rbindlist(list(data1, data2), use.names = TRUE, fill = TRUE)
data              <- column_to_rownames(data, var = "name")
data[is.na(data)] <- 0

# Then process the labbels
labels            <- rbind(labels1, labels2)
rownames(labels)  <- labels[,"cell_id"]

# Report results
cat(sprintf("\nInformation on combined datasets %s and %s\n", dataset_name1, dataset_name2))
cat(sprintf("There are %d cells and %d features.\n", dim(data)[1], dim(data)[2]))
for (i in 1:length(table(labels$ident))) {
  cat(sprintf("%-25s %d\n", names(table(labels$ident)[i]), table(labels$ident)[i]))
}

toc(log = TRUE, quiet = TRUE)
logger.info("Info: Finished combining data from dataset %s and dataset %s%s", dataset_name, dataset_name2, tic.log()[[1]])
tic.clearlog()

