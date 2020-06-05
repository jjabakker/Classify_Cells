
'
######################################################################################################################
The script reads in a data and labels fil in csv format.

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

read_dataset <- function (data_path, dataset_name) {
   
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
  
