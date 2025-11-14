# Takes a sequence as input and calculates te proportions of the nucleotides
# Prints the results
nucleo_content <- function(seq){
  # Count the frequency of the different nucleotides
  freq <- count(seq, 1, freq = TRUE)
  
  # Print frequencies
  for (i in 1:length(freq)) {
    cat(sprintf("%s : %.1f %%\n", toupper(names(freq[i])), 100*as.numeric(freq[i])))
  }
}