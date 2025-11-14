install.packages('seqinr')
library('seqinr')

# Set working directory and load fasta file
setwd("/Users/andrearaaschou/courses/BIOS13/github/human_genome")
mito_fasta <- read.fasta('datasets/NC_012920.fasta')

# Extract sequence 
mito_seq <- mito_fasta$NC_012920.1
