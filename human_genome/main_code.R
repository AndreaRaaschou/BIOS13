library('seqinr')
source("functions.R")

# Clear all variables
rm(list=ls())

# Set working directory and load fasta file
setwd("/Users/andrearaaschou/courses/BIOS13/github/human_genome")
mito_fasta <- read.fasta('datasets/NC_012920.fasta')

# Extract sequence 
mito_seq <- mito_fasta$NC_012920.1

print(length(mito_seq))

# Create complementary strand
comp_strand <- create_complement(mito_seq)

# Print nucleotide frequencies of both strands
nucleo_content(mito_seq)
nucleo_content(comp_strand)


# Check that the codon functions work correctly 
find_codon( mito_seq, 'ATG', 3 )
find_start_codon(mito_seq[1:500],1) # still missing some start codon
find_stop_codon(mito_seq[1:500],1)












