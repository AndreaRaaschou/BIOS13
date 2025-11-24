#' Calculates the proportions of the nucleotides in a sequence and prints the resutls
#' @param seq vector with sequence
nucleo_content <- function(seq){
  # Count the frequency of the different nucleotides
  freq <- count(seq, 1, freq = TRUE)
  
  # Print frequencies
  for (i in 1:length(freq)) {
    cat(sprintf("%s : %.1f %%\n", toupper(names(freq[i])), 100*as.numeric(freq[i])))
  }
}

#' Creates the complementary strand of a sequence
#' @param seq vector with sequence
#' @returns the complementary strand
create_complement <- function(seq){
  # Use the comp() functions from seqinr package to get the complementary strand, then reverse it
  comp_seq <- rev(comp(seq))
  return((comp_seq))
}

#' Find the positions of a codon in a sequence
#' @param seq vector with DNA sequence
#' @param codon string of length 3
#' @param read_fr int (1, 2 or 3) that controls the reading frame
#' @returns vector of positions of the codon in the sequence
find_codon <- function(seq, codon, read_fr){
  # Make the codon string into a vector of three characters
  cod <- tolower(strsplit(codon,'')[[1]])
  
  # Initiate empty vector to hold the positions of matching codons 
  pos <- c()
  
  # Loop through the sequence, looking at one reading frame at a time
  for (i in 0:((length(seq)-1) %/% 3)){
    # If the codon matches the current reading frame ...
    if (all(cod == seq[(i*3 + read_fr):(i*3 + read_fr + 2)])){
      # ... add the position to the pos vector
      pos <- append(pos, i*3 + read_fr)
    }
  }
  return(pos)
}

#' Search for ATG, ATA, ATT, GTG
find_start_codon <- function(seq, read_fr){
  # Find positions for the start-codons separately
  pos_ATG <- find_codon(seq, 'atg', read_fr)
  pos_ATA <- find_codon(seq, 'ata', read_fr)
  pos_ATT <- find_codon(seq, 'att', read_fr)
  pos_GTG <- find_codon(seq, 'gtg', read_fr)
  
  # Merge the vectors and sort them in increasing order
  pos <- sort(c(pos_ATG, pos_ATA, pos_GTG, pos_ATT))
  
  # Return vector with starting positions
  return(pos)
}

# Search for AGA, AGG, TAA, TAG
find_stop_codon <- function(seq, read_fr){
  # Find positions for the start-codons separately
  pos_AGA <- find_codon(seq, 'AGA', read_fr)
  pos_AGG <- find_codon(seq, 'AGG', read_fr)
  pos_TAA <- find_codon(seq, 'TAA', read_fr)
  pos_TAG <- find_codon(seq, 'TAG', read_fr)
  
  # Merge the vectors and sort them in increasing order
  pos <- sort(c(pos_AGA, pos_AGG, pos_TAA, pos_TAG))
  
  # Return vector with starting positions
  return(pos)
}

find_all_open_frames <- function(seq, read_fr){
  # Get start and stop positions
  start <- find_start_codon(seq, read_fr) # still missing some start codons
  stop <- find_stop_codon(seq, read_fr)
  
  # Find a reading-frame by taking the first start position and 
  # adding the first following stop position+2 a vector of dimension (1,2)
  first_start <- start[1]
  for (pos in stop){
    if (pos > start[1]){
      first_stop <- pos
      break
    }
  }
  first_stop <- stop
  open_frames <- matrix(c(), nrows = 1, ncols = 2)
  
  # Next, find the next start codon that is bigger than the previous stop codon
  # Add following open reading frames to the vector as new rows
  # Return the vector
}









