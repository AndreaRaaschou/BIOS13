is_palindrome <- function(str){
  pal = FALSE
  chrs <- unlist(strsplit(str, split = ''))
  rev_chrs <- rev(chrs)
  if (identical(chrs, rev_chrs)){
    pal = TRUE
  }
  return (pal)
}