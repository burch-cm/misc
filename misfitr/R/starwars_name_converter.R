# Star Wars Name Converter
# From a Twitter meme:
# "To find your Star Wars name, remove the fourth letter
# of your LAST name, delete all letters in your last name after the
# fifth, then add that to your first name spelled backwards.


#' Convert to Star Wars Name (a meme)
#' @details From a Twitter meme:
#' "To find your Star Wars name, remove the fourth letter
#' of your LAST name, delete all letters in your last name after the
#' fifth, add a random vowel to the end of it,
#'  then add that to your first name spelled backwards.
#'
#' @param name a character vector of length 1, in format "Firstname Lastname"
#' @param letter a vowel. If blank, a random vowel will be chosen.
#'
#' @return a string of the memeified name in Star Wars meme format
#' @export
#'
#' @examples
#' convert_name("Mike Honcho")
convert_name <- function(name, letter = NA) {
  .simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
  }
  name_vec  <- strsplit(name, ' ')[[1]]
  first     <- name_vec[1]
  last      <- name_vec[length(name_vec)]
  if (is.na(letter)) {
    vowels  <- c('a', 'e', 'i', 'o', 'u')
    letter  <- sample(vowels, 1)
  }
  first_vec <- strsplit(last, split = '')[[1]][-4]
  first_vec <- first_vec[1:min(4, length(first_vec))]
  first_swn <- paste0(paste(first_vec, collapse = ''), letter)

  last_vec  <- tolower(strsplit(first, split = '')[[1]])
  last_swn  <- tolower(paste(rev(last_vec), collapse = ''))

  return(.simpleCap(paste(first_swn, last_swn, sep = ' ')))
}
