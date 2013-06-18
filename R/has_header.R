#' Does a csv file have a header
#' 
#' Helps determine if a csv file has a header.
#' 
#' @param csv_file_path Full path to a csv file
#' @export
#' @return Returns TRUE if all values on first row are non-numeric. Otherwise, returns FALSE.
#' @examples
#' # has_header("~/EURUSD.csv")

has_header <- function(csv_file_path) {
    first_line <- read.csv(csv_file_path, header = FALSE, sep = ",", nrows = 1)
    if (sum(sapply(first_line, is.numeric)) > 0) return(FALSE)
    else return(TRUE)
}