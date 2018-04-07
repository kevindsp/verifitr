#' Convert Verifit xml to csv
#'
#' Takes input arguments of an xml file as exported by the Verifit and a
#' 'method' (see below), and creates a csv file containing results in the
#' current working directory. The csv file will share a filename with the input
#' file name.  The original xml file is preserved.
#'
#' @param filename An xml file exported from the Verifit
#' @param method speechmap_to_df or remdirectional_to_df
#'
#' @return Nothing can be directly returned to the R environment.
#'
#' @export verifit_to_csv

verifit_to_csv <- function(filename,
                           method = c(speechmap_to_df,
                                      remdirectional_to_df,
                                      boxdirectional_to_df)) {

  if (length(method) == 1){
    lambda <- as.function(method)
  } else {
    stop('Cannot pass an object of length > 1 to method argument.')
  }

  data_out <- lambda(filename)

  csv_filename <- stringr::str_replace(filename,
                                       '.xml',
                                       '.csv')
  k <- 0

  while(file.exists(csv_filename)){
    k <- k+1
    csv_filename <- stringr::str_replace(filename,
                                         '.xml',
                                         '')
    csv_filename <- paste(csv_filename,
                          '(',
                          k,
                          ').csv',
                          sep = '')
  }
  utils::write.csv(data_out,
                   file = csv_filename,
                   row.names = FALSE)
}
