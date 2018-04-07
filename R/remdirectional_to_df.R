#' Return on-ear directional data from Verifit xml to data frame
#'
#' Takes an input argument of an xml file as exported by the Verifit, and
#' returns a data frame containing on-ear directional results. Data frame is in
#' long format.
#'
#' @param filename An xml file exported from the Verifit
#' @return A data frame containing the on-ear directional results.
#' @examples
#' \dontrun{
#' df <- remdirectional_to_df("xxxAbcdE_directionality.xml")
#' head(df)
#' }
#'
#' @export remdirectional_to_df

remdirectional_to_df <- function(filename){
  testtype <- 'remdirectional'
  unit <- 'dBspl'
  scale <- '12th'

  data <- XML::xmlTreeParse(filename)
  top <- XML::xmlRoot(data)

  data_out <- data.frame()

  for (i in 2:length(top)){
    children <- XML::xmlChildren(top[[i]])
    top_attributes <- XML::xmlAttrs(top[[i]])

    if (top_attributes['name'] == 'frequencies'){
      for (a in 1:length(top[[i]])){
        attribs <- XML::xmlAttrs(top[[i]][[a]])
        if (attribs['name'] == '12ths'){
          frqs <- xml_to_numeric(top[[i]][[a]][[1]])
        }
      }
    }

    if (top_attributes['name'] == testtype){
      if (length(children) > 0){
        for (k in 1:length(children)){
          child_attributes <- XML::xmlAttrs(children[[k]])

          if (any(names(child_attributes) == 'yunit')){
            if (child_attributes['yunit'] == unit){
              if (any(names(child_attributes) == 'xscale')){
                if (child_attributes['xscale'] == scale){
                  if (!any(names(child_attributes)=='envelope')){
                    response <- xml_to_numeric(top[[i]][[k]][[1]])

                    side <- top_attributes['side']
                    side <- rep(side, length(response))

                    speaker <- child_attributes['speaker']
                    speaker <- rep(speaker, length(response))

                    test_num <- child_attributes['name']
                    test_num <- rep(test_num, length(response))

                    temp_data <- data.frame(ear = side,
                                            test_run = test_num,
                                            speaker = speaker,
                                            freq = frqs,
                                            respl = response)
                    data_out <- dplyr::bind_rows(data_out, temp_data)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(data_out)
}
