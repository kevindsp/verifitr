#' Return a numeric vector of data under an xml node
#'
#' Verifit data is interpreted by R to be a single string.  This function takes
#' an xml node (returned from \code{XML::xmlRoot}) and converts the content into
#' a numeric list.
#'
#' @param x A node returned from \code{XML::xmlRoot} that has no children.
#' @return A numeric vector comprising the data in \code{x}
#' @examples
#' \dontrun{
#' xml_to_numeric(xml_data_top[[a]][[b]][[c]])
#' }
#'
#' @export xml_to_numeric

xml_to_numeric<-function(x){
  temp<-unlist(x)
  temp2<-strsplit(temp,split=' ')
  temp3<-temp2$value
  temp4<-as.numeric(temp3)
  return(temp4)
}
