#' Plot Verifit speechmap results
#'
#' Takes the data frame returned by \code{speechmap_to_df} and plots the data
#' using ggplot2.  Data are plotted on a logx scale with color coding for
#' stimulus level.  The function can be configured to either average across ears
#' or separate the ears into facets.
#'
#' @param x A data frame of Verifit speechmap data, as returned from
#' \code{speechmap_to_df}
#' @param separate.ears An optional boolean value to indicate if ears should be
#' plotted separately in facets (\code{TRUE}) or averaged in a single plot
#' (\code{FALSE}).
#' @param main.title An optional string that will be used as a main title,
#' if present.
#' @return A ggplot2 plot of the data in x.
#'
#' @examples
#' \dontrun{
#' df <- speechmap_to_df(speechmap.xml)
#' plot_speechmap(df)
#' }
#'
#' @export plot_speechmap

plot_speechmap <- function(x, separate.ears = FALSE, main.title = NULL){

  x[,2] <- as.factor(x[,2])

  if (separate.ears){
    earlabs <- c('left' = 'Left ear',
                 'right' = 'Right ear')

    plt <- ggplot2::ggplot(data = x) +
            ggplot2::geom_line(stat = 'summary',
                               ggplot2::aes_string(x = 'freq',
                                                   y = 'respl',
                                                   color = 'stim_level')) +
            ggplot2::scale_x_log10(limits = c(100,10000),
                                   minor_breaks = c(seq(100,1000,100),
                                                    seq(1000,10000,1000))) +
            ggplot2::scale_color_hue(name = 'Stim level') +
            ggplot2::facet_grid(stats::as.formula(paste('ear', '~.')),
                                labeller = ggplot2::labeller(ear = earlabs)) +
            ggplot2::labs(x = 'Frequency [Hz]',
                          y = 'Real ear SPL (dB SPL)',
                          title = main.title)
  } else {
    plt <- ggplot2::ggplot(data = x) +
            ggplot2::geom_line(stat = 'summary',
                               ggplot2::aes_string(x = 'freq',
                                                   y = 'respl',
                                                   color = 'stim_level')) +
            ggplot2::scale_x_log10(limits = c(100,10000),
                                   minor_breaks = c(seq(100,1000,100),
                                                    seq(1000,10000,1000))) +
            ggplot2::scale_color_hue(name = 'Stim level') +
            ggplot2::labs(x = 'Frequency [Hz]',
                          y = 'Real ear SPL (dB SPL)',
                          title = main.title)
  }
  print(plt)
}
