#' Plot Verifit on-ear or testbox directional results
#'
#' Takes the data frame returned by \code{XXXdirectional_to_df} and plots the data
#' using ggplot2.  Data are plotted on a logx scale with color coding for
#' stimulus level.  The function can be configured to either average across ears
#' or separate the ears into facets.
#'
#' @param x A data frame of Verifit on-ear or testbox directional data, as returned from
#' \code{XXXdirectional_to_df}
#' @param separate.ears An optional boolean value to indicate if ears should be
#' plotted separately in facets (\code{TRUE}) or averaged in a single plot
#' (\code{FALSE}).
#' @param main.title An optional string that will be used as a main title,
#' if present.
#' @param plot.tests A numeric vector corresponding to which tests should be
#' included in the plot.  If used with \code{separate.tests = TRUE}, the vector
#' passed to \code{test.labs} must match \code{plot.tests} in length. Note that
#' this argument should be numeric, while the data frame returned from
#' \code{XXXdirectional_to_df} labels tests with a string (e.g., 'test1', test2').
#' @param separate.tests An optional boolean value to indicate if test runs should be
#' plotted separately in facets (\code{TRUE}) or averaged in a single plot
#' (\code{FALSE}).  This is convenient if multiple directional settings were used
#' during the test.
#' @param test.labs A vector of strings used to label test facets.  Only required
#' if \code{separate.tests = TRUE}.
#'
#' @return A ggplot2 plot of the data in x.
#'
#' @examples
#' \dontrun{
#' df <- remdirectional_to_df(speechmap.xml)
#' plot_remdirectional(df)
#' }
#'
#' @export plot_directional

plot_directional <- function(x,
                             separate.ears = FALSE,
                             main.title = NULL,
                             plot.tests = c(1, 2, 3, 4),
                             separate.tests = FALSE,
                             test.labs = c('Test 1', 'Test 2', 'Test 3', 'Test 4')){

  x[,1] <- as.factor(x[,1])
  x[,3] <- as.factor(x[,3])

  tests <- paste('test',
                 plot.tests,
                 sep = '')

  x <- subset(x,
              x[,2] %in% tests)

  if (separate.ears){
    earlabs <- c('left' = 'Left ear',
                 'right' = 'Right ear')

    if(separate.tests){

      testlabs <- test.labs
      names(testlabs) <- levels(as.factor(x[,2]))

      plt <- ggplot2::ggplot(data = x) +
        ggplot2::geom_line(stat = 'summary',
                           ggplot2::aes_string(x = 'freq',
                                               y = 'respl',
                                               color = 'speaker')) +
        ggplot2::scale_x_log10(limits = c(100,10000),
                               minor_breaks = c(seq(100,1000,100),
                                                seq(1000,10000,1000))) +
        ggplot2::scale_color_hue(name = 'Speaker',
                                 labels = c("Back", "Front")) +
        ggplot2::facet_grid(stats::as.formula(paste('test_run', '~', 'ear')),
                            labeller = ggplot2::labeller(ear = earlabs,
                                                         test_run = testlabs)) +
        ggplot2::labs(x = 'Frequency [Hz]',
                      y = 'Real ear SPL (dB SPL)',
                      title = main.title)
    } else {
      plt <- ggplot2::ggplot(data = x) +
        ggplot2::geom_line(stat = 'summary',
                           ggplot2::aes_string(x = 'freq',
                                               y = 'respl',
                                               color = 'speaker')) +
        ggplot2::scale_x_log10(limits = c(100,10000),
                               minor_breaks = c(seq(100,1000,100),
                                                seq(1000,10000,1000))) +
        ggplot2::scale_color_hue(name = 'Speaker',
                                 labels = c("Back", "Front")) +
        ggplot2::facet_grid(stats::as.formula(paste('ear', '~.')),
                            labeller = ggplot2::labeller(ear = earlabs)) +
        ggplot2::labs(x = 'Frequency [Hz]',
                      y = 'Real ear SPL (dB SPL)',
                      title = main.title)
    }
  } else {
    if(separate.tests){

      testlabs <- test.labs
      names(testlabs) <- levels(as.factor(x[,2]))

      plt <- ggplot2::ggplot(data = x) +
        ggplot2::geom_line(stat = 'summary',
                           ggplot2::aes_string(x = 'freq',
                                               y = 'respl',
                                               color = 'speaker')) +
        ggplot2::scale_x_log10(limits = c(100,10000),
                               minor_breaks = c(seq(100,1000,100),
                                                seq(1000,10000,1000))) +
        ggplot2::scale_color_hue(name = 'Speaker',
                                 labels = c("Back", "Front")) +
        ggplot2::facet_grid(stats::as.formula(paste('.~', 'test_run')),
                            labeller = ggplot2::labeller(test_run = testlabs)) +
        ggplot2::labs(x = 'Frequency [Hz]',
                      y = 'Real ear SPL (dB SPL)',
                      title = main.title)
    } else {
      plt <- ggplot2::ggplot(data = x) +
        ggplot2::geom_line(stat = 'summary',
                           ggplot2::aes_string(x = 'freq',
                                               y = 'respl',
                                               color = 'speaker')) +
        ggplot2::scale_x_log10(limits = c(100,10000),
                               minor_breaks = c(seq(100,1000,100),
                                                seq(1000,10000,1000))) +
        ggplot2::scale_color_hue(name = 'Speaker',
                                 labels = c("Back", "Front")) +
        ggplot2::labs(x = 'Frequency [Hz]',
                      y = 'Real ear SPL (dB SPL)',
                      title = main.title)
    }
  }
  print(plt)
}
