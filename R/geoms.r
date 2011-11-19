#' An interval represented by a horizontal line
#'
#' @export GeomLinerangeh geom_linerangeh
#' @name geom_linerangeh
#' @aliases geom_linerangeh GeomLinerangeh
GeomLinerangeh <- proto(Geom, {
  objname <- 'linerangeh'
  desc <- 'an interval represented by a horizontal line'
  default_stat <- function(.) StatIdentity
  default_aes <- function(.)
    aes(colour = 'black', size=0.5, linetype=1, alpha = 1)
  guide_geom <- function(.) 'path'
  required_aes <- c("y", "xmin", "xmax")
  draw <- function(., data, scales, coordinates, ...) {
    munched <- coordinates$transform(data, scales)
    ggname(.$my_name(),
           GeomSegment$draw(transform(data, yend=y, x=xmin, xend=xmax),
                            scales, coordinates, ...))
  }
})

#' An interval represented by a vertical line, with a point in the middle
#'
#' @export GeomPointrangeh geom_pointrangeh
#' @name geom_pointrangeh
#' @aliases GeomPointrangeh geom_pointrangeh
GeomPointrangeh <- proto(Geom, {
  objname <- 'pointrangeh'
  desc <- 'An interval represented by a vertical line, with a point in the middle'
  default_stat <- function(.) StatIdentity
  default_aes <- function(.)
    aes(colour = 'black', size=0.5, linetype=1, shape=16,
        fill=NA, alpha = 1)
  guide_geom <- function(.) 'pointrangeh'
  required_aes <- c('x','y','xmin','xmax')
  draw <- function(., data, scales, coordinates, ...) {
    if (is.null(data$x))
      return(GeomLinerangeh$draw(data, scales, coordinates, ...))
    ggname(.$my_name(),gTree(children=gList(
      GeomLinerangeh$draw(data, scales, coordinates, ...),
      GeomPoint$draw(transform(data, size=size*4),
                     scales, coordinates, ...)
    )))
  }
  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    grobTree(GeomPath$draw_legend(data, ...),
             GeomPoint$draw_legend(transform(data, size=size*4), ...))
  }
})

geom_linerangeh <- GeomLinerangeh$build_accessor()
geom_pointrangeh <- GeomPointrangeh$build_accessor()
