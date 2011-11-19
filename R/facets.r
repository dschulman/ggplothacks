#' Select one or more rows from a grobGrid
#'
#' @param grid a grobGrid
#' @param rows a vector of one or more rows
#' @return a grobGrid containing the specified row
#' @export
grobGridRows <- function(grid, rows) {
  structure(list(names=grid$names[rows,],
                 grobs=grid$grobs[rows,],
                 clip=grid$clip[rows,],
                 heights=grid$heights[rows],
                 widths=grid$widths,
                 respect=grid$respect),
            class='grobGrid')
}

#' Select one or more columns from a grobGrid
#'
#' @param grid a grobGrid
#' @param cols a vector of one or more columns
#' @return a grobGrid containing the specified columns
#' @export
grobGridCols <- function(grid, cols) {
  structure(list(names=grid$names[,cols],
                 grobs=grid$grobs[,cols],
                 clip=grid$clip[,cols],
                 heights=grid$heights,
                 widths=grid$widths[cols],
                 respect=grid$respect),
            class='grobGrid')
}

#' Lay out panels in a rectangular/tabular manner, with added options
#'
#' This is an enhanced version of ggplot2's \code{facet_grid}.  It adds
#' options to control whether the facet rows and/or columns are associated
#' with the axes, and displayed on the left/bottom rather than the right/top.
#'
#' @name facet_gridx
#' @aliases facet_gridx FacetGridx
#' @export facet_gridx FacetGridx
FacetGridx <- proto(FacetGrid, {
  objname <- 'gridx'
  desc <- 'Lay out panels in a rectangular/tabular manner, with added otions'
  
  new <- function(., facets = . ~ ., margins = FALSE,
                  scales = "fixed", space = "fixed",
                  labeller = "label_value", as.table = TRUE,
                  widths = NULL, heights = NULL,
                  on.axis.x = FALSE, on.axis.y = FALSE) {
    proto(.super$new(., facets, margins, scales, space,
                     labeller, as.table, widths, heights),
          on.axis.x=on.axis.x, on.axis.y=on.axis.y)
  }

  add_guides <- function(., data, panels_grob, coord, theme) {
    m <- .super$add_guides(., data, panels_grob, coord, theme)
    nc <- ncol(m)
    nr <- nrow(m)
    if (.$on.axis.x)
      m <- grobGridRows(m, c(nr-1,nr,1:(nr-2)))
    if (.$on.axis.y)
      m <- grobGridCols(m, c(nc-1,nc,1:(nc-2)))
    m
  }
})

facet_gridx <- FacetGridx$build_accessor()

