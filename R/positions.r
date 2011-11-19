#' Adjust position by dodging overlaps vertically
#'
#' @name position_dodgev
#' @aliases position_dodgev PositionDodgev
#' @export PositionDodgev position_dodgev
PositionDodgev <- proto(Position, {
  objname <- 'dodgev'
  desc <- 'Adjust position by dodging overlaps vertically'
  adjust <- function(., data, scales) {
    if (empty(data)) return(data.frame())
    check_required_aesthetics('y', names(data), 'position_dodgev')
    swapxy <- function(d) {
      rename(d, c(x='y', y='x',
                  xmin='ymin', xmax='ymax',
                  ymin='xmin', ymax='xmax'))
    }
    swapxy(collide(swapxy(data),
                   .$height, .$my_name(), pos_dodge, check.width=F))
  }
})

position_dodgev <- PositionDodgev$build_accessor()
