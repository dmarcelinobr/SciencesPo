#' Black backgrounds
#' @examples
#' options(device=RStudioGD)
#' @export
RStudioGD <- function()
{
  .Call("rs_createGD")
  graphics::par(
    bg = "black",
    col = "white",
    col.axis = "white",
    col.lab = "white",
    col.main = "white",
    col.sub = "white")
}
