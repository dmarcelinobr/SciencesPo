#' @encoding UTF-8
#' @title Stratified Sampling
#'
#' @description Sample row values of a data frame conditional to some strata attributes.
#'
#' @param .data the data frame.
#' @param group the grouping factor, may be a list.
#' @param size the sample size.
#' @param select if sampling from a specific group or list of groups.
#' @param replace should sampling be with replacement?
#' @param both.sets if \code{TRUE}, both `sample` and `.data` are returned.
#'
#' @keywords Manipulation
#'
#' @examples
#'
#' data(pollster2008)
#'
#' # Let's take a 10% sample from all -PollTaker- groups in pollster2008
#'  Stratify(pollster2008, "PollTaker", 0.1)
#'
#'  # Let's take a 10% sample from only 'LV' and 'RV' groups from -Pop- in pollster2008
#'  Stratify(pollster2008, "Pop", 0.1, select = list(Pop = c("LV", "RV")))
#'
#'  # Let's take 3 samples from all -PollTaker- groups in pollster2008,
#'  # specified by column 1
#' Stratify(pollster2008, group = 1, size = 3)
#'
#' # Let's take a sample from all -Pop- groups in pollster2008, where we
#' # specify the number wanted from each group
#' Stratify(pollster2008, "Pop", size = c(3, 5, 4))
#'
#' # Use a two-column strata (-Pop- and -PollTaker-) but only interested in
#' # cases where -Pop- == 'LV'
#' Stratify(pollster2008, c("Pop", "PollTaker"), 0.15, select = list(Pop = "LV"))
#'
#' @export
`Stratify` <- function(.data, group, size, select = NULL,
                       replace = FALSE, both.sets = FALSE) {
  if (is.null(select)) {
    .data <- .data
  } else {
    if (is.null(names(select))) stop("'select' must be a named list")
    if (!all(names(select) %in% names(.data)))
      stop("Please verify your 'select' argument")
    temp <- sapply(names(select),
                   function(x) .data[[x]] %in% select[[x]])
    .data <- .data[rowSums(temp) == length(select), ]
  }
  .data.interaction <- interaction(.data[group], drop = TRUE)
  .data.table <- table(.data.interaction)
  .data.split <- split(.data, .data.interaction)
  if (length(size) > 1) {
    if (length(size) != length(.data.split))
      stop("Number of groups is ", length(.data.split),
           " but number of sizes supplied is ", length(size))
    if (is.null(names(size))) {
      n <- stats::setNames(size, names(.data.split))
      message(sQuote("size"), " vector entered as:\n\nsize = structure(c(",
              paste(n, collapse = ", "), "),\n.Names = c(",
              paste(shQuote(names(n)), collapse = ", "), ")) \n\n")
    } else {
      ifelse(all(names(size) %in% names(.data.split)),
             n <- size[names(.data.split)],
             stop("Named vector supplied with names ",
                  paste(names(size), collapse = ", "),
                  "\n but the names for the group levels are ",
                  paste(names(.data.split), collapse = ", ")))
    }
  } else if (size < 1) {
    n <- round(.data.table * size, digits = 0)
  } else if (size >= 1) {
    if (all(.data.table >= size) || isTRUE(replace)) {
      n <- stats::setNames(rep(size, length.out = length(.data.split)),
                    names(.data.split))
    } else {
      message(
        "Some groups\n---",
        paste(names(.data.table[.data.table < size]), collapse = ", "),
        "---\ncontain fewer observations",
        " than desired number of samples.\n",
        "All observations have been returned from those groups.")
      n <- c(sapply(.data.table[.data.table >= size], function(x) x = size),
             .data.table[.data.table < size])
    }
  }
  temp <- lapply(
    names(.data.split),
    function(x) .data.split[[x]][sample(.data.table[x],
                                     n[x], replace = replace), ])
  set1 <- do.call("rbind", temp)

  if (isTRUE(both.sets)) {
    set2 <- .data[!rownames(.data) %in% rownames(set1), ]
    list(SET1 = set1, SET2 = set2)
  } else {
    set1
  }
}### end -- stratify function
NULL
