dummy <-
function (x, data = NULL, drop = TRUE) 
{
    if (is.null(data)) {
        varname <- as.character(sys.call(1))[2]
        varname <- sub("^(.*\\$)", "", varname)
        varname <- sub("\\[.*\\]$", "", varname)
    }
    else {
        if (length(x) > 1) 
            stop("More than one variable to create dummies at same  time.")
        varname <- x
        x <- data[, varname]
    }
    if (drop == FALSE && class(x) == "factor") {
        x <- factor(x, levels = levels(x), exclude = NULL)
    }
    else {
        x <- factor(x, exclude = NULL)
    }
    if (length(levels(x)) < 2) {
            warning(varname, " has only 1 dimension. Generating dummy variable anyway.")
        return(matrix(rep(1, length(x)), ncol = 1, dimnames = list(rownames(x), 
            c(paste(varname, "_", x[[1]], sep = "")))))
    }
    mat <- model.matrix(~x - 1, model.frame(~x - 1), contrasts = FALSE)
    colnames.mm <- colnames(mat)
        cat(" ", varname, ":", ncol(mat), "dummy variables generated\n")
    mat <- matrix(as.integer(mat), nrow = nrow(mat), ncol = ncol(mat), dimnames = list(NULL, 
        colnames.mm))
    colnames(mat) <- sub("^x", paste(varname, "_", sep = ""), colnames(mat))
    if (!is.null(row.names(data))) 
        rownames(mat) <- rownames(data)
    return(mat)
}
