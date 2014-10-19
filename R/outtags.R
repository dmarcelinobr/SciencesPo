outtags <-
function(x, tab=FALSE){
        x[] <- sub("\u00a4", "", x)
        x[] <- sub("\u00a2", "", x) 
        x[] <- sub("\u00a3", "", x) 
        x[] <- sub("\u00a5", "", x) 
        x[] <- sub("\u65e5", "", x) 
        x[] <- sub("\u672c", "", x) 
        x[] <- sub("\u2602", "", x)
        x[] <- sub("[\u002a\u2217]", "", x)
        x[] <- sub("\u00c6", "", x) 
        x[] <- sub("\u0098", "", x) 
        x[] <- sub("\u0080", "", x) 
        x[] <- sub("\u00a7", "", x) 
        x[] <- sub("\u00a9", "", x) 
        x[] <- sub("[\u00ab\u00bb]", "", x) 
        x[] <- sub("\u0083", "", x) 
        x[] <- sub("\u0085", "", x) 
        x[] <- sub("\u0040", "", x) 
        x[] <- sub("\u007e", "", x) 
        x[] <- sub("[\u003a\u003b\u002c\u003f\u0021\u00a1]", "", x) 
        x[] <- sub("\u0026", "", x)
        x[] <- sub("\u001b", "", x) #escape
        x[] <- sub("\u0a8a", "", x)
        x[] <- sub("\u220f", "", x)
        x[] <- sub("\u0023", "", x)
        x[] <- sub("[\u0024\u2047\ufe69\u00a2\u0e3f\u00a5\u00a4\u00a3\u20a4\u20a6\u20a9\u20ab\u20ac\u20ad\u20b1\u20b5]", "", x) #currency
        x[] <- sub("\u0025", "", x)
        x[] <- sub("\u0026", "", x)
        x[] <- gsub(" {2,}"," ",x) #extra space in the middle
        x[] = gsub(" +$", "", x) #extra space
        x[] = gsub("^ +", "", x) #extra space

    if (tab) {
        x[] <- sub("[\t\u000B\u0009]","", x)
    }
    else {
    return(x)
}
return(x)
}
