tableau <-
function(var, title = NULL){
        counts <- table(var)
        percts <- 100 * prop.table(counts)       
        
        print(
            xtable(
                cbind(
                    Count = counts
                    , Percent = percts
                )
                , caption = title
                , digits = c(0,0,2)
            )
            , caption.placement="top"
        )
    }
