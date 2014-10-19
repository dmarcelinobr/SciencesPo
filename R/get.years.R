get.years <-
function(earlier, later)
{
    lt <- data.frame(earlier, later)
    age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
    later.year.day <- ifelse(format(lt[,1],format="%m-%d")!="02-29",
                             as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                             ifelse(as.numeric(format(later,format="%Y")) %% 400 == 0 | as.numeric(format(later,format="%Y")) %% 100 != 0 & as.numeric(format(later,format="%Y")) %% 4 == 0,
                                    as.Date(paste(format(lt[,2],format="%Y"),"-",format(lt[,1],format="%m-%d"),sep="")),
                                    as.Date(paste(format(lt[,2],format="%Y"),"-","02-28",sep=""))))
    age[which(later.year.day > lt$later)] <- age[which(later.year.day > lt$later)] - 1
    return(age)
}
