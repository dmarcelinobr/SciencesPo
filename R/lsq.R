lsq <-
function(size, rep=1, seed=NA, id=FALSE) {
    
    # If any old random seed, this saves and will use the new one, if it exists.
    if (!is.na(seed)) {
        if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
        else                         { saved.seed <- NA }
        set.seed(seed)
    }
    
    # This matrix will contain all the individual squares
    allsq <- matrix(nrow=rep*size, ncol=size)
    
    # Storing a string id for each square if it is requested
    if (id) {  sqid <- vector(mode = "character", length = rep) }
    
    # Get a random element from a vector. The built-in sample function annoyingly
    #   has different behaviour if there's only one element in x)
    sample1 <- function(x) {
        if (length(x)==1) { return(x) }
        else              { return(sample(x,1)) }
    }
    
    # Generate each of size n individual squares
    for (n in 1:rep) {
        
        # Generate an empty square to store it
        sq <- matrix(nrow=size, ncol=size) 
        
        # If we fill the square sequentially from top left, some latin squares
        # are more probable than others.  So we have to do it random order,
        # all over the square.
        # The rough procedure is:
        # - randomly select a cell that is currently NA (call it the target cell)
        # - find all the NA cells sharing the same row or column as the target
        # - fill the target cell
        # - fill the other cells sharing the row/col
        # - If it ever is impossible to fill a cell because all the numbers
        #    are already used, then quit and start over with a new square.
        # In short, it picks a random empty cell, fills it, then fills in the 
        # other empty cells in the "cross" in random order. If we went totally randomly
        # (without the cross), the failure rate is much higher.
        while (any(is.na(sq))) {
            
            # Pick a random cell which is currently NA
            k <- sample1(which(is.na(sq)))
            
            i <- (k-1) %% size +1       # Get the row num
            j <- floor((k-1) / size) +1 # Get the col num
            
            # Find the other NA cells in the "cross" centered at i,j
            sqrow <- sq[i,]
            sqcol <- sq[,j]
            
            # A matrix of coordinates of all the NA cells in the cross
            openCell <-rbind( cbind(which(is.na(sqcol)), j),
                              cbind(i, which(is.na(sqrow))))
            # Randomize fill order
            openCell <- openCell[sample(nrow(openCell)),]
            
            # Put centre cell at top of list, so that it gets filled first
            openCell <- rbind(c(i,j), openCell)
            # There will now be three entries for the centre cell, so remove duplicated entries
            # Need to make sure it's a matrix -- otherwise, if there's just 
            # one row, it turns into a vector, which causes problems
            openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
            
            # Fill in the centre of the cross, then the other open spaces in the cross
            for (c in 1:nrow(openCell)) {
                # The current cell to fill
                ci <- openCell[c,1]
                cj <- openCell[c,2]
                # Get the numbers that are unused in the "cross" centred on i,j
                freeNum <- which(!(1:size %in% c(sq[ci,], sq[,cj])))
                
                # Fill in this location on the square
                if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) }
                else  {
                    # Failed attempt - no available numbers
                    # Re-generate empty square
                    sq <- matrix(nrow=size, ncol=size)
                    
                    # Break out of loop
                    break;
                }
            }
        }
        
        # Store the individual square into the matrix containing all squares
        allsqrows <- ((n-1)*size) + 1:size
        allsq[allsqrows,] <- sq
        
        # Store a string representation of the square if requested. Each unique
        # square has a unique string.
        if (id) { sqid[n] <- paste(sq, collapse="") }
        
    }
    
    # Restore the old random seed, if present
    if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }
    
    if (id) { return(sqid) }
    else { return(allsq) }
}
