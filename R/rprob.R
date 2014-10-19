rprob <-
function (x, df = nrow(x) - 2) {
        r <- cor(x, use="pairwise.complete.obs")
        table <- row(r) < col(r)
        r2 <- r[table]^2
        F <- r2 * df/(1 - r2)
        r[table] <- 1 - pf(F, 1, df)
        r[row(r) == col(r)] <- 1
        round(r, digits=2)
    }
