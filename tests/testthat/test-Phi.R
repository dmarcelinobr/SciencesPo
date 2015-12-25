context("Cramer's Phi coefficient for tables expectations.\n Friendly (2000), 'Visualizing Categorical Data', SAS Institute Inc., p. 61.")
pdf(NULL) # suppress generating any PDFs
test_that("The Cramer's Phi coefficient for tables", {
  tab = as.table(rbind(c(757, 731), c(726, 2621)));
  tab %>% Phi %>%
    expect_equal(.292, tolerance=.005)
})




