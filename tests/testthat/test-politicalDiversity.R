context("Political Diversity Measures")
pdf(NULL) # suppress generating any PDFs
test_that("The effective number of parties given Laakso/Taagepera and Golosov", {
    B <- c(.75,.10,rep(0.01,15));
      B %>% politicalDiversity %>%
      expect_equal(1.74, tolerance=.005)
      B %>% politicalDiversity(index = "Golosov") %>%
      expect_equal(1.42, tolerance=.005)
  })
