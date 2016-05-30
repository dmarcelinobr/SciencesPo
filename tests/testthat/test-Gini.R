context("Gini expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Gini", {
  dat = c(1000, 2000, 3000, 4000, 5000)
  dat %>% Gini %>% as.numeric %>%
    expect_equal(0.2667, tolerance=.005)
})
