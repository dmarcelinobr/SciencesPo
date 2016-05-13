context("CV expectations")
pdf(NULL) # suppress generating any PDFs
test_that("computes coefficient of variation", {
  x <- c(1, 2.3, 2, 3, 4, 8, 12, 43, -1,-4)
  x %>% CV %>% as.numeric %>%
    expect_equal(1.906, tolerance=.005)
})
