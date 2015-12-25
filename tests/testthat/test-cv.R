context("cv expectations")
pdf(NULL) # suppress generating any PDFs
test_that("computes coefficient of variation", {
  set.seed(51)
  x <- sample(100)
  x %>% cv %>%
    expect_equal(0.574, tolerance=.005)
})
