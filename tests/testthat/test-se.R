context("SE expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Standard Error", {
  dat = c(1, 2.3, 2, 3, 4, 8, 12, 43, -1, -4)
  dat %>% SE %>% as.numeric %>%
    expect_equal(4.236, tolerance=.005)
  })



