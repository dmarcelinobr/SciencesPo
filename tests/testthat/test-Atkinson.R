context("Atkinson expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Atkinson", {
  dat = c(1000, 2000, 3000, 4000, 5000)
  dat %>% Atkinson %>% as.numeric %>%
    expect_equal(0.06315, tolerance=.005)
})
