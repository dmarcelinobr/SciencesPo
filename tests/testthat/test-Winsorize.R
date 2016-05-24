context("Winsorized mean expectations")
pdf(NULL) # suppress generating any PDFs
test_that("90% winsorization would result in", {
  dat = c(92, 19, 101, 58, 1053, 91, 26, 78, 10, 13, -40, 101, 86, 85, 15, 89, 89, 28, -5, 41)
  dat %>% Winsorize %>% as.numeric %>%
    expect_equal(55.65, tolerance=.005)
  })
