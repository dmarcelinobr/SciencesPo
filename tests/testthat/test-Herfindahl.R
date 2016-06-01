context("Herfindahl expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Herfindahl", {
  dat = c(80, 60, 10, 20, 30)
  dat %>% Herfindahl %>% as.numeric %>%
    expect_equal(0.285, tolerance=.005)
})
