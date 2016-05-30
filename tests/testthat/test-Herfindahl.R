context("Herfindahl expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Herfindahl", {
  dat = c(1000, 2000, 3000, 4000, 5000)
  dat %>% Herfindahl %>% as.numeric %>%
    expect_equal(0.2444, tolerance=.005)
})
