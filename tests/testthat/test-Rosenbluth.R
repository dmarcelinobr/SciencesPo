context("Rosenbluth expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Rosenbluth", {
  dat = c(1000, 2000, 3000, 4000, 5000)
  dat %>% Rosenbluth %>% as.numeric %>%
    expect_equal(0.2727, tolerance=.005)
})
