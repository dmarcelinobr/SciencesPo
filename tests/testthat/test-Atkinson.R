context("Atkinson expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Atkinson", {
  dat = c(10, 15, 20, 25, 40, 20, 30, 35, 45, 90)
   Atkinson(dat, epsilon=0.5) %>%
    expect_equal(0.08692, tolerance=.005)

   Atkinson(dat, epsilon=1) %>%
     expect_equal(0.1641, tolerance=.005)

   Atkinson(dat, epsilon=2) %>%
     expect_equal(0.2902, tolerance=.005)
})
