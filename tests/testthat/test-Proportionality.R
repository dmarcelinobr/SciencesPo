context("Proportionality expectations")
pdf(NULL) # suppress generating any PDFs
test_that("Proportionality (Loosemore-Hanby)", {
  votes=c(Cons=26.7, Lab=22.6, Ind=16.1, Lib=14.9, Green=6.3, Scottish=1.4, Plaid=1.0, Others=11.0)
  seats=c(Cons=36.0, Lab=25.3, Ind=16.0, Lib=16.0, Green=2.7, Scottish=2.7, Plaid=1.3, Others=0)
 Proportionality(votes, seats, index="loosemorehanby") %>%
    expect_equal(0.147, tolerance=.005)
})



test_that("Proportionality (Rae)", {
  votes=c(Cons=26.7, Lab=22.6, Ind=16.1, Lib=14.9, Green=6.3, Scottish=1.4, Plaid=1.0, Others=11.0)
  seats=c(Cons=36.0, Lab=25.3, Ind=16.0, Lib=16.0, Green=2.7, Scottish=2.7, Plaid=1.3, Others=0)
  Proportionality(votes, seats, index="Rae") %>%
    expect_equal(0.0367, tolerance=.005)
})




test_that("Proportionality (Rose)", {
  votes=c(Cons=26.7, Lab=22.6, Ind=16.1, Lib=14.9, Green=6.3, Scottish=1.4, Plaid=1.0, Others=11.0)
  seats=c(Cons=36.0, Lab=25.3, Ind=16.0, Lib=16.0, Green=2.7, Scottish=2.7, Plaid=1.3, Others=0)
  Proportionality(votes, seats, index="Rose") %>%
    expect_equal(0.853, tolerance=.005)
})



test_that("Proportionality (Gallagher)", {
  votes=c(Cons=26.7, Lab=22.6, Ind=16.1, Lib=14.9, Green=6.3, Scottish=1.4, Plaid=1.0, Others=11.0)
  seats=c(Cons=36.0, Lab=25.3, Ind=16.0, Lib=16.0, Green=2.7, Scottish=2.7, Plaid=1.3, Others=0)
  Proportionality(votes, seats, index="Gallagher") %>%
    expect_equal(0.107, tolerance=.005)
})




test_that("Proportionality (Inverted Gallagher)", {
  votes=c(Cons=26.7, Lab=22.6, Ind=16.1, Lib=14.9, Green=6.3, Scottish=1.4, Plaid=1.0, Others=11.0)
  seats=c(Cons=36.0, Lab=25.3, Ind=16.0, Lib=16.0, Green=2.7, Scottish=2.7, Plaid=1.3, Others=0)

  Proportionality(votes, seats, index="inv.Gallagher") %>%
    expect_equal(0.893, tolerance=.005)
})
