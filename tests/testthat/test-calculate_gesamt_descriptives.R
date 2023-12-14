test_that("calculate_frequencies", {
  out <- calculate_frequencies(iris, by_var = "Species", new_var_name = "Anzahl")
  expect_equal(names(out), c("Species", "Anzahl", "kumulativ"))
  expect_equal(out$Anzahl, c(50, 50, 50))
  expect_equal(out$kumulativ, c(50, 100, 150))
})


test_that("pivot_longer_frequencies", {
  input <- data.frame(Jahr = c(2010, 2011, 2012),
                      Freq = c(10, 8, 3),
                      cumFreq = c(10, 18, 21))
  out <- pivot_longer_frequencies(input, exclude_col = "Jahr", names_to = "Statistik")
  expect_equal(names(out), c("Jahr", "Statistik", "value"))
  expect_equal(out$Jahr, ordered(c(2010, 2010, 2011, 2011, 2012, 2012)))
  expect_equal(out$Statistik, rep(c("Freq", "cumFreq"), 3))
})
