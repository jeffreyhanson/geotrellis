context('benchmark')

test_that('benchmark', {
  # init
  skip_if_not(!is.null(rscala::scalaInfo()), 'Scala is not installed')
  # run code
  b <- suppressWarnings(benchmark(c(100, 400), times = 3L))
  print(b)
  plot(b)
})
