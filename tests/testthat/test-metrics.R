test_that("perfect predictions yield bacc 1.0", {
  expect_equal(m_bacc(c(1L,1L,0L,0L), c(1L,1L,0L,0L)), 1)
})

test_that("inverse predictions yield bacc 0.0", {
  expect_equal(m_bacc(c(1L,1L,0L,0L), c(0L,0L,1L,1L)), 0)
})

test_that("balanced random yields bacc 0.5", {
  expect_equal(m_bacc(c(1L,0L), c(1L,1L)), 0.5)
})

test_that("score_all returns the expected fields", {
  set.seed(1)
  y <- as.integer(rbinom(40, 1, 0.4))
  p <- as.integer(ifelse(runif(40) < 0.8, y, 1L - y))
  s <- score_all(list(y_test = y, pred = p, n_cues_per_row = sample(1:4, 40, TRUE),
                      train_time = 0.05))
  expect_named(s, c("acc","bacc","f1","auc","frugality","train_time","n_test","n_pos_test"))
  expect_gte(s$bacc, 0); expect_lte(s$bacc, 1)
})

test_that("confusion handles NA predictions", {
  c1 <- confusion(c(1L,1L,0L,0L), c(1L, NA_integer_, 0L, 0L))
  expect_equal(c1$n_dropped, 1)
  expect_equal(c1$hi + c1$cr, 3)  # 3 valid + 1 dropped
})
