test_that("greedy algorithm works", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  input_rkgs2 <- matrix(c(3, 2, 5, 4,1,2,3,1,5,4,5,1,3,4,2,1,2,4,5,3), ncol = 4)
  expect_equal(rap_greedy_alg(c(1,2,3,4),input_rkgs1,1), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(rap_greedy_alg(c(1,2,3,4),input_rkgs1,2), list(ConsensusRanking = c(1,3,2,4), KemenyDistance = 6, tau = 2/3))
  expect_equal(rap_greedy_alg(c(1,2,3,4,5),input_rkgs2), list(ConsensusRanking = c(1,2,3,5,4), KemenyDistance = 26, tau = 0.35))
  expect_equal(rap_greedy_alg(c(5,4,3,2,1),input_rkgs2,2), list(ConsensusRanking = c(4,1,3,5,2), KemenyDistance = 22, tau = 0.45))
})

test_that("error is raised for incomplete or incorrect inputs", {
  input_rkgs1 <- matrix(c(1,2,4,3,2,3,1,4,1,3,2,4), ncol=3)
  expect_error(rap_greedy_alg(c(1:4),input_rkgs1,-1))
  expect_error(rap_greedy_alg(c(1:3),input_rkgs1,2))
  expect_error(rap_greedy_alg(c(1:4),input_rkgs1[1:2,1:2],2))
})
