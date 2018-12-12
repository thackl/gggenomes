library(gggenomes)
context("examples files")
test_that("list example files", {
  expect_equal(example_sets(quiet=TRUE), tibble(value=c("cyanophages", "MT", "prochlorococcus")))
  expect_equal(example_files(quiet=TRUE), example_sets(quiet=TRUE))
  expect_match(example_files("MT", "*.fa", quiet=TRUE), "MT-(human|orang).fa", perl=TRUE)
})

context("reading gffs")
test_that("read gffs", {
  expect_equal(dim(read_gffs(example_files("cyanophages", "*.gff", quiet=TRUE))), c(815,24))
  expect_equal(dim(read_gffs_as_contigs(example_files("cyanophages", "*.gff", quiet=TRUE))), c(9,3))
})
