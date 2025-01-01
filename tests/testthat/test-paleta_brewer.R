# Tests for paleta_create functions --------------------------------------------

test_that("paleta_create functions work as expected", {
  expect_vector(
    paleta_create_sequential(n = 5, org = "acdc", name = "blues"),
    size = 5
  )
  expect_vector(
    paleta_create_divergent(n = 5, org = "acdc", name = "rdylgn"),
    size = 5
  )
  expect_vector(
    paleta_create_qualitative(n = 5, "pastel1", "acdc"),
    size = 5
  )
  expect_message(paleta_create_sequential(n = 2, org = "acdc", name = "blues"))
  expect_message(paleta_create_sequential(n = 10, org = "acdc", name = "blues"))
  expect_message(paleta_create_divergent(n = 2, org = "acdc", name = "rdylgn"))
  expect_message(paleta_create_divergent(n = 12, org = "acdc", name = "rdylgn"))
  expect_message(paleta_create_qualitative(n = 12, "pastel1", "acdc"))

  expect_vector(
    paleta_create_brewer(n = 5, name = "blues", org = "nhs")
  )

  expect_error(paleta_create_brewer(n = 5, name = "ylorbr", org = "nhs"))
  expect_error(
    paleta_create_brewer(n = 5, name = "blues", org = "nhs", type = "divergent")
  )
})
