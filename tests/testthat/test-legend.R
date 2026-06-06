test_that("legend has the expected structure and groupings", {
  leg <- mb_legend(10)
  expect_true(all(c("code", "class_en", "class_pt", "level1", "group") %in% names(leg)))
  expect_false(any(duplicated(leg$code)))
  expect_setequal(
    unique(leg$group),
    c("natural", "anthropic", "water", "other", "not_observed")
  )
  g <- function(code) leg$group[leg$code == code]
  expect_identical(g(3), "natural")       # Forest Formation
  expect_identical(g(12), "natural")      # Grassland
  expect_identical(g(15), "anthropic")    # Pasture
  expect_identical(g(39), "anthropic")    # Soybean
  expect_identical(g(24), "anthropic")    # Urban
  expect_identical(g(33), "water")        # River/Lake/Ocean
  expect_identical(g(25), "other")        # Other non-vegetated
  expect_identical(g(27), "not_observed") # Not observed
})

test_that("mb_groups returns code vectors per group", {
  grp <- mb_groups(10)
  expect_true(15 %in% grp$anthropic)
  expect_true(3 %in% grp$natural)
})
