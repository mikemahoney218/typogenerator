test_that("methods are stable", {
  expect_true(
    all(
      typo_vowelswap("Michael")[[1]] %in% typo_bitsquat(
        "Michael",
        c("a", "e", "i", "o", "u")
      )[[1]]
    )
  )

  expect_equal(
    length(typo_doublehit("Michael")[[1]]),
    60
  )

  expect_equal(
    typo_hyphenation("Michael", ";"),
    typo_subdomain("Michael", ";")
  )

  expect_true(
    all(nchar(typo_replace("Michael")[[1]]) == 7)
  )

})
