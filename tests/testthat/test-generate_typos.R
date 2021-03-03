test_that("generate_typos is stable", {
  expect_equal(
    length(generate_typos("Mike", "*")[[1]]),
    351
  )

  expect_equal(
    generate_typos("Michael", "typo_repetition"),
    typo_repetition("Michael")
  )

  expect_equal(
    length(
      generate_typos(
        "Michael",
        c("typo_addition", "typo_prefix"),
        c(gh_allowed(), gh_allowed())
        )[[1]]
      ),
    length(gh_allowed()) * 2
  )

})
