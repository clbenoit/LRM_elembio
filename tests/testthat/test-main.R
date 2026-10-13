box::use(
  shiny[testServer],
  testthat[expect_true, test_that],
)
box::use(
  app/main[server],
)

test_that("main server works", {
  testServer(server, {
    # smoke test : le server principal s'initialise sans erreur
    # (cache, data manager, modules import_indexes + s5)
    expect_true(exists("appDataManager"))
  })
})
