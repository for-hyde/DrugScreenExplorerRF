test_that("buildApp assembles the default app without launching", {
  skip_if_not_installed("shiny")

  app <- buildApp(launch = FALSE)

  expect_s3_class(app, "shiny.appobj")
})

test_that("buildApp validates module entries", {
  skip_if_not_installed("shiny")

  expect_error(
    buildApp(appModules = list(incomplete = list(ui = identity)),
             launch = FALSE),
    "ui.*server|server.*ui"
  )
  custom_app <- buildApp(
    appModules = list(one = list(ui = identity, server = identity),
                      two = list(ui = identity, server = identity)),
    launch = FALSE
  )
  expect_s3_class(custom_app, "shiny.appobj")
})
