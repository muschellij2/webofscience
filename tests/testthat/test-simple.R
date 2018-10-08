test_that("check simple error", {

  testthat::expect_silent(ws_have_incites_key())

  ws_incites_key = getOption("ws_incites_key")
  ws_incites_key_filename = getOption("ws_incites_key_filename")
  INCITES_KEY = Sys.getenv("INCITES_KEY")

  options(ws_incites_key_filename = NULL)
  options(ws_incites_key = NULL)
  Sys.setenv(INCITES_KEY = "")

  testthat::expect_false(ws_have_incites_key(api_key = NULL))
  testthat::expect_null(ws_incites_key(api_key = "", error = FALSE))
  testthat::expect_error( {
    ws_incites_key(api_key = NULL, error = TRUE)
  })

  Sys.setenv(INCITES_KEY = INCITES_KEY)
  options(ws_incites_key_filename = ws_incites_key_filename)
  options(ws_incites_key_filename = ws_incites_key)

})
