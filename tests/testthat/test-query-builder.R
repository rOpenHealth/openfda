library("openfda")
library("ggplot2")
library("magrittr")

context("query-builder")
#openfda::fda_debug(FALSE)

test_that("build enforcement query", {
  df = fda_query("/drug/enforcement.json") %>%
    fda_filter("openfda.product_type", "otc") %>%
    fda_count("classification.exact") %>%
    fda_exec()

  expect_that(df, is_a("data.frame"))
})

test_that("build and query", {
  df = fda_query("/drug/event.json") %>%
    fda_filter("patient.patientsex", "2") %>%
    fda_filter("patient.patientonsetage", "[1+TO+40]") %>%
    fda_count("patient.drug.drugindication.exact")  %>%
    fda_exec()

  expect_that(df, is_a("data.frame"))
})

test_that("api key", {
  url = fda_query("/drug/event.json") %>%
    fda_api_key("BLAH") %>%
    fda_filter("patient.patientsex", "2") %>%
    fda_url()
  
  expect_equal(url,
    "https://api.fda.gov/drug/event.json?search=patient.patientsex:2&api_key=BLAH")
})

context("fetch");

test_that("age counting", {
  df = fda_query("/drug/event.json") %>%
    fda_filter("patient.drug.openfda.generic_name", "paroxetine") %>%
    fda_filter("patient.patientonsetageunit", "801") %>%
    fda_count("patientonsetage") %>%
    fda_exec()
})

test_that("retrieve a field", {
  df = fda_query("/drug/event.json") %>%
    fda_filter("patient.drug.openfda.generic_name", "paroxetine") %>%
    fda_search("patient.drug.drugindication") %>%
    fda_exec()
})

test_that("basic fetching works", {
  df = fda_fetch("http://api.fda.gov/drug/event.json?count=receivedate")$result
  df$time = as.Date(df$time, "%Y%m%d")
  plot = ggplot(df, aes(x=time, y=count)) + stat_identity()
  expect_that(plot, is_a("gg"))
});

context("error handling")
test_that("handle 404s", {
  df = fda_fetch("http://api.fda.gov/drug/event.json?search=patientsex:99")
});
