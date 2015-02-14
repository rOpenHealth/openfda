library(openfda)
library(ggplot2)
library(magrittr)

context("query-builder")
openfda::fda_debug(FALSE)

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
  
  expect_that(url, 
    equals("https://api.fda.gov/drug/event.json?search=patient.patientsex:2&key=BLAH"))
})

test_that("age counting", {
  df = fda_query("/drug/event.json") %>%
             fda_filter("patient.drug.openfda.generic_name", "paroxetine") %>%
             fda_filter("patient.patientonsetageunit", "801") %>%
             fda_count("patientonsetage") %>%
             fda_exec()
})

context("fetch");

test_that("basic fetching works", {
  df = openfda::fetch_url("http://api.fda.gov/drug/event.json?count=receivedate");
  df$time = as.Date(df$time, "%Y%m%d");
  plot = ggplot(df, aes(x=time, y=count)) + stat_identity();
  expect_that(plot, is_a("gg"));
});
