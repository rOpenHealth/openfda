library("ggplot2")
library("plyr")
library("openfda")

my_query = fda_query("/drug/event.json")

ages = ldply(c('Aspirin', 'Fexofenadine', 'Clonazepam'), function(drug) {
  df = my_query %>%
    fda_filter("patient.drug.openfda.generic_name", drug) %>%
    fda_filter("patient.patientonsetageunit", "801") %>%
    fda_count("patient.patientonsetage") %>%
    fda_exec()
  df$drug = drug
  df
})

ages = ldply(c(0, 1, 2), function(gender) {
  df = my_query %>%
    fda_filter("patient.patientsex", gender) %>%
    fda_filter("patient.patientonsetageunit", "801") %>%
    fda_count("patient.patientonsetage") %>%
    fda_exec()
  
  df$gender = switch(as.character(gender),
                     "0"="Male",
                     "1"="Female",
                     "2"="Unknown")
  df
})
