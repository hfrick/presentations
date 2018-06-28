## gp --------------------------------------------

library(goodpractice)
g <- gp()

gp(checks = "lintr_setwd_linter")

## gp customisation ---------------------------------

## check only ----

# make a simple version of the T/F check
check_simple_tf <- make_check(
  
  description = "TRUE and FALSE is used, not T and F",
  gp = "avoid 'T' and 'F', use 'TRUE' and 'FALSE' instead.",
  check = function(state) {
    length(tools::checkTnF(dir = state$path)) == 0
  }
)

# set FALSE back to F to see the check fail
gp(".", checks = "simple_tf",
   extra_checks = list(simple_tf = check_simple_tf))


## checks with prep step ----

# prep: process DESCRIPTION file
desc_fun <- function(path, quiet) {
  desc::description$new(path)
}

prep_desc <- make_prep(name = "desc", func = desc_fun)

# check for an URL field
check_url <- make_check(
  description = "URL field in DESCRIPTION",
  preps = "desc",
  gp = "have a URL field in DESCRIPTION",
  check = function(state) state$desc$has_fields("URL")
)

# check for a BugReport field
check_bugreports <- make_check(
  description = "BugReports in DESCRIPTION",
  preps = "desc",
  gp = "add a BugReports field to DESCRIPTION",
  check = function(state) state$desc$has_fields('BugReports')
)

# run the two checks with their corresponding prep step
gp(".", checks = c("url", "bugreports"),
   extra_preps = list("desc" = prep_desc),
   extra_checks = list("url" = check_url, "bugreports" = check_bugreports))



## -----

goodpractice::all_checks()
goodpractice::checks(g)
goodpractice::failed_checks(g)
goodpractice::results(g)


