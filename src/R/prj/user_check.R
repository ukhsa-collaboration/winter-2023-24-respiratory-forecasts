# USER CHECK FUNCTIONS
# README
# This is a function designed to make sure a user is happy with the way a script
# is progressing before starting something long and complicated.
# You can write in message to print next to the prompt, in the lead_message,
# or do that before calling this function.
# You can also add particular pass or fail state keywords if you anticipate a
# response I didn't think to put in here.
#
# Took out the user text input cleaner into its own function at the bottom.
# It's used to keep strings consistent for matching.
# Feel free to export it if you find a good use for it.
#
# There is also a disease name checker to make sure an entered string matches
# the name in the file structure of this repository.


box::use(box / deps_)
.on_load <- function(ns) {
  deps_$need(
    "dplyr", # might not even need this?
    "stringr"
  )

  deps_$min_version("dplyr", "1.1.0")
}



#' Check user is happy to continue
#'
#' @param lead_message A string printed before asking for the user's prompt.
#'                     Anything that can be no-quote-printed is also acceptable.
#' @param extra_fails String, possibly vector of strings, to be added to the
#'                    list of unacceptable responses that will halt the script.
#' @param extra_passes String, possibly vector of strings, to be added to the
#'                     list of acceptable responses to continue running script
#'
#' @returns Nothing; called for side effect.
#'          Will print an acknowledgement on pass or error on user stoppage.
#'
#' TODO: I don't know what happens if I use ATexample for these.
#' example usage 1: checking dates are correct in recent data pull:
#' print(summary_statistics) # printing a data frame may be best done separately
#' user_check(
#'   lead_message = paste( # A helpful message, possibly pasting together info
#'     "Please check these dates are sufficiently recent.",
#'     "Today's date is:",
#'     lubridate::today(tzone = "GMT")),
#'   extra_passes = "Open the pod bay doors Hal.", #would now accept this string
#'   extra_fails = c("I'm sorry Dave", "I'm afraid I can't do that.")
#' ) # either of these strings will now stop the script if user types one in.
#'
#'  example usage 2: Graph check:
#'  user_check(ggplot(data, aes(x = date, y = value) + {etc.}))
#'   # would print graph and then ask for your verdict
#'
#' @export
user_check <- function(
    lead_message = paste(
      "Please check whether this is tolerable",
      "before passing on the data."),
    extra_fails = NULL,
    extra_passes = NULL
) {
  # Message for user
  print(noquote(lead_message))

  # Main user input:
  userOK <- readline(prompt = "Press [enter] to acknowledge & continue: ") |>
    clean_input()

  fail_states <-
    c(
      "0",
      "f",
      "n",
      "no",
      "help",
      "kill",
      "stop",
      "break",
      "false",
      clean_input(extra_fails)
    )

  pass_states <-
    c(
      "",
      "1",
      "y",
      "t",
      "go",
      "yes",
      "yup",
      "true",
      "fine",
      "continue",
      "keep going",
      "keep calm and carry on",
      clean_input(extra_passes)
    )

  if (userOK %in% fail_states) {
    stop("User Termination") # as in terminated by the user, not ... you get it.
  } else if (userOK %in% pass_states) {
    print(noquote("Acknowledged."))
  } else {
    print(noquote(
      paste(
        "Can't say I thought of that responce.",
        "Maybe add to the extra_fails|passes list?",
        "Continuing."
      )
    ))
  }
}

#' Common user input text cleaner
#' Bundles some common code into one internal function,
#' shouldn't need to be exported; unless you find a use case.
#' @param text String to be cleaned.
#' @returns A lower-case string without any obvious SQL injections or any other
#'          characters likely to break a pattern match.
#' ATexport # not currently looking to export this one

clean_input <- function(text) {
  return(
    as.character(text) |>
      stringr::str_to_lower() |> # cleaning user input... what?
      stringr::str_remove_all("\\\\") |> # You never know who the user is.
      stringr::str_remove_all("[.,!?'|/]") |> # no SQL injection here!
      trimws()
  )
}



#' Checks the disease will match the name in the file structure
#'
#' This function attempts to correct an entered disease name,
#' and will ask for user conformation that its interpretation is correct.
#'
#' @param disease String of the disease name
#'
#' @returns String with the standardised disease name.
#'
#' @export
disease_checker <- function(disease) {
  avalible_diseases <- c("covid", "influenza", "norovirus", "rsv")
  # Add more as necessary, include grepl terms below:
  if (!disease %in% avalible_diseases) {
    user_disease <- disease
    disease <- dplyr::case_when(
      grepl("cov|rona|plague", disease, ignore.case = TRUE) ~ "covid",
      grepl("flu", disease, ignore.case = TRUE) ~ "influenza",
      grepl("rsv|respi|syncyt", disease, ignore.case = TRUE) ~ "rsv",
      grepl("noro", disease, ignore.case = TRUE) ~ "norovirus",
      TRUE ~ disease
    )

    if (!disease %in% avalible_diseases) {
      print(noquote(paste0(
        'This function is not familiar with "',
        disease,
        '" as a disease. This may cause problems in the following code.'
      )))
    }
    if (disease != user_disease) {
      print(noquote(paste0(
        'You entered: "', user_disease,
        '", which this function interprets as: "', disease, '".')))
      user_check("Is this a correct interpretation?")
    }
  }
  return(disease)
}
