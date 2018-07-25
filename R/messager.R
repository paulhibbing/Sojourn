#' Function for tidy assembly of text for printing in the console
#'
#' @param n message number to assemble and return
#' @param theGrid a grid entry from the package's internal data
#' @param id character scalar giving the participant ID to print
#'
#' @return text to print to the console
#' @keywords internal
#'
messager <- function(n, theGrid = NULL, id = "`Unspecified ID`") {

  text_message <-
    switch(
      n,

      ## 1-5
      paste(
        "\n  Using the",
        paste(
          theGrid$Site,
          theGrid$Output,
          "grid:\n    Difference =",
          theGrid$Difference,
          "\n    Threshold =",
          theGrid$Threshold,
          "\n    Short =",
          theGrid$Short
        )
      ),
      paste("\nIdentifying Sojourns ..."),
      paste("\nDone!\n"),
      paste("\n***\t***\t***\t***\nCalculating prediction for",
        id, "\n"),
      paste("\nGetting 15s features..."),

      ## 6-10
      paste("\nGetting Sojourns with default settings..."),
      paste("\nGetting Sojourn features..."),
      paste("... Done!\n"),
      paste(9)
    )

  return(text_message)
}
