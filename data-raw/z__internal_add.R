#' Add an object to internal package data
#'
#' A wrapper for \code{devtools::use_data(internal = TRUE, overwrite = TRUE)}
#' that adds the object to existing sysdata.rda, instead of overwriting the
#' existing file with only the data being added
#'
#' @param object The object to add to systdata.rda
#' @param sysdata The path to sysdata.rda
#'
#' @keywords internal
#'
internal_add <- function(object, sysdata = "R/sysdata.rda") {

  new_object <- deparse(substitute(object))
  stopifnot(length(new_object) == 1)

  # Test single-object save cases (No sysdata exists, or the only sysdata object
  # is being overwritten)

  single_save <- FALSE

  if (!file.exists(sysdata)) {
    message("No internal data currently saved.")
    single_save <- TRUE
  } else {

    load(sysdata)

    internal_objects <-
      setdiff(ls(), c("object", "sysdata", "new_object", "single_save"))

    if (identical(new_object, internal_objects)) {
      message(
        paste(
          "`", new_object, "`",
          "already exists in sysdata.rda, and is",
          "\n  the only object in sysdata.rda. Overwriting.",
          sep = "")
      )
      single_save <- TRUE
    }

  }

  # Proceed with the saving -- Note that the case must still be treated where
  # the new object already exists in sysdata.rda, but is not the only object in
  # sysdata.rda. This is addressed with the if statement on condition
  # `new_object %in% internal_objects`

  if (single_save) {
    command <-
      paste(
        "devtools::use_data(",
        new_object,
        ", ",
        "internal = TRUE, overwrite = TRUE)",
        sep = "")

    assign(new_object, object)
    eval(parse(text = command))
    return(invisible(command))
  }

  if (new_object %in% internal_objects) {
    message(paste("`", new_object, "`",
      "already exists in sysdata.rda.",
      "\n  Will replace it with new version.",
      sep = ""))
    rm(list = new_object)
  }

  internal_objects <-
    internal_objects[internal_objects != new_object]

  message(paste("Previous object(s) in sysdata.rda, besides ",
    new_object, ":\n  ",
    paste(internal_objects, collapse = ", "),
    sep = ""))

  command <-
    paste(
      "devtools::use_data(",
      paste(internal_objects, collapse = ", "),
      ", ",
      new_object,
      ", ",
      "internal = TRUE, overwrite = TRUE)",
      sep = "")

  assign(new_object, object)
  eval(parse(text = command))

  invisible(command)

}
