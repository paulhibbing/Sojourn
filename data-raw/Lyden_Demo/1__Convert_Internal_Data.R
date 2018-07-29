## Load and identify the objects
  rm(list = ls())
  source("data-raw/z__internal_add.R")

  files <- list.files(
    "data-raw/Lyden_Demo", pattern = "RData",
    full.names = TRUE, ignore.case = TRUE
  )

  pre_load_ls <- ls()

  invisible(
    lapply(files, load, envir = globalenv())
  )

## Really inefficient way to do this, but...
  lapply(post_load_ls,
    function(x) {
      command <- paste(
        "internal_add(",
        x,
        ")",
        sep = ""
      )
      eval(parse(text = command))
    })
