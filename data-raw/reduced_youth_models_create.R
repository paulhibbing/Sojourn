## Load everything

  rm(list = ls())
  load("R/sysdata.rda")

## Track the original number of objects, and the total size

  num_objects <- length(ls())
  total_size <- sum(
    sapply(
      setdiff(ls(), "num_objects"),
      function(x) object.size(get(x, envir = globalenv()))
    )
  )

## Isolate the offending objects that need to be shrunk down

  objects <- sapply(
    ls(),
    function(x) object.size(get(x, envir = globalenv()))
  )
  objects <- names(
    objects[order(objects, decreasing = TRUE)][1:4]
  )
  objects <- setNames(
    lapply(objects, get, envir = globalenv()),
    objects
  )

  rm(
    list = setdiff(
      ls(), c("objects", "num_objects", "total_size")
    )
  )

## Reduce the object sizes (reduction tracking in the comments)

  # size <- data.frame(
  #   object = names(objects),
  #   pre = sapply(objects, object.size),
  #   stringsAsFactors = FALSE,
  #   row.names = NULL
  # )

  objects <- lapply(
    objects,
    function(x) x$finalModel
  )

  # size$post <- sapply(objects, object.size)
  # size$pct_reduction <- with(
  #   size,
  #   paste(
  #     round(((pre - post) / pre) * 100, 1),
  #     "%", sep = ""
  #   )
  # )

  list2env(objects, globalenv())
  rm(objects)

## Save in data-raw

  save(
    youth_hipCounts,
    youth_hipRaw,
    youth_wristCounts,
    youth_wristRaw,
    num_objects,
    total_size,
    file = "data-raw/reduced_youth_models.RData"
  )

## Load existing sysdata to overwrite with old objects

  objects <- setdiff(ls(), c("num_objects", "total_size"))
  load("R/sysdata.rda")
  rm(list = objects)
  load("data-raw/reduced_youth_models.RData")
  suppressMessages(rm(objects))

## Check against the original number/size of objects in sysdata

  # ORIGINAL:
  #   > num_objects
  #   [1] 43
  #   > total_size
  #   [1] 10576064

  rm(num_objects, total_size)

  num_objects <- length(ls())
  total_size <- sum(
    sapply(
      setdiff(ls(), "num_objects"),
      function(x) object.size(get(x, envir = globalenv()))
    )
  )

  # NEW:
  #   > num_objects
  #   [1] 43
  #   > total_size
  #   [1] 5972720

  # > diff(c(5972720, 10576064))
  # [1] 4603344

  rm(num_objects, total_size)

  # SANITY CHECK:
  # > length(ls())
  # [1] 43

## Save new sysdata file

  objects <- ls()
  file.remove("R/sysdata.rda")
  command <- paste(
    "devtools::use_data(",
    paste(objects, collapse = ", "),
    ", ",
    "internal = TRUE, overwrite = TRUE)",
    sep = ""
  )
  eval(parse(text = command))

# Test apply_youth_sojourn() with smaller models

  rm(list = ls())
  devtools::load_all()
  load("data/example_data.rda")
  new_results <- apply_youth_sojourn(
    AG = example_data, vm = "Vector.Magnitude", Site = "Hip"
  )

  save(
    new_results,
    file = "data-raw/example_results_new.RData"
  )

# Compare with predictions from the original method (using the enormous objects)

  ##NOTE: These results generated in a very particular way. See
  ##"data-raw/example_results_old_create.R" for clarification.

  load("data-raw/example_results_old.RData")

  all.equal(new_results[ ,names(old_results)], old_results)
  identical(new_results[ ,names(old_results)], old_results)

# GOOD TO GO
