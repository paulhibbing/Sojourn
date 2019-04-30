rm(list = ls())
devtools::load_all()

# Process file from Laura with SIP ----------------------------------------

  ## Locate files

    ag_file <- file.path(
      "data-raw/SIP_Testing",
      "LB004 (2016-03-29)1secDataTable.csv"
    )

    ap_file <- file.path(
      "data-raw/SIP_Testing",
      "LB004-AP471706 29Mar16 02-34pm for 7d 2h 25m Events.csv"
    )

  ## Read files

    ag <- AGread::read_AG_counts(
      ag_file, skip = 11, verbose = TRUE
    )

    ap <- read_AP(ap_file)

  ## Combine and apply SIP

    ag <- enhance_actigraph(ag, ap)
    ag <- sojourn_3x_SIP(ag)

# Compare against output from Laura ---------------------------------------

  ref_file <- file.path(
    "data-raw/SIP_Testing",
    "LB004 (2016-03-29)1secDataTable_with_activpal_sojourns.csv"
  )

  ref_data <- data.frame(
    data.table::fread(ref_file, stringsAsFactors = FALSE, skip = 11),
    stringsAsFactors = FALSE, row.names = NULL
  )

  ref_data$Timestamp <- gsub("-....$", "", ref_data$Timestamp)
  ref_data$Timestamp <- as.POSIXct(
    ref_data$Timestamp, lubridate::tz(ag$Timestamp[1]),
    format = "%Y-%m-%dT%H:%M:%S"
  )

  # > anyNA(ref_data$Timestamp)
  # [1] FALSE

  ## Line the names and formats up

    names(ag) <- gsub("^Steps$", "steps", names(ag))

    # > all(names(ref_data) %in% names(ag))
    # [1] TRUE

    ag <- ag[ ,names(ref_data)]

    attributes(ag$perc.soj) <- NULL

    ref_data$vect.mag <- round(ref_data$vect.mag, 2)

  ## Test

    all.equal(ag, ref_data, scale = 1)
    all(
      PAutilities::test_errors(
        ag, ref_data, names(ag)[-1]
      )
    )
