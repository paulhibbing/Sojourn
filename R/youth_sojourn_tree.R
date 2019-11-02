#' Decision tree for making final predictions in the youth Sojourn method
#'
#' @param Vector.Magnitude numeric vector of triaxial acceleration (vector
#'   magnitude for activity counts, and Euclidian Norm Minus One for raw
#'   acceleration)
#' @param Block.Est predictions obtained from the neural networks using static
#'   segmentation
#' @param Sojourns.Est predictions obtained from the neural networks using
#'   dynamic segmentation (i.e., Sojourn-specific predictions)
#' @param Hip logical. Are data from a hip-worn device?
#' @inheritParams youth_network_shape
#'
#' @return A factor vector of predicted physical activity intensities
#'   (Sedentary, Light, or moderate-to-vigorous physical activity (MVPA))
#' @keywords internal
#'
youth_sojourn_tree <- function(
  Vector.Magnitude, Block.Est, Sojourns.Est,
  Hip = TRUE, RAW = FALSE, epoch = 1
) {

  ##Initialize threshold for counts variables

    threshold <- ifelse(
      Hip, floor(100*(epoch/60)), floor(275*(epoch/5))
    )

    if(RAW){
      threshold <- ifelse(Hip, 63.3, 35.6)
    }

  ##Initialize the final vector

    Intensity.Est <-
      cbind(Vector.Magnitude, Block.Est, Sojourns.Est) %>%
      stats::complete.cases(.) %>%
      ifelse("", "NA")

    nas <- sum(Intensity.Est == "NA")

  ##Different opening steps for RAW vs counts

    if(RAW){

      Intensity.Est <-
        (Intensity.Est == "") %>%
        {. & (
          Sojourns.Est == "Sedentary" |
          Block.Est    == "Sedentary"
        )} %>%
        {. & Vector.Magnitude < threshold} %>%
        ifelse("Sedentary", Intensity.Est)

      Intensity.Est <-
        (Intensity.Est == "") %>%
        {. & (Sojourns.Est == "MVPA")} %>%
        {. & (Block.Est    == "MVPA")} %>%
        ifelse("MVPA", Intensity.Est)

    } else{

      Intensity.Est <-
        (Intensity.Est == "") %>%
        {. & (
          Sojourns.Est == "Sedentary" |
          Block.Est    == "Sedentary"
        )} %>%
          ifelse("Sedentary", Intensity.Est)


    }

  ##Now the same steps for both

    counter <-
      (Vector.Magnitude <= threshold) %>%
      PAutilities::index_runs(.)
    n <-
      ifelse(counter$values %in% TRUE, counter$lengths, 1) %>%
      rep(counter$lengths)

    Intensity.Est <-
      (Intensity.Est == "") %>%
      {. & n >= 60} %>%
      ifelse("Sedentary", Intensity.Est)

    counter <-
      (Sojourns.Est == "MVPA" | Block.Est == "MVPA") %>%
      PAutilities::index_runs(.)
    n <-
      ifelse(counter$values %in% TRUE, counter$lengths, 1) %>%
      rep(counter$lengths)

    Intensity.Est <-
      (Intensity.Est == "") %>%
      {. & n >= 60} %>%
      ifelse("MVPA", Intensity.Est)

    Intensity.Est <- ifelse(
      Intensity.Est == "", "Light", Intensity.Est
    )

    stopifnot(sum(Intensity.Est == "NA") == nas)

    factor(
      Intensity.Est,
      levels = c("Sedentary", "Light", "MVPA")
    ) %>%
      unname(.)

}
