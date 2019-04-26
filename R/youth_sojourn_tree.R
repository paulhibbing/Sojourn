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
    Hip,
    floor(100*(epoch/60)),
    floor(275*(epoch/5))
  )

  if(RAW){
    threshold <- ifelse(
      Hip,
      63.3,
      35.6
    )
  }

  Intensity.Est <- ""

  if(RAW){
    Intensity.Est <- ifelse(
      Vector.Magnitude<threshold &
        (Sojourns.Est=="Sedentary"|Block.Est=="Sedentary"),
      "Sedentary",
      Intensity.Est
    )
    Intensity.Est <- ifelse(
      Sojourns.Est=="MVPA" &
        Block.Est=="MVPA",
      "MVPA",
      Intensity.Est
    )
  } else{
    Intensity.Est <- ifelse(
      Intensity.Est=="" &
        (Sojourns.Est=="Sedentary"|Block.Est=="Sedentary"),
      "Sedentary",
      Intensity.Est
    )
  } # If one says it's sedentary, give sedentary

  Counter <- cumsum(Vector.Magnitude>threshold)
  n <- rep(
    tapply(Counter, Counter, length),
    tapply(Counter, Counter, length)
  )
  Intensity.Est <- ifelse(
    Intensity.Est=="" &
      n>=60,
    "Sedentary",
    Intensity.Est
  )

  Counter <- cumsum(Sojourns.Est!="MVPA"|Block.Est!="MVPA")
  n <- rep(
    tapply(Counter, Counter, length),
    tapply(Counter, Counter, length)
  )
  Intensity.Est <- ifelse(Intensity.Est==""&n>60, "MVPA",Intensity.Est)

  Intensity.Est <- ifelse(Intensity.Est=="","Light",Intensity.Est)

  Intensity.Est <- factor(Intensity.Est, levels = c("Sedentary","Light","MVPA"))

  return(Intensity.Est)

}
