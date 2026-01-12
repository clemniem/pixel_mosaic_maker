package mosaic.domain

sealed trait BuildStatus
object BuildStatus {
  case object InProgress extends BuildStatus
  case object Completed extends BuildStatus
}

final case class MosaicBuild(
  id: String,
  configId: MosaicConfigId,
  currentStep: Int,
  status: BuildStatus
)
