package exercises.errorhandling.project

sealed trait OrderError
object OrderError {
  case class InvalidStatus(currentStatus: String) extends OrderError
  case object EmptyBasket                         extends OrderError
  case object MissingDeliveryAddress                   extends OrderError
  case object MissingSubmittedAtTimestamp         extends OrderError
}
