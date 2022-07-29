package exercises.errorhandling.project
import exercises.errorhandling.NEL

sealed trait OrderStatus
object OrderStatus {
  case class Draft(basket: List[Item])                                     extends OrderStatus
  case class Checkout(basket: NEL[Item], deliveryAddress: Option[Address]) extends OrderStatus
  case class Submitted(basket: NEL[Item], deliveryAddress: Address)        extends OrderStatus
  case class Delivered(basket: NEL[Item], deliveryAddress: Address)        extends OrderStatus

}
