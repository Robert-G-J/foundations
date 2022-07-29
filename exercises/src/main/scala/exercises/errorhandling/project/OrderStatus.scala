package exercises.errorhandling.project
import exercises.errorhandling.NEL

sealed trait OrderStatus
object OrderStatus {
  case class Draft(basket: List[Item])     extends OrderStatus
  case class Checkout(basket: NEL[Item])  extends OrderStatus
  case class Submitted(basket: NEL[Item]) extends OrderStatus
  case class Delivered(basket: NEL[Item]) extends OrderStatus

}
