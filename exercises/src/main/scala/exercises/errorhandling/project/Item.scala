package exercises.errorhandling.project

case class Item(id: String, quantity: Quantity, unitPrice: BigDecimal)

case class Quantity(value: Nat)

final case class Nat(n: Int)
object Nat {
  def fromInt(n: Int): Option[Nat] =
    if (n >= 0) Some(new Nat(n)) else None
}
