package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError._
import exercises.errorhandling.project.OrderStatus._
import exercises.errorhandling.NEL

import java.time.{Duration, Instant}

case class OrderId(value: String)

case class Order(
  id: OrderId,
  status: OrderStatus,
  createdAt: Instant
) {

  def addItem(item: Item): Either[OrderError, Order] =
    addItems(NEL(item))

  def addItems(items: NEL[Item]): Either[OrderError, Order] =
    status match {
      case x: Draft    => Right(copy(status = Draft(x.basket ++ items.toList)))
      case x: Checkout => Right(copy(status = Draft((x.basket ++ items).toList)))
      case _           => Left(InvalidStatus(status))
    }

  def checkout: Either[OrderError, Order] =
    status match {
      case x: Draft =>
        NEL.fromList(x.basket) match {
          case None      => Left(EmptyBasket)
          case Some(nel) => Right(copy(status = Checkout(nel, None)))
        }
      case _ => Left(InvalidStatus(status))
    }

  def updateDeliveryAddress(address: Address): Either[OrderError, Order] =
    status match {
      case x: Checkout => Right(copy(status = Checkout(x.basket, Some(address))))
      case _           => Left(InvalidStatus(status))
    }

  def submit(now: Instant): Either[OrderError, Order] =
    status match {
      case x: Checkout =>
        x.deliveryAddress match {
          case None          => Left(MissingDeliveryAddress)
          case Some(address) => Right(copy(status = Submitted(x.basket, address, now)))
        }
      case _ => Left(InvalidStatus(status))
    }

  def deliver(now: Instant): Either[OrderError, Order] =
    status match {
      case x: Submitted =>
        val updatedOrder = copy(status = Delivered(x.basket, x.deliveryAddress, x.submittedAt, now))
        Right(updatedOrder)
      case _ => Left(InvalidStatus(status))
    }

}

object Order {
  // Creates an empty draft order.
  def empty(id: String, now: Instant): Order =
    Order(
      id = OrderId(id),
      status = Draft(List.empty),
      createdAt = now
    )
}
