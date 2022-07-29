package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus, MissingDeliveryAddress}
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatus._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.Days
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val basket = NEL(Item("A1", 2, 12.99))
    val order = Order(
      id = "AAA",
      status = Draft(basket.toList),
      createdAt = Instant.now()
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout(basket, None))
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = Draft(Nil),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val items = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(1, "EX31")
    val submittedAt = Instant.EPOCH
    val deliveredAt = submittedAt.plus(Duration.ofDays(200))
    val order = Order(
      id = "AAA",
      status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(InvalidStatus(Delivered(items, deliveryAddress, submittedAt, deliveredAt))))
  }

  test("submit successful example") {
    val basket = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(1, "EX31 8TR")
    val createdAt = Instant.EPOCH
    val submittedAt = createdAt.plus(Duration.ofDays(200))
    val order = Order(
      id = "AAA",
      status = Checkout(basket, Some(deliveryAddress)),
      createdAt = createdAt
    )

    order.submit(submittedAt, deliveryAddress) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted(basket, deliveryAddress, submittedAt))
    }
  }

  test("submit no address example") {
    val basket = NEL(Item("A1", 2, 12.99))
    val submittedAt = Instant.EPOCH
    val order = Order(
      id = "AAA",
      status = Checkout(basket, None),
      createdAt = Instant.now()
    )

    assert(
      order.submit(submittedAt) == Left(MissingDeliveryAddress)
    )
  }

  test("submit invalid status example") {
    val basket = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(12, "E16 8TR")
    val submittedAt = Instant.EPOCH
    val deliveredAt = submittedAt.plus(Duration.ofDays(200))
    val order = Order(
      id = "AAA",
      status = Delivered(basket, deliveryAddress, submittedAt, deliveredAt),
      createdAt = Instant.now()
    )

    assert(order.submit(submittedAt) == Left(InvalidStatus(Delivered(basket, deliveryAddress, submittedAt, deliveredAt))))
  }

 test("checkout is not allowed if order is in check, submitted or delivered status"){
   forAll(Gen.oneOf(checkoutGen, submittedGen, deliveredGen)){order =>
      assert(order.checkout == Left(InvalidStatus(order.status)))
   }
 }

  test("happy path") {
    val orderId         = "ORD0001"
    val createdAt       = Instant.now()
    val submittedAt     = createdAt.plusSeconds(5)
    val deliveredAt     = submittedAt.plusSeconds(3600 * 30) // 30 hours
    val order           = Order.empty(orderId, createdAt)
    val item1           = Item("AAA", 2, 24.99)
    val item2           = Item("BBB", 1, 15.49)
    val deliveryAddress = Address(23, "E16 8FV")

    val result = for {
      order         <- order.addItem(item1)
      order         <- order.addItem(item2)
      order         <- order.checkout
      order         <- order.updateDeliveryAddress(deliveryAddress)
      order         <- order.submit(submittedAt)
      orderDuration <- order.deliver(deliveredAt)
    } yield orderDuration

    val basket = NEL(item1, item2)

    assert(
      result.map(_._1) == Right(
        Order(
          id = orderId,
          status = Delivered(basket, deliveryAddress, submittedAt, deliveredAt),
          createdAt = createdAt
        )
      )
    )

    assert(result.map(_._2) == Right(Duration.ofHours(30)))
  }

  test("Order workflow pbt") {
    forAll(orderIdGen, instantGen, durationGen, durationGen, nelOf(itemGen), addressGen){ (orderId, createdAt, submittedDelay, deliveredDelay, items, deliveryAddress) =>
      val submittedAt     = createdAt.plus(submittedDelay)
      val deliveredAt     = submittedAt.plus( deliveredDelay)
      val order           = Order.empty(orderId, createdAt)

      val result = for {
        order         <- order.addItems(items)
        order         <- order.checkout
        order         <- order.updateDeliveryAddress(deliveryAddress)
        order         <- order.submit(submittedAt)
        orderDuration <- order.deliver(deliveredAt)
      } yield orderDuration

      assert(
        result.map(_._1) == Right(
          Order(
            id = orderId,
            status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
            createdAt = createdAt
          )
        )
      )

      assert(result.map(_._2) == Right(deliveredDelay))
    }
  }

}
