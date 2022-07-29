package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus, MissingDeliveryAddress}
import exercises.errorhandling.project.OrderGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Checkout")
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = Nil,
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(InvalidStatus("Delivered")))
  }

  test("submit successful example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.submit(Instant.now()) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Submitted")
    }
  }

  test("submit no address example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(
      order.submit(Instant.now()) == Left(MissingDeliveryAddress)
    ) // replace ??? by the error you created for that scenario
  }

  test("submit invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus("Delivered")))
  }

  test("submit empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = Nil,
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(EmptyBasket))
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

    assert(
      result.map(_._1) == Right(
        Order(
          id = orderId,
          status = "Delivered",
          basket = List(item1, item2),
          deliveryAddress = Some(deliveryAddress),
          createdAt = createdAt,
          submittedAt = Some(submittedAt),
          deliveredAt = Some(deliveredAt)
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
            status = "Delivered",
            basket = items.toList,
            deliveryAddress = Some(deliveryAddress),
            createdAt = createdAt,
            submittedAt = Some(submittedAt),
            deliveredAt = Some(deliveredAt)
          )
        )
      )

      assert(result.map(_._2) == Right(deliveredDelay))
    }
  }

}
