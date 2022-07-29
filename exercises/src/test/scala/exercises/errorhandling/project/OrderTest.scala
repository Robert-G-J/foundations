package exercises.errorhandling.project

import exercises.errorhandling.NEL
import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus, MissingDeliveryAddress}
import exercises.errorhandling.project.OrderGenerator._
import exercises.errorhandling.project.OrderStatus._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val id     = OrderId("AAA")
    val basket = NEL(Item("A1", 2, 12.99))
    val order = Order(
      id,
      status = Draft(basket.toList),
      createdAt = Instant.now()
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Checkout(basket, None))
    }
  }

  test("checkout empty basket example") {
    val id = OrderId("AAA")
    val order = Order(
      id,
      status = Draft(Nil),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val id              = OrderId("AAA")
    val items           = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(1, "EX31")
    val submittedAt     = Instant.EPOCH
    val deliveredAt     = submittedAt.plus(Duration.ofDays(200))
    val order = Order(
      id,
      status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
      createdAt = Instant.now()
    )

    assert(order.checkout == Left(InvalidStatus(Delivered(items, deliveryAddress, submittedAt, deliveredAt))))
  }

  test("submit successful example") {
    val id              = OrderId("AAA")
    val basket          = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(1, "EX31 8TR")
    val createdAt       = Instant.EPOCH
    val submittedAt     = createdAt.plus(Duration.ofDays(200))
    val order = Order(
      id = id,
      status = Checkout(basket, Some(deliveryAddress)),
      createdAt = createdAt
    )

    order.submit(submittedAt) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == Submitted(basket, deliveryAddress, submittedAt))
    }
  }

  test("submit no address example") {
    val id          = OrderId("AAA")
    val basket      = NEL(Item("A1", 2, 12.99))
    val submittedAt = Instant.EPOCH
    val order = Order(
      id,
      status = Checkout(basket, None),
      createdAt = Instant.now()
    )

    assert(
      order.submit(submittedAt) == Left(MissingDeliveryAddress)
    )
  }

  test("submit invalid status example") {
    val id              = OrderId("AAA")
    val basket          = NEL(Item("A1", 2, 12.99))
    val deliveryAddress = Address(12, "E16 8TR")
    val submittedAt     = Instant.EPOCH
    val deliveredAt     = submittedAt.plus(Duration.ofDays(200))
    val order = Order(
      id,
      status = Delivered(basket, deliveryAddress, submittedAt, deliveredAt),
      createdAt = Instant.now()
    )

    assert(
      order.submit(submittedAt) == Left(InvalidStatus(Delivered(basket, deliveryAddress, submittedAt, deliveredAt)))
    )
  }

  test("addItems invalid in Submitted and delivered") {
    forAll(Gen.oneOf(submittedGen, deliveredGen), nelOf(itemGen)) { (order, items) =>
      assert(order.addItems(items).isLeft)
    }
  }

  test("checkout invalid in checkout, submitted and delivered") {
    forAll(Gen.oneOf(checkoutGen, submittedGen, deliveredGen)) { order =>
      assert(order.checkout.isLeft)
    }
  }

  test("updateDeliveryAddress invalid in draft, submitted and delivered") {
    forAll(Gen.oneOf(draftGen, submittedGen, deliveredGen), addressGen) { (order, address) =>
      assert(order.updateDeliveryAddress(address).isLeft)
    }
  }

  test("checkout invalid checkout, submitted and delivered") {
    forAll(Gen.oneOf(checkoutGen, submittedGen, deliveredGen)) { order =>
      assert(order.checkout.isLeft)
    }
  }

  test("deliver invalid in draft, checkout and delivered") {
    forAll(Gen.oneOf(draftGen, checkoutGen, deliveredGen), instantGen) { (order, now)=>
      assert(order.deliver(now).isLeft)
    }
  }

  test("happy path") {
    val orderId         = OrderId("ORD0001")
    val createdAt       = Instant.now()
    val submittedAt     = createdAt.plusSeconds(5)
    val deliveredAt     = submittedAt.plusSeconds(3600 * 30) // 30 hours
    val order           = Order.empty(orderId.value, createdAt)
    val item1           = Item("AAA", 2, 24.99)
    val item2           = Item("BBB", 1, 15.49)
    val deliveryAddress = Address(23, "E16 8FV")

    val result = for {
      order <- order.addItem(item1)
      order <- order.addItem(item2)
      order <- order.checkout
      order <- order.updateDeliveryAddress(deliveryAddress)
      order <- order.submit(submittedAt)
      order <- order.deliver(deliveredAt)
    } yield order

    val basket = NEL(item1, item2)

    assert(
      result == Right(
        Order(
          id = orderId,
          status = Delivered(basket, deliveryAddress, submittedAt, deliveredAt),
          createdAt = createdAt
        )
      )
    )
  }

  test("Order workflow pbt") {
    forAll(orderIdGen, instantGen, durationGen, durationGen, nelOf(itemGen), addressGen) {
      (orderId, createdAt, submittedDelay, deliveredDelay, items, deliveryAddress) =>
        val submittedAt = createdAt.plus(submittedDelay)
        val deliveredAt = submittedAt.plus(deliveredDelay)
        val order       = Order.empty(orderId.value, createdAt)

        val result = for {
          order         <- order.addItems(items)
          order         <- order.checkout
          order         <- order.updateDeliveryAddress(deliveryAddress)
          order         <- order.submit(submittedAt)
          orderDuration <- order.deliver(deliveredAt)
        } yield orderDuration

        assert(
          result == Right(
            Order(
              id = orderId,
              status = Delivered(items, deliveryAddress, submittedAt, deliveredAt),
              createdAt = createdAt
            )
          )
        )
    }
  }

}
