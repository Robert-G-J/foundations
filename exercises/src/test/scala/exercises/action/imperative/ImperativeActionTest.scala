package exercises.action.imperative

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.{Failure, Success, Try}

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.imperative.ImperativeActionTest
class ImperativeActionTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("retry when maxAttempt is 0") {
    val result = Try(retry(0)(() => ""))
    assert(result.isFailure)
  }

  test("retry when action fails") {
    var counter = 0
    val error = new Exception("HELLO")

    val result = Try(retry(5) { () => {
      counter += 1
      throw error
      }
    })

    assert(result == Failure(error))
    assert(counter == 5)
  }

  test("retry until action succeeds") {
    var counter = 0
    val result = Try(retry(5) { () => {
      counter += 1
      require(counter >= 3, "Counter is too low")
      "Hello"
      }
    })
    assert(result == Success("Hello"))
    assert(counter == 3)
  }

  test("onError when failure") {
    var counter = 0
    val error = new Exception("Boom")

    val result = Try(onError(() => {
      counter += 1
      throw error
    }, e => println(s"An error occurred: ${e.getMessage}")))

    assert(result == Failure(error))
  }

  test("onError when success") {
    var counterAction, counterError = 0
    val result = Try(
      onError(
        () => {
          counterAction += 1
          "Success"
        },
        e => counterError += 1
      )
    )
  }

  test("onError pbt") {
    forAll {
      (actionResult: Try[String], cleanupResult: Try[Int]) =>
        val result = Try(onError(
          () => actionResult.get,
          _ => cleanupResult.get
          )
        )
        assert(result == actionResult)
      }
  }
}
