package exercises.errorhandling.validation

import Validation._
import exercises.errorhandling.NEL
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ValidationTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("Validation sequence") {
    val listOfValidations = List(1.valid, 2.invalid, 3.invalid, 4.valid)
    val expected          = Invalid(NEL(2, 3))

    assert(sequence(listOfValidations) == expected)
  }

}
