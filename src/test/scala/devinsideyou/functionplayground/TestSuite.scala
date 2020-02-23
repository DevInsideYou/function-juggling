package devinsideyou
package functionplayground

import org.scalacheck._
import org.scalatest._
import org.scalatestplus.scalacheck._

trait TestSuite
    extends funsuite.AnyFunSuite
    with matchers.should.Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with ScalaCheckPropertyChecks
    with ScalacheckShapeless {
  final protected type Assertion = compatible.Assertion
}
