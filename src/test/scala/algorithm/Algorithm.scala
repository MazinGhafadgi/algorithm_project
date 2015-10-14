package algorithm


import chapter2.Node2
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

class AlgorithmTest extends FunSpec with Matchers with GivenWhenThen {
  describe("Linked test") {
    it("should should delete the element from the middle") {
      Given("the linked list have five elements only.")
      val n = Node2(1)

      n.append(2)
      n.append(3)
      n.append(4)
      n.append(5)
      n.append(6)
      When("delete fron the middle called.")
      val ne = n deleteFromTheMiddle

      Then("the contains method should return false if we try to check number 3.")
      val value = ne.contains(3)
      value should be(true)
    }

    it("should delete any duplicate number from the linked list.") {
      Given("The list has duplicate numbers ")
      val n = new Node2(1)
      n.append(1)
      n.append(2)
      n.append(3)
      When("calling length method the result ")
      val l = (n length)
      Then("the length should be 4")
      l should be(4)
    }

    it("should return nth number of elements") {
      Given("The linked list has four elements")
      val n = new Node2(1)
      n.append(2)
      n.append(3)
      n.append(4)
      When("calling the nthToLast method starting from 2 ")
      val l = n nthToLast (2)
      Then("The length of the linked list should be 2 ")
      (l.length) should be(2)
    }

    it("should delete one elements from the linked list.") {
      Given("the list has three elements ")
      val n = new Node2(1)
      n.append(2)
      n.append(3)
      When("calling delete method ")
      val n1 = n delete2 (2)
      Then("the list should have only two elements.")
      (n1 length) should be(2)
    }
  }
}