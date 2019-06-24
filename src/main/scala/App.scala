object App {
    def main(args: Array[String]): Unit = {
        println(new ABParser("ab").Main.run().get)
    }
}

import org.parboiled2._
import shapeless._

trait A

trait B

case class B2A(b: B) extends A

case class A2B(a: A) extends B

class ABParser(override val input: ParserInput) extends Parser {
    def Main: Rule1[A] = rule {
        BRule ~ BReduction ~ B2ARule
    }

    def BRule: Rule1[B] = rule {
        push(new B {})
    }

    def BReduction: Rule[B :: HNil, B :: HNil] = rule {
        ((B2ACapture | B2ARule) ~ A2BCapture).*
    }

    def B2ARule: Rule[B :: HNil, A :: HNil] = rule {
        MATCH ~> B2A
    }

    def B2ACapture: Rule[B :: HNil, A :: HNil] = rule {
        str("a") ~> B2A
    }

    def A2BCapture: Rule[A :: HNil, B :: HNil] = rule {
        str("b") ~> A2B
    }
}
