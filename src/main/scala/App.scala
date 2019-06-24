object App {
    def main(args: Array[String]): Unit = {
        println(new AParser("ab").Main.run().get)
    }
}

import org.parboiled2._
import shapeless._

trait B

trait A

case class A2B(a: A) extends B

case class B2A(b: B) extends A

class AParser(override val input: ParserInput) extends Parser {
    def Main: Rule1[A] = rule {
        ARule ~ AReduction
    }

    def ARule: Rule1[A] = rule {
        push(new A {})
    }

    def AReduction: Rule[A :: HNil, A :: HNil] = rule {
        ((A2BConsumer | A2BRule) ~ B2AConsumer).*
    }

    def A2BRule: Rule[A :: HNil, B :: HNil] = rule {
        MATCH ~> A2B
    }

    def A2BConsumer: Rule[A :: HNil, B :: HNil] = rule {
        str("a") ~> A2B
    }

    def B2AConsumer: Rule[B :: HNil, A :: HNil] = rule {
        str("b") ~> B2A
    }
}
