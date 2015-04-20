import org.scalatest.{Matchers, FlatSpec}

import playfair.Playfair

class PlayfairTest extends FlatSpec with Matchers {

  val p = new Playfair

  "A readCommand method " should "return false if an unknown String is passed as an arg." in {
    p.readCommand("skdjfhaio") should be(false)
  }

  it should "return true if \"quit\" is passed as an arg" in {
    p.readCommand("quit") should be(true)
  }

}
