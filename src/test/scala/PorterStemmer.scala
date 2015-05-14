import org.scalatest._
import org.scalatest.matchers.ShouldMatchers
import com.rodic.PorterStemmer
import scala.io.Source._

class PorterStemmerSpec extends FunSpec with ShouldMatchers {
 
  def stepTest(fn: List[Char] => List[Char]): (String, String) => Unit = {
    (input: String, output: String) => fn(input.toList) shouldEqual output.toList
  }

  val step1aTest = stepTest(PorterStemmer.step1a)
  val step1bTest = stepTest(PorterStemmer.step1b)
  val step1cTest = stepTest(PorterStemmer.step1c)
  val step2Test  = stepTest(PorterStemmer.step2)
  val step3Test  = stepTest(PorterStemmer.step3)
  val step4Test  = stepTest(PorterStemmer.step4)
  val step5aTest = stepTest(PorterStemmer.step5a)
  val step5bTest = stepTest(PorterStemmer.step5b)

  describe("step1a") {

    it("should replace 'sses' for 'ss'") {
      step1aTest("kisses", "kiss")
      step1aTest("possesses", "possess")
    }

    it("should replace 'ies' for 'i'") {
      step1aTest("abilities", "abiliti")
      step1aTest("zombies", "zombi")
      step1aTest("societies", "societi")
      step1aTest("ties", "ti")
    }

    it("should leave 'ss'") {
      step1aTest("toss", "toss")
      step1aTest("sugarless", "sugarless")
      step1aTest("progress", "progress")
    }

    it("should remove single 's'") {
      step1aTest("cats", "cat")
      step1aTest("tires", "tire")
      step1aTest("uses", "use")
    }

    it("should not replace anything else") {
      step1aTest("morning", "morning")
      step1aTest("screem", "screem")
    }
  }

  describe("step1b") {

    it("should replace 'eed' with 'ee' when m > 0") {
      step1bTest("feed", "feed")
      step1bTest("speed", "speed")
      step1bTest("agreed", "agree")
      step1bTest("hayseed", "haysee")
    }

    it("should remove 'ed' if there's a vowel before it and then trim it") {
      step1bTest("plastered", "plaster")
      step1bTest("psychoanalyzed", "psychoanalyz")      
      step1bTest("hyped", "hype")      
      step1bTest("bled", "bled")
      step1bTest("conflated", "conflate")
      step1bTest("troubled", "trouble")
      step1bTest("sized", "size")
      step1bTest("tanned", "tan")
      step1bTest("fizzed", "fizz")
    }

    it("should remove 'ing' if there's a vowel before it and then trim it") {
      step1bTest("motoring", "motor")
      step1bTest("sing", "sing")
      step1bTest("stymying", "stymy")
      step1bTest("eyeing", "eye")
      step1bTest("hopping", "hop")
      step1bTest("falling", "fall")
      step1bTest("hissing", "hiss")
      step1bTest("failing", "fail")
      step1bTest("filing", "file")
      step1bTest("zooming", "zoom")
      step1bTest("baying", "bay")
    }
  }

  describe("step1c") {
    
    it("should replace 'y' with 'i' at the end of the word if there's a vowel before it") {
      step1cTest("happy", "happi")
      step1cTest("sky", "sky")
      step1cTest("bay", "bai")
    }
  }

  describe("step2") {

    it("should replace 'ational' with 'ate' if m before it is greater than 0") {
      step2Test("relational", "relate")
    }

    it("should replace 'tional' with 'tion' if m before it is greater than 0") {
      step2Test("conditional", "condition")
      step2Test("rational", "rational")
    }

    it("should replace 'enci' with 'ence' if m before it is greater than 0") {
      step2Test("valenci", "valence")
    }

    it("should replace 'anci' with 'ance' if m before it is greater than 0") {
      step2Test("hesitanci", "hesitance")
    }

    it("should replace 'izer' with 'ize' if m before it is greater than 0") {
      step2Test("digitizer", "digitize")
    }

    it("should replace 'bli' with 'ble' if m before it is greater than 0") {
      step2Test("conformabli", "conformable")
    }

    it("should replace 'alli' with 'al' if m before it is greater than 0") {
      step2Test("radicalli", "radical")
    }

    it("should replace 'entli' with 'ent' if m before it is greater than 0") {
      step2Test("differentli", "different")
    }

    it("should replace 'eli' with 'e' if m before it is greater than 0") {
      step2Test("vileli", "vile")
    }

    it("should replace 'ousli' with 'ous' if m before it is greater than 0") {
      step2Test("analogousli", "analogous")
    }

    it("should replace 'ization' with 'ize' if m before it is greater than 0") {
      step2Test("vietnamization", "vietnamize")
    }

    it("should replace 'ation' with 'ate' if m before it is greater than 0") {
      step2Test("predication", "predicate")
    }

    it("should replace 'ator' with 'ate' if m before it is greater than 0") {
      step2Test("operator", "operate")
    }

    it("should replace 'alism' with 'al' if m before it is greater than 0") {
      step2Test("feudalism", "feudal")
    }

    it("should replace 'iveness' with 'ive' if m before it is greater than 0") {
      step2Test("decisiveness", "decisive")
    }

    it("should replace 'fulness' with 'ful' if m before it is greater than 0") {
      step2Test("hopefulness", "hopeful")
    }

    it("should replace 'ousness' with 'ous' if m before it is greater than 0") {
      step2Test("callousness", "callous")
    }
  
    it("should replace 'aliti' with 'al' if m before it is greater than 0") {
      step2Test("formaliti", "formal")
    }

    it("should replace 'iviti' with 'ive' if m before it is greater than 0") {
      step2Test("sensitiviti", "sensitive")
    }

    it("should replace 'biliti' with 'ble' if m before it is greater than 0") {
      step2Test("abiliti", "abiliti")
      step2Test("sensibiliti", "sensible")
    }
  }

  describe("step3") {

    it("should replace 'icate' with 'ic' when m before it is greater than 0") {
      step3Test("triplicate", "triplic")
    }

    it("should remove 'ative' when m before it is greater than 0") {
      step3Test("formative", "form")
    }

    it("should replace 'alize' with 'al' when m before it is greater than 0") {
      step3Test("formalize", "formal")
    }

    it("should replace 'iciti' with 'ic' when m before it is greater than 0") {
      step3Test("electriciti", "electric")
    }

    it("should replace 'ical' with 'ic' when m before it is greater than 0") {
      step3Test("electrical", "electric")
    }

    it("should remove 'ful' when m before it is greater than 0") {
      step3Test("hopeful", "hope")
    }

    it("should remove 'ness' when m before it is greater than 0") {
      step3Test("goodness", "good")
      step3Test("dryness", "dryness")
    }
  }

  describe("step4") {

    it("should remove 'al' if m before it is greater than 1") {
      step4Test("revival", "reviv")
    }

    it("should remove 'ance' if m before it is greater than 1") {
      step4Test("allowance", "allow")
    }

    it("should remove 'ence' if m before it is greater than 1") {
      step4Test("inference", "infer")
    }

    it("should remove 'er' if m before it is greater than 1") {
      step4Test("airliner", "airlin")
    }

    it("should remove 'ic' if m before it is greater than 1") {
      step4Test("gyroscopic", "gyroscop")
    }

    it("should remove 'able' if m before it is greater than 1") {
      step4Test("adjustable", "adjust")
    }

    it("should remove 'ible' if m before it is greater than 1") {
      step4Test("defensible", "defens")
    }

    it("should remove 'ant' if m before it is greater than 1") {
      step4Test("irritant", "irrit")
    }

    it("should remove 'ement' if m before it is greater than 1") {
      step4Test("replacement", "replac")
    }

    it("should remove 'ment' if m before it is greater than 1") {
      step4Test("adjustment", "adjust")
      step4Test("agreement", "agreement")
      step4Test("argument", "argument")
    }

    it("should remove 'ent' if m before it is greater than 1") {
      step4Test("dependent", "depend")
    }

    it("should remove 'ion' if there's 't' before it and m is greater than 1") {
      step4Test("adoption", "adopt")
    }

    it("should remove 'ion' if there's 's' before it and m is greater than 1") {
      step4Test("transfusion", "transfus")
    }

    it("should remove 'ou' if m before it is greater than 1") {
      step4Test("homologou", "homolog")
    }

    it("should remove 'ism' if m before it is greater than 1") {
      step4Test("communism", "commun")
    }

    it("should remove 'ate' if m before it is greater than 1") {
      step4Test("activate", "activ")
    }

    it("should remove 'iti' if m before it is greater than 1") {
      step4Test("abiliti", "abil")
      step4Test("angulariti", "angular")
    }

    it("should remove 'ous' if m before it is greater than 1") {
      step4Test("homologous", "homolog")
    }

    it("should remove 'ive' if m before it is greater than 1") {
      step4Test("effective", "effect")
    }

    it("should remove 'ize' if m before it is greater than 1") {
      step4Test("bowdlerize", "bowdler")
    }
  }

  describe("step5a") {
    
    it("should remove 'e' when m before it is greater than 1") {
      step5aTest("probate", "probat")
      step5aTest("rate", "rate")
      step5aTest("yclepe", "yclepe")
    }

    it("should remove 'e' when m=1 and ending before it isn't cvc, where the second c is not W, X or Y") {
      step5aTest("cease", "ceas")
      step5aTest("accrue", "accru")
    }
  }

  describe("step5b") {

    it("should remove doble 'l' when m is bigger than 0") {
      step5bTest("controll", "control")
      step5bTest("roll", "roll")
    }
  }

  describe("findStem") {

    it("should integrate separate steps into one call") {
      PorterStemmer.findStem("abbominable") shouldEqual "abbomin"
      PorterStemmer.findStem("abilities") shouldEqual "abil"
      PorterStemmer.findStem("captivate") shouldEqual "captiv"
      PorterStemmer.findStem("pocketing") shouldEqual "pocket"
      PorterStemmer.findStem("secondarily") shouldEqual "secondarili"
    }

    it("should pass the big test set") {
      val inputs  = fromFile("./src/test/scala/input.txt").getLines
      val outputs = fromFile("./src/test/scala/output.txt").getLines

      for( (input, output) <- (inputs zip outputs) )
        PorterStemmer.findStem(input) shouldEqual output
    }
  }
}
