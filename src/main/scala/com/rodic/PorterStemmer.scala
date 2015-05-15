package com.rodic

import scala.annotation.tailrec

object PorterStemmer {

  val vowelPat = "([aeiouy])".r
  val consPat  = "([^aeiou])".r

  @tailrec
  def calcM(cs: List[Char], m: Int = 0): Int = {
    // reversed list is passed here, that's why the check for 'cv' and not for 'vc'
    cs match {
      case Nil => m
      case consPat(c1)::'y'::Nil           => m
      case consPat(c1)::vowelPat(c2)::tail => calcM(tail, m+1)
      case c::tail                         => calcM(tail, m)  
    }
  }

  def step1a(cs: List[Char]): List[Char] = {
    cs match {
      case Nil                     => Nil
      case 's'::'s'::'e'::'s'::Nil => 's'::'s'::Nil
      case 'i'::'e'::'s'::Nil      => 'i'::Nil
      case 's'::'s'::Nil           => cs
      case c::'s'::Nil             => c::Nil
      case c::tail                 => c::step1a(tail)
    }
  }

  def step1b(cs: List[Char]): List[Char] = {
    // Making the main fn tail rec makes it easy to call trim fn if -ing or -ed are removed
    @tailrec
    def main(cs: List[Char], m: Int, vowel: Boolean, acc: List[Char]): List[Char] = {
      cs match {
        case Nil                             => acc
        case 'e'::'e'::'d'::Nil              => if (m > 0) 'e'::'e'::acc else 'd'::'e'::'e'::acc 
        case 'e'::'d'::Nil | 
        'i'::'n'::'g'::Nil if vowel          => trim(acc.reverse)
        case vowelPat(c1)::consPat(c2)::tail => main(c2::tail, m+1, true, c1::acc)
        case vowelPat(c)::tail               => main(tail, m, true, c::acc)
        case c::tail                         => main(tail, m, vowel, c::acc)
      }
    }

    @tailrec
    def trim(cs: List[Char], acc: List[Char] = Nil): List[Char] = {
      cs match {
        case Nil           => acc
        case 'a'::'t'::Nil => 'e'::'t'::'a'::acc
        case 'b'::'l'::Nil => 'e'::'l'::'b'::acc
        case 'i'::'z'::Nil => 'e'::'z'::'i'::acc
        case consPat(c1)::consPat(c2)::Nil 
          if c1 == c2 && c1 != 'l' && c1 != 's' && c1 != 'z' => c1::acc
        case consPat(c1)::vowelPat(c2)::consPat(c3)::Nil 
          if calcM((c3::c2::c1::acc).reverse) == 1 && 
            c3 != 'w' && c3 != 'x' && c3 != 'y' => 'e'::c3::c2::c1::acc
        case c::tail => trim(tail, c::acc) 
      }
    }

    main(cs, 0, false, Nil).reverse
  }

  def step1c(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], vowel: Boolean): List[Char] = {
      cs match {
        case Nil               => Nil
        case 'y'::Nil if vowel => 'i'::Nil
        case vowelPat(c)::tail => c::aux(tail, true)
        case c::tail           => c::aux(tail, vowel)  
      }
    }
    aux(cs, false)
  }

  def step2(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], m: Boolean, checked: List[Char]): List[Char] = {
      cs match {
        case Nil                                    => Nil
        case 'a'::'t'::'i'::'o'::'n'::'a'::'l'::Nil => if (m) 'a'::'t'::'e'::Nil else cs
        case 't'::'i'::'o'::'n'::'a'::'l'::Nil      => if (m) 't'::'i'::'o'::'n'::Nil else cs
        case 'e'::'n'::'c'::'i'::Nil                => if (m) 'e'::'n'::'c'::'e'::Nil else cs
        case 'a'::'n'::'c'::'i'::Nil                => if (m) 'a'::'n'::'c'::'e'::Nil else cs
        case 'i'::'z'::'e'::'r'::Nil                => if (m) 'i'::'z'::'e'::Nil else cs
        case 'b'::'l'::'i'::Nil                     => if (m) 'b'::'l'::'e'::Nil else cs
        case 'a'::'l'::'l'::'i'::Nil                => if (m) 'a'::'l'::Nil else cs
        case 'l'::'o'::'g'::'i'::Nil                => if (m) 'l'::'o'::'g'::Nil else cs
        case 'e'::'n'::'t'::'l'::'i'::Nil           => if (m) 'e'::'n'::'t'::Nil else cs
        case 'e'::'l'::'i'::Nil                     => if (m) 'e'::Nil else cs
        case 'o'::'u'::'s'::'l'::'i'::Nil           => if (m) 'o'::'u'::'s'::Nil else cs
        case 'i'::'z'::'a'::'t'::'i'::'o'::'n'::Nil => if (m) 'i'::'z'::'e'::Nil else cs
        case 'a'::'t'::'i'::'o'::'n'::Nil
           | 'a'::'t'::'o'::'r'::Nil                => if (m) 'a'::'t'::'e'::Nil else cs
        case 'a'::'l'::'i'::'s'::'m'::Nil           => if (m) 'a'::'l'::Nil else cs
        case 'i'::'v'::'e'::'n'::'e'::'s'::'s'::Nil => if (m) 'i'::'v'::'e'::Nil else cs
        case 'f'::'u'::'l'::'n'::'e'::'s'::'s'::Nil => if (m) 'f'::'u'::'l'::Nil else cs
        case 'o'::'u'::'s'::'n'::'e'::'s'::'s'::Nil => if (m) 'o'::'u'::'s'::Nil else cs
        case 'a'::'l'::'i'::'t'::'i'::Nil           => if (m) 'a'::'l'::Nil else cs
        case 'i'::'v'::'i'::'t'::'i'::Nil           => if (m) 'i'::'v'::'e'::Nil else cs
        case 'b'::'i'::'l'::'i'::'t'::'i'::Nil      => if (m) 'b'::'l'::'e'::Nil else cs
        case c::tail                                => 
          if(!m) c::aux(tail, calcM(c::checked) > 0, c::checked) else c::aux(tail, m, c::checked)
      }
    }
    aux(cs, false, Nil)
  }

  def step3(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], checked: List[Char]): List[Char] = {
      lazy val m = calcM(checked)
      cs match {
        case Nil                          => Nil
        case 'i'::'c'::'a'::'t'::'e'::Nil => if (m > 0) 'i'::'c'::Nil else cs
        case 'a'::'t'::'i'::'v'::'e'::Nil => if (m > 0) Nil else cs
        case 'a'::'l'::'i'::'z'::'e'::Nil => if (m > 0) 'a'::'l'::Nil else cs
        case 'i'::'c'::'i'::'t'::'i'::Nil
           | 'i'::'c'::'a'::'l'::Nil      => if (m > 0) 'i'::'c'::Nil else cs
        case 'f'::'u'::'l'::Nil
           | 'n'::'e'::'s'::'s'::Nil      => if (m > 0) Nil else cs
        case c::tail                      => c::aux(tail, c::checked)
      }
    }
    aux(cs, Nil)
  }

  def step4(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], checked: List[Char]): List[Char] = {
      lazy val m = calcM(checked)
      cs match {
        case Nil => Nil
        case 'a'::'l'::Nil
           | 'a'::'n'::'c'::'e'::Nil
           | 'e'::'n'::'c'::'e'::Nil
           | 'e'::'r'::Nil
           | 'i'::'c'::Nil
           | 'a'::'b'::'l'::'e'::Nil
           | 'i'::'b'::'l'::'e'::Nil
           | 'a'::'n'::'t'::Nil           => if (m > 1) Nil else cs
        case 'e'::'m'::'e'::'n'::'t'::Nil => if (m > 1) Nil else cs
        case 'm'::'e'::'n'::'t'::Nil      => if (m > 1) Nil else cs
        case 'e'::'n'::'t'::Nil           => if (m > 1) Nil else cs
        case 'i'::'o'::'n'::Nil           => 
          if (m > 1 && (checked.head == 't' || checked.head == 's')) Nil else cs
        case 'o'::'u'::Nil
           | 'i'::'s'::'m'::Nil
           | 'a'::'t'::'e'::Nil
           | 'i'::'t'::'i'::Nil
           | 'o'::'u'::'s'::Nil
           | 'i'::'v'::'e'::Nil
           | 'i'::'z'::'e'::Nil => if (m > 1) Nil else cs
        case c::tail => c::aux(tail, c::checked)
      }
    }
    aux(cs, Nil)
  }
  
  def step5a(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], checked: List[Char]): List[Char] = {
      lazy val m = calcM(checked)
      cs match {
        case Nil                                => Nil
        case 'e'::Nil if m > 1                  => Nil
        case consPat(c1)::vowelPat(c2)::consPat(c3)::'e'::Nil
          if calcM(c3::c2::c1::checked) == 1 && 
            c3 != 'w' && c3 != 'x' && c3 != 'y' => cs
        case 'e'::Nil             if m == 1     => Nil
        case c::tail                            => c::aux(tail, c::checked)
      }
    }
    aux(cs, Nil)
  }

  def step5b(cs: List[Char]): List[Char] = {
    def aux(cs: List[Char], m: Int): List[Char] = {
      cs match {
        case Nil                              => Nil
        case 'l'::'l':: Nil                   => if (m > 1) 'l'::Nil else cs
        case vowelPat(c1)::consPat(c2)::tail  => c1::aux(c2::tail, m+1)
        case c::tail                          => c::aux(tail, m)
      }     
    }
    aux(cs, 0)
  }

  def findStem(input: String): String = {
    if(input != input.toLowerCase)
      throw new IllegalArgumentException("You must pass string in lower cases")

    if (input.length < 3)
      return input

    val fs = List( step1a _, step1b _, step1c _, step2 _, step3 _, step4 _, step5a _, step5b _ )

    (fs.foldLeft(input.toList)( (acc, f) => f(acc) )).mkString
  }
}
