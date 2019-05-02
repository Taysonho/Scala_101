/* Implement the REPL for a scientific calculator that
 evaluates expressions including basic
 arithmetic operations along with exponentiation,
 logs, inversion etc.
* */

// Pour avoir toutes les fonctions : import calculator_o._
object calculator_o {

  // valeur absolue
  def abs(x: Double) = if (x >= 0) x else -x

  // définition de l'opérateur de puissance '**' pour les exposants entier
  implicit class Power(i: Double) {
    def **(x: Int): Double = x match {
      case 0 => 1
      case _ if x > 0 => i* **(x-1)
      case _ if x < 0 => 1/ (i * **(abs(x+1).toInt))
    }
  }

  // fonction inverse des nombres réels
  def inverse(x: Double) = x match{
    case 0 => println("Cannot divide by 0")
    case _ => 1/x
  }

  // définition de l'opérateur factoriel '!' pour n entier jusqu'à n=20
  def factorial(n : Int):Long= n match {
      case 0 => 1
      case _ => n * factorial(n - 1)
    }
  implicit def fact(i: Int) = new {def !():Long = factorial(i)}

  // fonction carrée et cubique
  def square(x: Double): Double = x*x

  def cube(x: Double): Double = x*x*x

  // fonction exponentielle avec e nombre d'euler
  def exp(x: Double): Double = x match {
    case 0 => 1
    case _ => powR(2.718281828459045, x)
  }

  // logarithme naturel
  def ln(x: Double): Double = x match {
    case x if x <= 0 => throw new IllegalArgumentException("Valeur indéfinie")
    case _ => scala.math.log(x)
  }

  // logarithme base 10
  def log10(x: Double): Double = ln(x)/ln(10)

  // logarithme base y
  def log(x:Double, y:Double):Double = ln(x)/ln(y)

  // Implémentation d'une pseudo méthode de Newton qui vérifie le résultat par approximation à un seuil
  val seuil = 0.000001
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < seuil

  //itération d'une fonction f jusqu'à tomber sur la valeur proche du seuil
  def newton(f: Double => Double)(firstGuess: Double) = {
    def iter(guess: Double): Double = {
      val next = f(guess)
      // si next inférieur au seuil, alors retourne next arrondi à 0,000001
      if (isCloseEnough(guess, next)) scala.math.BigDecimal(next).setScale(6,BigDecimal.RoundingMode.HALF_UP).toDouble
      else iter(next)
    }
    iter(firstGuess)
  }
  // Définition de la méthode de Héron
  def heron(f: Double => Double)(x: Double) = (x + f(x)) / 2

  // racine carrée
  def sqrt(x: Double) = newton(heron(y => x / y))(1.0)

  // racine de n-ième. Définir le nombre a, puis la racine n
  // on cherche à résoudre avec la méthode de newton la fonction f(x) = x**n - a
  def nroot(a: Double, n:Int) = {
    def rooting(f: Double => Double)(x:Double):Double = ((n-1)*x + f(x)) / n

    newton(rooting(y => a / (y ** (n - 1))))(1.0)
  }

  // définition de la fonction de puissance pour un exposant réel.
  // Le résultat est une approximation à la précision voulue.
  def powR(base: Double, exposant: Double, precision: Double): Double = exposant match {
    // exposant négatif
    case _ if exposant < 0 => 1/powR(base, -exposant ,precision)
    // si exposant élevé, pour aller plus vite on calcule le carré de l'exposant/2
    case _ if exposant >=10 => square(powR(base, exposant/2, precision/2))
    // si exposant faible, on commence à réduire de 1 par récursion
    case _ if exposant >=1 => base * powR(base,exposant-1,precision)
    // si la précision surpasse 1, retourne la racine carrée de la base
    case _ if precision >= 1 => sqrt(base)
    // On utilise le principe des puissances fractionnaires simples, ici la racine carrée.
    // Sachant que x**1/2 = sqrt(x), x**1/4 = sqrt(sqrt(x)), x**1/8 = sqrt(sqrt(sqrt(x)))
    case _ => sqrt(powR(base,exposant*2,precision*2))
  }
  def powR(base: Double, exposant: Double): Double = powR(base, exposant, 0.000001)


  //déclaration implicite des fonctions trigo
  val cos =scala.math.cos _
  val sin =scala.math.sin _
  val tan =scala.math.tan _

  val acos =scala.math.acos _
  val asin =scala.math.asin _
  val atan =scala.math.atan _

  //fonctions hyperboliques
  def cosh(x: Double): Double = (exp(x) + exp(-x)) / 2
  def sinh(x: Double): Double = (exp(x) - exp(-x)) / 2
  def tanh(x: Double): Double = sinh(x)/cosh(x)

  def acosh(x: Double): Double = ln(x + sqrt((x**2)-1))
  def asinh(x: Double): Double = ln(x + sqrt((x**2)+1))
  def atanh(x: Double): Double = ln((1+x)/(1-x))/2


}
