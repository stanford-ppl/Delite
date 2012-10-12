package ppl.dsl.opticvx.dcp

sealed abstract class Signum {
  //encapsulates the idea of adding two expressions
  //we will always have A + B <= A and A + B <= B
  def +(d: Signum): Signum
  //encapsulates the idea of combining two expressions
  //we will always have A && B >= A and A && B >= B
  def *(d: Signum): Signum
  def unary_-(): Signum
  //A <= B if all A expressions are B.  i.e. Affine <= Convex
  def <=(d: Signum): Boolean
  //def >=(d: Signum): Boolean = (d <= this)
}

object Signum {
  def sgn(x: Double): Signum = {
    if(x > 0.0) Positive
    else if (x == 0.0) Zero
    else if (x < 0.0) Negative
    else All
  }

  case object All extends Signum {
    override def +(d: Signum) = d match {
      case All => All
      case Positive => All
      case Negative => All
      case Zero => All
    }
    override def *(d: Signum) = d match {
      case All => All
      case Positive => All
      case Negative => All
      case Zero => Zero
    }
    override def unary_-() = All
    override def <=(d: Signum) = d match {
      case All => true
      case Positive => false
      case Negative => false
      case Zero => false
    }
  }

  case object Positive extends Signum {
    override def +(d: Signum) = d match {
      case All => All
      case Positive => Positive
      case Negative => All
      case Zero => Positive
    }
    override def *(d: Signum) = d match {
      case All => All
      case Positive => Positive
      case Negative => Negative
      case Zero => Zero
    }
    override def unary_-() = Negative
    override def <=(d: Signum) = d match {
      case All => true
      case Positive => true
      case Negative => false
      case Zero => false
    }
  }

  case object Negative extends Signum {
    override def +(d: Signum) = d match {
      case All => All
      case Positive => All
      case Negative => Negative
      case Zero => Negative
    }
    override def *(d: Signum) = d match {
      case All => All
      case Positive => Negative
      case Negative => Positive
      case Zero => Zero
    }
    override def unary_-() = Positive
    override def <=(d: Signum) = d match {
      case All => true
      case Positive => false
      case Negative => true
      case Zero => false
    }
  }

  case object Zero extends Signum {
    override def +(d: Signum) = d match {
      case All => All
      case Positive => Positive
      case Negative => Negative
      case Zero => Zero
    }
    override def *(d: Signum) = d match {
      case All => Zero
      case Positive => Zero
      case Negative => Zero
      case Zero => Zero
    }
    override def unary_-() = Zero
    override def <=(d: Signum) = d match {
      case All => true
      case Positive => true
      case Negative => true
      case Zero => true
    }
  }
}

