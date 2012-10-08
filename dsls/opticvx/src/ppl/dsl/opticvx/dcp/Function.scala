package ppl.dsl.opticvx.dcp

trait DCPFunction {
  self: DCPExpr with DCPShape =>
  
  case class FunDesc(val va: Signum, val sg: Signum, val name: String)
  case class ArgDesc(val t: Signum, val nt: Signum, val sh: Shape, val name: String)
  
  case class FunBuilder0(fd: FunDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none, 
      name: String = ""): FunBuilder1
    = FunBuilder1(fd, ArgDesc(tonicity, niltonicity, shape, name))
  }
  
  case class FunBuilder1(fd: FunDesc, ad0: ArgDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none, 
      name: String = ""): FunBuilder2
    = FunBuilder2(fd, ad0, ArgDesc(tonicity, niltonicity, shape, name))
    
    def body(fx: (Expr) => Expr) = DcpFun1(fd, ad0, fx)
  }
  
  case class FunBuilder2(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc) {
    def arg(
      tonicity: Signum = Tonicity.none, 
      shape: Shape = ShapeScalar(), 
      niltonicity: Signum = Tonicity.none,
      name: String = ""): FunBuilder3
    = FunBuilder3(fd, ad0, ad1, ArgDesc(tonicity, niltonicity, shape, name))
    
    def body(fx: (Expr,Expr) => Expr) = DcpFun2(fd, ad0, ad1, fx)
  }
  
  case class FunBuilder3(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc) {
    def body(fx: (Expr,Expr,Expr) => Expr) = DcpFun3(fd, ad0, ad1, ad2, fx)
  }
  
  case class DcpFun1(fd: FunDesc, ad0: ArgDesc, fx: (Expr) => Expr) {
    def apply(a0: Expr): Expr = dcpfun1apply(fd, ad0, fx, a0)
  }
  
  case class DcpFun2(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, fx: (Expr,Expr) => Expr) {
    def apply(a0: Expr, a1: Expr): Expr = dcpfun2apply(fd, ad0, ad1, fx, a0, a1)
  }
  
  case class DcpFun3(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc, fx: (Expr,Expr,Expr) => Expr) {
    def apply(a0: Expr, a1: Expr, a2: Expr): Expr = dcpfun3apply(fd, ad0, ad1, ad2, fx, a0, a1, a2)
  }
  
  def dcpfun1apply(fd: FunDesc, ad0: ArgDesc, fx: (Expr) => Expr, a0: Expr): Expr
  def dcpfun2apply(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, fx: (Expr,Expr) => Expr, a0: Expr, a1: Expr): Expr
  def dcpfun3apply(fd: FunDesc, ad0: ArgDesc, ad1: ArgDesc, ad2: ArgDesc, fx: (Expr,Expr,Expr) => Expr, a0: Expr, a1: Expr, a2: Expr): Expr

}
