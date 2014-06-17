package ppl.delite.framework.transform

import java.io._
import reflect.{Manifest, SourceContext}
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{AbstractSubstTransformer,Transforming}
import ppl.delite.framework.DeliteApplication
import ppl.delite.framework.ops.{DeliteOpsExp, BaseDeliteOpsTraversalFat}
import ppl.delite.framework.ops.DeliteCollection
import ppl.delite.framework.Config

/**
 * Transform DeliteOpForeachReduce instances into a composition of ops that implements it.
 */

trait ForeachReduceTransformExp extends DeliteTransform 
  with DeliteApplication with DeliteOpsExp 
  with BooleanOpsExp with MiscOpsExp with StringOpsExp with ObjectOpsExp with PrimitiveOpsExp with RangeOpsExp
  with LiftString with LiftBoolean with LiftPrimitives { // may want to use self-types instead of mix-in to decrease risk of accidental inclusion
  
  self =>
  
  private val t = new ForeachReduceTransformer { val IR: self.type = self }
  appendTransformer(t)    
  
  /**
   * These IR nodes represent the spliced out components of the DeliteOpForeachReduce after transformation.
   */
  case class TransformedForeach(size: Exp[Int], func: Exp[Int] => Exp[Unit]) extends DeliteOpIndexedLoop 
  
  case class TransformedReduce[R:Manifest](oldV: Sym[Int], size: Exp[Int], zero: Block[R], rhs: Block[R], rFunc: (Exp[R],Exp[R]) => Exp[R]) extends DeliteOpReduceLike[R] {
    lazy val body: Def[R] = copyBodyOrElse(DeliteReduceElem[R](
      func = t.withSubstScope((oldV -> v)) { t.transformBlock(this.rhs) },
      accInit = reifyEffects(this.accInit),
      zero = this.zero,
      rV = this.rV,
      rFunc = reifyEffects(rFunc(this.rV._1, this.rV._2)),  
      stripFirst = false,
      numDynamicChunks = this.numDynamicChunks
    ))            
    
    val mR = manifest[R]
  }
      
  def transformForeachReduceToLoops[A:Manifest](s: Sym[_], fr: DeliteOpForeachReduce[_]): Exp[Unit] = {   
    // NOTE: because we only remove the reductions from the foreach function, and not the entire dependency
    // chain leading up to it, we may end up without useless work in the foreach if it is not fused back together
    // with the reductions.
    
    val xFunc = fr.xFunc
    val deliteReductions = fr.deliteReductions
    
    // we have to add the the spliced block we created to the current scope, or the transformer won't work
    t.addToScope(findDefinition(xFunc.res.asInstanceOf[Sym[Any]]).get)
    
    /**
     * construct IR nodes for the different components
     */
    
    // foreach
    val z = TransformedForeach(t(fr.size), {n => transformBlockWithBound(t, xFunc, List(fr.v -> n)) })
    // if we use Simple() also, then it can't fuse with the reduces.
    reflectEffect(z, summarizeEffects(z.body.asInstanceOf[DeliteForeachElem[Any]].func).star /*andAlso Simple()*/)
        
    // reductions    
    def commit[L:Manifest,R:Manifest](dr: DeliteReduction[_,_], value: Exp[_]) {
      val r = dr.asInstanceOf[DeliteReduction[L,R]]
      r.updateValue(t(r.lhs.asInstanceOf[Exp[L]]), t(value.asInstanceOf[Exp[R]]))
    }    
    
    def makeReduceBody[L:Manifest,R:Manifest](dr: DeliteReduction[_,_]) = {
      // hack! lots of type gymnastics going on here, and not all of it is happy.
      val r = dr.asInstanceOf[DeliteReduction[L,R]]      
      reflectPure(TransformedReduce[R](fr.v, t(fr.size), reifyEffects(dr.zero.asInstanceOf[Exp[R]]), reifyEffects(dr.rhs.asInstanceOf[Exp[R]]), dr.reduce.asInstanceOf[(Exp[R],Exp[R]) => Exp[R]]))
    }
            
    for (r <- deliteReductions) {
      val b = makeReduceBody(r)(r.mL,r.mR)
      commit(r,b)(r.mL,r.mR)
    }     
  }    
  
  //////////////
  // mirroring

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    // implemented as DeliteOpLoop    
    case e@TransformedForeach(s,b) => reflectPure(new { override val original = Some(f,e) } with TransformedForeach(f(s),f(b)))(mtype(manifest[A]),implicitly[SourceContext])
    case e@TransformedReduce(v,s,z,rhs,r) => 
      e.asInstanceOf[TransformedReduce[A]] match {
        case e@TransformedReduce(v,s,z,rhs,r) => 
          reflectPure(new { override val original = Some(f,e) } with TransformedReduce(f(v).asInstanceOf[Sym[Int]],f(s),f(z),f(rhs),f(r))(e.mR))(mtype(manifest[A]),implicitly[SourceContext])
      }
    
    // reflected
    case Reflect(e@TransformedForeach(s,b), u, es) => reflectMirrored(Reflect(new { override val original = Some(f,e) } with TransformedForeach(f(s),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
    case Reflect(e@TransformedReduce(v,s,z,rhs,r), u, es) => 
      e.asInstanceOf[TransformedReduce[A]] match {
        case e@TransformedReduce(v,s,z,rhs,r) => 
          reflectMirrored(Reflect(new { override val original = Some(f,e) } with TransformedReduce(f(v).asInstanceOf[Sym[Int]],f(s),f(z),f(rhs),f(r))(e.mR), mapOver(f,u), f(es)))(mtype(manifest[A]), ctx)
      }
    
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]   
}

trait ForeachReduceTransformer extends ForwardPassTransformer {  
  val IR: ForeachReduceTransformExp
  import IR._
        
  def addToScope(x: Stm) = innerScope ::= x
    
  /* 
   * The first thing we do is scrub all references to DeliteReductions from effect lists, now
   * that we've already aggregated them inside DeliteOpForeachReduce instances.
   * 
   * The strategy we use is just to scrub every definition in the symbol table. This is
   * perhaps a bit heavy-handed, but easier than doing a recursive traversal to mirror individual nodes
   * without the unwanted effects - especially when the end goal is to never have a reference to a DeliteReduction
   * left in an effect list. The DeliteReduction effects were solely to allow us to extract them
   * from the function block, and also to compute the correct effect summaries for the foreach blocks,
   * both of which have already happened.
   */   
  
  def withoutReductions(es: List[Exp[Any]]) = es.filter(e => e match {
    case Def(Reflect(r:DeliteReduction[_,_],u,_)) => false
    case _ => true      
  })
      
  def scrubDeliteReductions(x: List[Stm]): List[Stm] = {
    x.map(d => d match {
      case TP(s,Reflect(x,u,es)) =>
        val cleanEs = withoutReductions(es)
        if (cleanEs != es) { 
          TP(s, Reflect(x,u,cleanEs))
        }
        else d
      case TP(s,Reify(x,u,es)) =>
        val cleanEs = withoutReductions(es)
        if (cleanEs != es) {
          x match {
            case Def(Reflect(r:DeliteReduction[_,_],u,_)) => TP(s, Reify(Const(),u,cleanEs))
            case _ => TP(s, Reify(x,u,cleanEs))
          }          
        }
        else d
      case _ => d           
    })
  }  
  
  override def isDone = {
    if (!globalDefs.exists(e => e.rhs match { case Reflect(x:DeliteOpForeachReduce[_],_,_) => true; case _ => false })) true
    else (nextSubst.isEmpty && runs > 0) // super.isDone
  }
  
  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    // deliteReductions is lazy, so first run through and touch them to make sure they are evaluated before we scrub the effects
    globalDefs.foreach { d => d.rhs match {
      case Reflect(x: DeliteOpForeachReduce[_],_,_) => x.deliteReductions
      case _ =>
    }}
    
    globalDefs = scrubDeliteReductions(globalDefs)
    localDefs = scrubDeliteReductions(localDefs)
    super.runOnce(b)
  }                 
       
  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s,Reflect(l:DeliteOpForeachReduce[_], u, es)) => 
      implicit def ctx: SourceContext = if (s.pos.length > 0) s.pos(0) else null
      // transformForeachReduceToLoops(s,l)
      reflectTransformed(this.asInstanceOf[IR.Transformer], transformForeachReduceToLoops(s,l), u, es) // cast needed why?      
    case _ => super.transformStm(stm)
  }
}
