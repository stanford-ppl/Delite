package ppl.delite.framework.transform

import ppl.delite.framework.ops._
import ppl.delite.framework.datastructures._
import ppl.delite.framework.Util._
import scala.virtualization.lms.common._
import scala.reflect.{SourceContext,RefinedManifest}

import ppl.delite.framework.analysis._
import ppl.delite.framework.DeliteApplication

/**** Mix in MultiArray implementation Exp traits here ****/
trait MultiArrayTransform extends DeliteApplication
  with FlatArrayImplExp with LayoutMetadata { self =>

  private val t = new MultiArrayTransformer{val IR: self.type = self}
  prependVisitor(t)

  private val l = new LayoutAnalyzer{val IR: self.type = self}
  prependVisitor(l)

  private val w = new MultiArrayWrapTransformer{val IR: self.type = self}
  prependVisitor(w)

  private val c = new RankChecker{val IR: self.type = self}
  prependVisitor(c)

  private val r = new RankAnalyzer{val IR: self.type = self}
  prependVisitor(r)
}

/**** Mix in MultiArray implementer traits here ****/
trait MultiArrayTransformer extends TransformerBase with MultiArrayHelperStageThree 
  with FlatArrayImplementer { self =>

  val IR: MultiArrayTransform
  import IR._
  override val name = "MultiArray Transformer"
  override val debugMode = true
   
  // Get inner element type for given data structure type
  def dataTp[A,B:Manifest](x: Exp[A]): Manifest[B] = dataTp(x.tp)

  protected def dataTp[A,B:Manifest](tp: Manifest[A]): Manifest[B] = tp match {
    case StructType(_,elems) => elems.find(_._1 == "data").getOrElse(
      throw new RuntimeException("Can't find data field for " + tp)
    )._2.asInstanceOf[Manifest[B]]
    case t if !t.typeArguments.isEmpty => t.typeArguments(0).asInstanceOf[Manifest[B]]
    case _ => sys.error("Cannot get data type of " + tp + " - type has no type arguments")
  }

  /**
   * Transform saved manifests to match transformed IR
   * @param tp - element type manifest, to be transformed
   * @param p  - symbol properties matching type 
   */
  final def ttype[A,B:Manifest](tp: Manifest[A], p: SymbolProperties): Manifest[B] = (tp,p) match { 
    case (StructType(_,elems), s: StructProperties) => 
      new RefinedManifest[Record] {
        def runtimeClass = classOf[Record]
        val fields = elems.map{f => f._1 -> ttype(f._2, s.child(f._1).get) }
      }.asInstanceOf[Manifest[B]]
    case (tp, a: ArrayProperties) => 
      val inner = ttype(tp.typeArguments(0), a.child.get)
      transformManifest(tp, a, inner).asInstanceOf[Manifest[B]]
    case (tp, a: ScalarProperties) => tp.asInstanceOf[Manifest[B]]
    case _ => sys.error("Don't know how to transform type " + tp + " with associated properties " + p)
  }

  // TODO: This can be generalized as part of an "AbstractNodeTransformer" trait if we ever need one
  override def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: AnalysisContext): Option[Exp[Any]] = d match {
    case d: AbstractNode[_] if d.family == "multiarray" => Some(transformAbstract(s, d))
    case NestedAtomicWrite(_,trace,f) =>
      if ((f.isInstanceOf[AbstractNode[_]] && f.asInstanceOf[AbstractNode[_]].family == "multiarray") ||
          trace.map{case t: AbstractAtomicTracer => t.family == "multiarray"; case _ => false }.fold(false){_||_})
        Some(transformAbstract(s, d))
      else 
        None  
    case Reflect(f: AbstractNode[_],_,_) if f.family == "multiarray" => Some(transformAbstract(s, d))
    case Struct(tag, elems) if hasMultiArrayChild(s.tp) => Some(transformAbstract(s, d))
    case Reflect(Struct(tag, elems),_,_) if hasMultiArrayChild(s.tp) => Some(transformAbstract(s, d))
    case Reify(x,u,es) => 
      val x2 = f(x)
      Some(reflectPure(Reify(x2,mapOver(f,u),f(es)))(x2.tp,mpos(ctx.defPos)))

    case _ => None
  }

  /**
   * Remove an intermediate (dead) sym from local def table, global def table,
   * and context symbol list. Needed to keep intermediate steps from causing 
   * code duplication by getting into reflect/reify node symbol lists
   * TODO: Does NOT remove from symbol table - should it?
   * TODO: This is rather hacky - is there API for this kind of thing?
   */
  def scrubSym(sym: Sym[Any]) = {
    def scrubIntermediateSym(stms: List[Stm]) = stms filterNot {
      case TP(lhs,rhs) => (lhs == sym)
      case _ => false
    }
    localDefs = scrubIntermediateSym(localDefs)
    globalDefs = scrubIntermediateSym(globalDefs)
    context = context filterNot {s => s == sym}
  }

  def transformAbstract[A](s: Exp[A], d: Def[A])(implicit ctx: AnalysisContext): Exp[Any] = transformMultiArrayDef(s, d)

  /**
   * Transforms abstract MultiArray IR nodes to concrete implementations using types like
   * DeliteArray, DeliteArrayBuffer, and anonymous Structs
   */
  def transformMultiArrayDef(s: Exp[Any], d: Def[Any])(implicit ctx: AnalysisContext): Exp[Any] = d match {
    // --- Nested Writes
    case op@NestedAtomicWrite(sym, trace, d) => transformNestedAtomicWrite(f(sym), trace.map{r => mirrorTrace(r,f)}, d)(dataTp(f(sym)), mpos(ctx.defPos))

    // --- Properties
    case DeliteMultiArrayRank(ma) => Const(rank(f(ma)))
    case op@DeliteMultiArrayDim(ma,i) => implementDim(f(ma),i)(dataTp(f(ma)),mpos(ctx.defPos))
    case op@DeliteMultiArraySize(ma) => implementSize(f(ma))(dataTp(f(ma)),mpos(ctx.defPos))

    // --- Array Constructors
    case op@DeliteMultiArrayNew(dims) => 
      val out = implementNew(f(dims), props(s))(ttype(op.mA, mdat(s)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    
    case op@DeliteMultiArrayView(ma,o,t,d) => 
      val out = implementView(f(ma),f(o),f(t),f(d),props(s))(dataTp(f(ma)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    // --- Single element ops
    case op@DeliteMultiArrayApply(ma,i) => implementApply(f(ma),f(i))(dataTp(f(ma)), mpos(ctx.defPos))
    case op@DeliteMultiArrayUpdate(ma,i,x) => implementUpdate(f(ma),f(i),f(x))(dataTp(f(ma)), mpos(ctx.defPos))

    // --- Array permute / reshape
    case op@DeliteMultiArrayPermute(ma,config) => 
      val out = implementPermute(f(ma),config,props(s))(dataTp(f(ma)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    case op@DeliteMultiArrayReshape(ma,dims) => 
      val out = implementReshape(f(ma),f(dims),props(s))(dataTp(f(ma)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    // --- Parallel ops
    case op@DeliteMultiArrayFromFunction(dims,_) => 
      val body = f(op.body)
      val out = implementFromFunction(f(dims),body,op.v,op.i,props(s))(mtype(body.tp),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    case op@DeliteMultiArrayMap(ma,_) => 
      val body = f(op.body)
      val out = implementMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    case op@DeliteMultiArrayZipWith(ma,mb,_) => 
      val body = f(op.body)
      val out = implementZipWith(f(ma),f(mb),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(f(mb)),mtype(body.tp),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    case op@DeliteMultiArrayReduce(ma,_,_) => 
      val out = implementReduce(f(ma),f(op.lookup),f(op.body),f(op.zero),op.v,op.i,op.rV,props(s))(dataTp(f(ma)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    case op@DeliteMultiArrayForeach(ma,_) => 
      val body = f(op.body)
      implementForeach(f(ma),body,op.v,op.i)(dataTp(f(ma)),mpos(ctx.defPos))

    case op@DeliteMultiArrayForIndices(ma,_) => implementForIndices(f(ma),f(op.body),op.v,op.i)(dataTp(f(ma)),mpos(ctx.defPos))
    
    // TODO
    case op@DeliteMultiArrayNDMap(ma,_,_) => 
      val out = implementNDMap(op,props(s))(dataTp(f(ma)),ttype(op.mB,mdat(s)),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    case op@DeliteMultiArrayGroupBy(ma,_) => implementGroupBy(op)(dataTp(f(ma)),op.keyFunc.tp,mpos(ctx.defPos))
    case op@DeliteMultiArrayGroupByReduce(ma,_,_,_) => implementGroupByReduce(op)(dataTp(ma.tp),op.keyFunc.tp,op.valFunc.tp,mpos(ctx.defPos))

    // --- 1D Parallel ops
    case op@DeliteMultiArrayMapFilter(ma,_,_) => 
      val body = f(op.mapFunc)
      val filt = f(op.filtFunc)
      val out = implementMapFilter(f(ma),body,filt,op.v,op.i,props(s))(dataTp(f(ma)),mtype(body.tp),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    case op@DeliteMultiArrayFlatMap(ma,_) => 
      val body = f(op.body)
      val out = implementFlatMap(f(ma),body,op.v,op.i,props(s))(dataTp(f(ma)),dataTp(body.tp),mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    // --- Buffer ops
    case DeliteMultiArrayInsert(ma,x,i) => implementInsert(f(ma),f(i),f(x))(dataTp(f(ma)),mpos(ctx.defPos))
    case DeliteMultiArrayAppend(ma,x) => implementAppend(f(ma),f(x))(dataTp(f(ma)), mpos(ctx.defPos))
    case DeliteMultiArrayInsertAll(ma,x,a,i) => implementInsertAll(f(ma),a,f(i),f(x))(dataTp(f(ma)),mpos(ctx.defPos))
    case DeliteMultiArrayRemove(ma,a,s,l) => implementRemove(f(ma),a,f(s),f(l))(dataTp(f(ma)),mpos(ctx.defPos))

    // --- Misc
    case op@DeliteMultiArrayMkString(ma, dels) => implementMkString(f(ma),f(dels))(dataTp(f(ma)))
    
    // --- 1D Ops
    case DeliteMultiArraySortIndices(len,i,body) => implementSortIndices(f(len),(f(i._1).asInstanceOf[Sym[Int]],f(i._2).asInstanceOf[Sym[Int]]),f(body))
    case DeliteMultiArrayStringSplit(str,split,lim) => implementStringSplit(f(str),f(split),f(lim))(mpos(ctx.defPos))

    // --- 2D Ops
    case op@DeliteMatrixMultiply(lhs,rhs) => 
      val out = implementMatrixMultiply(f(lhs),f(rhs))(op.mA,op.nA,mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)
    case op@DeliteMatrixVectorMultiply(mat,vec) => 
      val out = implementMatrixVectorMultiply(f(mat),f(vec))(op.mA,op.nA,mpos(ctx.defPos))
      copyMetadata(out, props(s))
      (out)

    // --- Structs
    case Struct(tag, elems) =>
      val fields = elems map {elem => elem._2 match {
        case Def(ReadVar(v)) => (elem._1, true, (x: Exp[_]) => f(elem._2))
        case _ => (elem._1, false, (x: Exp[_]) => f(elem._2))
      }}
      val out = record_new(fields)(ttype(s.tp, props(s)))
      elems foreach {elem => setField(out, getProps(f(elem._2)), elem._1) }
      copyMetadata(out, props(s))
      (out)

    // --- Wrapping
    case MultiArrayBuffify(ma) => implementBuffify(f(ma),props(s))(dataTp(f(ma)))
    case MultiArrayViewify(ma) => implementViewify(f(ma),props(s))(dataTp(f(ma)))

    case Reflect(d, u, es) => 
      //val t = self.asInstanceOf[Transformer]
      val e = transformAbstract(s, d)
      e match {
        case Def(Reflect(d2, u2, es2)) => 
          val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(e.tp), mpos(ctx.defPos))
          setProps(out, getProps(e))
            
          // Also need to scrub intermediate Reflect node, otherwise its in the context list
          if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
          (out)
        case Def(d2) =>
          val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(e.tp), mpos(ctx.defPos))
          setProps(out, getProps(e))

          // Scrub intermediate def, otherwise its in the context list
          if (out != e) { scrubSym(e.asInstanceOf[Sym[Any]]) }
          (out)
        case c: Const[_] => (c)   // This should never happen?
      }
  }

  override def transferMetadata(orig: Exp[Any], e2: Exp[Any], d: Def[Any])(implicit ctx: AnalysisContext) = d match {
    case _ => // Nothing
  }
}
