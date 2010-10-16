package ppl.delite.framework.embedded.scala

import ppl.delite.framework.DeliteApplication

//trait CodeGeneratorScalaPkgAll extends CodeGeneratorScalaMisc

trait ScalaOpsPkg3 extends
    ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
    with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
    with Equal with IfThenElse with Variables with While
    { this: DeliteApplication => }

trait ScalaOpsPkgExp3 extends ScalaOpsPkg3
    with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
    with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
    with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp
    with DSLOpsExp { this: DeliteApplication => }