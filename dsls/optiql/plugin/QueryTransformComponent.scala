import collection.mutable
import collection.mutable.{ListBuffer, ArrayBuffer}
import scala.tools.nsc._
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.ast._
import scala.tools.nsc.symtab._
import scala.tools.nsc.symtab.Flags._

/**
 * Author: Lere Williams
 * Date: July 2009
 *
 * Description:
 */

/** This class implements a plugin component using tree transformers. If
 *  a <code>Typer</code> is needed during transformation, the component
 *  should mix in <code>TypingTransformers</code>. This provides a local
 *  variable <code>localTyper: Typer</code> that is always updated to
 *  the current context.
 */
class QueryTransformComponent(val global: Global) extends PluginComponent with Transform {
  import global._
  import global.definitions._

  val runsAfter = List[String]("parser")
  override val runsBefore = List[String]("namer")

  val phaseName = "querySyntaxTransformation"

  def newTransformer(unit: CompilationUnit) = new QueryTransformer(unit)

  /** The tree transformer that implements the behavior of this
   *  component. Change the superclass to <code>TypingTransformer</code>
   *  to make a local typechecker <code>localTyper</code> available.
   */
  class QueryTransformer(val unit: CompilationUnit) extends Transformer {
    /** When using <code>preTransform</code>, each node is
     *  visited before its children.
     */

    //TODO: Improve selectMember (1. fix multiple arg methods e.g. getNameSubstring(start, end) method in Customer)
    //TODO: Improve error checking/reporting

    private val debug = false

    //Query syntax keywords
    private val fromKW = "from"
    private val joinKW = "join"
    private val letKW  = "let"
    private val whereKW = "where"
    private val orderbyKW = "orderby"
    private val selectKW = "select"
    private val groupKW = "group"

    //Pseudo-keywords
    private val inKW = "in"
    private val intoKW = "into"
    private val newKW = "new"
    private val valKW = "val"
    private val descKW = "desc"
    private val thenbyKW = "thenby"
    private val orderbyDescKW = "orderbyDesc"
    private val thenbyDescKW = "thenbyDesc"
    private val groupByKW = "groupBy"
    private val byKW = "by"
    private val ofTypeKW = "ofType"
    private val groupJoinKW = "groupJoin"
    private val selectManyKW = "selectMany"

    //Internal (to plugin)
    private var joined = false
    private var anonPrefix = "QSanon"
    private var valPrefix = "QSval"
    private var selPrefix = "QSsel"

    /*Creates unique name (used for creating anonymous class,
      intermediate vals or implicit select clauses)*/
    private var anonCounter = 0
    private def newAnonNameString() = {
      val name = anonPrefix + anonCounter
      anonCounter += 1
      name
    }

    private var valCounter = 0
    private def newValNameString() = {
      val name = valPrefix + valCounter
      valCounter += 1
      name
    }

    private var selCounter = 0
    private def newSelNameString() = {
      val name = selPrefix + selCounter
      selCounter += 1
      name
    }

    //
    private def newInitMethod() = {
      val block = Block(List(Apply(Select(Super(newTypeName(""), newTypeName("")),
        newTermName("<init>")), Nil)), Literal(()))
      DefDef(Modifiers(0), newTermName("<init>"), List(), List(List()), TypeTree(NoType), block)
    }

    override def transform(tree: Tree): Tree = {
      postTransform(super.transform(preTransform(tree)))
    }

    def preTransform(tree: Tree): Tree = tree match {
      case _ => tree
    }

    def postTransform(tree: Tree): Tree = {

      tree match {
        case v @ ValDef(mods, name, tpt, rhs) => {
          if (!rhs.isEmpty && rhs.toString.startsWith(fromKW)) {
            if (debug) println("MODS:" + mods + "  NAME:" + name + "  TYPE:" + tpt + "  TERM:" + rhs)
            val transformed = transformQuery(processQuery(rhs), rhs)
            //println("New tree: " + transformed)
            treeCopy.ValDef(v, mods, name, tpt, transformed)
          }
          else tree
        }
        case _ => tree
      }
    }

    //Transforms a query expression (must be a valdef whose rhs begins with "from"!) to the appropriate SQO methods
    private def transformQuery(qry: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("transformQuery() --")

      //println("Query array buffer:"); qry.foreach(println)
      var newTree:Tree = Ident(newTermName(qry(0)))
      val stats = new ListBuffer[Tree]

      var continuation = false
      var i = 1
      while (i < qry.size) {

        val kW = qry(i)

        if (isKeyWord(kW) || isSQO(kW)) {

          val select = Select(newTree, newTermName(kW))
          i += 1
          kW match {
            case "selectMany" => unit.error(rhs.pos, "can't handle multiple from clauses yet")
            case "join" => newTree = Apply(select, getJoinArgs(i, qry, rhs))
            case "groupJoin" => newTree = Apply(select, getJoinArgs(i, qry, rhs))
            case "let" => unit.error(rhs.pos, "can't handle let clauses yet")
            case "where" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getWhereBody(qry(i)))))
            case "orderby" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getOrderingBody(qry(i)))))
            case "thenby" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getOrderingBody(qry(i)))))
            case "orderbyDesc" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getOrderingBody(qry(i)))))
            case "thenbyDesc" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getOrderingBody(qry(i)))))
            case "select" => newTree = Apply(select, List[Tree](Function(getVParams(i, qry), getSelectBody(i, qry, rhs))))
            case "groupBy" => newTree = Apply(select, getGroupArgs(i, qry, rhs))
            case "into" => {
              continuation = true
              val name = newValNameString
              stats += ValDef(Modifiers(0), newTermName(name), TypeTree(NoType), newTree)
              newTree = Ident(newTermName(name))
            }
          }
        } else {
          unit.error(rhs.pos, kW + " is not valid query syntax")
        }

        i += 1
        if (i == qry.size && !joined && !(kW == selectKW || kW == groupByKW)) unit.error(rhs.pos, "query expression must end with select or group clause")
      }
      joined = false
      if (debug) println("-- transformQuery()")
      if (continuation) Block(stats.toList, newTree) else newTree
    }

    //Returns true if the string is a "major" logical operator, false otherwise
    private def isLogicalOp(s: String) = if (s == "$eq$eq" || s == "$amp$amp" || s == "$bar$bar") true else false

    //Returns true if string is a query syntax keyword, false otherwise.
    private def isKeyWord(s: String) = {
      if (s == fromKW || s == joinKW
              || s == letKW || s == whereKW
              || s == orderbyKW || s == selectKW
              || s == groupKW || s == intoKW) true else false
    }


    //Returns true if string is an SQO method, false otherwise.
    private def isSQO(s: String) = if (s == thenbyKW || s == orderbyDescKW
            || s == thenbyDescKW || s == groupByKW || s == groupJoinKW || s == selectManyKW) true else false

    /*Creates an array buffer of tokens that contains all information from the
      query expression needed to create the new AST*/
    private def processQuery(rhs: Tree) = {
      if (debug) println("processQuery() --")
      val pure = ignoreParensAndDots(rhs.toString)
      var tokens = toArrayBuffer(pure.split("[ ]+"))
      val itemNames = new mutable.HashMap[String, ArrayBuffer[String]]
      rewriteSelects(tokens)
      groupArguments(tokens)
      //println("Pre extraction:"); tokens.foreach(println)
      extractFromClauses(itemNames, tokens, rhs)
      //println("Post extraction:"); tokens.foreach(println)
      prelimErrorCheck(tokens, rhs)
      rewriteOrderings(tokens, rhs)
      rewriteGroups(tokens, rhs)
      rewriteJoins(itemNames, tokens, rhs)
      recordContinuations(itemNames, tokens, rhs)
      convertReferences(itemNames, tokens)
      completeMethodCalls(tokens)
      tokens.foreach(_.trim)
      if (debug) println("-- processQuery()")
      tokens
    }

    //Replaces parens and dots in input string with spaces
    private def ignoreParensAndDots(s: String) = {
      var neater = ""
      for (ch <- s) ch match {
        case '(' => neater += " "
        case ')' => neater += " "
        case '.' => neater += " "
        case c => neater += c
      }
      neater
    }

    //Returns an array buffer with same elements QRY
    private def toArrayBuffer[A](arr: Array[A]) = {
      val arrBuf = new ArrayBuffer[A]
      arr.foreach((x) => arrBuf += x)
      arrBuf
    }

    /*Simplifies select and group clauses that create new class instances (removes class def
      put in by parser for example) so that they are of the form: select new <valdefs>, or
      group new <valdefs> by <selector>.*/
    private def rewriteSelects(tokens: ArrayBuffer[String]) = {
      if (debug) println("rewriteSelects() --")
      var i = 0
      while (i < tokens.size) {
        var tkn = tokens(i)
        if ((tkn == selectKW || tkn == groupKW) && tokens(i + 1).contains("{")) {
          i += 1
          tokens.insert(i, newKW)
          i += 1
          while (i < tokens.size && !isKeyWord(tokens(i))) {
            if (tokens(i) != valKW && tokens(i) != byKW) tokens.remove(i)
            else {
              if (tokens != byKW) {
                while (i < tokens.size && !tokens(i).contains("}")) {
                  if (tokens(i).contains(";")) tokens(i) = (tokens(i).split(";")(0)) + " ;"
                  else tokens(i) = tokens(i).stripLineEnd
                  i += 1
                }
              }
            }
          }
        }
        i += 1
      }
      if (debug) println("-- rewriteSelects()")
    }

    /*Groups arguments to SQO methods.  On return, the first element of TOKENS will be the source
      collection, and for each pair of subsequent elements, the first element will be an SQO
      method name and the second element will be all of the arguments to that method.*/
    private def groupArguments(tokens: ArrayBuffer[String]) = {
      if (debug) println("groupArguments() --")

      var i = 0
      var prev = ""

      def fakeIntoKW = tokens(i) == intoKW && prev == joinKW

      while (i < tokens.size) {
        if (isKeyWord(tokens(i)) && !fakeIntoKW) {
          prev = tokens(i)
          val index = i + 1
          var grouped = tokens(index)
          i += 2
          while (i < tokens.size && (!isKeyWord(tokens(i)) || fakeIntoKW)) {
            grouped += " " + tokens(i)
            i += 1
          }
          tokens(index) = grouped
          i -= 1
        }
        i += 1
      }

      i = 0
      prev = ""
      while (i < tokens.size) {
        if (isKeyWord(tokens(i)) && !fakeIntoKW) {
          prev = tokens(i)
          i += 2
          while (i < tokens.size && (!isKeyWord(tokens(i)) || fakeIntoKW)) tokens.remove(i)
          i -= 1
        }
        i += 1
      }

      if (debug) println("-- groupArguments()")
    }

    //Extracts a from clause from the query expression
    private def extractFromClauses(itemNames: mutable.HashMap[String, ArrayBuffer[String]], tokens: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("extractFromClause() --")
      var numFroms = 0
      var prev = new Array[String](0)
      var i = 0
      while (i < tokens.size) {
        if (tokens(i) == fromKW) {
          val args = tokens(i + 1).split("[ ]+")
          args.foreach(_.trim)
          if (args(1) != "in") unit.error(rhs.pos, "from clause must be of form: from <itemName> in <sourceCollection>")
          if (numFroms == 0) {
            tokens(i + 1) = args(2)
            tokens.remove(i)
            if (!itemNames.contains(args(0))) {
              val arrBuf = new ArrayBuffer[String]
              arrBuf += args(0)
              itemNames += Tuple2[String, ArrayBuffer[String]](args(0), arrBuf)
            } else unit.error(rhs.pos, "identifier " + args(0) + " (in from clause) is already defined")
          } else {
            tokens(i + 1) = selectManyKW
            val newArgs = args.slice(2).mkString(" ") + ",, new val _" + prev(0) + " = " + prev(0) + " ; val _" + args(0) + " = " + args(0)
            tokens.insert(i + 2, newArgs)
            if (!itemNames.contains(args(0))) {
              val arrBuf = new ArrayBuffer[String]
              arrBuf += args(0)
              itemNames += Tuple2[String, ArrayBuffer[String]](args(0), arrBuf)
            } else unit.error(rhs.pos, "identifier " + args(0) + " (in from clause) is already defined")
            val name = newSelNameString
            tokens.insert(i + 3, intoKW)
            tokens.insert(i + 4, name)
            itemNames(prev(0)) += name + "._" + itemNames(prev(0))(itemNames(prev(0)).size - 1).split("[:]")(0)
            itemNames(args(0)) += name + "._" + itemNames(args(0))(itemNames(args(0)).size - 1).split("[:]")(0)
            tokens.remove(i)
          }
          numFroms += 1
          prev = args
        } else i += 1
      }
      //itemNames.keySet.foreach((x) => {println("Key:" + x); println("Value:"); println(itemNames(x))})
      if (debug) println("-- extractFromClause()")
    }

    //Performs preliminary error checks to ensure basic correctness before proceeding
    private def prelimErrorCheck(tokens: ArrayBuffer[String], rhs: Tree) {
      var i = 1
      while (i < tokens.size) {
        tokens(i) match {
          case "join" =>
          case "let" => unit.error(rhs.pos, "let clauses are currently unsupported")
          case "where" =>
          case "orderby" =>
          case "select" =>
          case "selectMany" =>
          case "group" =>
          case "into" => {
            if (i - 2 < 0 || !(tokens(i - 2) == selectKW || tokens(i - 2) == selectManyKW || tokens(i - 2) == groupKW))
              unit.error(rhs.pos, "query continuation (into clause) must be preceded by select or group clause")
          }
          case _ => unit.error(rhs.pos, tokens(i) + " is not valid query syntax")
        }
        i += 2
      }
    }

    //Rewrites orderby clauses to fill in TOKENS with the appropriate methods and arguments
    private def rewriteOrderings(tokens: ArrayBuffer[String], rhs: Tree) {
      if (debug) println("rewriteOrderings() --")
      var i = 0
      while (i < tokens.size) {
        if (tokens(i) == orderbyKW) {
          val args = if (i + 1 < tokens.size) tokens(i + 1).trim else unit.error(rhs.pos, "no arguments to orderby")
          val innerTokens = args.asInstanceOf[String].split("[,]")
          innerTokens.foreach(_.trim)
          if (innerTokens.size > 1) {
            var j = 1
            while (j < innerTokens.size) {
              val k = i + j * 2
              if (innerTokens(j).endsWith(descKW)) {
                val newMethod = "thenbyDesc"
                val newArgs = innerTokens(j).substring(0, innerTokens(j).length - descKW.length).trim
                tokens.insert(k, newMethod)
                tokens.insert(k + 1, newArgs)
              } else {
                val newMethod = "thenby"
                val newArgs = innerTokens(j)
                tokens.insert(k, newMethod)
                tokens.insert(k + 1, newArgs)
              }
              j += 1
            }
          }
          if (innerTokens(0).endsWith(descKW)) {
            tokens(i) = "orderbyDesc"
            tokens(i + 1) = innerTokens(0).substring(0, innerTokens(0).length - descKW.length).trim
          } else tokens(i + 1) = innerTokens(0)
        }
        i += 1
      }
      if (debug) println("-- rewriteOrderings()")
    }

    //Rewrites group clauses to fill in TOKENS with the appropriate methods and arguments
    private def rewriteGroups(tokens: ArrayBuffer[String], rhs: Tree) {
      if (debug) println("rewriteGroups() --")

      var i = 0
      while (i < tokens.size) {
        if (tokens(i) == groupKW) {
          val args = if (i + 1 < tokens.size) tokens(i + 1).trim else unit.error(rhs.pos, "no arguments to group")
          val innerTokens = args.asInstanceOf[String].split(" by ")
          innerTokens.foreach(_.trim)
          tokens(i + 1) = innerTokens.mkString(",,")
          tokens(i) = "groupBy"
        }
        i += 1
      }

      if (debug) println("-- rewriteGroups()")
    }

    //Rewrites join clauses to fill in TOKENS with the appropriate methods and arguments
    private def rewriteJoins(itemNames: mutable.HashMap[String, ArrayBuffer[String]], tokens: ArrayBuffer[String], rhs: Tree) {
      if (debug) println("rewriteJoins() --")

      var i = 0
      while (i < tokens.size) {
        if (tokens(i) == joinKW) {
          val args = if (i + 1 < tokens.size) tokens(i + 1).trim else unit.error(rhs.pos, "no arguments to join")
          val halves = args.asInstanceOf[String].split(" on ")
          if (halves.size != 2) unit.error(rhs.pos, "join clause must specify both target collection and keys to join on")
          val (srcColl, typ) = extractJoinCollection(halves(0), itemNames, rhs)
          halves(0) = srcColl
          val keySels = halves(1).split(" equals ")
          //assert keySels of size 2?
          val ending = keySels(1).split(" into ")
          ending.foreach(_.trim)
          if (ending.size > 1) {
            val arrBuf = new ArrayBuffer[String]
            arrBuf += ending(1) + ":Queryable[" + typ + "]"
            itemNames += Tuple2[String, ArrayBuffer[String]](ending(1), arrBuf)
            keySels(1) = ending(0)
            tokens(i) = groupJoinKW
          }
          halves(1) = keySels.mkString(",,")
          tokens(i + 1) = halves.mkString(",,")

          //insert implicit select (and into clause?) and fix itemNames
          val first = keySels(0).split("[ ]+")(0)
          val second = if (ending.size > 1) ending(1) else keySels(1).split("[ ]+")(0)
          tokens(i + 1) += ",,new val _" + first + " = " + first + " ; val _" + second + " = " + second
          val name = newSelNameString
          tokens.insert(i + 2, intoKW)
          tokens.insert(i + 3, name)
          itemNames(first) += name + "._" + itemNames(first)(itemNames(first).size - 1).split("[:]")(0)
          itemNames(second) += name + "._" + itemNames(second)(itemNames(second).size - 1).split("[:]")(0)

          joined = true
        }
        i += 1
      }

      if (debug) println("-- rewriteJoins()")
    }

    /*Extracts name of join collection (first argument to .join()) from join clause and also fills in the
      item name that acts as a range variable over that collection.*/
    private def extractJoinCollection(s: String, itemNames: mutable.HashMap[String, ArrayBuffer[String]], rhs: Tree) = {
      if (debug) println("extractJoinCollection() --")
      val innerTokens = s.split("[ ]+")
      innerTokens.foreach(_.trim)
      if (innerTokens.size != 5) unit.error(rhs.pos, "join clause must begin with an expression of the form: join <itemName> ofType <typeName> in <sourceCollection>")
      if (innerTokens(1) != ofTypeKW) unit.error(rhs.pos, "join clause is missing \"" + ofTypeKW + "\" keyword")
      if (innerTokens(3) != inKW) unit.error(rhs.pos, "join clause is missing \"" + inKW + "\" keyword")
      val arrBuf = new ArrayBuffer[String]
      arrBuf += innerTokens(0) + ":" + innerTokens(2)
      itemNames += Tuple2[String, ArrayBuffer[String]](innerTokens(0), arrBuf)
      if (debug) println("-- extractJoinCollection()")
      (innerTokens(4), innerTokens(2))
    }

    //Records the range variable names introduced in into clauses so that their references can be properly converted
    private def recordContinuations(itemNames: mutable.HashMap[String, ArrayBuffer[String]], tokens: ArrayBuffer[String], rhs: Tree) = {
      var i = 0
      while (i < tokens.size) {
        if (tokens(i) == intoKW) {
          if (itemNames.keySet.contains(tokens(i + 1))) unit.error(rhs.pos, "identifier " + tokens(i + 1)
                  + " (from into clause) is already defined")
          else {
            val arrBuf = new ArrayBuffer[String]
            arrBuf += tokens(i + 1)
            itemNames += Tuple2[String, ArrayBuffer[String]](tokens(i + 1), arrBuf)
          }
        }
        i += 1
      }
    }

    /*Patches references made to the range variables declared in from or join clauses (e.g. collapses the appropriate
      elements of TOKENS so that an attempt to access a field of the range variable (e.g. c.name) is contained
      in a single element of TOKENS rather than two elements containing c and name respectively).  Also creates
      a parameter list to add to the beginning of the arguments element in TOKENS for each method call.*/
    private def convertReferences(itemNames: mutable.HashMap[String, ArrayBuffer[String]], tokens: ArrayBuffer[String]) = {
      if (debug) println("convertReferences() --")
      itemNames.keySet.foreach((x) => {println("Key:" + x); println("Value:"); println(itemNames(x))})
      var joinNum = 0
      var i = 0
      while (i < tokens.size) {
        val kW = tokens(i)
        if (isKeyWord(kW) || isSQO(kW)) {
          i += 1
          val join = (kW == joinKW || kW == groupJoinKW)
          if (join) joinNum += 1
          val methodArgs = tokens(i).split("[,][,]")
          methodArgs.foreach(_.trim)
          var j = 0
          while (j < methodArgs.size) {
            if (kW != intoKW) {
              val innerTokens = toArrayBuffer(methodArgs(j).split("[ ]+"))
              innerTokens.foreach(_.trim)
              var paramList = ""
              var k = 0
              while (k < innerTokens.size) {
                for (name <- itemNames.keySet; if name == innerTokens(k); options = itemNames(name)) {
                  //add to paramList if necessary
                  val replacement = options.last.split("[.]")(0)
                  if (!paramList.contains(replacement + ",")) {
                    val newParam = {
                      if (join) {
                        if (joinNum < options.size) options(joinNum - 1).split("[.]")(0)
                        else options(0).split("[.]")(0)
                      }
                      else {
                        if (joinNum < options.size) options(joinNum).split("[.]")(0)
                        else options.last.split("[.]")(0)
                      }
                    }
                    if (newParam.contains(':')) paramList += newParam + ", "
                    else paramList = newParam + ", " + paramList
                  }
                  //change identifier if necessary
                  innerTokens(k) = {
                    if (join) {
                      if (joinNum < options.size) options(joinNum - 1).split("[:]")(0)
                      else options(0).split("[:]")(0)
                    }
                    else {
                      if (joinNum < options.size) options(joinNum).split("[:]")(0)
                      else options.last.split("[:]")(0)
                    }
                  }
                  //add field selection if necessary
                  if (k + 1 < innerTokens.size) {
                    if (innerTokens(k + 1) != ";") innerTokens(k) += "."
                    innerTokens(k) += innerTokens(k + 1)
                    innerTokens.remove(k + 1)
                    k -= 1
                  }
                }
                k += 1
              }
              //remove excess comma and space and wrap param list in parens
              if (paramList.endsWith(", ")) paramList = paramList.substring(0, paramList.length - 2)
              paramList = "(" + paramList + ") "
              //separate multiple args to method call by ",,"
              if (j == 0) tokens(i) = paramList + innerTokens.mkString(" ")
              else tokens(i) += ",," + paramList + innerTokens.mkString(" ")
              //remove empty parens on first arg to join if necessary
              if (join && j == 0) tokens(i) = tokens(i).substring(3)
            }
            j += 1
          }
        }
        i += 1
      }
      println("Post conversion:"); tokens.foreach(println)
      if (debug) println("-- convertReferences()")
    }

    /*Patches arguments to methods with parens and dots as necessary.  Needed (for e.g) in order to pass function
      objects that themselves contain method invocations with arguments, and for predicate function objects that
      contain a "major" logical operator (conjunction, disjunction, equality).*/
    private def completeMethodCalls(tokens: ArrayBuffer[String]) = {
      //println("Pre completion:"); tokens.foreach(println)
      var i = 2
      while (i < tokens.size) {
        val args = tokens(i).split("[,][,]")
        args.foreach(_.trim)
        val join = (tokens(i - 1) == joinKW || tokens(i - 1) == groupJoinKW)
        var first = true
        for (a <- args) {
          if (!(join && first) && !(tokens(i - 1) == intoKW)) {
            val splits = a.split("[)][ ]")
            splits.foreach(_.trim)
            val paramList = splits(0) + ")"
            var body = splits(1).split("[ ]")
            body.foreach(_.trim)
            var logOpCount = 0
            if (body(0) != newKW && body.size > 1) {
              var j = 1
              if (body.size < 3) body(0) += "(" + body(j) + ")"
              else {
                while (j < body.size) {
                  if (j % 2 != 0) body(0) += "." + (if (body(j) == "$eq$eq") "equals" else body(j)) //unabashed hack (prevents problems deep in backend)
                  else body(0) += "(" + body(j) + ")"
                  if (isLogicalOp(body(j))) {
                    logOpCount += 1
                    body(0) += '(' + body(j + 1)
                    j += 1
                  }
                  j += 1
                }
                body(0) += ")" * logOpCount
              }
              tokens(i) = if (first) paramList + " " + body(0) else tokens(i) + " ,, " + paramList + " " + body(0)
            }
          }
          first = false
        }
        i += 2
      }
      //println("Post completion:"); tokens.foreach(println)
    }

    //Constructs the list of ValDefs that act as value parameters for a method
    private def getVParams(i: Int, qry: ArrayBuffer[String]) = {
      if (debug) println("getVParams() --")
      val vdefs = new ListBuffer[ValDef]
      def unnecessary(paramNames: ArrayBuffer[String]) = {
        var i = 0
        while (i < paramNames.size) {
          val paramTokens = paramNames(i).split("[:]")
          val tpt = if (paramTokens.size > 1) {
            val typTokens = paramTokens(1).split('[')
            typTokens.foreach(_.trim)
            if (typTokens.size > 1) AppliedTypeTree(Ident(newTypeName(typTokens(0))),
              List[Tree](Ident(newTypeName(typTokens(1).substring(0, typTokens(1).length - 1)))))
            else Ident(newTypeName(paramTokens(1)))
          } else TypeTree(NoType)
          val name = newTermName(paramTokens(0))
          vdefs += ValDef(Modifiers(PARAM), name, tpt, EmptyTree)
          i += 1
        }
      }
      unnecessary(toArrayBuffer(getParamList(i, qry).split("[,]")))
      if (debug) println("-- getVParams()")
      vdefs.toList
    }

    //Returns the value parameter list of a method as a string (without the enclosing parens)
    private def getParamList(index: Int, qry: ArrayBuffer[String]) = {
      if (debug) println("getParamList() --")
      var paramList = ""
      var i = 0
      var done = false
      val tkn = qry(index)

      //swallow param list
      while (i < tkn.size && !done) {
        val curr = stringWrapper(tkn)(i)
        if (!(curr == ' ' || curr == '(' || curr == ')')) paramList += curr
        if (curr == ')') done = true
        i += 1
      }

      //update token in query buffer
      qry(index) = tkn.substring(i).trim

      if (debug) println("-- getParamList()")
      paramList
    }

    //Constructs the argument list for a call to groupBy
    private def getGroupArgs(i: Int, qry: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("getGroupArgs() --")
      val args = new ListBuffer[Tree]
      val funcs = qry(i).split("[,][,]")
      funcs.foreach(_.trim)
      var j = funcs.size - 1
      while (j >= 0) {
        qry(i) = funcs(j)
        args += Function(getVParams(i, qry), getSelectBody(i, qry, rhs))
        j -= 1
      }
      if (debug) println("-- getGroupArgs()")
      args.toList
    }

    //Constructs the argument list for a call to join or groupJoin
    private def getJoinArgs(i: Int, qry: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("getJoinArgs() --")
      val applyArgs = new ListBuffer[Tree]
      val joinArgs = qry(i).split("[,][,]")
      joinArgs.foreach(_.trim)
      applyArgs += Ident(newTermName(joinArgs(0)))
      var j = 1
      while (j < joinArgs.size) {
        qry(i) = joinArgs(j)
        applyArgs += Function(getVParams(i, qry), getSelectBody(i, qry, rhs))
        j += 1
      }
      if (debug) println("-- getJoinArgs()")
      applyArgs.toList
    }

    //Retrieves and constructs the arguments to a call to .select()
    private def getSelectBody(i: Int, qry: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("getSelectBody() --")
      //println("Select body string: " + qry(i))
      if (debug) println("-- getSelectBody()")
      if (qry(i).startsWith(newKW)) selectClass(i, qry, rhs)
      else selectMember(qry(i))
    }

    //Retrieves and constructs the arguments to a call to .where()
    private def getWhereBody(whereArg: String) = {
      if (debug) println("getBody() --")
      //println("Where body string:" + whereArg)
      if (debug) println("-- getBody()")
      selectMember(whereArg)
    }

    //Retrieves and constructs the arguments to calls to .orderby(), .orderbyDesc(), .thenby() and .thenbyDesc()
    private def getOrderingBody(orderingArg: String) = {
      if (debug) println("getOrderingBody() --")
      //println("Ordering body string: " + orderingArg)
      if (debug) println("-- getOrderingBody()")
      selectMember(orderingArg)
    }

    /*Constructs a class instance that was created within a projection function
     (usually a call to .select() or .groupBy())*/
    private def selectClass(i: Int, qry: ArrayBuffer[String], rhs: Tree) = {
      if (debug) println("selectClass() --")

      //swallow "new"
      qry(i) = qry(i).substring(4)

      var tkn = peekToken(qry(i), " ", true)
      if (tkn == valKW) {
        //construct anonymous class
        val vdefs = new ListBuffer[Tree]
        while (qry(i) != "") {
          var stat = nextToken(i, qry, ";", false).trim
          if (stat != "}") {
            if (stat.startsWith(valKW)) {
              //add named field (val def)
              val (name, rhs) = nextSubToken(stat.substring(3), "=", false)
              vdefs += ValDef(Modifiers(0), newTermName(name.trim), TypeTree(NoType), selectMember(rhs.trim))
            } else {
              // disallow statement because it's not a value definition
              unit.error(rhs.pos, "anonymous class body must contain only value definitions")
            }
          }
        }
        val name = newAnonNameString
        val body = (newInitMethod +: vdefs).toList
        val impl = Template(List(Select(Ident(newTermName("scala")), newTypeName("AnyRef"))), emptyValDef, body)
        val stats = ClassDef(Modifiers(FINAL), newTypeName(name), List(), impl)
        val expr = Apply(Select(New(Ident(newTypeName(name))), newTermName("<init>")), Nil)
        if (debug) println("-- selectClass()")
        Block(List(stats), expr)
      } else {
        //construct named class or anonymous instance of trait
        unit.error(rhs.pos, "Can't handle selecting named classes or anonymous trait instances yet")
        EmptyTree
      }
    }

    //Constructs the tree representing a member (field or method, where the method may have arguments) selection
    private def selectMember(s: String):Tree = {

      def ignoreDots(s: String) = {
        var neater = ""
        for (ch <- s) ch match {
          case '.' => neater += ' '
          case char => neater += char
        }
        neater
      }

      def unnecessary(tokens: Array[String]) = {
        val tkns = toArrayBuffer(tokens)
        groupArgLists(tkns)
        var tree:Tree = makeTokenTree(tkns(0))

        var i = 1
        while (i < tkns.size) {
          val (sel, args) = nextSubToken(tkns(i), "(", false)
          tree = Select(tree, newTermName(sel))
          if (args != "") tree = Apply(tree, getMethodArgs(args))
          i += 1
        }
        tree
      }

      unnecessary(ignoreDots(s).split("[ ]+"))
    }

    //Groups together arguments to a single method invocation
    private def groupArgLists(tokens: ArrayBuffer[String]) = {
      def countParens(s: String) = {
        var openCount = 0
        for (ch <- s) {
          ch match {
            case '(' => openCount += 1
            case ')' => openCount -= 1
            case _ =>
          }
        }
        openCount
      }

      var i = 0
      while (i < tokens.size) {
        if (tokens(i).contains("(")) {
          var openCount = countParens(tokens(i))
          var original = i
          while (openCount != 0) {
            i += 1
            openCount += countParens(tokens(i))
            tokens(original) += " " + tokens(i)
            tokens.remove(i)
            i -= 1
          }
        }
        i += 1
      }
    }

    //Makes a tree node from a token representing a basic node (i.e. a literal or an ident)
    private def makeTokenTree(tkn: String) = {
      if (stringWrapper(tkn)(0) >= '0' && stringWrapper(tkn)(0) <= '9') {
        //numeric literal
        if (tkn.contains(".")) Literal(tkn.toDouble)
        else Literal(tkn.toInt)
      } else if (tkn == "true" || tkn == "false"){
        //boolean literal
        Literal(tkn.toBoolean)
      } else if (tkn.contains("\"")) {
        //string literal
        Literal(tkn.substring(1, tkn.length - 1))
      } else {
        Ident(newTermName(tkn))
      }
    }

    //Constructs the argument list for a method (for all arguments other than function objects)
    private def getMethodArgs(s: String) = {
      var unwrapped = s

      //take off outer parens if necessary (assumes balanced parens!)
      if (s.startsWith("(")) unwrapped = s.substring(0)
      if (s.endsWith(")")) unwrapped = s.take(s.size - 1)

      val args = unwrapped.split("[,]")
      args.foreach(_.trim)
      val parsed = new ListBuffer[Tree]
      for (arg <- args) parsed += selectMember(arg)
      parsed.toList
    }

    //Returns and removes the next token from the string at index I in QRY
    private def nextToken(i: Int, qry: ArrayBuffer[String], delim: String, ignoreMultiple: Boolean) = {
      val (tkn, qryString) = nextSubToken(qry(i), delim, ignoreMultiple)
      qry(i) = qryString
      tkn
    }

    //Returns the next token in the string and the string without that token
    private def nextSubToken(s: String, delim: String, ignoreMultiple: Boolean) = {
      val regex = if (ignoreMultiple) "[" + delim + "]+" else "[" + delim + "]"
      val split = s.split(regex)
      (split(0), split.drop(1).mkString(delim))
    }

    //Returns the next token from string (without removing it)
    private def peekToken(s: String, delim: String, ignoreMultiple: Boolean) = nextSubToken(s, delim, ignoreMultiple)._1

  }
}
