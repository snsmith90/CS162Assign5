import scala.io._
import cs162.miniJS.syntax._
import cs162.miniJS.levels._
import scala.collection.mutable.{ Queue, Set, ListBuffer }

// the main entry point
object Checker {

  def main(args:Array[String]) {
    // parse the given program
    val ast = ParseL.getAST( Source.fromFile( args(0) ).mkString )
    
    // either print the AST or typecheck it
   /* if ( args.length > 1 && args(1) == "--ast" )
      //printDot( ast )
    else */
      //println( (inScope( Env(), Public() ) eval ast).find )
      println(ast)

		// adding secret, alice, bob, and public to setOfSets


		setOfSets.addToBigSet(Secret)
		setOfSets.addToBigSet(Alice)
		setOfSets.addToBigSet(Bob)
		setOfSets.addToBigSet(Public)

  	//System.exit(1)
    evalLevel().eval(Env(),Public, ast)

		println("bigSet: " +setOfSets.bigSet)
		println("Secure") 

		
  }
  
}

                        //HELP
// type environment
case class Env( env:Map[String, LVar] = Map() ) {

  // retrieve a variable's type or throw exception if variable
  // isn't in the environment
  def apply( x:String ): LVar =     ///Should we return LVar here?
    env get x match {
      case Some(l) ⇒ l
      case None ⇒ throw illtyped
    }  

  def ++( bindings:Seq[(String, LVar)] ): Env =
    Env( env ++ bindings )

  def +++( bindings:Seq[(Var, LVar)] ): Env =      ///HELP
    this ++ ( bindings map { case (x, l) ⇒ (x.x, l) } )
}


// every term is typechecked inside a scope corresponding to a
// particular environment
case class evalLevel(){
      def eval( ρ:Env, l_w:Level, t:Term ): Level = t match {
        case Then( ts ) ⇒ {
          // ts.map(eval).last              //TODO: for each loops through ts and calls eval returns level of last term
          var temp = l_w // temp holder
          ts.foreach(  i => temp = eval( ρ, l_w, i) )
          temp
        }

        case Assign( Var(x), e ) ⇒
        {
           var l1 = eval( ρ, l_w, e )
           var l2 =  ρ( x )
          println("Assign " + l1 + " ⊑ " + l2)
					setOfSets.addToListBuffer(l1, l2)
          println("Assign " + l_w + " ⊑ " + l2)
					setOfSets.addToListBuffer(l_w, l2)
          l_w
        }

        case w @ While( e, t ) ⇒
        {
          eval( ρ, l_w, e )
          eval( ρ, l_w, t )
          l_w
        }

        case Output( e, l ) ⇒
        {
          println("Output " + l_w + " ⊑ " + l)
					setOfSets.addToListBuffer(l_w, l)
					val result = BFS.algo(l_w)

					if (!result) { System.exit(1); println("Not Secure") }
          var l1 = eval( ρ, l_w, e )
          println("Output " + l1 + " ⊑ " + l)
					setOfSets.addToListBuffer(l1, l)

          l_w
        }

        case Num( n ) ⇒
          l_w

        case Bool( b ) ⇒
          l_w

        case Str( s ) ⇒
          l_w

        case Undef() ⇒
          l_w

        case Var( x ) ⇒
           var L = LVar()
					 setOfSets.addToBigSet(L)
           var l1 = ρ( x )
           println("Var " + l_w + " ⊑ " + L)
						setOfSets.addToListBuffer(l_w, L)
           println("Var " + l1 + " ⊑ " + L)
						setOfSets.addToListBuffer(l1, L)
          L

        case UnOp( op, e ) ⇒
        {
          eval( ρ, l_w, e )
        }

        case BinOp( op, e1, e2 ) ⇒
        {
          val l1 = eval(ρ, l_w, e1 )
          val l2 = eval( ρ, l_w, e2 )
          var L = LVar()
					setOfSets.addToBigSet(L)
          println("BinOp " + l1 + " ⊑ " + L)
					setOfSets.addToListBuffer(l1, L)
          println("BinOP " + l2 + " ⊑ " + L)
					setOfSets.addToListBuffer(l2, L)

          L
        }

        // we're not sure about l2 and l3
        case If( e, t1, t2 ) ⇒
        {
          val l1 = eval( ρ, l_w, e )
          val l2 = eval( ρ, l1, t1 )
          val l3 = eval( ρ, l1, t2 )
          var L = LVar()
					setOfSets.addToBigSet(L)
          println("If " + l2 + " ⊑ " + L)
					setOfSets.addToListBuffer(l2, L)
          println("If " + l3 + " ⊑ " + L)
					setOfSets.addToListBuffer(l3, L)

          L
        }

        case In( typ, l ) ⇒ {
            var L = LVar()
					  setOfSets.addToBigSet(L)
            println("In " + l_w + " ⊑ " + L)
						setOfSets.addToListBuffer(l_w, L)
            println("In " + l + " ⊑ " + L)
						setOfSets.addToListBuffer(l, L)
          L
        }

        // TODO: Comeback to
        // Assign variables to Li
        case Let( xes, t ) ⇒
        {
            println("Whats in xes? " + xes)
          val xls = xes map { case _ ⇒ LVar()}
          println("Whats in xls? " + xls)



          val ρ1 = ρ +++ (xes zip xls)
           println("Whats in ρ1? " + ρ1)
           xls.foreach(i => { println("Let " + l_w + " ⊑ " + i) 
																setOfSets.addToBigSet(i)
																setOfSets.addToListBuffer(l_w, i)})

          eval(ρ1, l_w, t )

        }
      }
}
