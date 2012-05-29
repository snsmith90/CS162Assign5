import scala.io._
import cs162.miniJS.syntax._
import cs162.miniJS.levels._

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
      inScope( Env(), Public ).eval(ast)
  }
  
}

// type environment
case class Env( env:Map[String, Level] = Map() ) {

  // retrieve a variable's type or throw exception if variable
  // isn't in the environment
  def apply( x:String ): Level =
    env get x match {
      case Some(l) ⇒ l
      case None ⇒ throw illtyped
    }  

  def ++( bindings:Seq[(String, Level)] ): Env =
    Env( env ++ bindings )

  def +++( bindings:Seq[(Var, Level)] ): Env =
    this ++ ( bindings map { case (x, l) ⇒ (x.x, l) } )
}


// every term is typechecked inside a scope corresponding to a
// particular environment
case class inScope( ρ:Env, l_w:Level ) {

  def eval( t:Term ): Level = t match {
    case Then( ts ) ⇒ 
      (ts map eval) last
    
    case Assign( Var(x), e ) ⇒ 
    {
      eval( e ) ⊑ ρ( x )
      l_w ⊑ ρ( x )
      l_w
    }
    
    case w @ While( e, t ) ⇒ 
    {
      eval( e )
      eval( t )
      l_w
    }

    case Output( e, l ) ⇒
    {
      l_w ⊑ l
      eval( e ) ⊑ l
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
      l_w ⊑ L()
      ρ( x ) ⊑ L()
      L()

    case UnOp( op, e ) ⇒ 
    {
      eval( e )
    }
    
    case BinOp( op, e1, e2 ) ⇒ 
    {
      val l1 = eval( e1 )
      val l2 = eval( e2 )

      l1 ⊑ L()
      l2 ⊑ L()

      L()
    }

    // we're not sure about l2 and l3
    case If( e, t1, t2 ) ⇒ 
    {
      val l1 = eval( e )
      val l2 = inScope( Env(), l1 ).eval(t1)
      val l3 = inScope( Env(), l1 ).eval(t2)

      l2 ⊑ L()
      l3 ⊑ L()

      L()
    }
    
    case In( typ, l ) ⇒ {
      l_w ⊑ L()
      l ⊑ L()
      L()
    }

    // TODO: Comeback to
    // Assign variables to Li
    case Let( xes, t ) ⇒ 
    {
     /* val (xs, es) = xes unzip;
      val xτs = xs map { case _ ⇒ LVar() }

      l_w ⊑ L()


      val ρ1 = ρ +++ (xs zip xτs)
      val eτs = es map ( inScope( ρ1 ) eval(_) )

      (xτs zip eτs) foreach { case (τ1, τ2) ⇒ τ1 ≡ τ2 }
      inScope( ρ1 ) eval( t ) */
      L()
    }
  }

}
