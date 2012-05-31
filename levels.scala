package cs162.miniJS.levels
import scala.collection.mutable.{ Queue, Set, ListBuffer }

// exception for illtyped programs
object illtyped extends Exception

// levels
sealed abstract class Level {
  // level completeness
   /*def ⊑( l:Level ): Unit = (this.find, l.find) match {
    //TODO
   case (τ1, τ2) if τ1 == τ2 ⇒ ()
    case (tv:TVar, τ2) if !(τ2.tvars contains tv) ⇒ tv ▷ τ2
    case (τ1, tv:TVar) if !(τ1.tvars contains tv) ⇒ tv ▷ τ1

    case _ ⇒ throw illtyped
  }  */

 /* def ⊑( l:Level ) : Boolean = {
    true

  }  */

  // return the set of type variable in this type
 /* def tvars: Set[TVar] = this match {
    case NumT() | BoolT() | StrT() | UnitT() ⇒ Set()
    case tv:TVar ⇒ Set( tv )
    case ListT(τ) ⇒ τ.tvars
    case FunT(τs, τ) ⇒ τs.foldLeft( τ.tvars )( (vs, τ) ⇒ vs ++ τ.tvars )
  }

  // union-find data structure: find a type's set representative (uses
  // path compression)
  def find: Type = this match {
    case tv:TVar ⇒ tv.parent match {
      case None ⇒ tv
      case Some(τ) ⇒ { tv.parent = Some( τ.find ); tv.parent.get }
    }
    case ListT(τ) ⇒ ListT( τ.find )
    case FunT(τs, τ) ⇒ FunT( for (τ ← τs) yield τ.find, τ.find )
    case _ ⇒ this
  }     */

  // common to all types
 /* def T = BoolT()
  def ≈ ( l:Level ) = BoolT()
  def ≠ ( l:Level ) = BoolT()
  def ∧ ( l:Level ) = BoolT()
  def ∨ ( l:Level ) = BoolT()
  def not = BoolT()

  // type specific
  def  + ( l:Level ): Level
  def  − ( l:Level ): Level
  def  × ( l:Level ): Level
  def  ÷ ( l:Level ): Level
  def  ≤ ( l:Level ): Level
  def  < ( l:Level ): Level
  def :: ( l:Level ): Level
  def neg: Level    */
}

case object Bob extends Level {

}

case object Alice extends Level {

}

case object Public extends Level {

 }

case object Secret extends Level {

}


 //TODO: FIX THIS
case class L() extends Level {
/*
   def  + ( l:Level ) = { l }
   def  − ( l:Level ) = { l }
   def  × ( l:Level ) = { l }
   def  ÷ ( l:Level ) = { l }
   def  ≤ ( l:Level ) = { l }
   def  < ( l:Level ) = { l }
   def :: ( l:Level ) = throw illtyped
   def neg = Public()   */
 }

case class LVar(x:String) extends Level {

}
object LVar {
  var id = 0;

  def apply(): LVar =
  {
    id += 1
    new LVar("L" + id)
  }
}

 /*
case class NumT() extends Type {
  def  + ( l:Level ) = { τ ≡ NumT() ; NumT() }
  def  − ( l:Level ) = { τ ≡ NumT() ; NumT() }
  def  × ( l:Level ) = { τ ≡ NumT() ; NumT() }
  def  ÷ ( l:Level ) = { τ ≡ NumT() ; NumT() }
  def  ≤ ( l:Level ) = { τ ≡ NumT() ; BoolT() }
  def  < ( l:Level ) = { τ ≡ NumT() ; BoolT() }
  def :: ( l:Level ) = throw illtyped
  def neg = NumT()
}

case class BoolT() extends Type {
  def  + ( l:Level ) = throw illtyped
  def  − ( l:Level ) = throw illtyped
  def  × ( l:Level ) = throw illtyped
  def  ÷ ( l:Level ) = throw illtyped
  def  ≤ ( l:Level ) = throw illtyped
  def  < ( l:Level ) = throw illtyped
  def :: ( l:Level ) = throw illtyped
  def neg = throw illtyped
}

case class StrT() extends Type {
  def  + ( l:Level ) = { τ ≡ StrT() ; StrT() }
  def  − ( l:Level ) = throw illtyped
  def  × ( l:Level ) = throw illtyped
  def  ÷ ( l:Level ) = throw illtyped
  def  ≤ ( l:Level ) = { τ ≡ StrT() ; BoolT() }
  def  < ( l:Level ) = { τ ≡ StrT() ; BoolT() }
  def :: ( l:Level ) = throw illtyped
  def neg = throw illtyped
}

case class UnitT() extends Type {
  def  + ( l:Level ) = throw illtyped
  def  − ( l:Level ) = throw illtyped
  def  × ( l:Level ) = throw illtyped
  def  ÷ ( l:Level ) = throw illtyped
  def  ≤ ( l:Level ) = throw illtyped
  def  < ( l:Level ) = throw illtyped
  def :: ( l:Level ) = throw illtyped
  def neg = throw illtyped
}

case class FunT(τs:Seq[Type], l:Level) extends Type {
  def  + ( τ1:Type ) = throw illtyped
  def  − ( τ1:Type ) = throw illtyped
  def  × ( τ1:Type ) = throw illtyped
  def  ÷ ( τ1:Type ) = throw illtyped
  def  ≤ ( τ1:Type ) = throw illtyped
  def  < ( τ1:Type ) = throw illtyped
  def :: ( τ1:Type ) = throw illtyped
  def neg = throw illtyped
}

case class ListT(l:Level) extends Type {
  def  + ( τ1:Type ) = throw illtyped
  def  − ( τ1:Type ) = throw illtyped
  def  × ( τ1:Type ) = throw illtyped
  def  ÷ ( τ1:Type ) = throw illtyped
  def  ≤ ( τ1:Type ) = throw illtyped
  def  < ( τ1:Type ) = throw illtyped
  def :: ( τ1:Type ) = { τ1 ≡ τ ; ListT(τ) }
  def neg = throw illtyped
}

// type variables. ann is a flag indicating whether this TVar is
// annotated (true) or not (false); it's made mutable in order to
// transfer annotations when TVars are unioned together
case class TVar(x:String, var ann:Boolean) extends Type {
  def  + ( τ1:Type ) = { ann = true ; this ≡ τ1 ; τ1 }
  def  − ( τ1:Type ) = { this ≡ τ1 ; τ1 ≡ NumT() ; NumT() }
  def  × ( τ1:Type ) = { this ≡ τ1 ; τ1 ≡ NumT() ; NumT() }
  def  ÷ ( τ1:Type ) = { this ≡ τ1 ; τ1 ≡ NumT() ; NumT() }
  def  ≤ ( τ1:Type ) = { ann = true ; this ≡ τ1 ; τ1 }
  def  < ( τ1:Type ) = { ann = true ; this ≡ τ1 ; τ1 }
  def :: ( τ1:Type ) = { this ≡ ListT(τ1) ; this }
  def neg = { this ≡ NumT() ; NumT() }

  // union-find data structure: union this TVar with some other type
  // (the other type will always be the new root)
  def ▷(l:Level) {
    // check/transfer type annotation
    if ( ann ) τ match {
      case tv:TVar ⇒ tv.ann = true
      case NumT() | StrT() | _:TVar ⇒ ()
      case _ ⇒ throw illtyped
    }

    // merge equivalence classes
    this.parent = Some( τ )
  }

  // union-find data structure: parent pointer (None if this is a root)
  var parent:Option[Type] = None
}

// TVar companion object for factory method (generates fresh type variables)
object TVar {
  var id = 0;

  def apply(ann:Boolean = false): TVar = 
  { 
    id += 1
    new TVar("T" + id, ann) 
  }
}


*/

// element 0 in each subset in bigSet denotes the level the set belongs to
object setOfSets {
	var bigSet = Set.empty[ListBuffer[Level]]
	var littleTempListBuffer = new ListBuffer[Level]

	def addToBigSet( l:Level ) {
		littleTempListBuffer += l
		bigSet += littleTempListBuffer.clone
		littleTempListBuffer.clear()
	}

	def addToListBuffer (l:Level, levelToAdd:Level) {
			bigSet.foreach( i => if (i.head == l) {
						i += levelToAdd
					}

  			)	 
	}

}

object BFS {
	var queue = new Queue[Level]
	var visited = new ListBuffer[Level]

	def algo(rootNode:Level) {

			queue += rootNode
			visited += rootNode
		
		while (!queue.isEmpty) {
			// dequeue a node from queue
				val head = queue.dequeue()
				if(rootNode == Secret) {
					if(head == Alice || head == Bob || head == Public) {
						println("Not Secure")
						System.exit(1) 
					}
				}
				else if (rootNode == Bob) {
					if(head == Alice || head == Public) {
						println("Not Secure")
						System.exit(1) 
					}
				}
				else if (rootNode == Alice) {
					if(head == Public) {
						println("Not Secure")
						System.exit(1) 
					}
				}
			//  and examine elements 1..n for that subset
			setOfSets.bigSet.foreach( i => if (i.head == head) {
						var s = i.tail.toSet
						s.foreach( j => { if(!visited.contains(j)) {
						                        queue += j
						                        visited += j
						                   }
						                   })

					}

  			)
  			 queue = queue.distinct.toQueue
  			 //println(queue)
  			 //println(visited)
			}			

				//  if it's still secure, enqueue all its childer
				// if it's not, stop program
	
			// if queue is empty, entire graph has been searched --> program is secure
              visited.clear
			// if queue not empty, dequeue node again (repeat step 2)
			true
	}
}
