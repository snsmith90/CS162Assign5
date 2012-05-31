package cs162.miniJS.levels
import scala.collection.mutable.{ Queue, Set, ListBuffer }

// exception for illtyped programs
object illtyped extends Exception

// levels
sealed abstract class Level {}

case object Bob extends Level {}
case object Alice extends Level {}
case object Public extends Level {}
case object Secret extends Level {}

case class LVar(x:String) extends Level {}
object LVar {
  var id = 0;

  def apply(): LVar =
  {
    id += 1
    new LVar("L" + id)
  }
}

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
					if(head == Public || head == Bob) {
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
			}
	}
}
