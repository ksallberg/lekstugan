package test

object TestaLite {

    def main( args : Array[String] ) {

        //Map fibonacci
        val nums = Array(1,2,3,4,5,6,7,8,9,10)
        val nums2 = nums map { fib }
        println( nums2.deepToString() )

        //Filter
        val nums3 = nums filter { filt }
        println( nums3.deepToString() )

        //List comprehension with filter
        for( i <- Iterator.range(0, 20) if i % 2 == 0 )
            println( i )
    }

    //Recursive fibonacci
    def fib( i : Int ) : Int = {

        i match {

            case 0 => 0
            case 1 => 1
            case _ => fib( i - 2 ) + fib( i - 1 )
        }
    }

    //Keep only 2's
    def filt( i : Int ) : Boolean = {

        i match {

            case 2 => true
            case _ => false
        }
    }
}
