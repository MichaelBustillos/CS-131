fun <T> everyNth(l: List<T>, size : Int): List<T> {
	val newList = ArrayList<T>()
    for(i in (size-1)..(l.size - 1) step (size))
   		  newList.add(l.get(i))
		  val immutable: List<T> = newList
		  return immutable

}

fun main(args: Array<String>) {
    var l1 = mutableListOf(9, 8 ,3, 1, 7, 2, 4)
    var l2 = listOf("print", "list", "of", "objects", "to", "test", "on")
    var newL1 = everyNth(l1, 2)
    var newL2 = everyNth(l2, 3)
    l1.add(2231)

    for(element in newL1){
        println(element)
    }
    for(element in newL2){
        println(element)
    }
}