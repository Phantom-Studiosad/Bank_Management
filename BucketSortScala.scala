object Evaluation{

def insertionSort(a:Array[Double]):Unit = {
	for(i <- 1 until a.length) {
		var j = i-1
		var tmp = a(i)
		while(j>=0 && tmp < a(j)) {
			a(j+1) = a(j)
			j -= 1
		}
		a(j+1) = tmp
	}
	for (i <- 0 until a.length) {
		print(a(i) + " ")
	}
	println()
}


def bucketSort(a:Array[Double]):Unit = {
	println("Before Sorting")
	for (i <- 0 until a.length) {
		print(a(i) + " ")
	}
	println()
	println("After Sorting")
	val min = a.min
	val max = a.max
	val buckets = Array.fill(a.length)(List[Double]())
	for(x <- a) {
		val b = ((x-min)*(buckets.length-1)/(max-min)).toInt
		buckets(b) ::= x
	}
	var i = 0
	for(bucket <- buckets; x <- bucket) {
		a(i) = x
		i += 1
	}	
	insertionSort(a)
}



def timeFunc(sortFunc: Array[Double] => Unit, a:Array[Double]):Double = {
	val copy = a.map(x => x)
	val start = System.nanoTime()
	sortFunc(copy)
	val end = System.nanoTime()
	(end-start)/1e9
}

def main(args: Array[String])= {
    val arr = Array.fill(20)(math.random)
    println("Bucket sort "+timeFunc(bucketSort, arr))
   }
}