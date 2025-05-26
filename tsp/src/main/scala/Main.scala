object Main {
    val INF: Int = Int.MaxValue / 2

    def solve(M: Array[Array[Int]]): (Int, List[Int], Double) = {
        val startTime = System.nanoTime()

        val N = M.length
        val dp = Array.fill(1 << N, N)(INF)
        val pre = Array.fill(1 << N, N)(-1)

        dp(1)(0) = 0

        for (mask <- 0 until (1 << N)) {
            for (i <- 0 until N if (mask & (1 << i)) != 0) {
                val oldMask = mask ^ (1 << i)
                for (j <- 0 until N if (mask & (1 << j)) != 0) {
                    val cost = dp(oldMask)(j) + M(j)(i)
                    if (cost < dp(mask)(i)) {
                        dp(mask)(i) = cost
                        pre(mask)(i) = j
                    }
                }
            }
        }

        var minCost = INF
        var last = -1
        for (i <- 0 until N) {
            val cost = dp((1 << N) - 1)(i) + M(i)(0)
            if (cost < minCost) {
                minCost = cost
                last = i
            }
        }

        var mask = (1 << N) - 1
        val path = scala.collection.mutable.ArrayBuffer[Int]()
        while (mask != 1) {
            path += last
            val to = pre(mask)(last)
            mask ^= (1 << last)
            last = to
        }
        path += 0

        val finalPath = path.reverse.toList :+ 0

        val endTime = System.nanoTime()
        val elapsedTime = (endTime - startTime) / 1e6

        (minCost, finalPath, elapsedTime)
    }
}