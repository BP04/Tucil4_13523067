object Test {
    def printMatrix(matrix: Array[Array[Int]]): Unit = {
        matrix.foreach(row => println(row.mkString(", ")))
    }

    def main(args: Array[String]): Unit = {
        val testCases = List(
            (
            "Test 1",
                Array(
                    Array(0, 10, 15, 20),
                    Array(10, 0, 35, 25),
                    Array(15, 35, 0, 30),
                    Array(20, 25, 30, 0)
                )
            ),
            (
                "Test 2",
                Array(
                    Array(0, 1, 1, 1, 1),
                    Array(1, 0, 1, 1, 1),
                    Array(1, 1, 0, 1, 1),
                    Array(1, 1, 1, 0, 1),
                    Array(1, 1, 1, 1, 0)
                )
            ),
            (
                "Test 3",
                Array(
                    Array(0, 345, 872, 123, 567, 789, 234, 901),
                    Array(345, 0, 456, 678, 234, 890, 123, 567),
                    Array(872, 456, 0, 345, 678, 234, 890, 123),
                    Array(123, 678, 345, 0, 456, 789, 234, 901),
                    Array(567, 234, 678, 456, 0, 345, 678, 234),
                    Array(789, 890, 234, 789, 345, 0, 456, 678),
                    Array(234, 123, 890, 234, 678, 456, 0, 345),
                    Array(901, 567, 123, 901, 234, 678, 345, 0)
                )
            ),
            (
                "Test 4",
                Array(
                    Array(0, 234, 567, 890, 123, 456, 789, 321, 654, 987),
                    Array(234, 0, 345, 678, 901, 234, 567, 890, 123, 456),
                    Array(567, 345, 0, 456, 789, 123, 890, 234, 567, 901),
                    Array(890, 678, 456, 0, 345, 678, 123, 456, 789, 234),
                    Array(123, 901, 789, 345, 0, 567, 890, 123, 456, 678),
                    Array(456, 234, 123, 678, 567, 0, 345, 789, 234, 890),
                    Array(789, 567, 890, 123, 890, 345, 0, 567, 901, 345),
                    Array(321, 890, 234, 456, 123, 789, 567, 0, 678, 123),
                    Array(654, 123, 567, 789, 456, 234, 901, 678, 0, 345),
                    Array(987, 456, 901, 234, 678, 890, 345, 123, 345, 0)
                )
            ),
            (
                "Test 5",
                Array(
                    Array(0, 789, 234, 567, 890, 123, 456, 789, 321, 654, 987, 123),
                    Array(789, 0, 345, 678, 234, 901, 567, 890, 123, 456, 789, 234),
                    Array(234, 345, 0, 456, 789, 123, 890, 234, 567, 901, 345, 678),
                    Array(567, 678, 456, 0, 345, 678, 123, 456, 789, 234, 890, 567),
                    Array(890, 234, 789, 345, 0, 567, 890, 123, 456, 678, 234, 901),
                    Array(123, 901, 123, 678, 567, 0, 345, 789, 234, 890, 567, 345),
                    Array(456, 567, 890, 123, 890, 345, 0, 567, 901, 345, 678, 890),
                    Array(789, 890, 234, 456, 123, 789, 567, 0, 678, 123, 456, 789),
                    Array(321, 123, 567, 789, 456, 234, 901, 678, 0, 345, 789, 234),
                    Array(654, 456, 901, 234, 678, 890, 345, 123, 345, 0, 567, 890),
                    Array(987, 789, 345, 890, 234, 567, 678, 456, 789, 567, 0, 123),
                    Array(123, 234, 678, 567, 901, 345, 890, 789, 234, 890, 123, 0)
                )
            ),
            (
            "Test 6",
                Array(
                    Array(0, 921, 372, 874, 628, 196, 538, 884, 347, 779, 190, 935, 421, 578, 436, 233, 337, 601, 498, 255),
                    Array(921, 0, 188, 337, 804, 972, 421, 628, 812, 477, 590, 786, 608, 483, 667, 915, 753, 192, 696, 800),
                    Array(372, 188, 0, 608, 599, 723, 207, 914, 681, 898, 418, 834, 912, 700, 742, 385, 471, 486, 584, 355),
                    Array(874, 337, 608, 0, 939, 667, 544, 386, 597, 932, 499, 448, 286, 642, 617, 783, 494, 987, 706, 314),
                    Array(628, 804, 599, 939, 0, 211, 476, 554, 258, 812, 891, 746, 596, 857, 177, 339, 292, 753, 483, 918),
                    Array(196, 972, 723, 667, 211, 0, 488, 798, 152, 726, 873, 621, 330, 879, 117, 285, 153, 600, 431, 703),
                    Array(538, 421, 207, 544, 476, 488, 0, 760, 709, 833, 495, 771, 888, 697, 592, 300, 453, 619, 672, 622),
                    Array(884, 628, 914, 386, 554, 798, 760, 0, 968, 487, 727, 357, 329, 750, 806, 964, 765, 967, 313, 401),
                    Array(347, 812, 681, 597, 258, 152, 709, 968, 0, 901, 594, 862, 515, 952, 174, 262, 181, 498, 278, 674),
                    Array(779, 477, 898, 932, 812, 726, 833, 487, 901, 0, 910, 255, 368, 543, 837, 947, 841, 693, 339, 316),
                    Array(190, 590, 418, 499, 891, 873, 495, 727, 594, 910, 0, 943, 460, 781, 710, 389, 581, 597, 702, 237),
                    Array(935, 786, 834, 448, 746, 621, 771, 357, 862, 255, 943, 0, 336, 513, 776, 842, 775, 926, 202, 339),
                    Array(421, 608, 912, 286, 596, 330, 888, 329, 515, 368, 460, 336, 0, 742, 547, 431, 190, 790, 487, 305),
                    Array(578, 483, 700, 642, 857, 879, 697, 750, 952, 543, 781, 513, 742, 0, 664, 746, 802, 832, 362, 392),
                    Array(436, 667, 742, 617, 177, 117, 592, 806, 174, 837, 710, 776, 547, 664, 0, 401, 170, 553, 473, 679),
                    Array(233, 915, 385, 783, 339, 285, 300, 964, 262, 947, 389, 842, 431, 746, 401, 0, 348, 671, 510, 715),
                    Array(337, 753, 471, 494, 292, 153, 453, 765, 181, 841, 581, 775, 190, 802, 170, 348, 0, 528, 455, 631),
                    Array(601, 192, 486, 987, 753, 600, 619, 967, 498, 693, 597, 926, 790, 832, 553, 671, 528, 0, 707, 811),
                    Array(498, 696, 584, 706, 483, 431, 672, 313, 278, 339, 702, 202, 487, 362, 473, 510, 455, 707, 0, 241),
                    Array(255, 800, 355, 314, 918, 703, 622, 401, 674, 316, 237, 339, 305, 392, 679, 715, 631, 811, 241, 0)
                )
            )
        )

        for ((name, matrix) <- testCases) {
            println(s"$name")
            printMatrix(matrix)
            val (cost, path, time) = Main.solve(matrix)
            println(s"Minimum Cost: $cost")
            println(s"Path: ${path.mkString(" -> ")}")
            println(f"Time taken: $time%.4f ms")
            println("-" * 40)
        }
    }
}
