package samples.scala

import scala.collection.mutable._

/**
 * Created by izeye on 14. 12. 8..
 */
object SumOfTwoNumbers {

  def solve(numbers: List[Int]): Int = {
    val sortedNonZeroNumbers = numbers.filter(number => number != 0).sorted
    var zeroCount = numbers.length - sortedNonZeroNumbers.length

    val buckets = List(MutableList[Int](), MutableList[Int]())
    var bucketIndex = 0;

    var index = 0;
    while (index < sortedNonZeroNumbers.length) {
      val number = sortedNonZeroNumbers(index)
      val bucket = buckets(bucketIndex)

      if (bucket.length != 0 && zeroCount != 0) {
        bucket += 0
        zeroCount -= 1
      } else {
        bucket += number
        index += 1
      }

      // Switch buckets alternatively.
      bucketIndex ^= 1
    }

    if (buckets(0).isEmpty || buckets(1).isEmpty) {
      return -1
    }
    return buckets.map(_.mkString("").toInt).sum
  }

  def main(args: Array[String]) {
    assert(solve(List(1, 2, 4, 7, 9)) == 176);
    assert(solve(List(1, 2, 3, 1, 2, 3)) == 246);
    assert(solve(List(1, 2, 3, 4, 5, 6, 7)) == 1603);
    assert(solve(List(0, 1, 2, 3, 0, 1, 2, 3, 4)) == 11257);
    assert(solve(List(0, 0, 1)) == -1);
  }

}
