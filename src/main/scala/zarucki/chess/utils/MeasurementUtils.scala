package zarucki.chess.utils

import java.util.concurrent.TimeUnit

object MeasurementUtils {
	def measureTimeInMs[T](block: => T): (T, Long) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
		val timeInSeconds = TimeUnit.NANOSECONDS.toMillis(end - start)
		(result, timeInSeconds)
	}
}
