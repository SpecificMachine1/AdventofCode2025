(import (rnrs)
        (prefix (day day01) day01-)
        (test csv))
(test-start)
;;day 1 tests
(let ((data (day01-get-data "../data/day01-example1.dat")))
  (test-equal "zeros" 3 (day01-count-zeros data))
  (test-equal "zero passes" 6 (day01-count-zero-passes data)))
(test-end)
