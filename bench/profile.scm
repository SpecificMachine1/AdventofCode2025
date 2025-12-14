(import (rnrs)
        (prefix (day day01) day01-)
        (prefix (day day02) day02-)
        (prefix (day day03) day03-)
        (prefix (day day04) day04-)
        (bench runner))
(let ((runs 10)
      (units micro))
  (start-run runs units)
  (run "1.1 ends on zero"
       runs
       units
       (day01-count-zeros (day01-get-data "../data/day01-input.dat")))
  (run "1.2 all zero passes"
       runs
       units
       (day01-count-zero-passes (day01-get-data "../data/day01-input.dat")))
  (run "2.1 doubles invalid"
       runs
       units
       (fold-left + 0 (day02-invalid-ids-numeric (day02-get-data "../data/day02-input.dat"))))
  (run "2.2 all n-tuples invalid"
       runs
       units
       (fold-left + 0 (day02-invalid-all-reps (day02-get-data "../data/day02-input.dat"))))
  (run "3.1 normal total joltage"
       runs
       units
       (day03-total-output (day03-get-data "../data/day03-input.dat")))
  (run "3.2 override joltage"
       runs
       units
       (day03-total-overide-output (day03-get-data "../data/day03-input.dat")))
  (run "4.1 first pass rolls"
       runs
       units
       (day04-count-accessible (day04-get-data "../data/day04-input.dat") 4))
  (run "4.2 all passes rolls"
       runs
       units
       (day04-count-all-accessible (day04-get-data "../data/day04-input.dat") 4)))
