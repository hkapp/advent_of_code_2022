]box on
]rows on
⎕IO ← 0

td ← 'vJrwpWtwJgWrhcsFMMfFFhFp' 'jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL' 'PmmdzqPrVvPwwTWBwg' 'wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn' 'ttgJtRGJQctTZtZT' 'CrZsJsPPZsGzwwsLwLmpwMDw'
rd←⊃⎕NGET 'data/day3.data.txt' 1

uniq ← {⊃(((≢⍵)÷2)↑⍵)∩(((≢⍵)÷2)↓⍵)}
prio ← {1+'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ' ⍳ ⍵}
d3p1 ← {+/prio uniq¨⍵}
d3p1 td
d3p1 rd

eg←{(1+⌊(⍳⍴⍵)÷3)⊆⍵}
badges←{(⊃¨∩/)¨⍵}
d3p2 ← {+/prio badges eg ⍵}
d3p2 td
d3p2 rd
