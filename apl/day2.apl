]box on
]rows on
⎕IO ← 0

td ← 'A Y' 'B X' 'C Z'
rd←⊃⎕NGET '/home/hugo/code/advent_of_code_2022/data/day2.data.txt' 1

parse←{
    inm←(↑⍵)[;0 2]
    idxs←'ABCXYZ' ⍳ inm
    3|idxs
}

draw ← 3
win ← 6
lose ← 0
om ← 3 3 ⍴ draw lose win win draw lose lose win draw
score1←{om[↓⌽⍵]}

sh ← 1 2 3
score2 ← {sh[⍵[;1]]}

d2p1 ← {+/((score1 ⍵) + (score2 ⍵))}
d2p1 parse td
d2p1 parse rd

dm ← 3 3 ⍴ 2 0 1 0 1 2 1 2 0
 dec ← {
    w ← ⍵
    w[;1] ← dm[↓⍵]
    w
 }
d2p2 ← {d2p1 dec ⍵}
d2p2 parse td
d2p2 parse rd
