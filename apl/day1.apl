readLines ← {⊃⎕NGET ⍵ 1}
raw ← readLines 'data/day1.data.txt'
td ← (1000 2000 3000) 4000 (5000 6000) (7000 8000 9000) 10000

split←{(0≠≢¨⍵)⊆⍵}
parseInts←{(⍎¨⊣)¨⍵}
preprocess←{parseInts split ⍵}

d1p1←{⌈/+/↑⍵}
d1p1 td
d1p1 preprocess raw

calories ← {+/↑⍵}
d1p2 ← {3↑(calories ⍵)[⍒calories ⍵]}
d1p2 preprocess raw
