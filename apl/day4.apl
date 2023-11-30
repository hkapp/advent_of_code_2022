]box on
]rows on
⎕IO ← 1

td ← '2-4,6-8' '2-3,4-5' '5-7,7-9' '2-8,3-7' '6-6,4-6' '2-6,4-8'
rd←⊃⎕NGET 'data/day4.data.txt' 1

parseLine ← {⍎¨(~⍵∊'-,')⊆⍵}
parse ← {parseLine¨⍵}
range ← {⍺≤⍳⍵}
allRanges ← {↑↑{(⍵[1] range ⍵[2]) (⍵[3] range ⍵[4])}¨⍵}
eitherSubsumes ← {∨/∧/⍵=(⍴⍵)⍴2⌿∧⌿[2]⍵}
d4p1 ← {+/ eitherSubsumes allRanges parse ⍵ }

d4p1 td
d4p1 rd

anyOverlap ← {∨/∧⌿[2]⍵}
d4p2 ← {+/ anyOverlap allRanges parse ⍵ }

d4p2 td
d4p2 rd
