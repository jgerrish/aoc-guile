(define-module (aoc y2021 day14 polymer-instructions-parser)
  #:use-module (ice-9 peg)
  ;; Export a bunch of the PEG patterns for testing
  ;; Normally you wouldn't want to do that, I wanted to show
  ;; how to test the PEGs along with the code
  :export (polymer-instructions line template template-line pair element rule rule-line
				test-data my-peg-tree))

;; NL* captures the empty-line so it is not needed
;; There's a bug with the test data and and the input data
;; The test data uses > in rules, the input data uses -> in rules.
(define-peg-string-patterns
  "polymer-instructions <- line* !.
line <- rule-line / template-line
template <-- [A-Z]+
template-line <- template !SPACE NL*
pair <-- [A-Z][A-Z]
element <-- [A-Z]
rule <-- pair cSP HYPHEN GT cSP element
rule-line <- rule NL*
HYPHEN < '-'
GT < '>'
SPACE < ' '
cSP < [ \t]*
NL < '\n'")


;; (define test-data "CH > B\nHH > N\n")

(define test-data "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C\n")


(define *my-peg-tree* (peg:tree (match-pattern polymer-instructions test-data)))
(define my-peg-tree (peg:tree (match-pattern polymer-instructions test-data)))

