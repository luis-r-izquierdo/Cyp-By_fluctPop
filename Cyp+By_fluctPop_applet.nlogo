;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Cyp+By_varPop_applet
;; Cyp+By_varPop_applet is a model designed to analyse
;; the effect of conditional dissociation
;; in the evolutionary emergence of cooperation.
;; Copyright (C) 2009 Segismundo S. Izquierdo & Luis R. Izquierdo
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Contact information:
;; Segismundo S. Izquierdo
;;   University of Valladolid, Spain.
;;   e-mail: segis@eis.uva.es


;;;;;;;;;;;;;;;;;
;;; Variables ;;;
;;;;;;;;;;;;;;;;;

globals [
  current-num-players     ;; number of players
  paired-turtles          ;; agenteset containing the paired turtles

  numCC numCD numDD       ;; these variables store how many times each of the outcomes has been observed in one match
  num-outcomes

  strategy-frequencies
  strategy-names

  C-if-C-players  D-if-C-players
  C-if-D-players  D-if-D-players
  action-first-C-players

  cum-num-cooperative-regime
  cum-num-defective-regime
  last-regime
  num-transitions-cc-to-dd
  num-transitions-dd-to-cc
]

turtles-own [
  action-first       ;; the action is either 0 (C) or 1 (D)
  decision-if-C      ;; The decision is either 0 (Stay and next-action = Cooperate) or 1 (Stay and next-action = Defect) or 2 (Leave).
  decision-if-D
  next-action        ;; If the partnership is broken, next-action = action-first.
  active-action
  break-decision     ;; 0 if the player does not split up after the stage outcome. 1 otherwise.
  mate
  payoff
  strategy-number
  new-partnership?
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; the following procedure is called when the model is first loaded
to startup
  ;; (for this model to work with NetLogo's new plotting features,
  ;; __clear-all-and-reset-ticks should be replaced with clear-all at
  ;; the beginning of your setup procedure and reset-ticks at the end
  ;; of the procedure.)
  __clear-all-and-reset-ticks
  set strategy-names ["C-C-C" "C-C-D" "C-C-L" "C-D-C" "C-D-D" "C-D-L" "C-L-C" "C-L-D" "C-L-L" "D-C-C" "D-C-D" "D-C-L" "D-D-C" "D-D-D" "D-D-L" "D-L-C" "D-L-D" "D-L-L"]
  setup-players
end

to setup-players
  crt initial-num-players [
    set action-first 0
    set decision-if-C 0  set decision-if-D 0
    set strategy-number 0
    set mate nobody
    set payoff 0
    set hidden? true
  ]

  if-else initial-strategy = "random"
     [ ;; random distribution of initial strategies for the whole population
       ask turtles [
         set action-first random 2
         set decision-if-C random 3  set decision-if-D random 3
         set strategy-number (genome-to-decimal genome)    ;; genome is a reporter that reports the set of genes
         set next-action action-first
         ;; strategies are numbered from 0 (0 0 0) to 17 (1 2 2)
       ]
     ]
     [
       ask turtles [
         set strategy-number (position initial-strategy strategy-names)
         setup-newborns
       ]
     ]
  no-display
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run-time procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  set current-num-players (count turtles)
  if current-num-players < 2 [stop]

  tick

  make-couples
  play

  gather-data
  split-up
  update-graphs

  ;; new generation
  kill-and-breed

end

to make-couples
  ask turtles [set new-partnership? false]

  let singles (turtles with [mate = nobody])
  ask singles [set new-partnership? true]

  let num-to-pair (2 * floor ( count singles / 2))   ;; even number of individuals to pair
  let to-pair-turtles n-of num-to-pair singles

  ask to-pair-turtles [
      if (mate = nobody) [
        set mate one-of to-pair-turtles with [mate = nobody and self != myself]
        ask mate [set mate myself]
      ]
    ]
end

to play
  ask turtles [set active-action next-action]
  set paired-turtles turtles with [mate != nobody]
  ask paired-turtles [
    set payoff payoff-for active-action ([active-action] of mate)
    set next-action ifelse-value ([active-action] of mate = 0)  [decision-if-C] [decision-if-D]
        ;; next-action here can take the value 2 (leave), but will take value action-first later in that case.
    set break-decision floor (next-action / 2)
    ]
end


to split-up
  ask turtles[
    if (mate != nobody) [
      if (break-decision = 1)
         ;; if programmed to split up after the outcome.
        [ask mate [set next-action action-first  set mate nobody]
         set next-action action-first  set mate nobody ]
      ]
    ]
end

to kill-and-breed

  let num-newborn 0
  ask turtles [if (random-float 1.0 < (1.0 / expected-life)) [set num-newborn (num-newborn + 1)]]  ;;number of births

  let list-fitness n-values 18 [sum [payoff] of turtles with [strategy-number = ?]]
    ;; calculation of list of cumulative payoffs for the strategies
  if (sum list-fitness = 0) [set list-fitness n-values (18) [1]]
    ;; Applies when all players have zero fitness
  let cum-fitness [0]
    ;; cum-fitness last value is 0 and is 19 items long
  foreach list-fitness [set cum-fitness fput (? + first cum-fitness) cum-fitness]

  ;; KILL
  ask turtles [
    if (random-float 1.0 < (1.0 / expected-life)) [ ;; prob-deathLife = 1 / exp-life
      if (mate != nobody)  [ ask mate [set mate nobody] ]
      die
    ]
  ]

  ;;BREED
  crt num-newborn [
    if-else (random-float 1.0 < prob-mutation)
      [set strategy-number random 18 ]
      [
        set strategy-number 17
        let tmp random-float first cum-fitness
           ;; select the new strategy with probability proportional to fitness
        foreach butfirst cum-fitness [ if ( tmp < ?) [set strategy-number (strategy-number - 1)] ]
      ]
    setup-newborns
  ]

end

to setup-newborns    ;; This procedure is only used by newborns
  set mate nobody
  ;; first, update strategy variables from strategy-number
  let remain strategy-number
  set decision-if-D  (remain mod 3)  set remain int (remain / 3)
  set decision-if-C  (remain mod 3)  set remain int (remain / 3)
  set action-first (remain mod 2)

  ;; and then set next-action
  set next-action action-first
  set hidden? true
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Statistics    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to gather-data
  set numCC (count paired-turtles with [active-action = 0 and [active-action] of mate = 0]) / 2
  set numDD (count paired-turtles with [active-action = 1 and [active-action] of mate = 1]) / 2
  set numCD (count paired-turtles with [active-action = 0 and [active-action] of mate = 1])
  set num-outcomes (numCC + numDD + numCD)

  set strategy-frequencies n-values 18 [count turtles with [strategy-number = ?]]

  set C-if-C-players count turtles with [decision-if-C = 0]
  set D-if-C-players count turtles with [decision-if-C = 1]

  set C-if-D-players count turtles with [decision-if-D = 0]
  set D-if-D-players count turtles with [decision-if-D = 1]

  set action-first-C-players count turtles with [action-first = 0]

  update-regimes
end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;    Reporters     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to-report payoff-for [my-action her-action]
  ;; my-action is 0 if C, 1 if D
  report
    (1 - my-action) * (1 - her-action) * CC-payoff +
    (1 - my-action) * her-action * CD-payoff +
    my-action * (1 - her-action) * DC-payoff +
    my-action * her-action * DD-payoff
end

to-report genome
  report (list action-first decision-if-C decision-if-D)
end

to-report genome-to-decimal [genome-list]
  report reduce [3 * ?1 + ?2] genome-list
end

;; Regime-related reporters ;;

to-report cooperative-regime
  let %-CC (numCC / num-outcomes)
  report (cc-lower-limit <= %-CC and %-CC <= cc-upper-limit)
end

to-report defective-regime
  let %-DD (numDD / num-outcomes)
  report (dd-lower-limit <= %-DD and %-DD <= dd-upper-limit)
end

to update-regimes

  if cooperative-regime [
    set cum-num-cooperative-regime (cum-num-cooperative-regime + 1)
    if last-regime = 2 [
      set num-transitions-dd-to-cc (num-transitions-dd-to-cc + 1)
    ]
    set last-regime 1
  ]

  if defective-regime [
    set cum-num-defective-regime (cum-num-defective-regime + 1)
    if last-regime = 1 [
      set num-transitions-cc-to-dd (num-transitions-cc-to-dd + 1)
    ]
    set last-regime 2
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;
;;;      Plots       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to update-graphs
  ;; all graphs refer to the situation before the new breed comes in.

  set-current-plot "Outcome Frequencies"
    set-current-plot-pen "DD"     plot 1
    set-current-plot-pen "CD"     plot 1 - numDD / num-outcomes
    set-current-plot-pen "CC"     plot numCC / num-outcomes

  let single-players (count turtles with [mate = nobody])

  set-current-plot "Behaviour after C"
    set-current-plot-pen "Leave-D"    plot 1
    set-current-plot-pen "Leave-C"    plot 1 - ((count turtles with [decision-if-C = 2 and action-first = 1]) / current-num-players)
    set-current-plot-pen "Stay-D"     plot (C-if-C-players / current-num-players) + (D-if-C-players / current-num-players)
    set-current-plot-pen "Stay-C"     plot (C-if-C-players / current-num-players)

  set-current-plot "Behaviour after D"
    set-current-plot-pen "Leave-D"    plot 1
    set-current-plot-pen "Leave-C"    plot 1 - ((count turtles with [decision-if-D = 2 and action-first = 1]) / current-num-players)
    set-current-plot-pen "Stay-D"     plot (C-if-D-players / current-num-players) + (D-if-D-players / current-num-players)
    set-current-plot-pen "Stay-C"     plot (C-if-D-players / current-num-players)

  set-current-plot "Behaviour new partner"
    set-current-plot-pen "D"     plot 1
    set-current-plot-pen "C"     plot (action-first-C-players / current-num-players)

  set-current-plot "% Individuals separated"
    plot 100 * (single-players / current-num-players)

  set-current-plot "Avg Payoff"
    set-current-plot-pen "all"
    plotxy ticks mean [payoff] of turtles
    if (any? turtles with [new-partnership?]) [
      set-current-plot-pen "new couples"
      plotxy ticks mean [payoff] of turtles with [new-partnership?]
    ]

  set-current-plot "Strategy Distribution"
    let barra sum strategy-frequencies
    foreach (n-values 18 [?]) [
      set-current-plot-pen item ? strategy-names
      plot barra
      set barra barra - (item ? strategy-frequencies)
    ]
end

@#$#@#$#@
GRAPHICS-WINDOW
304
289
549
488
1
1
56.0
1
10
1
1
1
0
1
1
1
-1
1
-1
1
1
1
1
ticks
30.0

SLIDER
11
121
225
154
initial-num-players
initial-num-players
2
5000
1000
2
1
NIL
HORIZONTAL

SLIDER
12
34
132
67
CC-payoff
CC-payoff
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
139
35
256
68
CD-payoff
CD-payoff
0
10
0
1
1
NIL
HORIZONTAL

SLIDER
12
70
132
103
DC-payoff
DC-payoff
0
10
4
1
1
NIL
HORIZONTAL

SLIDER
139
71
256
104
DD-payoff
DD-payoff
0
10
1
1
1
NIL
HORIZONTAL

BUTTON
14
303
113
336
Setup
startup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
14
380
113
413
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
266
10
632
212
Outcome Frequencies
NIL
NIL
0.0
1.0
0.0
1.0
true
true
"" ""
PENS
"DD" 1.0 1 -2674135 true "" ""
"CD" 1.0 1 -4539718 true "" ""
"CC" 1.0 1 -13345367 true "" ""

MONITOR
267
217
362
262
number of CCs
numCC
3
1
11

MONITOR
537
217
632
262
number of DDs
numDD
3
1
11

MONITOR
405
217
500
262
number of CDs
numCD
3
1
11

PLOT
861
186
1079
397
Avg Payoff
NIL
NIL
0.0
10.0
0.0
0.0
true
true
"" ""
PENS
"all" 1.0 0 -16777216 true "" ""
"new couples" 1.0 0 -955883 true "" ""

PLOT
11
430
260
634
% Individuals separated
NIL
% Separated
0.0
10.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
11
159
225
192
expected-life
expected-life
1
100
25
1
1
NIL
HORIZONTAL

BUTTON
13
341
113
374
Go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
11
196
224
229
prob-mutation
prob-mutation
0
1
0.05
0.01
1
NIL
HORIZONTAL

PLOT
642
404
857
635
Behaviour after C
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Leave-D" 1.0 1 -1184463 true "" ""
"Leave-C" 1.0 1 -10899396 true "" ""
"Stay-D" 1.0 1 -2674135 true "" ""
"Stay-C" 1.0 1 -13345367 true "" ""

CHOOSER
11
241
158
286
initial-strategy
initial-strategy
"random" "C-C-C" "C-C-D" "C-C-L" "C-D-C" "C-D-D" "C-D-L" "C-L-C" "C-L-D" "C-L-L" "D-C-C" "D-C-D" "D-C-L" "D-D-C" "D-D-D" "D-D-L" "D-L-C" "D-L-D" "D-L-L"
0

MONITOR
642
109
742
162
cc-regime
cum-num-cooperative-regime
17
1
13

MONITOR
976
111
1080
164
dd-regime
cum-num-defective-regime
17
1
13

MONITOR
748
110
845
163
cc to dd ->
num-transitions-cc-to-dd
17
1
13

MONITOR
159
364
257
417
time-step
ticks
17
1
13

SLIDER
641
36
788
69
cc-lower-limit
cc-lower-limit
0
1
0.61
0.01
1
NIL
HORIZONTAL

SLIDER
641
71
788
104
cc-upper-limit
cc-upper-limit
0
1
0.81
0.01
1
NIL
HORIZONTAL

SLIDER
937
37
1080
70
dd-lower-limit
dd-lower-limit
0
1
0.8
0.01
1
NIL
HORIZONTAL

SLIDER
937
73
1080
106
dd-upper-limit
dd-upper-limit
0
1
1
0.01
1
NIL
HORIZONTAL

MONITOR
873
111
971
164
<- dd to cc
num-transitions-dd-to-cc
17
1
13

PLOT
862
404
1078
635
Behaviour after D
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"Leave-D" 1.0 1 -1184463 true "" ""
"Leave-C" 1.0 1 -10899396 true "" ""
"Stay-D" 1.0 1 -2674135 true "" ""
"Stay-C" 1.0 1 -13345367 true "" ""

PLOT
641
186
857
398
Behaviour new partner
NIL
NIL
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"D" 1.0 1 -2674135 true "" ""
"C" 1.0 1 -13345367 true "" ""

PLOT
267
267
633
635
Strategy Distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"C-C-C" 1.0 1 -9276814 true "" ""
"C-C-D" 1.0 1 -14730904 true "" ""
"C-C-L" 1.0 1 -13345367 true "" ""
"C-D-C" 1.0 1 -5298144 true "" ""
"C-D-D" 1.0 1 -1184463 true "" ""
"C-D-L" 1.0 1 -8431303 true "" ""
"C-L-C" 1.0 1 -7171555 true "" ""
"C-L-D" 1.0 1 -723837 true "" ""
"C-L-L" 1.0 1 -526419 true "" ""
"D-C-C" 1.0 1 -12087248 true "" ""
"D-C-D" 1.0 1 -8330359 true "" ""
"D-C-L" 1.0 1 -14835848 true "" ""
"D-D-C" 1.0 1 -817084 true "" ""
"D-D-D" 1.0 1 -2674135 true "" ""
"D-D-L" 1.0 1 -955883 true "" ""
"D-L-C" 1.0 1 -3425830 true "" ""
"D-L-D" 1.0 1 -4699768 true "" ""
"D-L-L" 1.0 1 -2064490 true "" ""

TEXTBOX
14
10
74
28
Payoffs
13
0.0
1

TEXTBOX
803
10
925
28
Definition of regimes
13
0.0
1

TEXTBOX
794
61
856
79
cc regime
13
0.0
1

TEXTBOX
874
61
942
79
dd regime
13
0.0
1

MONITOR
158
303
258
356
num-players
current-num-players
17
1
13

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

link
true
0
Line -7500403 true 150 0 150 300

link direction
true
0
Line -7500403 true 150 150 30 225
Line -7500403 true 150 150 270 225

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
