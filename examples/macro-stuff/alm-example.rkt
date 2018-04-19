#lang racket

(require "alm-dsl.rkt")

(alm a4. a+2 a#2 a-2 ~ l4 c c+2.)
(alm a4. a+2 a#2 a-2 ~ a-4 c c+2. ~)
(alm l8 a b c e8 d8 l16 b- b- a8 b- b- a8)
(alm a4. > v10 b+2 < p4 a#4 l8 p v32 a ~ b ~ c o5 a-2 ~ a-4 c c+2. ~)
