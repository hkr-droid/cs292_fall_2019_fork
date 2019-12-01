
aik@kazimir:~/PersonalProjects/Fall2019/PLLab/cs292_fall_2019/examples$ racket minilang_small.rkt
#<state: (#(struct:: #(struct:par #(struct:: #(struct::= a 2) #(struct:止)) #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止))) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ()>
-->
#<state: (#(struct:par #(struct:: #(struct::= a 2) #(struct:止)) #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ()>
-->
#<state: (#(struct:: #(struct::= a 2) #(struct:止))) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ())>
-->
#<state: (#(struct::= a 2) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ())>
-->
#<state: (2 := #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 (a . #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ()))>
-->
#<state: (:= #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 (2 a . #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ()))>
-->
#<state: (#(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash((a . 2)))) t1 #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ())>
-->
#<state: () #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash((a . 2)))) t1 #(struct:EvalArg (#(struct:par hole227 #(struct:: #(struct::= a 1) #(struct:止))) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash())) t1 ())>
-->
#<state: #(struct:: #(struct::= a 1) #(struct:止)) #hash((global . #hash((A . 5) (B . 50))) (t1 . #hash((a . 2)))) t1 ()>
-->
match: no matching clause for (state (: (:= 'a 1) (止)) '#hash((global . #hash((A . 5) (B . 50))) (t1 . #hash((a . 2)))) 't1 '())
  location...:
   minilang_small.rkt:54:2
  context...:
   /usr/share/racket/collects/racket/match/runtime.rkt:24:0: match:error
   /home/aik/PersonalProjects/Fall2019/PLLab/cs292_fall_2019/examples/minilang_small.rkt:134:0: run
   "/home/aik/PersonalProjects/Fall2019/PLLab/cs292_fall_2019/examples/minilang_small.rkt": [running body]
   temp37_0
   for-loop
   run-module-instance!125
   perform-require!78
aik@kazimir:~/PersonalProjects/Fall2019/PLLab/cs292_fall_2019/examples$ 
