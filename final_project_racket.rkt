#lang pl

#| The rules of grammar 
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> }
        |  { intersect <SOL> <SOL> } ;;Cutting, gets groups and displays the members common to both.
        |  { union <SOL> <SOL> } ;; Union, gets groups and represents all members in both groups (once)
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment
<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
    [Set  SET]
    [Smult Number SOL] ;; Scalar multiplication, a number multiplied in a group (SOL), according to the rules of grammar
    [Inter SOL SOL] ;; Cutting, gets groups and displays the members common to both.
    [Union SOL SOL] ;; Union, gets groups and represents all members in both groups (once)
    [Id    Symbol]
;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
    [Fun   Symbol Symbol SOL]
    [CallS SOL SOL SOL]
    [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs

;;ismember takes a number and a list of numbers and returns t# if the number is a member of the list and #f otherwise.
  (: ismember? : Number SET  -> Boolean)
  (define (ismember? n l)
    (cond [(null? l) #f]
          [(= n (first l)) #t]
          [else (ismember? n (rest l))]))

;;Tests
  (test (not (ismember? 1 '(3 4 5))))
  (test (not (ismember? 1 '( 3 2 3 5 6))))
  (test (ismember? 1 '(3 4 5 1 3 4)))
  (test (ismember? 1 '(1)))


#|
remove-duplicates takes a list of numbers and returns the partial list to it without repetitions.
The recursive function, if the list is empty or if there is one member then returns the list
otherwise if the first member is in the rest of the list then it is skipped and recursively done for the rest of the list
otherwise (the first member is not in the rest of the list there are no duplicates of the first member) then the first member is saved and the rest are recursively performed the organs.
|#
  (: remove-duplicates : SET  -> SET)
  (define (remove-duplicates l)
    (cond [(or (null? l) (null? (rest l))) l]
          [(ismember? (first l) (rest l)) (remove-duplicates (rest l))] ;;If the first member is also in the rest of the list (there are duplicates) then skip it and recursively go through the rest of the list
          [else (cons (first l) (remove-duplicates (rest l)))]))


;;Tests
  (test (remove-duplicates '(3 4 5)) => '(3 4 5))
  (test (remove-duplicates '(3 2 3 5 6)) => '(2 3 5 6))
  (test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
  (test (remove-duplicates '(1 2 2 3 4 4 5)) => '(1 2 3 4 5))
  (test (remove-duplicates '(1)) => '(1))
  (test (remove-duplicates '(1 1 1 1)) => '(1)) 
  (test (remove-duplicates '()) => '()) 
  (test (remove-duplicates '(1 2 2 3 -4 4 5)) => '(1 2 3 -4 4 5))
  (test (remove-duplicates '(5 4 3 2 1 0)) => '(5 4 3 2 1 0))


#|
create-sorted-set takes a list of numbers and returns the partial list to it without repetitions - sorted according to the relation <.
Sorts the list by a built-in function of the language 'sort'
and then sends the sorted list to remove-duplicates which removes all duplicates of the members and returns the list
|#
  (: create-sorted-set : SET -> SET)
  (define (create-sorted-set l)
    (remove-duplicates (sort l <))) ;; Sorts the list by a built-in function of the language 'sort' and then sends the sorted list to remove-duplicates which removes all duplicates of the members and returns the list

;;Tests
  (test (create-sorted-set '(3 4 5)) => '(3 4 5))
  (test (create-sorted-set '(3 2 3 5 6)) => '(2 3 5 6))
  (test (create-sorted-set '(3 4 5 1 3 4)) => '(1 3 4 5))
  (test (create-sorted-set '(1 2 2 3 4 4 5)) => '(1 2 3 4 5))
  (test (create-sorted-set '(1)) => '(1))
  (test (create-sorted-set '(1 1 1 1)) => '(1)) 
  (test (create-sorted-set '()) => '()) 
  (test (create-sorted-set '(1 2 2 3 -4 4 5)) => '(-4 1 2 3 4 5))
  (test (create-sorted-set '(5 4 3 2 1 0)) => '(0 1 2 3 4 5))

#|
set-union takes two lists of numbers and returns the list of all numbers that belong to at least one of the two - sorted and without repetitions.
set-union receives 2 lists and uses 'append' (built into the language) to merge the 2 lists into one and
sends the combined list to create-sorted-set which returns a list without duplicates and sorted.
|#
  (: set-union : SET SET -> SET)
  (define (set-union A B)
    (create-sorted-set (append A B))) ;; Using 'append' (built into the language) to combine the 2 lists into one and sends the combined list to create-sorted-set which returns a list without duplicates and sorted.


; Test
  (test (set-union '(1 2 3) '(2 3 4)) => '(1 2 3 4))
  (test (set-union '(1 2 3) '(4 5 6)) => '(1 2 3 4 5 6))
  (test (set-union '(1 3 2) '()) => '(1 2 3))
  (test (set-union '() '(6 5 4)) => '(4 5 6))
  (test (set-union '(3 2 1) '(2 1 3)) => '(1 2 3))
  (test (set-union '(3 2 1) '(2 -1 3)) => '(-1 1 2 3))
  (test (set-union '() '()) => '())
  (test (set-union '(3 2 1) '(2 1 3 4)) => '(1 2 3 4))


#|
set-intersection takes two lists of numbers and returns the list of all belonging to each of the two - sorted and without repetitions.
mem-filter A helper function that checks if a number is an element in list A.
Using a 'filter' that sends any number from B and checks if it is also in A and then returns all the members that are in both B and A.
This list is sent to create-sorted-set to return a list without duplicates and sorted.
|#
  (: set-intersection : SET SET -> SET)
  (define (set-intersection A B)
    (: mem-filter : Number -> Boolean)
    (define (mem-filter n)
      (ismember? n A))
    (create-sorted-set (filter mem-filter B))) ;; filter takes only the members that by using the helper function mem-filter returned that those numbers in B are also in A and then sends to create-sorted-set to return a list without duplicates and sorted.


;; Test
(test (set-intersection '(1 2 3) '(3 4 5)) =>'(3))
(test (set-intersection '(1 2 3) '(4 5 6)) =>'())
(test (set-intersection '() '(1 2 3))=> '())
(test (set-intersection '() '())=> '())
(test (set-intersection '(1 3 2) '(3 2 1)) =>'(1 2 3))
(test (set-intersection '(3 1 2) '(2 4 3))=> '(2 3))
(test (set-intersection '(1 2 2 3 3) '(3 3 4 4 5 5)) =>'(3))
(test (set-intersection '(1) '(1 2 3))=> '(1))



;; ---------------------------------------------------------
;; Parser
  (: parse-sexpr : Sexpr -> SOL)
  ;; to convert s-expressions into SOLs
  (define (parse-sexpr sexpr)
    (match sexpr
      [(list (number: ns) ...) (Set (remove-duplicates (sort ns <)) )] ;; sort and remove-duplicates
      [(symbol: name) (Id name)] ;; convert symbol to Id
      [(cons 'with more)
       (match sexpr
         [(list 'with (list (symbol: name) named) body)
          (let ([named-parsed (parse-sexpr named)])
          (CallS (Fun name 'S (parse-sexpr body)) named-parsed named-parsed))] ;; There is no 'With' so it needs to be represented by structures that exist (CallS, CallD) call-static is that as soon as it is written the rules are clear from that moment. call-dynamic means that the rules are in real time and not only from the moment it is written, so it is better to use call-static with the values being correct from the moment the expression is written, this helps prevent errors;;; there is no With constructor replace with existing constructors
         [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
      [(cons 'fun more)
       (match sexpr
         [(list 'fun (list (symbol: name1) (symbol: name2)) body)
          (if (eq? name1 name2)
              (error 'parse-sexpr "parse-sexpr: `fun' has a duplicate param name in ~s" sexpr) ;; The text in the error should match the test. ;; cannot use the same param name twice
              (Fun name1 name2 (parse-sexpr body)))]
         [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
      [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
      [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
      [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;; According to the syntax of call-static it receives 3 groups and each of them has to call the function
      [(list 'call-dynamic fun arg1 arg2)(CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;; Like 'call-static' also 'call-dynamic' should receive the 3 groups and send them to the function.
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

    


  (: parse : String -> SOL)
  ;; parses a string containing a SOL expression to a SOL AST
  (define (parse str)
    (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "x") => (Id 'x))
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{intersect {1 2 3} {4 2 3}}") => (Inter (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters

  

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set AND
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env),extend(x1,eval(E1,env),envf)) ) 
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env),envf)) 
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf> 
             = error!          otherwise

|#

;; Types for environments, values, and a lookup function

;; Defines an ENV data type, an empty , and an Extend with Symbol VAL and the rest of the environment.
  (define-type ENV
    [EmptyEnv]
    [Extend Symbol VAL ENV])

;;Defines a VAL data type, two options: a value that is SET, and an environment that is a function value with Symbol Symbol SOL and the continuation of the environment
  (define-type VAL
    [SetV SET]
    [FunV Symbol Symbol SOL ENV])

;;The function receives a Symbol and an environment ENV and checks if the symbol matches someone in the environment, if so it returns the value VAL
  (: lookup : Symbol ENV -> VAL)
  (define (lookup name env)
    (cases env
      [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
      [(Extend id val rest-env)
       (if (eq? id name) val (lookup name rest-env))]))



;; Auxiliary procedures for eval 
  (: SetV->set : VAL -> SET)
    (define (SetV->set v)
      (cases v
        [(SetV S) S]
        [else (error 'SetV->set "expects a set, got: ~s" v)]))

#|
Multiplication by a scalar
Helper function mult-op that multiplies a number by n and returns the product.
For each (map) of the value s (VAL) extracts the set and with the help of the mult-op function multiplies it by n and in the results rebuilds SetV
|#
  (: smult-set : Number VAL -> VAL)
  (define (smult-set n s)
    (: mult-op : Number -> Number)
    (define (mult-op k)
      (* k n))
    (SetV (map mult-op (SetV->set s)))) ;; For each (map) of the value s (VAL) extracts the set and with the help of the mult-op function multiplies it by n and in the results rebuilds SetV


;;Test
(test (smult-set 2 (SetV '())) => (SetV '()))
(test (smult-set 2 (SetV '(5))) => (SetV '(10)))
(test (smult-set 2 (SetV '(1 2 3 4))) => (SetV '(2 4 6 8)))
(test (smult-set -1 (SetV '(-2 -5 -6))) => (SetV '(2 5 6)))
(test (smult-set -3 (SetV '(-1 0 1))) => (SetV '(3 0 -3)))


;;receives a binary SET operator and receives 2 values of VAL The function builds a new SetV by performing an op on each of the sets extracted from val
 (: set-op : (SET SET -> SET) VAL VAL -> VAL ) 
  ;; gets a binary SET operator, and uses it within a SetV
  ;; wrapper
  (define (set-op op val1 val2)
     (SetV (op (SetV->set val1) (SetV->set val2))))



;;---------  the eval procedure ------------------------------
(: eval : SOL ENV -> VAL)
  ;; evaluates SOL expressions by reducing them to set values
  (define (eval expr env)
    (cases expr
      [(Set S) (SetV (create-sorted-set S))] ;; Accepts an expression and creates a sorted set of S
      [(Smult n set) (smult-set n (eval set env))] ;; Gets an expression to multiply by a scalar, which sends it to the smult-set function with the number n.
      [(Inter l r) (set-op set-intersection (eval l env) (eval r env))] ;; Gets an intersection expression, which sends the 2 groups to the set-intersection function to find the common group of members that are in both the first and the second group, sorted and without duplicates.
      [(Union l r) (set-op set-union (eval l env) (eval r env))] ;; Gets an expression for the union, which sends the 2 groups to the set-union function to return all members in both groups, sorted and without duplicates.
      [(Id name) (lookup name env)] ;;Looking for ID around
      [(Fun bound-id1 bound-id2 bound-body)
       (FunV bound-id1 bound-id2 bound-body env)] ;; Receives an expression and creates a function for the environment.
      [(CallS fun-expr arg-expr1 arg-expr2);; receives the expression and contains a static reading of it by the variables in the current environment (as soon as it is written the rules are clear from that moment. )
       (let ([fval (eval fun-expr env)])
         (cases fval
           [(FunV bound-id1 bound-id2 bound-body f-env)
            (eval bound-body (extend bound-id1 (eval arg-expr1 env) (extend bound-id2 (eval arg-expr2 env) f-env)))] 
           [else (error 'eval "`call-static' expects a function, got: ~s"
                              fval)]))]
      [(CallD fun-expr arg-expr1 arg-expr2);;Receives the expression and contains a dynamic reading of it by the variables in the extended environment (the rules are in real time and not only from the moment it is written)
       (let ([fval (eval fun-expr env)])
         (cases fval
          [(FunV bound-id1 bound-id2 bound-body f-env)
           (let ([extended-env (extend bound-id2 (eval arg-expr2 env) f-env)])
            (eval bound-body (extend bound-id1 (eval arg-expr1 env) extended-env)))] 
           [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                              fval)]))]))


;; The function links the Symbol VAL ENV variables so that it will be an env environment and it will be possible to access the values that are defined as env
(: extend : Symbol VAL ENV -> ENV)
(define (extend id val env)
  (Extend id val env))


#|
 ;;createGlobalEnv extends the language to recognize the functions conts,first,second.
'first returns the first member of the pair
'second returns the second member of the pair
'cons creates a new pair in two members
|#
(: createGlobalEnv : -> ENV)
  (define (createGlobalEnv)
    (Extend 'second (FunV 'p 'spare-param (Id 'spare-param) (EmptyEnv))
            (Extend 'first (FunV 'p 'spare-param (Id 'p) (EmptyEnv))
                    (Extend 'cons (FunV 'f 's (Fun 'x 'y (Union (Id 'x) (Id 'y))) (EmptyEnv))
                          (EmptyEnv))))) 

;;Receives the string, parses it and returns the result
(: run : String -> (U SET VAL))
  ;; evaluate a SOL program contained in a string
  (define (run str)
    (let ([result (eval (parse str) (createGlobalEnv))]) 
       (cases result
         [(SetV S) S] 
         [else result]))) 


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))
(test (run "{fun {x x} x}") =error> "parse-sexpr: `fun' has a duplicate param name in (fun (x x) x)")
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")




;;Question 5 d

(test (run "{with {first {fun {p spare-param} {call-static first p {}}}} first}")=>
      (FunV
 'p
 'spare-param
 (CallS (Id 'first) (Id 'p) (Set '()))
 (Extend
  'second
  (FunV 'p 'spare-param (Id 'spare-param) (EmptyEnv))
  (Extend 'first (FunV 'p 'spare-param (Id 'p) (EmptyEnv)) (Extend 'cons (FunV 'f 's (Fun 'x 'y (Union (Id 'x) (Id 'y))) (EmptyEnv)) (EmptyEnv))))))

(test (run "{with {second {fun {p spare-param} {call-static second p {}}}} second}")=>
      (FunV
 'p
 'spare-param
 (CallS (Id 'second) (Id 'p) (Set '()))
 (Extend
  'second
  (FunV 'p 'spare-param (Id 'spare-param) (EmptyEnv))
  (Extend 'first (FunV 'p 'spare-param (Id 'p) (EmptyEnv)) (Extend 'cons (FunV 'f 's (Fun 'x 'y (Union (Id 'x) (Id 'y))) (EmptyEnv)) (EmptyEnv)))))
)


(test (run "{with {cons {fun {f s} {call-dynamic cons f s}}} cons}")=>
   (FunV
 'f
 's
 (CallD (Id 'cons) (Id 'f) (Id 's))
 (Extend
  'second
  (FunV 'p 'spare-param (Id 'spare-param) (EmptyEnv))
  (Extend
   'first
   (FunV 'p 'spare-param (Id 'p) (EmptyEnv))
   (Extend
    'cons
    (FunV 'f 's (Fun 'x 'y (Union (Id 'x) (Id 'y))) (EmptyEnv))
    (EmptyEnv)))))   )

