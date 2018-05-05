open Util
open StringSetMap

(*shows divide by zero exception*)
exception DIV_BY_0
exception IMAGINARY
exception NAN

type number = 
    |Zero
    |Succ of number
    |Pred of number
    |Add of number * number
    |Sub of number * number
    |Mult of number * number
    |Div of number * number
    [@@deriving show]

(*don't call genPos or genNeg to generate a positive number,
 * rather call genNum which calls them based on n's sign*)
let rec  genPos (n : int) (x : number) : number=
    if not( n = 0)
    then genPos (n-1) (Succ(x))
    else x
let rec genNeg (n : int) (x : number) : number = 
    if not(n=0)
    then genNeg (n+1) (Pred(x))
    else x
let genNum(n : int) : number = 
    if(n > 0)
    then genPos n Zero
    else if (n < 0)
    then genNeg n Zero
    else Zero
    
let add1 (n : number) : number = match n with
    |Pred(n') -> n'
    |_-> Succ(n)

let sub1 (n : number) : number = match n with
    |Succ(n') -> n'
    |_->Pred(n)

(*all need to match on n1 as well as n2*)
let rec solve (n0 : number) : number = match n0 with
    |Add(n1, n2) -> begin match n1 with
        |Zero -> solve n2
        |Succ(n1') -> begin match n2 with
            |Zero -> solve n1
            |Succ(n2') -> solve(Add(Succ(n1), n2'))
            |Pred(n2') -> solve(Add(n1',n2' ))
            |_-> let x = solve n2 in
                solve(Add(n1,x))
            end(*match n2 in Add*)
        |Pred(n1') -> begin match n2 with
            |Zero -> n1
            |Succ(n2') -> solve(Add(n1',n2'))
            |Pred(n2') -> solve(Add(Pred(n1),n2'))
            |_-> let x = solve n2 in 
                solve(Add(n1,x))
            end(*match n2 in Add*)
        |_ -> let x = solve n1 in
             solve(Add(x, n2))
        end (*match n1 in Add*)

    |Sub(n1,n2) -> begin match n1 with
        |Zero -> begin match n2 with(*if 0-n2, switch sign of n2*)
            |Zero -> Zero
            |Succ(n2') -> solve(inverse(n2))
            |Pred(n2') -> solve(inverse(n2))
            |_-> solve(inverse(n2))
            end(*match n2 in Sub(Zero,n2)*)
        |Succ(n1') -> begin match n2 with
            |Zero -> solve n1
            |Succ(n2') -> solve(Sub(n1',n2'))
            |Pred(n2') -> solve(Add(n1,inverse(n2)))
            |_-> let x = solve n2 in
                solve(Sub(n1,x))
            end(*match n2 in sub(Succ(n1'),n2)*)
        |Pred(n1') -> begin match n2 with
            |Zero -> solve n1
            |Succ(n2') -> solve(Sub(Pred(n1),n2'))
            |Pred(n2') -> solve(Sub(n1',n2'))
            |_-> let x = solve n2 in 
                solve(Sub(n1,x))
            end(*match n2 in sub(Pred(n1'),n2)*)
        |_ -> let x = solve n1 in
            solve(Sub(x, n2))
        end(*match n1 in Sub*)

    |Mult(n1,n2) -> begin match n1 with
        |Zero -> solve(Zero)
        |Succ(n1') -> begin match n2 with
            |Zero -> solve(Zero)
            |Succ(n2') -> solve(Add(Mult(n1,n2'),n1))
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in 
                solve(Mult(n1, x))
            end(*match n2 in Mult(Succ(n1'),n2)*) 
        |Pred(n1') -> begin match n2 with
            |Zero -> Zero
            |Succ(n2') -> raise TODO
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in
                solve(Mult(n1,x))
            end(*match n2 in Mult(Pred(n1'),n2)*)
        |_-> let x = solve n1 in
            solve(Mult(x,n2))
        end(*match n1 in Mult*)

    |Div(n1,n2) -> begin match n1 with
        |Zero -> Zero
        |Succ(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0
            |Succ(n2') -> raise TODO
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in
                solve(Div(n1, x))
            end(*match n2 Div(Succ(n1'),n2)*)
        |Pred(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0
            |Succ(n2') -> raise TODO
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in
                solve(Div(n1, x))
            end(*match n2 in Div(Pred(n1'),n2)*)
        |_-> let x = solve n1 in
            solve(Div(x,n2))
        end(*match n1 in Div*)
    |_-> n0(*otherwise it is Zero|Succ(n0')|Pred(n0')*)

(*creates the inverse of the number passed in, or solves number then makes inverse*)
and inverse (n : number) : number = match n with
    |Zero -> Zero
    |Succ(n') -> Pred(inverse n')
    |Pred(n') -> Succ(inverse n')
    |_-> let x = solve n in
        inverse x

  (*testing*)

let tests = 
        (*tests a number*)
        let seven : number = genNum(7)  in
        let seven_ans : number = genNum(7)in
        (*add tests*)
        let adder : number = Add(genNum(3),genNum(4)) in
        let adder_ans : number = genNum(7) in
        let addNeg : number = Add(genNum(-3),genNum(4)) in
        let addNeg_ans : number  = genNum(1) in
        let addNeg2 : number = Add(genNum(5),genNum(-4)) in
        let addNeg2_ans : number = genNum(1) in 
        let add2Neg : number = Add(genNum(-4),genNum(-4)) in
        let add2Neg_ans : number = genNum(-8) in
        (*subtraction tests*)
        let subtr : number = Sub(genNum(7),genNum(6)) in
        let subtr_ans : number = genNum(1) in
        let sub0 : number = Sub(genNum(0),genNum(8)) in
        let sub0_ans : number = genNum(-8) in
        let subNeg : number = Sub(genNum(-1), genNum(5)) in
        let subNeg_ans : number = genNum(-6) in
        let subNeg2 : number = Sub(genNum(4), genNum(-6)) in
        let subNeg2_ans : number = genNum(10) in
        let sub2Neg : number = Sub(genNum(-3),genNum(-3)) in
        let sub2Neg_ans : number = genNum(0) in
        (*multiplication tests*)
        let mult : number = Mult(genNum(5),genNum(4)) in
        let mult_ans : number = genNum(20) in
        let multNeg : number = Mult(genNum(-4),genNum(2)) in
        let multNeg_ans : number = genNum(-8) in
        let multNeg2 : number = Mult(genNum(3), genNum(-5)) in
        let multNeg2_ans : number = genNum(-15) in
        let mult2Neg : number = Mult(genNum(-6),genNum(-2)) in
        let mult2Neg_ans : number = genNum(12) in
        (*division tests*)
        (*compound tests*)
        (*tests something with a TODO block*)

let arith_test : Util.test_block = 
        TestBlock
        ("Arith",
        [seven , seven_ans
        ;adder, adder_ans
        ;addNeg, addNeg_ans
        ;addNeg2, addNeg2_ans
        ;add2Neg, add2Neg_ans
        ;subtr, subtr_ans
        ;sub0, sub0_ans
        ;subNeg, subNeg_ans
        ;subNeg2, subNeg2_ans
        ;sub2Neg, sub2Neg_ans
        ;mult, mult_ans
        ;multNeg, multNeg_ans
        ;multNeg2, multNeg2_ans
        ;mult2Neg, mult2Neg_ans
        ],solve, (=),show_number, show_number) in
run_tests[arith_test]
