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
            |Zero -> n1
            |Succ(n2') -> solve(Add(add1 n1, n2'))
            |Pred(n2') -> solve(Add(n1',n2' ))
            |_-> let x = solve n2 in
                solve(Add(n1,x))
            end(*match n2 in Add*)
        |Pred(n1') -> begin match n2 with
            |Zero -> n1
            |Succ(n2') -> solve(Add(n1',n2'))
            |Pred(n2') -> solve(Add(sub1 n1,n2'))
            |_-> let x = solve n2 in 
                solve(Add(n1,x))
            end(*match n2 in Add*)
        |_ -> let x = solve n1 in
             solve(Add(x, n2))
        end (*match n1 in Add*)

    |Sub(n1,n2) -> begin match n1 with
        |Zero -> begin match n2 with
            |Zero -> Zero
            |Succ(n2') -> raise TODO
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in
                solve(Sub(n1,x))
            end(*match n2 in Sub(Zero,n2)*)
        |Succ(n1') -> begin match n2 with
            |Zero -> solve n1
            |Succ(n2') -> solve(Sub(n1',n2'))
            |Pred(n2') -> solve(Sub(add1 n1,n2'))
            |_-> let x = solve n2 in
                solve(Sub(n1,x))
            end(*match n2 in sub(Succ(n1'),n2)*)
        |Pred(n1') -> begin match n2 with
            |Zero -> solve n1
            |Succ(n2') -> raise TODO
            |Pred(n2') -> raise TODO
            |_-> let x = solve n2 in 
                solve(Sub(n1,x))
            end(*match n2 in sub(Pred(n1'),n2)*)
        |_ -> let x = solve n1 in
            solve(Sub(x, n2))
        end(*match n1 in Sub*)

    |Mult(n1,n2) -> begin match n1 with
        |Zero -> Zero
        |Succ(n1') -> begin match n2 with
            |Zero -> Zero
            |Succ(n2') -> raise TODO
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
        
        (*tests something with a TODO block*)
        let bad : number = Div(Mult(genNum(8),genNum(-2)),genNum(4)) in
        let bad_ans : number = genNum(-4) in
let arith_test : Util.test_block = 
        TestBlock
        ("Arith",
        [seven , seven_ans
        ;adder, adder_ans
        ;addNeg, addNeg_ans
        ;bad, bad_ans
        ],solve, (=),show_number, show_number) in
run_tests[arith_test]
