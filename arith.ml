(*TODO:
        * should solve use int division or return fraction??
        * can use more number terms i.e Power(base, exp), Absval(number), squareroot(number) 
        * finish modulo, which moght be helpful for div
        * modulo tests*)

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
    |Mod of number * number
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
            |Pred(n2') -> solve(inverse(Mult(n1,inverse n2)))
            |_-> let x = solve n2 in 
                solve(Mult(n1, x))
            end(*match n2 in Mult(Succ(n1'),n2)*) 
        |Pred(n1') -> begin match n2 with
            |Zero -> Zero
            |Succ(n2') -> solve(inverse(Mult(inverse n1,n2)))
            |Pred(n2') -> solve(Mult(inverse n1, inverse n2))
            |_-> let x = solve n2 in
                solve(Mult(n1,x))
            end(*match n2 in Mult(Pred(n1'),n2)*)
        |_-> let x = solve n1 in
            solve(Mult(x,n2))
        end(*match n1 in Mult*)

    (*Div returns the floor of integer division*)
    |Div(n1,n2) -> begin match n1 with
        |Zero -> solve(Zero)
        |Succ(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0
            |Succ(n2') -> begin match n2' with
                |Zero -> solve(n1) (*x/1=x*)
                |Succ(n2'') -> (*where x/d where d >=2*)
                    if(equals n1 n2)
                    then solve(Succ(Zero)) (* x/x = 1 *)
                    else if(graterThan n1 n2)
                    then solve(Add(Div(Sub(n1, n2), n2),Succ(Zero)))(*x/y*) (*raise TODOx/d where d >= 2 *)
                    else solve(Zero)(*n1 < n2*) 
                |_ -> raise IMPOSSIBLE
                end(*match n2' in Div(Succ(n1'), Succ(n2') *)
            |Pred(n2') -> solve(inverse(Div(n1,inverse n2)))
            |_-> let x = solve n2 in
                solve(Div(n1, x))
            end(*match n2 Div(Succ(n1'),n2)*)
        |Pred(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0
            |Succ(n2') -> solve(inverse(Div(inverse n1,n2)))
            |Pred(n2') -> solve(Div(inverse n1, inverse n2))
            |_-> let x = solve n2 in
                solve(Div(n1, x))
            end(*match n2 in Div(Pred(n1'),n2)*)
        |_-> let x = solve n1 in
            solve(Div(x,n2))
        end(*match n1 in Div*)

    (*always returns a positive integer*)
    |Mod(n1,n2) -> begin match n1 with
        |Zero -> Zero(*0 mod x = 0*)
        |Succ(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0(*x mod 0 is undefined*)
            |Succ(n2') ->
                if(equals n1 n2)
                then Zero(*x mod x = 0*)
                else if(n1 < n2)
                then n1(*x mod y = x if x < y*)
                else solve(Mod(Sub(n1,n2),n2))(*x mod y = (x-y) mod y if x > y*)
            |Pred(n2') -> solve(Mod(n1 ,inverse  n2))
            |_ -> let x = solve n2 in
                solve(Mod(n1, x))
            end(*match n2 in Mod(Succ(n1'), n2)*)
        |Pred(n1') -> begin match n2 with
            |Zero -> raise DIV_BY_0
            |Succ(n2') -> solve(Mod(inverse n1, n2))
            |Pred(n2') -> solve(Mod(inverse n1, n2))
            |_ -> let x = solve n2 in 
                solve(Mod(n1, x))
            end(*match n2 in Mod(Pred(n1'),n2)*)
        |_ -> let x = solve n1 in 
            solve(Mod(x, n2))
        end(*match n1 in Mod*)

    |_-> n0(*otherwise it is Zero|Succ(n0')|Pred(n0')*)


(*creates the inverse of the number passed in, or solves number then makes inverse*)
and inverse (n : number) : number = match n with
    |Zero -> Zero
    |Succ(n') -> Pred(inverse n')
    |Pred(n') -> Succ(inverse n')
    |_-> let x = solve n in
        inverse x

(*evaluation functions*)
(*returns true if n1 < n2, otherwise false*)
and lessThan (n1 : number) (n2 : number) : bool = match solve(Sub(n1,n2)) with
    |Zero -> false
    |Succ(x) -> false
    |Pred(x) -> true
    |_ -> raise IMPOSSIBLE

(*returns true if n1 > n2, otherwise false*)    
and graterThan (n1 : number) (n2 : number) : bool = match solve(Sub(n1,n2)) with
    |Zero -> false
    |Succ(x) -> true
    |Pred(x) -> false
    |_ -> raise IMPOSSIBLE

(*returns true if n1 = n2, otherwise false*)
and equals (n1 : number) (n2 : number) : bool = match solve(Sub(n1,n2)) with
    |Zero -> true
    |Succ(x) -> false
    |Pred(x) -> false
    |_ -> raise IMPOSSIBLE



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
        let div1 : number = Div(genNum(4), genNum(1)) in
        let div1_ans : number = genNum(4) in
        let div : number = Div(genNum(9), genNum(3)) in
        let div_ans : number = genNum(3) in
        let divNeg : number = Div(genNum(-12),genNum(2)) in
        let divNeg_ans : number = genNum(-6) in
        let divNeg2 : number = Div(genNum(5), genNum(-1)) in
        let divNeg2_ans : number = genNum(-5) in
        let div2Neg : number = Div(genNum(-6), genNum(-3)) in
        let div2Neg_ans : number = genNum(2) in
        (*Modulo tests*)
        let moder : number = Mod(genNum(10), genNum(4)) in
        let moder_ans : number = genNum(2) in
        let negMod : number = Mod(genNum(-6),genNum(3)) in
        let negMod_ans : number = genNum(0) in
        let negMod2 : number = Mod(genNum(4), genNum(-2)) in
        let negMod2_ans : number = genNum(0) in
        let mod2Neg : number = Mod(genNum(-4), genNum(-1)) in
        let mod2Neg_ans : number = genNum(0) in
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
        ;div1, div1_ans
        ;div, div_ans
        ;divNeg, divNeg_ans
        ;divNeg2, divNeg2_ans
        ;div2Neg, div2Neg_ans
        ;moder, moder_ans
        ;negMod, negMod_ans
        ;negMod2, negMod2_ans
        ;mod2Neg, mod2Neg_ans
        ],solve, (=), show_number, show_number) in
run_tests[arith_test]
