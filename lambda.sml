datatype term = V of string | L of string * term | A of term * term;

fun show(V(x)) = x
| show(L(x,y))= "L"^x^"."^ show(y)
| show(A(x,y)) = "(" ^ show(x) ^ " " ^ show(y) ^ ")";

fun chk_var(x,y,[])= x=y
|chk_var(x,y,(v1,v2)::t)= if x=v1 andalso y=v2
						  then true
						  else if x<>v1 andalso y<>v2
								then chk_var(x,y,t)
								else false;

fun alpha2(V(x), V(y), env) = chk_var(x,y,env)
   | alpha2(L(x1,t1), L(x2,t2), env) = let
											val env1= [(x1,x2)] @ env
										in 
											alpha2(t1,t2,env1)
										end
   | alpha2(A(t1,t2), A(t3,t4), env) = alpha2(t1,t3,env) andalso alpha2(t2,t4,env)
   | alpha2(_,_,env)= false;

fun alpha(x,y) = alpha2(x,y,[]);
