% Author: Maja Jaakson


% We will implement the ASTs as follows:
% 
% expr := sum(expr(X),expr(Y)) | prd(expr(X),expr(Y)) | num | expo
% expo := expo(x) | expo(x,Num)
% num := num(X) where X is an integer greater than 1.

isInt(Num) :- integer(Num), Num > 1. % We will use this throughout.

% 'parse' has two arguments: (1) a List of tokens (the expression) and
% (2) the accompanying AST.
parse(List,AST) :- ast(List,AST,[]).

% 'ast' has three arguments: (1) a list of tokens; (2) the AST we get
% by parsing the leftmost expression; (3) the Rest of the list.
ast([Num|Tail],num(Num),Tail) :- isInt(Num). 
ast([^,x,Num|Tail],expo(x,num(Num)),Tail) :- isInt(Num). 
ast([x|Tail],expo(x),Tail).
ast([+|Tail],sum(Addend1,Addend2),Rest) :- ast(Tail,Addend1,PassTail),
											ast(PassTail,Addend2,Rest).
ast([*|Tail],prd(Factor1,Factor2),Rest) :- ast(Tail,Factor1,PassTail),
											ast(PassTail,Factor2,Rest).


% 'postfix' has two arguments: (1) an AST; (2) its expression in postfix
% notation.
postfix(num(Num),[Num]) :- isInt(Num).
postfix(expo(x),[x]).
postfix(expo(x,num(Num)),[x,Num,^]) :- isInt(Num). 
postfix(sum(Addend1,Addend2),SumString) :- postfix(Addend1,X),
											 postfix(Addend2,Y), 
											 append(X,Y,Addenda),
											 append(Addenda,[+],SumString).
postfix(prd(Factor1,Factor2),PrdString) :- postfix(Factor1,X),
											 postfix(Factor2,Y), 
											 append(X,Y,Factors),
											 append(Factors,[*],PrdString).


% 'ev' has three arguments: (1) an AST, (2) the Environment in which x is 
% evaluated; (3) the Value of the AST.

ev(num(Num),_,Num):- isInt(Num).
ev(expo(x),Env,Env) :- isInt(Env).
ev(expo(x,num(Num)),Env,Val) :- isInt(Num), isInt(Env), Val is Env^Num.
ev(sum(Addend1,Addend2),Env,Val) :- isInt(Env), ev(Addend1,Env,V1),
									   ev(Addend2,Env,V2), Val is V1 + V2.
ev(prd(Factor1,Factor2),Env,Val) :- isInt(Env), ev(Factor1,Env,V1),
									   ev(Factor2,Env,V2), Val is V1 * V2.


% We begin by defining the ASTs for D.
genDnum(num(Num)) :- isInt(Num).
genDexp(expo(x)).
genDexp(expo(x,num(Num))) :- genDnum(num(Num)).
genDmul(X) :- genDexp(X).
genDmul(prd(N,X)) :- genDnum(N), genDexp(X).
genDsum(N) :- genDnum(N).
genDsum(M) :- genDmul(M).
genDsum(sum(M,S)) :- genDmul(M), genDsum(S).

% 'genAst' has three arguments: (1) the evaluated Value of (3) an Ast, given
% an (2) evaluation Environment.

genAst(Val,_,num(Num)) :- Num is Val.
genAst(Val,Env,expo(x)) :- Env is Val.
genAst(Val,Env,expo(x,num(Num))) :- between(2,Val,Num), Val is Env^Num.
genAst(Val,Env,prd(num(F1),X)) :- between(2,Val,F1), between(2,Val,F2),
								   Val is F1 * F2, genAst(F1,Env,num(F1)),
								   genAst(F2,Env,X), genDexp(X).
genAst(Val,Env,sum(M,S)) :- between(2,Val,A1), between(2,Val,A2),
							 Val is A1 + A2, genAst(A1,Env,M), genDmul(M),
							 genAst(A2,Env,S), genDsum(S).



% 'generer' has two arguments; (1) a list of constraints and (2) an AST.
generer([In:Out],Ast) :- genAst(Out,In,Ast),!.
generer([In:Out|Tail],Ast) :- genAst(Out,In,Ast), generer(Tail,Ast).

% EXERCISE 7
% 'losning' has three arguments: (1) a Value to which the (2) AST is 
% evaluated under the (3) Environment. It makes use of the helper method
% 'genInteger', which generates integers greater than 1.

genInteger(2).
genInteger(X) :- genInteger(Y), X is 1 + Y.

losning(Val,num(Val),Env) :- genInteger(Env).
losning(Val,expo(x),Env) :- Env is Val.
losning(Val,expo(x,num(Num)),Env) :- X is log(Val) * Num, Env is log(X).
losning(Val,prd(num(Num),X),Env) :- Left is Val/Num, losning(Left,X,Env).
losning(Val,sum(M,S),Env) :- between(2,Val,A1), between(2,Val,A2),
							  Val is A1 + A2, losning(A1,M,Env),
							  losning(A2,S,Env).
