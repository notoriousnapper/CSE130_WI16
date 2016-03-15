% List of parent relationships
parent(kim,holly).
parent(margaret,kim).  
parent(herbert,margaret). 
parent(john,kim).
parent(felix,john).  
parent(albert,felix).
parent(albert,dana).
parent(felix,maya).

























%queries
parent(margaret,X).
parent(X,kim).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% unification
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t1 = t2.
foo(bar) = foo(bar).
foo(X) = foo(bar).
f(X,dog) = f(cat,Y).
f(cat) = f(dog).
q(X,dog,X) = q(cat,Y,Y).
a(W,foo(W,Y),Y) = a(2,foo(X,3),Z).
a(W,foo(W,Y),Y) = a(2,foo(X,3),X).
    
% so what happens when we ask: parent(margaret, john). Unification!
parent(margaret, john).
% what about: parent(X,kim). Again unification!
parent(X,kim).
% What about: parent(X,Y).    
parent(X,Y).
    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% Conjunction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parent(margaret, X), parent(X, holly).
parent(X,Y), parent(Y,Z), parent(Z,kim).
    
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
% rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grandparent(X,Y) :- parent(X,Z), parent(Z,Y).
greatgrandparent(X,Y) :- parent(X,Z),grandparent(Z,Y).


% queries
grandparent(X,kim).
greatgrandparent(X,holly).

    
% Multiple clauses: disjunction    
has_family(X) :- parent(X,_).
has_family(X) :- parent(_,X).
















% queries
has_family(holly).
has_family(mugatu).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% recursion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% let's write ancestor(X,Y), which should hold if X is an ancestor of Y










X
.
.   ancerstor(X,Z)
.
Z
|   parent(Z,Y)
Y







% soln
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y),ancestor(X,Z).




%queries:
ancestor(kim,X).
ancestor(X,kim).


ancestor(felix,holly).

%		ancestor(felix,holly)?
%		  /		\
%     parent(felix,holly)    parent(Z,holly) 
%	  NO		    ancestor(felix,Z)
%				|
%				| Z = kim  (by fact)
%				|
%			  ancestor(felix,kim)
%			  /        \
%	  parent(felix,kim)     parent(Z',kim)
%	      NO                ancestor(felix,Z')  ----------|
%			         |                            | Z'=john
%		     Z'=margaret |                            |
%				 |                        ancestor(felix,john)
%		     ancestor(felix,margaret)                 |
%		        /        \                         parent(felix,john)
%	parent(felix,margaret)   parent(Z'',margaret)         YES
%		    NO           ancestor(felix,Z'')
%                                    |
%		    Z'' = herbert    |
%		                     |
%			   ancestor(felix,herbet)
%			     /              |
%		 parent(felix,herbert)   parent(Z''',herbert)
%		      NO			NO


% Different ordering
ancestor1(X,Y) :- ancestor1(X,Z), parent(Z,Y).
ancestor1(X,Y) :- parent(X,Y).


%queries:
ancestor1(X,kim).
ancestor1(felix,holly).








%		ancestor1(felix,holly)?
%		         |
%			 |
%			 |
%		ancestor1(felix,Z)  %prove first subgoal, 
%			 |          %then parent(Z,holly) 
%			 | 
%			 |
%		ancestor1(felix,Z') %prove first subgoal,
%			 |	    %then parent(Z',Z)
%			 |
%			 |
%		ancestor1(felix,Z'')
%			 .
%			 .
%			 .



% another ordering
ancestor2(X,Y) :- parent(Z,Y),ancestor2(X,Z).
ancestor2(X,Y) :- parent(X,Y).

%queries:
ancestor2(X,kim).



% another ordering
ancestor3(X,Y) :- parent(X,Y).
ancestor3(X,Y) :- ancestor3(X,Z),parent(Z,Y).

%queries:
ancestor3(X,kim).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% not
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sibling(X,Y) :- parent(P,X),parent(P,Y).

%queries:
sibling(X,Y).


















sibling1(X,Y) :- not(X=Y),parent(P,X),parent(P,Y).
    
%queries:
sibling1(john,maya).
sibling1(X,Y).












X = Y.
not(X = Y).

sibling2(X,Y) :- parent(P,X),parent(P,Y),not(X=Y).


%queries:
sibling2(john,maya).
sibling2(X,Y).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[1,2,3].
X = [1,2,3].
X = [1 | [2,3] ].
[X|T] = [1,2,3].
[1,2,3] = [X|T].
[X,2,3] = [1|T].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% append
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% let rec ml_app x y =
%    match x with
%     | [] -> y
%     | h::t -> let ty = (ml_app t y) in h::ty
%




%     x        y        h    t
%    [1;2]   [3;4]      1   [2]   ==> return 1::[2;3;4], which is [1;2;3;4]
%    [2]     [3;4]      2   []    ==> return 2::[3;4], which is [2;3;4]
%    []      [3;4]  ==> return [3;4]





% we now have to write app in Prolog














































%% soln:
app([],Y,Y).
app([H|T],Y,[H|TY]) :- app(T,Y,TY).


%queries:

% app([1,2,3], [4, 5, 6], X).
%  answer: X = [1, 2, 3, 4, 5, 6].

% app([1,2,3], [4, 5, 6], [1]).
%  answer: false

% app(X, [4, 5, 6], [1, 4, 5, 6]).

















%  answer: X = [1]
% Wow, can run in reverse: give "output" of append
% and Prolog figures out input !


% app(X, [4, 5, 6], [1,2,4,5,6]).
%  answer: X = [1,2]

% app(X, [4, 5, 6], [1,2,5,6]).
%  answer: false

% app([1], Y, [1,2,5,6]).
%  answer: Y = [2,5,6]

% Now check this one out:
% app(X,Y,[1,2,3]).
%  answer :
%  X = [],
%  Y = [1, 2, 3] ;
%  X = [1],
%  Y = [2, 3] ;
%  X = [1, 2],
%  Y = [3] ;
%  X = [1, 2, 3],
%  Y = [] ;
%  false.
% Wow, it enumrates all ways of appending two lists to get [1,2,3]

% app(X,Y,Z).

















% answer:
% X = [],
% Y = Z ;
% X = [_G344],
% Z = [_G344|Y] ;
% X = [_G344, _G350],
% Z = [_G344, _G350|Y] ;
% X = [_G344, _G350, _G356],
% Z = [_G344, _G350, _G356|Y] ;
% X = [_G344, _G350, _G356, _G362],
% Z = [_G344, _G350, _G356, _G362|Y] ;
% X = [_G344, _G350, _G356, _G362, _G368],
% Z = [_G344, _G350, _G356, _G362, _G368|Y]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reverse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% Write it in Ocaml first


















%% soln:
%% let rec rev_helper(x,sofar) =
%%   match x with
%%      | [] -> sofar
%%      | h::t -> rev_helper(t, h::sofar);;

%% let rev(x) = 
%%   rev_helper(x,[]);;

%% now write it in Prolog

























%% soln:
rev(X,Res) :- rev_helper(X,[],Res).
rev_helper([],Res,Res).  
rev_helper([H|T],SoFar,Res) :- rev_helper(T,[H|SoFar],Res).

%queries:
% rev([1,2,3], Y).
% rev(Y,[1,2,3]).
% rev(X,Y).
% rev([1,2,3],[3,2,X]).
% rev([1,2,3],[4,2,X]).
% rev([1|T],[3,2,1]).
% rev(X,X).








% how can we write palyndrome?













%% soln:
palyndrome(X) :- rev(X,X)













%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tailof
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


tailof([_|X],X).

%queries:
% tailof([1,2,3], X).
% tailof(X, [1,2,3]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% has3ormore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has3ormore([_,_,_|_]).

%queries:
% has3ormore([1,2,3,4,5]).
% has3ormore([1,2]).
% has3ormore(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% isin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% you write this one

















%% soln:
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

%queries:
% isin(3, [1,2,3,4]).
% isin(3, [1,2,3,4,3]).
% isin(10, [1,2,3,4]).
% isin(X, [1,2,3,4]).
% isin(X, []).
% isin(3, X).


















% answer:
% X = [3|_G319] ;
% X = [_G318, 3|_G322] ;
% X = [_G318, _G321, 3|_G325] ;
% X = [_G318, _G321, _G324, 3|_G328] ;
% X = [_G318, _G321, _G324, _G327, 3|_G331] ;
% X = [_G318, _G321, _G324, _G327, _G330, 3|_G334]
% in other words: all possible lists with 3 as an element


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% len
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% you write this one

















%% soln:
len([],0).
len([_|T],N) :- len(T,Nt), N = Nt + 1.

%queries:
% len([1,2,3],X).


















% answer = X = 0+1+1+1.
% Yikes! didn't do what we wanted...

% X = 0+1.
% X+Y = 0+1.
% X+0 = 0+1.
% plus(X,0) = plus(0,1).
% plus(X,Y) = plus(0,1).
















% X is 0+1.
% 4 is X+Y.







% Let's try it again
% len([],0).
% len([_|T],N) :- len(T,Nt), N is Nt + 1.

%queries:
% len([1,2,3],X).
% ok, now we got it, answer = 3
% len(X,3).



























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numerical computations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% factorial
fac(0, 1).
fac(N, V) :-
  N > 0,
  N1 is N - 1,
  fac(N1, V1),
  V is N * V1.

% fac(3,X)
% fac(5,X)
% fac(5,120)
% fac(5,121)
% fac(X,120)


% You guys write fibonacci:

% let rec fib n =
%    if n = 0 then 0
%    else if n = 1 then 1
%    else if n > 1 fib(n-1) + fib(n-2)
%    else raise "Error"



















% soln:
fib(0, 0).
fib(1, 1).
fib(N, V) :-
  N > 1,
  N1 is N - 1,
  N2 is N - 2,
  fib(N1, V1),
  fib(N2, V2),
  V is V1 + V2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% foolists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foo(a).  
foo(b). 
foo(c).
foo(d).


% is L a list containing only foos, but with not duplciates?


















%% soln:
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

isfoolist(L) :- isfoolist_helper(L,[]).

isfoolist_helper([],_).

isfoolist_helper([X|T],SoFar) :- 
	foo(X),    
        not(isin(X,SoFar)),
        append(SoFar,[X],NewSoFar),
        isfoolist_helper(T,NewSoFar).
     
    
%queries:
% isfoolist([a,b]).
%   answer: true.
% isfoolist([a,b,b]).
%   answer: false. (because of duplicate)
% isfoolist([a,b,1]).
%   answer: false. (because 1 is not a foo)
% isfoolist([a,b|T]).
%   answer:
%     T = [] ;
%     T = [c] ;
%     T = [c, d] ;
%     T = [d] ;
%     T = [d, c] ;
%     false.
% isfoolist(L).
%   answer: all lists made up of foos...


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bagof, setof
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

p(1).
p(2).
p(3).
q(4).
q(5).
q(6).
pq(X,Y) :- p(X),q(Y).

% p(X).
% bagof(X, p(X), R).
% bagof(X, p(X), [A,B,C]).
% bagof(X, p(X), [A,B]).


% bagof([X,Y], pq(X,Y), R).


% Recall from before:
parent(kim,holly).
parent(margaret,kim).  
parent(herbert,margaret). 
parent(john,kim).
parent(felix,john).  
parent(albert,felix).
parent(albert,dana).
parent(felix,maya).
ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(Z,Y),ancestor(X,Z).



% Now say you want to write has_more_than_one_ancestor(X) which holds
% if X has more than one ancestor. I would suggest first writing
% ancestors(X,L) which holds if L is the list of X's ancestors





















%% soln:
ancestors(X,L) :- bagof(A, ancestor(A,X), L).
has_more_than_one_ancestor(X) :- ancestors(X,[_,_|_]).











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cut
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% p(X), q(Y).
% p(X), !, q(Y).
% p(X), q(Y), !.
% !, p(X), q(Y).

% Recall:
% isin(X,[X|_]).
% isin(X,[_|T]) :- isin(X,T).

% isin(bob, [bob, bob]).

% How can we modify isin so that it returns true, and doesn't ask for more?




















isin2(X,[X|_]) :- !.
isin2(X,[_|T]) :- isin2(X,T).









% isin(X, [bob,cat]).
% isin2(X, [bob,cat]).








% len(X, 3).

% How can we modify len so that it returns only one answer?















len2([],0).
len2([_|T],N) :- len(T,Nt), N is Nt + 1, !.







% write count(X,L,C) which holds if C is the count of X's in list L



















%% one approach:
count(X, [], 0).
count(X, [X|T], Res) :- count(X,T,RecRes), Res is RecRes + 1.
count(X, [_|T], Res) :- count(X,T,Res).

% but the above doesn't quite work. 
% 25 ?- count(5,[5,5,5],R).
% R = 3 ;
% R = 2 ;
% R = 2 ;
% R = 1 ;
% R = 2 ;
% R = 1 ;
% R = 1 ;
% R = 0 ;
% false.
% 

count_cut(X, [], 0) :- !.
count_cut(X, [X|T], Res) :- !, count_cut(X,T,RecRes), Res is RecRes + 1.
count_cut(X, [_|T], Res) :- !, count_cut(X,T,Res).

