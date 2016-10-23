:- use_module(library(semweb/turtle)).	% Turtle and TRiG
:- use_module(library(semweb/rdf_ntriples)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(semweb/rdf_db)).

% Load schema.org ontology
%:- rdf_load('http://schema.org/version/latest/all-layers.nt').

%?- rdf(S,'http://www.w3.org/2000/01/rdf-schema#subClassOf',P).

% inferred_type(I,C) means I can be inferred to be of type C
inferred_type(I,C) :-
rdf(I,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',C1),
subclass(C1,C).

% subclass(C1,C2) means C1 is a subclass of C2 (taking transitivity into account)
subclass(C1,C2) :-
rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2).
subclass(C1,C3) :-
rdf(C1,'http://www.w3.org/2000/01/rdf-schema#subClassOf',C2),
subclass(C2,C3).

% whatis(A,D) is true if D is an English description of A
whatis(T,D) :-
rdf(T,'http://www.w3.org/2000/01/rdf-schema#comment',D).

% type_from_domain(I,T) means I has type T can be derived from the domain declatation
type_from_domain(I,T) :-
rdf(I,P,_),
rdf(P,'http://www.w3.org/2000/01/rdf-schema#domain',T).

% type_from_range(I,T) means I has type T can be derived from the range declatation
type_from_range(I,T) :-
rdf(_,P,I),
rdf(P,'http://www.w3.org/2000/01/rdf-schema#range',T).


% noun_phrase(T0,T4,Ind) is true if
%  T0 and T4 are list of words, such that
%        T4 is an ending of T0
%        the words in T0 before T4 (written T0-T4) form a noun phrase
%  Ind is an individual that the noun phrase is referring to

% A noun phrase is a determiner followed by adjectives followed
% by a noun followed by an optional modifying phrase:
noun_phrase(T0,T4,Ind) :-
det(T0,T1,Ind),
adjectives(T1,T2,Ind),
noun(T2,T3,Ind),
mp(T3,T4,Ind).

% Determiners (articles) are ignored in this oversimplified example.
% They do not provide any extra constraints.
det([the | T],T,_).
det([a | T],T,_).
det(T,T,_).

% Adjectives consist of a sequence of adjectives.
% The meaning of the arguments is the same as for noun_phrase
adjectives(T0,T2,Ind) :-
adj(T0,T1,Ind),
adjectives(T1,T2,Ind).
adjectives(T,T,_).

% An optional modifying phrase / relative clause is either
% a relation (verb or preposition) followed by a noun_phrase or
% 'that' followed by a relation then a noun_phrase or
% nothing
mp(T0,T2,Subject) :-
reln(T0,T1,Subject,Object),
noun_phrase(T1,T2,Object).

mp([that|T0],T2,Subject) :-
reln(T0,T1,Subject,Object),
noun_phrase(T1,T2,Object).
mp(T,T,_).

% DICTIONARY

% adj(T0,T1,Ind) is true if T0-T1 is an adjective that is true of Ind
adj([computer, science | T],T,Ind) :- dept(Ind,comp_sci).
adj([math | T],T,Ind) :- dept(Ind,math).
adj([female | T],T,Ind) :- female(Ind).
adj([male | T],T,Ind) :- male(Ind).
adj([tall | T],T,Ind) :- tall(Ind).

% noun(T0,T1,Ind) is true if T0-T1 is a noun that is true of Ind
noun([course | T],T,Ind) :- course(Ind).
noun([student | T],T,Ind) :- student(Ind).
noun([building | T],T,Ind) :- building(Ind).
noun([friend|T], T, Ind) :- friend(Ind).

% The following are for proper nouns:
noun([Ind | T],T,Ind) :- course(Ind).
noun([Ind | T],T,Ind) :- student(Ind).
noun([Ind | T], T, Ind) :- friend(Ind).



% reln(T0,T1,Subject,Object) is true if T0-T1 is a relation
%   on individuals I1 and I2
reln([enrolled, in | T],T,Subject,Object) :- enrolled_in(Subject,Object).
reln([passed | T],T,Subject,Object) :- passed(Subject,Object).
reln([is a friend| T], T, Subject, Object) :- friend(Subject, Object).

% question(Question,QR,Ind) is true if Question-QR is true of Ind
question([is | T0],T2,Ind) :-
noun_phrase(T0,T1,Ind),
mp(T1,T2,Ind).
question([who,is | T0],T1,Ind) :-
mp(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
noun_phrase(T0,T1,Ind).
question([who,is | T0],T1,Ind) :-
adjectives(T0,T1,Ind).
question([what | T0],T2,Ind) :-
noun_phrase(T0,T1,Ind),
mp(T1,T2,Ind).


% The following has "is" betten the noun_phrase and the mp:
question([what | T0],T2,Ind) :-
noun_phrase(T0,[is|T1],Ind),
mp(T1,T2,Ind).


% askPatick(Q,A) gives answer A to question Q
askPatrick(Q,A) :-
question(Q,[],A).


%  The Database of Facts to be Queried

% course(C) is true if C is a course
course(cs312).
course(cs322).
course(math315).

dept(cs312,comp_sci).
dept(cs322,comp_sci).
dept(math315,math).

enrolled_in(john,cs312).
enrolled_in(mary,cs312).
enrolled_in(jane,math315).
enrolled_in(sally,cs322).
enrolled_in(sam,math315).

passed(S,C):-
grade(S,C,G),
G >= 50.

grade(sam,cs312,93).
grade(chris,cs312,82).

female(mary).
female(jane).
female(sally).
male(john).

tall(mary).
tall(jane).
tall(john).
tall(jordan).

student(mary).
student(jane).
student(sally).
student(john).
student(sam).
student(chris).

% Database triples

prop(Abby, friend, true).
prop(Beatrice, friend, true).
prop(Catherine, friend, true).
prop(David, friend, true).

friend(Abby).
friend(Beatrice).
friend(Catherine).
friend(David).

% To get the input from a line:

askPatrick(Ans) :-
write("Ask Patrick: "),
readln(Ln),
question(Ln,End,Ans),
member(End,[[],['?'],['.']]).

/*
?- askPatrick(Ans).
Ask me: who is a tall student enrolled in a computer science course?
Ans = mary ;
Ans = john ;
false.
*/
