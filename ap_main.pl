:- [dictionary].
:- [database].

% Interface ------------------------------------------

% the main querying function
% usage:
%	askPatrick([...], R). or askPatrick(Q, R), readln(Q).
%		where ... is a list of words


heyPatrick(Ans):- 
	write("AskPatrick:    "),
	readln(Ln),
	askPatrick(Ln, Ans).

askPatrick(Q, "That's a good question!") :- dl_question(Q, []).
askPatrick(Q, [I, do, not, know, what, you, mean]) :- \+ dl_question(Q, []).

askPatrick(Q, A) :- filter(Q, R), 
					answer(R, Ans_list),
					atomic_list_concat(Ans_list, " ", Atom),
					atom_string(Atom, A).

% what is an unknown thing?
answer([what|Q], R) :- dl_verb_be(Q,Q1, Conj), dl_nounphrase(Q1,[], Conj),
	\+ knownthing(Q1,[], _,_),
	dl_append([i,do,not,know,any,one,thing,that],[], Q,Q1, Q1,[], R).
	
% who is an unknown person?
answer([who|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	\+ knownthing(Q1,[], _,_),
	dl_append([i,do,not,know,any,one,person,who],[], Q,Q1, Q1,[], R).

% what is an example of a thing?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	knownthing(Q1,[], I,[]),
	dl_append(I,[], Q,Q1, Q1,[], R).

% who is an example of a person?
answer([who|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	knownthing(Q1,[], I,[]),
	type(I,[], [person],[]),
	dl_append(I,[], Q,Q1, Q1,[], R).
	
% what is a type of thing like?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	thingcanbetype(Q1,[], P,[]),
	dl_append([a|P],[], Q,Q1, Q1,[], R).

% what is a property of this type?
answer([what|Q], R) :-
	dl_verb_be(Q,[a|Q1], vc_tps),
	typeprop(Q1,[], P,[]),
	dl_append([a|Q1],[], Q,[a|Q1], P,[], R).
	
% what type of thing is this?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	type(Q1,[], T,[]),
	dl_append(Q1,[], Q,Q1, [a|T],[], R).
	
% what type of thing is a thing?
answer([what|Q], R) :- dl_verb_be(Q,[a|Q1], vc_tps),
	inherits([a|Q1],[], T,[]),
	dl_append([a|Q1],[], Q,Q1, T,[], R).
	
% what is a property of this person?
answer([who|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	prop(Q1,[], P,[]),
	type(Q1,[], [person],[]),
	dl_append([a|Q1],[], Q,Q1, P,[], R).
	
% who is a person that's like this?
answer([who|Q], R) :- dl_verb_be(Q,Q1, vc_tps), dl_adjectives(Q1,[]),
	props(I,[], Q1,[]),
	type(I,[], [person],[]),
	dl_append(I,[], Q,Q1, Q1,[], R).
	
% what is a thing that's like this?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps), dl_adjectives(Q1,[]),
	props(I,[], Q1,[]),
	dl_append(I,[], Q,Q1, Q1,[], R).
	
% what has these properties?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps), dl_adjectives(Q1,[]),
	s_typeprops(T,[], Q1,[]),
	dl_append([a|T],[], Q,Q1, Q1,[], R).
	
% what is a property of this known thing?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
	prop(Q1,[], P,[]),
	dl_append(Q1,[], Q,Q1, P,[], R).


% deny simple imperative orders
answer(Q, [maybe]) :- dl_verb(Q, [], vc_inf).
answer(Q, [hmmmm]) :- dl_verb(Q, [], vc_inf).
answer(Q, [i,think,i,would,rather,not]) :- dl_verb(Q, [], vc_inf).

% filter input to replace you with I, your with my, etc
filter([], []).
filter([I|T], [O|S]) :- filter_swap(I, O), filter(T, S).
filter([H|T], [H|S]) :- \+ filter_swap(H, H), filter(T, S).
filter_swap(i, you).
filter_swap(my, your).
filter_swap(you, i).
filter_swap(your, my).

% End Interface ---------------------------------------------------
% |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
% Difference List Helper Functions --------------------------------

% dl_equal(T,T1, S,S1) is true if the difference lists T,T1 and S,S1 are equal but do not 
dl_equal(T,T, S,S).
dl_equal([H|T],T1, [H|S],S1) :- dl_equal(T,T1, S,S1).

% dl_tolist(T,T1, L) is true if the difference list T,T1 and the normal list L have the same elements in the same order
dl_tolist(T,T, []).
dl_tolist([H|T],T1, [H|L]) :- dl_tolist(T,T1, L).

dl_append(T,T, S,S1, L) :- dl_tolist(S,S1, L).
dl_append([H|T],T1, S,S1, [H|L]) :- dl_append(T,T1, S,S1, L).

dl_append(R,R, T,T1, S,S1, L) :- dl_append(T,T1, S,S1, L).
dl_append([H|R],R1, T,T1, S,S1, [H|L]) :- dl_append(R,R1, T,T1, S,S1, L).

% End Difference List Helper Functions ----------------------------
% |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
% Language Processing ---------------------------------------------

% sentences
dl_statement(T, T2) :- dl_nounphrase(T, T1, Conj), dl_verbphrase(T1, T2, Conj).
dl_statement(T, T1) :- dl_verbphrase(T, T1, vc_inf).

% questions
dl_question([what | T], T1) :- dl_verbphrase(T, T1, vc_sp).
dl_question([what | T], T1) :- dl_verbphrase(T, T1, vc_tps).
dl_question([who | T], T1) :- dl_verbphrase(T, T1, vc_fp).
dl_question([who | T], T1) :- dl_verbphrase(T, T1, vc_sp).
dl_question([who | T], T1) :- dl_verbphrase(T, T1, vc_tps).
dl_question(T, T3) :- dl_verb_do(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_verbphrase(T2, T3, vc_inf).
dl_question(T, T3) :- dl_verb_be(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_nounphrase(T2, T3, _).
dl_question(T, T3) :- dl_verb_be(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_adjective(T2, T3).
dl_question([how | T], T3) :- dl_verb_do(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_verbphrase(T2, T3, vc_inf).
dl_question([why | T], T3) :- dl_verb_be(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_nounphrase(T2, T3, _).
dl_question([why | T], T3) :- dl_verb_be(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_adjective(T2, T3).
dl_question([why | T], T3) :- dl_verb_do(T, T1, Conj), dl_nounphrase(T1, T2, Conj), dl_verbphrase(T2, T3, vc_inf).

% a nounphrase consists of an article, optional adjectives, a noun and an optional modifying phrase
% usage:
%	dl_nounphrase(T, T1, Conj)
%		where T, T1 is a difference list and Conj is the standard atom for verb conjugation (see dl_verb usage)
dl_nounphrase(T, T4, Conj) :-
	dl_determiner(T, T1, Conj),
	dl_adjectives(T1, T2),
	dl_noun(T2, T3, Conj),
	dl_modphrase(T3, T4, Conj).
	
dl_nounphrase(T,T1, vc_tps) :- propernoun(T,T1).
	
dl_nounphrase([i | T], T, vc_fp).
dl_nounphrase([you | T], T, vc_sp).
dl_nounphrase([he | T], T, vc_tps).
dl_nounphrase([she | T], T, vc_tps).
dl_nounphrase([it | T], T, vc_tps).
dl_nounphrase([we | T], T, vc_sp).
dl_nounphrase([they | T], T, vc_sp).
dl_nounphrase([this | T], T, vc_tps).
dl_nounphrase([that | T], T, vc_tps).
dl_nounphrase([those | T], T, vc_sp).
dl_nounphrase([these | T], T, vc_sp).
	
% an article is the, this, a, an, etc.
% usage:
%	dl_determiner(T, T1, Conj)
%		where T, T1 is a difference list and Conj is the standard atom for verb conjugation (see dl_verb usage)
dl_determiner(T, T, vc_sp).
dl_determiner([a | T], T, vc_tps).
dl_determiner([the | T], T, vc_tps).
dl_determiner([the | T], T, vc_sp).
dl_determiner([this | T], T, vc_tps).
dl_determiner([these | T], T, vc_sp).
dl_determiner([that | T], T, vc_tps).
dl_determiner([those | T], T, vc_sp).
dl_determiner([my | T], T, vc_sp).
dl_determiner([my | T], T, vc_tps).
dl_determiner([his | T], T, vc_sp).
dl_determiner([his | T], T, vc_tps).
dl_determiner([their | T], T, vc_sp).
dl_determiner([their | T], T, vc_tps).
dl_determiner([her | T], T, vc_sp).
dl_determiner([her | T], T, vc_tps).
dl_determiner([our | T], T, vc_sp).
dl_determiner([our | T], T, vc_tps).
dl_determiner([your | T], T, vc_sp).
dl_determiner([your | T], T, vc_tps).
dl_determiner([its | T], T, vc_sp).
dl_determiner([its | T], T, vc_tps).
dl_determiner([many | T], T, vc_sp).
dl_determiner([several | T], T, vc_sp).
dl_determiner([some | T], T, vc_sp).
dl_determiner([quite, a, few | T], T, vc_sp).

% adjectives are either none or a list of adjectives
% usage:
%	dl_adjectives(T, T1)
%		where T, T1 is a difference list
:- discontiguous dl_adjective/2.
dl_adjectives(T, T).
dl_adjectives(T, T2) :- dl_adjective(T, [and | T1]), dl_adjective(T1, T2).
dl_adjectives(T, T2) :- dl_adjective(T, T1), dl_adjectives(T1, T2).
dl_adjective([W | T], T) :- adjective(W).
dl_adjective([W1, W2 | T], T) :- adjective_adverb(W1), adjective(W2).
dl_adjective([W1, W2 | T], T) :- adverb(W1), adjective(W2).

% a noun is a noun
% usage:
%	dl_noun(T, T1, Conj)
%		where T, T1 is a difference list and Conj is the standard atom for verb conjugation (see dl_verb usage)
:- discontiguous dl_noun/3.

dl_noun([W | T], T, vc_tps) :- noun(W, _).
dl_noun([W | T], T, vc_sp) :- noun(_, W).

% a modphrase is a total dud for now
dl_modphrase(T, T, _).

% a verb phrase can be a single verb
dl_verbphrase(T, T2, Conj) :- dl_verb_be(T, T1, Conj), dl_adjectives(T1, T2).
dl_verbphrase(T, T1, Conj) :- dl_verb(T, T1, Conj).
dl_verbphrase(T, T2, Conj) :- dl_verb(T, T1, Conj), dl_nounphrase(T1, T2, Conj).
dl_verbphrase(T, T3, Conj) :- dl_adverb(T, T1), dl_verb(T1, T2, Conj), dl_nounphrase(T2, T3, Conj).
dl_verbphrase(T, T3, Conj) :- dl_verb(T, T1, Conj), dl_adverb(T1, T2), dl_nounphrase(T2, T3, Conj).
dl_verbphrase(T, T4, Conj) :- dl_adverb(T, T1), dl_verb(T1, T2, Conj), dl_adverb(T2, T3), dl_nounphrase(T3, T4, Conj).

% an adverb is an adverb
dl_adverb([W | T], T) :- adverb(W).

% a verb is a verb with a given conjugation
% usage:
%	dl_verb(T, T1, conj)
%		where T, T1 is a difference list and conj denotes conjugation and is one of the following atoms:
%
% standard atoms for verb conjugation:
% vc_inf	-> infinitive (to 'be')
% vc_fp		-> first person	(I 'am')
% vc_sp		-> second person (you 'are')
%				- used for they and we
% vc_tps	-> third person singular (it 'is')
%				- used for he, she and it
:- discontiguous dl_verb/3.

dl_verb([W | T], T, vc_inf) :- verb(W, _, _, _).
dl_verb([W | T], T, vc_fp) :- verb(_, W, _, _).
dl_verb([W | T], T, vc_sp) :- verb(_, _, W, _).
dl_verb([W | T], T, vc_tps) :- verb(_, _, _, W).

% 'do' and 'be' have special usage
dl_verb_do([do | T], T, vc_inf).
dl_verb_do([do | T], T, vc_fp).
dl_verb_do([do | T], T, vc_sp).
dl_verb_do([does | T], T, vc_tps).

dl_verb_be([be | T], T, vc_inf).
dl_verb_be([am | T], T, vc_fp).
dl_verb_be([are | T], T, vc_sp).
dl_verb_be([is | T], T, vc_tps).

% End Language Processing -----------------------------------------

% Database Searching ----------------------------------------------

knownthing([a|T],T2, S,S1) :-
	props(S,S1, T,T1),
	type(S,S1, T1,T2).
	
thingcanbetype([a|T],T1, S,S1) :- 
	inherits([a|T],T1, S,S1).
	
thingcanbetype([a|T],T2, S,S1) :- 
	s_typeprops(S,S1, T,T1),
	inherits(T1,T2, S,S1).
	
thingcanbetype([a|T],T2, S,S1) :- 
	s_typeprops(S,S1, T,T1),
	inherits(S,S1, T1,T2).
	
% s_typeprops(T,T1, P,P1) is true if a thing of type T,T1 has all the properties in P,P1
s_typeprops(_,_, P,P).
s_typeprops(T,T1, P,P2) :- typeprop(T,T1, P,P1), s_typeprops(T,T1, P1,P2).
s_typeprops(T,T1, P,P2) :- typeprop(T,T1, P,[and|P1]), typeprop(T,T1, P1,P2).

% End Database Searching ------------------------------------------
