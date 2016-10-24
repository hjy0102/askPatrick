% Interface ------------------------------------------

% the main querying function
% usage:
%	askPatrick([...], R). or askPatrick(Q, R), readln(Q).
%		where ... is a list of words

% askPatrick(Q, [thats, a, good, question]) :- dl_question(Q, []).
% askPatrick(Q, [thats, a, strange, question]) :- \+ dl_question(Q, []).

% askPatrick(Q, A) :- filter(Q, R), answer(R, A).


% To get the input from a line:

askPatrick(Ans) :-
write("Ask Patrick: "),
readln(Ln),
dl_question(Ln, []),
write("That's a good question"),
answer(
member(End,[[],['?'],['.']]).

askPatrick(Ans) :-
write("Ask Patrick: "),
readln(Ln),
\+ dl_question(Ln, []),
write("That's a strange question"),
member(End,[[],['?'],['.']]).

/*
?- askPatrick(Ans).
Ask me: who is a tall student enrolled in a computer science course?
Ans = mary ;
Ans = john ;
false.
*/


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

% what is a thing that's like this?
answer([what|Q], R) :- dl_verb_be(Q,Q1, vc_tps),
thingcanbetype(Q1,[], P,[]),
type(I,[], P,[]),
dl_append(I,[], Q,Q1, Q1,[], R).

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
% |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
% Dictionary ------------------------------------------------------

% list of known adjectives
adjective(alive).
adjective(intelligent).
adjective(good).
adjective(male).
adjective(female).
adjective(evil).
adjective(bigoted).
adjective(lying).
adjective(old).
adjective(beautiful).
adjective(wise).
adjective(young).
adjective(helpful).
adjective(pink).
adjective(wet).
adjective(naked).
adjective(sentient).
adjective(mortal).
adjective(fleshy).
adjective(bony).
adjective(adult).
adjective(loud).
adjective(small).
adjective(needy).
adjective(mechanical).
adjective(useful).
adjective(heavy).
adjective(motorized).
adjective(electric).
adjective(metal).
adjective(magic).
adjective(moving).
adjective(big).
adjective(yellow).
adjective(white).
adjective(black).
adjective(grey).
adjective(red).
adjective(blue).
adjective(green).
adjective(edible).
adjective(sour).
adjective(delicious).
adjective(sweet).
adjective(mushy).
adjective(pink).
adjective(healthy).
adjective(brown).
adjective(pungent).

dl_adjective([man,made|T],T).
dl_adjective([self,aware|T],T).
dl_adjective([turing,complete|T],T).

% list of known adverbs
adverb(slightly).
adverb(heavily).
adverb(adoringly).
adverb(tightly).
adverb(graciously).
adverb(ingraciously).
adverb(haphazardly).
adverb(carefully).

% list of known adjective-adverbs, herein used as meaning an adverb that modulates
% the severity of an adjective but doesn't quite fit in front of a verb
adjective_adverb(highly).
adjective_adverb(so).
adjective_adverb(extremely).
adjective_adverb(really).
adjective_adverb(slightly).
adjective_adverb(very).
adjective_adverb(quite).
adjective_adverb(surprisingly).
adjective_adverb(somewhat).

% list of known nouns
% noun(Singular, Plural)
noun(thing, things).
noun(person, people).
noun(smell, smell).
noun(human, humans).
noun(man, men).
noun(woman, women).
noun(mom, moms).
noun(dad, dads).
noun(child, children).
noun(boy, boys).
noun(girl, girls).
noun(baby, babies).
noun(tool, tools).
noun(hammer, hammers).
noun(drill, drills).
noun(computer, computers).
noun(vehicle, vehicles).
noun(bus, buses).
noun(car, cars).
noun(train, trains).
noun(sailboat, sailboats).
noun(food, food).
noun(fruit, fruits).
noun(banana, bananas).
noun(apple, apples).
noun(lemon, lemons).
noun(lime, limes).
noun(orange, oranges).
noun(grapefruit, grapefruits).
noun(berry, berries).
noun(blueberry, blueberries).
noun(blackberry, blackberries).
noun(raspberry, raspberries).
noun(salmonberry, salmonberries).
noun(vegetable, vegetables).
noun(potato, potatoes).
noun(onion, onions).
noun(cucumber, cucumbers).
noun(yam, yams).
noun(squash, squashes).
noun(lunch, lunches).
noun(student, students).
noun(professor, professor).

dl_noun([citrus,fruit|T],T, vc_tps).
dl_noun([citrus,fruits|T],T, vc_sp).

dl_noun([teaching,assistant|T],T, vc_tps).
dl_noun([teaching,assistants|T],T, vc_sp).

% list of known proper nouns
% propernoun(T, T1)
%	where T, T1 is a difference list containing the proper noun
propernoun([donald, trump | T], T).
propernoun([justin, trudeau| T], T).
propernoun([david, poole| T], T).
propernoun([tim| T], T).
propernoun([ginny| T], T).
propernoun([rui| T], T).
propernoun([julin| T], T).
propernoun([patrick | T], T).

% list of known verbs
%	 verb(Inf, FP, SP, TPS) is verb with the following conjugations
%		Inf	->	infinitive, ie 'be'
%		FP	-> first person, ie 'am'
%		SP	-> second person, ie 'are'
%		TPS	-> third person singular, ie 'is'
% second person is used for you, they, and we
% third person singular is used for he, she, it
% when writing, think: "to ___, I ___, you ___, he/she ___" where '___' denotes the conjugated verb
% many verbs have the same form for infinitive, first person and second person conjugations
% for this there is the binary form of verb:
%	verb(Inf, TPS)
% which maps to the quaternary verb predicate as:
%	verb(Inf, Inf, Inf, TPS)

verb(Inf, Inf, Inf, TPS) :- verb(Inf, TPS).

verb(be, am, are, is).
verb(make, makes).
verb(create, creates).
verb(like, likes).
verb(want, wants).
verb(walk, walks).
verb(think, thinks).
verb(scratch, scratches).
verb(lick, licks).
verb(bite, bites).
verb(ring, rings).
verb(laugh, laughs).
verb(shout, shouts).
verb(scream, screams).
verb(blow, blows).
verb(knock, knocks).
verb(drink, drinks).
verb(eat, eats).
verb(chew, chews).
verb(work, works).
verb(feel, feels).

dl_verb([go,away|T],T, vc_inf).
dl_verb([go,away|T],T, vc_fp).
dl_verb([go,away|T],T, vc_sp).
dl_verb([goes,away|T],T, vc_tps).

% End Dictionary --------------------------------------------------
% |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
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
% |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
% Database --------------------------------------------------------

% maketype(I,I1, T,T1) means the individual I,I1 is defined as being of type T,T1
maketype([patrick|T],T, [person|S],S).
maketype([patrick|T],T, [computer|S],S).
maketype([donald,trump|T],T, [man|S],S).
maketype([hillary,clinton|T],T, [man|S],S).
maketype([justin,trudeau|T],T, [man|S],S).
maketype([david,poole|T],T, [man|S],S).
maketype([david,poole|T],T, [professor|S],S).
maketype([tim|T],T, [man|S],S).
maketype([tim|T],T, [student|S],S).
maketype([ginnie|T],T, [woman|S],S).
maketype([ginnie|T],T, [student|S],S).
maketype([rui|T],T, [human|S],S).
maketype([julin|T],T, [human|S],S).
maketype([peter|T],T, [human|S],S).
maketype([rui|T],T, [teaching,assistant|S],S).
maketype([julin|T],T, [teaching,assistant|S],S).
maketype([peter|T],T, [teaching,assistant|S],S).

maketype([this,baby,i,found|T],T, [baby|S],S).

maketype([my,lunch|T],T, [lemon|S],S).

% prop(I,I1, P,P1) means that the individual I,I1 has the property(P,P1).
prop([patrick|T],T, [living|S],S).
prop([patrick|T],T, [intelligent|S],S).
prop([patrick|T],T, [good|S],S).
prop([patrick|T],T, [male|S],S).
prop([patrick|T],T, [female|S],S).

prop([donald, trump|T],T, [evil|S],S).
prop([donald, trump|T],T, [bigoted|S],S).
prop([donald, trump|T],T, [lying|S],S).
prop([donald, trump|T],T, [old|S],S).

prop([hillary, clinton|T],T, [evil|S],S).
prop([hillary, clinton|T],T, [old|S],S).

prop([justin,trudeau|T],T, [beautiful|S],S).
prop([justin,trudeau|T],T, [intelligent|S],S).

prop([david,poole|T],T, [wise|S],S).
prop([david,poole|T],T, [intelligent|S],S).

prop([tim|T],T, [young|S],S).
prop([ginnie|T],T, [young|S],S).
prop([rui|T],T, [helpful|S],S).
prop([julin|T],T, [helpful|S],S).
prop([peter|T],T, [helpful|S],S).

prop([this,baby,i,found|T],T, [pink|S],S).
prop([this,baby,i,found|T],T, [wet|S],S).
prop([this,baby,i,found|T],T, [naked|S],S).

prop(I,I1, P,P1) :- typeprop(T,T1, P,P1), type(I,I1, T,T1).

% props(I,I1, P,P1) is true if the individual I,I1 has all the properties in P,P1
props(_,_, P,P).
props(I,I1, P,P2) :- prop(I,I1, P,P1), props(I,I1, P1,P2).
props(I,I1, P,P2) :- prop(I,I1, P,[and|P1]), prop(I,I1, P1,P2).

% typeprop(S,S1, R,R1) means that things of type S,S1 must have property R,R1
typeprop([person|S],S, [sentient|R],R).
typeprop([person|S],S, [self,aware|R],R).

typeprop([human|S],S, [mortal|R],R).
typeprop([human|S],S, [fleshy|R],R).
typeprop([human|S],S, [bony|R],R).
typeprop([human|S],S, [living|R],R).

typeprop([man|S],S, [male|R],R).
typeprop([man|S],S, [adult|R],R).
typeprop([woman|S],S, [female|R],R).
typeprop([woman|S],S, [adult|R],R).

typeprop([boy|S],S, [male|R],R).
typeprop([boy|S],S, [young|R],R).
typeprop([girl|S],S, [female|R],R).
typeprop([girl|S],S, [young|R],R).

typeprop([baby|S],S, [loud|R],R).
typeprop([baby|S],S, [small|R],R).
typeprop([baby|S],S, [young|R],R).
typeprop([baby|S],S, [needy|R],R).

typeprop([tool|S],S, [mechanical|R],R).
typeprop([tool|S],S, [useful|R],R).
typeprop([tool|S],S, [man,made|R],R).

typeprop([hammer|S],S, [heavy|R],R).
typeprop([machine|S],S, [motorized|R],R).
typeprop([angle,grinder|S],S, [loud|R],R).
typeprop([drill|S],S, [loud|R],R).
typeprop([drill|S],S, [electric|R],R).
typeprop([computer|S],S, [digital|R],R).
typeprop([computer|S],S, [electronic|R],R).
typeprop([computer|S],S, [electric|R],R).
typeprop([computer|S],S, [metal|R],R).
typeprop([computer|S],S, [turing,complete|R],R).
typeprop([computer|S],S, [magic|R],R).
typeprop([vehicle|S],S, [moving|R],R).
typeprop([vehicle|S],S, [big|R],R).
typeprop([bus|S],S, [loud|R],R).
typeprop([bus|S],S, [metal|R],R).
typeprop([bus|S],S, [yellow|R],R).
typeprop([car|S],S, [loud|R],R).
typeprop([car|S],S, [metal|R],R).
typeprop([car|S],S, [white|R],R).
typeprop([car|S],S, [black|R],R).
typeprop([car|S],S, [grey|R],R).
typeprop([car|S],S, [red|R],R).
typeprop([car|S],S, [blue|R],R).
typeprop([car|S],S, [yellow|R],R).
typeprop([car|S],S, [green|R],R).
typeprop([train|S],S, [loud|R],R).
typeprop([train|S],S, [magic|R],R).
typeprop([train|S],S, [metal|R],R).
typeprop([sailboat|S],S, [magic|R],R).
typeprop([sailboat|S],S, [beautiful|R],R).
typeprop([sailboat|S],S, [white|R],R).

typeprop([food|S],S, [edible|R],R).

typeprop([fruit|S],S, [small|R],R).
typeprop([citrus,fruit|S],S, [sour|R],R).
typeprop([lemon|S],S, [yellow|R],R).
typeprop([lemon|S],S, [delicious|R],R).
typeprop([lemon|S],S, [magic|R],R).
typeprop([lemon|S],S, [useful|R],R).
typeprop([lime|S],S, [green|R],R).
typeprop([orange|S],S, [orange|R],R).
typeprop([orange|S],S, [sweet|R],R).
typeprop([orange|S],S, [delicious|R],R).
typeprop([grapefruit|S],S, [orange|R],R).
typeprop([grapefruit|S],S, [sweet|R],R).
typeprop([grapefruit|S],S, [delicious|R],R).
typeprop([banana|S],S, [mushy|R],R).
typeprop([banana|S],S, [yellow|R],R).
typeprop([apple|S],S, [red|R],R).
typeprop([apple|S],S, [green|R],R).
typeprop([apple|S],S, [pink|R],R).
typeprop([apple|S],S, [yellow|R],R).
typeprop([apple|S],S, [delicious|R],R).

typeprop([vegetable|S],S, [healthy|R],R).
typeprop([potato|S],S, [delicious|R],R).
typeprop([potato|S],S, [yellow|R],R).
typeprop([potato|S],S, [red|R],R).
typeprop([potato|S],S, [brown|R],R).
typeprop([onion|S],S, [pungent|R],R).
typeprop([onion|S],S, [white|R],R).
typeprop([onion|S],S, [brown|R],R).
typeprop([onion|S],S, [red|R],R).
typeprop([cucumber|S],S, [green|R],R).
typeprop([yam|S],S, [delicious|R],R).
typeprop([yam|S],S, [orange|R],R).
typeprop([yam|S],S, [brown|R],R).
typeprop([squash|S],S, [orange|R],R).
typeprop([squash|S],S, [green|R],R).
typeprop([squash|S],S, [yellow|R],R).

typeprop(T,T1, P,P1) :- inherits(T,T1, R,R1), typeprop(R,R1, P,P1).

% extends(T,T1, S,S1) means that type T,T1 directly extends type S,S1
extends([person|T],T, [thing|S],S).
extends([student|T],T, [person|S],S).
extends([professor|T],T, [person|S],S).
extends([teaching,assistant|T],T, [person|S],S).
extends([human|T],T, [person|S],S).
extends([man|T],T, [human|S],S).
extends([woman|T],T, [human|S],S).
extends([boy|T],T, [human|S],S).
extends([girl|T],T, [human|S],S).
extends([baby|T],T, [human|S],S).

extends([tool|T],T, [thing|S],S).
extends([hammer|T],T, [tool|S],S).
extends([machine|T],T, [tool|S],S).
extends([angle,grinder|T],T, [machine|S],S).
extends([drill|T],T, [machine|S],S).
extends([computer|T],T, [machine|S],S).
extends([vehicle|T],T, [machine|S],S).
extends([bus|T],T, [vehicle|S],S).
extends([car|T],T, [vehicle|S],S).
extends([train|T],T, [vehicle|S],S).
extends([sailboat|T],T, [vehicle|S],S).

extends([food|T],T, [thing|S],S).

extends([fruit|T],T, [food|S],S).
extends([banana|T],T, [fruit|S],S).
extends([citrus,fruit|T],T, [fruit|S],S).
extends([apple|T],T, [fruit|S],S).
extends([lemon|T],T, [citrus,fruit|S],S).
extends([lime|T],T, [citrus,fruit|S],S).
extends([orange|T],T, [citrus,fruit|S],S).
extends([grapefruit|T],T, [citrus,fruit|S],S).
extends([berry|T],T, [fruit|S],S).
extends([blueberry|T],T, [berry|S],S).
extends([blackberry|T],T, [berry|S],S).
extends([raspberry|T],T, [berry|S],S).
extends([salmonberry|T],T, [berry|S],S).

extends([vegetable|T],T, [food|S],S).
extends([potato|T],T, [vegetable|S],S).
extends([onion|T],T, [vegetable|S],S).
extends([cucumber|T],T, [vegetable|S],S).
extends([yam|T],T, [vegetable|S],S).
extends([squash|T],T, [vegetable|S],S).

% types are inheritable.
type(I,I1, T,T1) :- maketype(I,I1, T,T1).
type(I,I1, T,T1) :- maketype(I,I1, S,S1), inherits(S,S1, T,T1).
inherits(T,T1, S,S1) :- extends(T,T1, S,S1).
inherits(T,T1, S,S1) :- extends(T,T1, R,[]), inherits(R,[], S,S1).

% End Database -----------------------------------------------------
