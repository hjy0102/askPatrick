% ToDo:
% - verb conjugation
% - 

% Interface ------------------------------------------

% the main querying function, to be used as: askPatrick([...], R). or askPatrick(Q, R), readln(Q).

askPatrick(Q, [yes]) :- dl_question(Q, []).
askPatrick(Q, [thats, strange]) :- \+ dl_question(Q, []).

% End Interface ------------------------------------------

% Language Processing ------------------------------------------

% a question can be 'what' followed by a verb phrase

dl_question([what | T], T1) :- dl_verbphrase(T, T1).

% a nounphrase consists of an article, optional adjectives, a noun and an optional modifying phrase
dl_nounphrase(T0, T4) :-
	dl_article(T0, T1),
	dl_adjectives(T1, T2),
	dl_noun(T2, T3),
	dl_modphrase(T3, T4).
	
% an article is the, this, a, an, etc.
dl_article(T, T).
dl_article([the | T], T).
dl_article([this | T], T).
dl_article([that | T], T).
dl_article([a | T], T).
dl_article([an | T], T).

% adjectives are either none or a list of adjectives
dl_adjectives(T, T).
dl_adjectives(T0, T2) :- dl_adjective(T0, T1), dl_adjectives(T1, T2).
dl_adjective([W | T], T) :- adjective(W).
dl_adjective([W1, W2 | T], T) :- adjective_adverb(W1), adjective(W2).
dl_adjective([W1, W2 | T], T) :- adverb(W1), adjective(W2).

% a noun is a noun
dl_noun([W | T], T) :- noun(W).

% a modphrase is a total dud for now
dl_modphrase(T, T).

% a verb phrase can be a single verb
dl_verbphrase(T, T1) :- dl_verb(T, T1).

% a verb phrase can be a verb followed by a noun phrase
dl_verbphrase(T, T2) :- dl_verb(T, T1), dl_nounphrase(T1, T2).

% a verb phrase can be an adverb and a verb followed by a noun phrase
dl_verbphrase(T, T3) :- dl_adverb(T, T1), dl_verb(T1, T2), dl_nounphrase(T2, T3).

% a verb phrase can be a verb and an adverb followed by a noun phrase
dl_verbphrase(T, T3) :- dl_verb(T, T1), dl_adverb(T1, T2), dl_nounphrase(T2, T3).

% a verb phrase can be an adverb and a verb and an adverb followed by a noun phrase
dl_verbphrase(T, T4) :- dl_adverb(T, T1), dl_verb(T1, T2), dl_adverb(T2, T3), dl_nounphrase(T3, T4).

% an adverb is an adverb
dl_adverb([W | T], T) :- adverb(W).

% a verb is a verb
dl_verb([W | T], T) :- verb(W).

% End Language Processing ------------------------------------------


% Dictionary ------------------------------------------

% list of known adjectives
adjective(big).
adjective(small).
adjective(hairy).
adjective(bald).
adjective(shaven).
adjective(bashful).
adjective(strong).
adjective(menial).
adjective(repetitive).
adjective(alcoholic).
adjective(tiny).
adjective(lost).
adjective(meaty).
adjective(wet).
adjective(sizzling).
adjective(spanking).

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
adjective_adverb(extremely).
adjective_adverb(slightly).
adjective_adverb(very).
adjective_adverb(quite).
adjective_adverb(surprisingly).
adjective_adverb(somewhat).

% list of known nouns
noun(dog).
noun(cat).
noun(person).
noun(fetus).
noun(womb).
noun(dingus).
noun(kangaroo).
noun(lemon).
noun(apple).
noun(machine).
noun(robot).
noun(computer).
noun(human).
noun(lynx).
noun(crypt).
noun(abyss).
noun(lagoon).
noun(pit).

% list of known verbs

verb(is).
verb(walks).
verb(thinks).
verb(scratches).
verb(licks).
verb(bites).
verb(rings).
verb(laughs).
verb(shouts).
verb(screams).
verb(blows).
verb(knocks).
verb(drinks).
verb(eats).
verb(chews).

% End Dictionary ------------------------------------------