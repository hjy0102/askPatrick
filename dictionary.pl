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