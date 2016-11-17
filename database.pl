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
