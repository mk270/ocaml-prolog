% -*- prolog -*-

% DCG for Quenya phonotaxis

% vowel length rules

%word --> initial, vowel, final.

word(Start, End) :- disyllable(Start, End).
word(Start, End) :- trisyllable(Start, End).

disyllable(Start, End) :- 
	initial(Start, S1), 
	vowel(S1, S2), 
	medial_cluster(S2, S3),
	vowel(S3, S4),
	final(S4, End).

trisyllable(Start, End) :-
	initial(Start, S1), 
	vowel(S1, S2), 
	medial_cluster(S2, S3),
	vowel(S3, S4),
	medial_cluster(S4, S5),
	vowel(S5, S6),
	final(S6, End).




vowel([i|X], X).
vowel([e|X], X).
vowel([a|X], X).
vowel([o|X], X).
vowel([u|X], X).

final([l|X], X).
final([n|X], X).
final([r|X], X).
final([s|X], X).
final([t|X], X).

initial([p|X], X).
initial([t|X], X).
initial([c|X], X).
initial([f|X], X).
initial([th|X], X).
initial([s|X], X).
initial([h|X], X).
initial([hy|X], X).
initial([hw|X], X).
initial([m|X], X).
initial([n|X], X).
initial([ng|X], X).
initial([v|X], X).
initial([l|X], X).
initial([r|X], X).
initial([y|X], X).
initial([w|X], X).
initial([hl|X], X).
initial([hr|X], X).

initial(S1, S2) :- initial_group(S1, S2).

% permitted initial groups, apparently

initial_group([ty|X], X).
initial_group([ny|X], X).
initial_group([ly|X], X).
initial_group([qu|X], X).
initial_group([ngw|X], X).



medial_cluster([ht|X], X).
medial_cluster([lc|X], X).
medial_cluster([lb|X], X).
medial_cluster([ld|X], X).
medial_cluster([ll|X], X).
medial_cluster([lm|X], X).
medial_cluster([lp|X], X).
medial_cluster([lqu|X], X).
medial_cluster([lt|X], X).
medial_cluster([lv|X], X).
medial_cluster([lw|X], X).
medial_cluster([mb|X], X).
medial_cluster([mm|X], X).
medial_cluster([mn|X], X).
medial_cluster([mp|X], X).
medial_cluster([nc|X], X).
medial_cluster([nd|X], X).
medial_cluster([ndy|X], X).
medial_cluster([ng|X], X).
medial_cluster([nn|X], X).
medial_cluster([nqu|X], X).
medial_cluster([nt|X], X).
medial_cluster([nty|X], X).
medial_cluster([pp|X], X).
medial_cluster([ps|X], X).
medial_cluster([pt|X], X).
medial_cluster([rc|X], X).
medial_cluster([rd|X], X).
medial_cluster([rm|X], X).
medial_cluster([rn|X], X).
medial_cluster([rp|X], X).
medial_cluster([rqu|X], X).
medial_cluster([rr|X], X).
medial_cluster([rs|X], X).
medial_cluster([rt|X], X).
medial_cluster([rv|X], X).
medial_cluster([rw|X], X).
medial_cluster([rth|X], X).
medial_cluster([ss|X], X).
medial_cluster([sc|X], X).
medial_cluster([sp|X], X).
medial_cluster([squ|X], X).
medial_cluster([st|X], X).
medial_cluster([sty|X], X).
medial_cluster([sw|X], X).
medial_cluster([ts|X], X).
medial_cluster([tt|X], X).
medial_cluster([tw|X], X).
medial_cluster([v|X], X).
medial_cluster([w|X], X).
medial_cluster(S1, S2) :- initial_group(S1, S2).
