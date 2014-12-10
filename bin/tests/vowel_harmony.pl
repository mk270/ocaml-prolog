% -*- prolog -*-

% DCG for Quenya-like phonotaxis, with vowel harmony

% vowel length rules

%word --> initial, vowel, final.

word(Start, End) :- disyllable(Start, End).
word(Start, End) :- trisyllable(Start, End).

disyllable(Start, End) :- 
	initial(Start, S1), 
	vowel(HarmonyGroup, S1, S2), 
	medial_cluster(S2, S3),
	vowel(HarmonyGroup, S3, S4),
	final(S4, End).

trisyllable(Start, End) :-
	initial(Start, S1), 
	vowel(HarmonyGroup, S1, S2), 
	medial_cluster(S2, S3),
	vowel(HarmonyGroup, S3, S4),
	medial_cluster(S4, S5),
	vowel(HarmonyGroup, S5, S6),
	final(S6, End).




vowel(front, [i|X], X).
vowel(front, [e|X], X).
vowel(back, [a|X], X).
vowel(back, [o|X], X).
vowel(back, [u|X], X).

final([l|X], X).
final([n|X], X).
final([r|X], X).
final([s|X], X).
final([t|X], X).

initial([p|X], X).
initial([t|X], X).
initial([c|X], X).
initial([f|X], X).
initial([s|X], X).
initial([h|X], X).
initial([m|X], X).
initial([n|X], X).
initial([v|X], X).
initial([l|X], X).
initial([r|X], X).
initial([y|X], X).

initial(S1, S2) :- initial_group(S1, S2).

% permitted initial groups, apparently

initial_group([ty|X], X).
initial_group([ny|X], X).
initial_group([ly|X], X).



medial_cluster([lb|X], X).
medial_cluster([ld|X], X).
medial_cluster([ll|X], X).
medial_cluster([lm|X], X).
medial_cluster([lp|X], X).
medial_cluster([nd|X], X).
medial_cluster([rp|X], X).
medial_cluster([rs|X], X).
medial_cluster([rt|X], X).
medial_cluster([rv|X], X).
medial_cluster([sp|X], X).
medial_cluster(S1, S2) :- initial_group(S1, S2).
