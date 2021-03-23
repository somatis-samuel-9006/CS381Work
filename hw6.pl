% Group members:
%  * No, group, done by Samuel Somatis, OSU id 933339006
%  * Name, ID
%  * Name, ID
%
% Grading notes: 10pts total
%  * Part 1: 6pts (1pt each)
%  * Part 2: 4pts (3pts for cmd, 1pt for prog)


% Part 1. It's a bird-eat-bug world out there!

% A small database of animals. Each relation gives the animal's name,
% it's habitat, and its biological class.
animal(cranefly, trees, insects).
animal(duck, ponds, birds).
animal(minnow, ponds, fish).
animal(scrubjay, trees, birds).
animal(squirrel, trees, mammals).
animal(waterstrider, ponds, insects).
animal(woodpecker, trees, birds).

% A small database capturing what each animal eats. Note that most animals eat
% more than one kind of food, but craneflies don't eat anything after they
% reach adulthood!
diet(scrubjay, insects).
diet(scrubjay, seeds).
diet(squirrel, nuts).
diet(squirrel, seeds).
diet(duck, algae).
diet(duck, fish).
diet(duck, insects).
diet(minnow, algae).
diet(minnow, insects).
diet(waterstrider, insects).
diet(woodpecker, insects).

% A binary predicate that includes all of the animals and where they live.
habitat(Animal, Where) :- animal(Animal, Where, _).

% A binary predicate that includes each animal and its biological class.
class(Animal, Class) :- animal(Animal, _, Class).


% 1. Define a predicate neighbor/2 that determines whether two animals live
%    in the same habitat. Note that two animals of the same kind always
%    live in the same habitat.
neighbor(Animal1, Animal2) :- habitat(Animal1, Place), habitat(Animal2, Place).

% 2. Define a predicate related/2 that includes all pairs of animals that
%    are in the same biological class but are not the same kind of animal.
related(Animal1, Animal2) :- class(Animal1, Class), class(Animal2, Class), Animal1 \= Animal2.

% 3. Define a predicate competitor/3 that includes two kinds of animals and
%    the food they compete for. Two animals are competitors if they live in
%    the same place and eat the same food.
competitor(Animal1, Animal2, Food) :- neighbor(Animal1, Animal2), diet(Animal1, Food), diet(Animal2, Food).

% 4. Define a predicate would_eat/2 that includes all pairs of animals where
%    the first animal would eat the second animal (because the second animal
%    is a kind of food it eats), if it could.
would_eat(Predator, Prey) :- diet(Predator, Food), class(Prey, Food).

% 5. Define a predicate does_eat/2 that includes all pairs of animals where
%    the first animal would eat the second, and both animals live in the same
%    place, so it probably does.
does_eat(Predator, Prey) :- would_eat(Predator, Prey), neighbor(Predator, Prey).

% 6. Define a predicate cannibal/1 that includes all animals that might eat
%    their own kind--eek!
cannibal(Animal) :- does_eat(Animal, Animal).



% Part 2. Implementing a stack language

% A slightly larger example program to use in testing.
example(P) :-
  P = [ 2, 3, 4, lte,            % [tru, 2]
        if([5, 6, add], [fls]),  % [11, 2]
        3, swap,                 % [11, 3, 2]
        4, 5, add,               % [9, 11, 3, 2]
        lte, if([tru], [mul]),   % [6]
        "whew!", swap,           % [6, "whew!"]
        "the answer is" ].       % ["the answer is", 6, "whew!"]

% 1. Define the predicate `cmd/3`.
cmd(C, L1, [C|L1]) :- string(C).
cmd(C, L1, [C|L1]) :- number(C).
cmd(tru, L1, [tru|L1]).
cmd(fls, L1, [fls|L1]).
cmd(dup, [H|L1], [H,H|L1]).
cmd(swap, [H,H2|L1], [H2,H|L1]).
cmd(add, [H,H2|L1], [R|L1]) :- R is H + H2.
cmd(mul, [H,H2|L1], [R|L1]) :- R is H * H2.
cmd(lte, [H,H2|L1], [tru|L1]) :- H >= H2.
cmd(lte, [H,H2|L1], [fls|L1]) :- H < H2.
cmd(if(P1,_), [tru|L1], L2) :- prog(P1, L1, L2).
cmd(if(_,P2), [fls|L1], L2) :- prog(P2, L1, L2).



% 2. Define the predicate `prog/3`.
prog([], L, L).
prog([H|T], L1, L2) :- cmd(H, L1, L3), prog(T, L3, L2).
