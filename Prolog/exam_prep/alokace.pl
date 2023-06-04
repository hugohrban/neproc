alokace([], Ob, [], Ob).
alokace([M|MS], Obsazeno, [Where|RW], NoveObsazeno) :-
    add(M, Obsazeno, TObs, Where),
    alokace(MS, TObs, RW, TT),
    smooth(TT,NoveObsazeno).



% velkost toho co pridavam, stav pamate, stav pamate po pridani
add(Size, [], [0-Size],Size-0).
add(Size, [F-FL], [F-UP],Size-Where) :-
    Where is F+FL,
    UP is FL+Size.
add(Size,[F-FL|[S-SL|Rest]],Updated, Size-Where) :-
    EF is F+FL+Size,
    ( EF =< S ->                            % ak sa zmesti medzi prve a druhe
        XD is EF - F,                       
        Where is F+FL,
        Updated = [F-XD|[S-SL|Rest]]
    ;
        add(Size, [S-SL|Rest], Up, Size-Where),
        Updated = [F-FL|Up]
    ).


smooth([],[]).
smooth([F-FL],[F-FL]).
smooth([F-FL|[S-SL|Rest]],U) :-
    EF is F + FL,
    (EF == S ->                 % if end of first equals start of second
        SE is FL + SL,          % length of merged part
        LeftUp = F-SE,          % merge first two
        smooth(Rest,RightUp),   % smooth rest
        U = [LeftUp|RightUp]    % put them together
    ; 
        smooth([S-SL|Rest],RU),   % otherwise try smoothing rest
        U = [F-FL|RU]
    ).


