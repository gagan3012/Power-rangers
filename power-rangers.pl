/* Power Rangers SPD, by Gagan Bhatia. */

:- use_module(library(process)).
:- use_module(library(ansi_term)).
:- dynamic player_health/1, player_special/2, player_ranger/1.

% Enemy stats: enemy_stats(EnemyName, MaxHP, BaseAttack)
enemy_stats(krybot,         15, 15).
enemy_stats(bluehead,       30, 20).
enemy_stats(orangehead,     60, 30).
enemy_stats(salimoht,      150, 25).
enemy_stats(rhinix,        350, 35).
enemy_stats(ringbah_robot, 400, 40).
enemy_stats(detagor_robot, 300, 40).
enemy_stats(scimatu_robot, 350, 45).
enemy_stats(drew,          350, 30).
enemy_stats(drew_giant,    550, 45).
enemy_stats(fake_benaag,   500, 35).
enemy_stats(benaag_robot,  500, 100).
enemy_stats(benaag,        750, 100).

ranger_color(red, red).
ranger_color(blue, blue).
ranger_color(green, green).
ranger_color(yellow, yellow).
ranger_color(pink, magenta).  
ranger_color(shadow, cyan).

colored_write(Color, Text) :-
    ansi_format([fg(Color)], Text, []).
colored_writeln(Color, Text) :-
    ansi_format([fg(Color)], Text, []),
    nl.
color_format(Color, Format, Args) :-
    ansi_format([fg(Color)], Format, Args).

colored_format(Color, Format, Args) :-
    ansi_format([fg(Color)], Format, Args).

start :-
    nl,
    colored_writeln(cyan, '============================================'),
    ansi_format([fg(white), bold], ' Welcome to Power Rangers SPD!~n', []),
    colored_writeln(cyan, '============================================'),
    play_sound_for_5_seconds,
    introduction,
    instructions,
    choose_ranger,
    play_all_episodes,
    colored_writeln(green, 'Thank you rangers for saving the world.'),
    nl,
    halt.

help :-
    nl,
    colored_writeln(white, '==================== HELP ===================='),
    colored_writeln(white, 'You can enter the following commands anytime:'),
    colored_writeln(white, 'Movement/Action Commands:'),
    colored_writeln(white, '  double_jump, wall_jump, fire_laser, forward_roll, jump, punch_beam, run_fast, track, catch, collect'),
    colored_writeln(white, 'Battle Commands:'),
    colored_writeln(white, '  attack, defend, special, dodge'),
    colored_writeln(white, 'Other Commands:'),
    colored_writeln(white, '  help   (Display this help message)'),
    colored_writeln(white, '=============================================='),
    nl.

introduction :- 
    colored_writeln(white, 'In this game, you step into the shoes of a mighty Ranger,'),
    colored_writeln(white, 'charged with protecting the Earth from dangerous enemies.'),

    colored_writeln(blue, 'Health Points (HP):'),
    colored_writeln(white, '  You begin with 100 HP, representing your life force.'),
    colored_writeln(white, '  Every enemy attack reduces your HP, and if it reaches zero, it is game over!'),

    colored_writeln(green, 'Attack:'),
    colored_writeln(white, '  Use your attack to deal random damage to enemies,'),
    colored_writeln(white, '  chipping away at their HP with every strike.'),

    colored_writeln(yellow, 'Defend:'),
    colored_writeln(white, '  Taking a defensive stance halves the damage you receive,'),
    colored_writeln(white, '  helping you survive even the toughest hits.'),

    colored_writeln(magenta, 'Special:'),
    colored_writeln(white, '  As you progress, you collect powerful special attacks.'),
    colored_writeln(white, '  These are stored in your inventory and appear as unique moves with counts,'),
    colored_writeln(white, '  e.g., "Emerald Glimmer (Bonus: 40) x 3".'),
    colored_writeln(white, '  When you use one, a copy is consumed and extra damage is dealt based on its bonus value.'),

    colored_writeln(red, 'Dodge:'),
    colored_writeln(white, '  Dodge enemy attacks entirely if timed well,'),
    colored_writeln(white, '  though it is a risky move that requires precision.'),

    colored_writeln(cyan, 'Plan your moves wisely, manage your HP carefully,'),
    colored_writeln(cyan, 'and unleash your special attacks to turn the tide of battle!'),
    colored_writeln(white, 'Are you ready to save the world? Let the battle begin!'),
    episode0, pause.


instructions :-
    nl,
    colored_writeln(white, 'Available commands during missions:'),
    colored_writeln(white, '  Movement/Action commands: double_jump, wall_jump, fire_laser,'),
    colored_writeln(white, '         forward_roll, jump, punch_beam, run_fast, track, catch, collect'),
    colored_writeln(white, '  Battle commands (during fights): attack, defend, special, dodge'),
    nl,
    colored_writeln(white, 'When prompted "Enter action:" type one of the commands (followed by a period).'),
    nl.

quit :-
    nl,
    colored_writeln(white, 'Exiting Power Rangers SPD...'),
    colored_writeln(white, 'Thank you for playing!'),
    halt.

choose_ranger :-
    nl,
    colored_writeln(white, 'Choose your Ranger:'),
    colored_writeln(red, '  1. Red Ranger      (Physical: Wall Jump; Special: Invisible)'),
    colored_writeln(blue, '  2. Blue Ranger     (Physical: Double Jump; Special: Forcefield)'),
    colored_writeln(green, '  3. Green Ranger    (Physical: Wall Jump; Special: Aura Tracking)'),
    colored_writeln(yellow, '  4. Yellow Ranger   (Physical: Forward Roll; Special: Duplicate)'),
    colored_writeln(magenta, '  5. Pink Ranger     (Physical: Forward Roll; Special: Stone Punches)'),
    colored_writeln(cyan, '  6. Shadow Ranger   (Physical: All abilities; Special: Sword Strike)'),
    colored_write(white, 'Enter the number of your choice: '),
    read(Choice),
    ranger_from_choice(Choice, Ranger),
    init_player(Ranger),
    ranger_color(Ranger, Color),
    ansi_format([fg(Color), bold], 'You selected ~w Ranger.~n', [Ranger]).

ranger_from_choice(1, red).
ranger_from_choice(2, blue).
ranger_from_choice(3, green).
ranger_from_choice(4, yellow).
ranger_from_choice(5, pink).
ranger_from_choice(6, shadow).
ranger_from_choice(_, red).  % default

init_player(Ranger) :-
    retractall(player_health(_)),
    retractall(player_special(_, _)),
    retractall(player_ranger(_)),
    retractall(player_specials(_)),
    assert(player_specials([])),
    assert(player_health(100)),
    ( \+ player_special(_, _) ->
         assert(player_special(none, 0))
    ; true ),
    assert(player_ranger(Ranger)).

play_sound_for_5_seconds :-
    nl, write('Playing the SPD theme song...  (Volume On)'), nl,
    process_create(path(python),
        ['-c', "import winsound, time; \c
                winsound.PlaySound('spd_song.wav', winsound.SND_FILENAME | winsound.SND_ASYNC | winsound.SND_LOOP); \c
                time.sleep(5); \c
                winsound.PlaySound(None, winsound.SND_PURGE)"],
        [process(PID)]),
    process_wait(PID, exit(0)).

play_all_episodes :-
    episode1, reset_health, choose_ranger_prompt,
    episode2, reset_health, choose_ranger_prompt,
    episode3, reset_health, choose_ranger_prompt,
    episode4, reset_health, choose_ranger_prompt,
    episode5, reset_health, choose_ranger_prompt,
    episode6, reset_health, choose_ranger_prompt,
    episode7, reset_health, choose_ranger_prompt,
    episode8, reset_health.

reset_health :-
    retractall(player_health(_)),
    % retractall(player_special(_, _)),
    assert(player_health(100)).
    % ( player_special(_, _) -> true ; assert(player_special(none, 0)) ).

reset_special :-
    retractall(player_specials(_)),
    assert(player_specials([])).

choose_ranger_prompt :-
    nl,
    colored_write(white, 'Would you like to change your ranger for the next episode? (yes/no): '),
    read(Answer),
    ( Answer == yes ->
         choose_ranger
    ; Answer == no ->
         true
    ; colored_writeln(white, 'Invalid response. Keeping current ranger.'), nl
    ).

pause :-
    nl, colored_writeln(cyan, 'Let\'s go...'), nl.

current_ranger(R) :-
    player_ranger(R).

% --- Episode 0 ---
episode0 :-
    nl, colored_writeln(blue, 'Episode 0 - Introduction'),
    colored_writeln(blue, 'Gruumm: Destroy the planet Nelandia! It is useless now that I have drained its resources dry.'),
    colored_writeln(blue, 'Gruumm: I have found a new target... Earth!'),
    colored_writeln(blue, 'Emperor Grumm has ordered the destruction of several planets and Earth is next!'),
    colored_writeln(blue, 'The new breed of Power Rangers: S.P.D. must rise to the challenge.').

% --- Episode 1 ---
episode1 :-
    nl, colored_writeln(blue, 'Episode 1 - Beginnings'),
    nl, colored_writeln(blue, 'Doggie Cruger: SPD emergency! An abnormal energy reading has been detected matching Gruumm!'),
    nl, colored_writeln(blue, 'Doggie Cruger: Rangers, you must stop the enemy forces and protect the city!'),
    current_ranger(R),
    ( R = blue   -> mission1_blue
    ; R = red    -> mission1_red
    ; R = green  -> mission1_green
    ; R = yellow -> mission1_yellow
    ; R = pink   -> mission1_pink
    ; R = shadow -> mission1_shadow
    ),
    colored_writeln(green, 'Episode 1 complete!'), nl.

mission1_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Blue Ranger, can you hear me? Krybots have invaded the warehouse!'),
    colored_writeln(blue, 'Use your double jump to clear an obstacle.'),
    colored_write(white, 'Type "double_jump." to perform your move: '),
    prompt_action(double_jump, Result),
    ( Result = success ->
         colored_writeln(green, 'You soared over the gap!'),
         battle([krybot, krybot, krybot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Krybots defeated!')
         ; colored_writeln(red, 'You were overwhelmed... Game Over.'), halt )
    ; colored_writeln(red, 'You failed the double jump. Try again.'), nl, mission1_blue ).

mission1_red :-
    nl, colored_writeln(white, 'Red Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Red Ranger, the enemy has set up a barrier!'),
    colored_writeln(blue, 'A wall blocks your path. Use "wall_jump." to scale it: '),
    prompt_action(wall_jump, R),
    ( R = success ->
         colored_writeln(green, 'You vaulted over the wall!'),
         colored_writeln(blue, 'Now, activate your special ability by typing "use_special." to become invisible and ambush the enemy.'),
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([bluehead, krybot], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: Bluehead defeated!')
              ; colored_writeln(red, 'You lost the fight... Game Over.'), halt )
         ; colored_writeln(red, 'Special move failed. Try again.'), nl, mission1_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission1_red ).

mission1_green :-
    nl, colored_writeln(white, 'Green Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Green Ranger, the enemy is hiding in the shadows!'),
    colored_writeln(blue, 'Use your aura tracking to locate hidden enemies.'),
    colored_write(white, 'Type "track." to locate your foe: '),
    prompt_action(track, R),
    ( R = success ->
         colored_writeln(green, 'Enemy located!'),
         battle([krybot, krybot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Green Ranger: Enemy defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Tracking failed. Try again.'), nl, mission1_green ).

mission1_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Yellow Ranger, evade the enemy with your forward roll!'),
    colored_writeln(blue, 'Evade danger with a forward roll.'),
    colored_write(white, 'Type "forward_roll." to roll past the enemy: '),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You rolled past the enemy!'),
         battle([krybot, krybot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Enemy defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission1_yellow ).

mission1_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Pink Ranger, break through the barrier with your stone punches!'),
    colored_writeln(blue, 'Break through barriers with your stone punches.'),
    colored_write(white, 'Type "punch_beam." to break through a barrier: '),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Barrier broken!'),
         battle([krybot, krybot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Enemy defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Your punch missed. Try again.'), nl, mission1_pink ).

mission1_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission:'),
    colored_writeln(blue, 'Doggie Cruger: Shadow Ranger, the enemy is strong, but you are stronger!'),
    colored_writeln(blue, 'Combine your skills! Execute "double_jump.", then "wall_jump.", then "use_special." in sequence.'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Shadow Ranger: Perfect combo executed!'),
         battle([bluehead, krybot, krybot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: Foes defeated!')
         ; colored_writeln(red, 'You were overwhelmed... Game Over.'), halt )
    ; colored_writeln(red, 'Your combo failed. Try again.'), nl, mission1_shadow ).

% --- Episode 2 ---
episode2 :-
    nl, colored_writeln(blue, 'Episode 2 - Confronted'),
    current_ranger(R),
    ( R = blue   -> mission2_blue
    ; R = red    -> mission2_red
    ; R = green  -> mission2_green
    ; R = yellow -> mission2_yellow
    ; R = pink   -> mission2_pink
    ; R = shadow -> mission2_shadow
    ),
    colored_writeln(green, 'Episode 2 complete!'), nl.

mission2_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission:'),
    colored_writeln(blue, 'Stop incoming missiles with your laser.'),
    colored_writeln(blue, 'Type "fire_laser." to shoot a missile.'),
    prompt_action(fire_laser, R),
    ( R = success ->
         colored_writeln(green, 'Missile destroyed!'),
         battle([bluehead, salimoht], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Salimoht defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Missile shot failed. Try again.'), nl, mission2_blue ).

mission2_red :-
    nl, colored_writeln(white, 'Red Ranger Mission:'),
    colored_writeln(blue, 'Blast through obstacles with your special ability to defeat Salimoht.'),
    colored_writeln(blue, 'Type "use_special." to activate your power: '),
    prompt_action(use_special, R),
    ( R = success ->
         battle([salimoht, bluehead, salimoht], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Salimoht defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Special ability failed. Try again.'), nl, mission2_red ).

mission2_green :-
    nl, colored_writeln(white, 'Green Ranger Mission:'),
    colored_writeln(blue, 'Use your tracking ability to locate hidden enemies.'),
    colored_writeln(blue, 'Type "track." to find them: '),
    prompt_action(track, R),
    ( R = success ->
         battle([krybot, krybot, bluehead, salimoht], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Enemies defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Tracking failed. Try again.'), nl, mission2_green ).

mission2_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission:'),
    colored_writeln(blue, 'Evade danger with your forward roll.'),
    colored_writeln(blue, 'Type "forward_roll." to roll past danger: '),
    prompt_action(forward_roll, R),
    ( R = success ->
         battle([krybot, bluehead, salimoht], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Enemy defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission2_yellow ).

mission2_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission:'),
    colored_writeln(blue, 'Break obstacles with your stone punches.'),
    colored_writeln(blue, 'Type "punch_beam." to break through: '),
    prompt_action(punch_beam, R),
    ( R = success ->
         battle([bluehead, bluehead, salimoht], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Bluehead defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Punch failed. Try again.'), nl, mission2_pink ).

mission2_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission:'),
    colored_writeln(blue, 'Combine your skills in a combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         battle([bluehead, salimoht, bluehead], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Foes defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission2_shadow ).

% --- Episode 3 ---
episode3 :-
    nl, colored_writeln(blue, 'Episode 3 - Dogged'),
    current_ranger(R),
    ( R = blue   -> mission3_blue
    ; R = red    -> mission3_red
    ; R = green  -> mission3_green
    ; R = yellow -> mission3_yellow
    ; R = pink   -> mission3_pink
    ; R = shadow -> mission3_shadow
    ),
    colored_writeln(green, 'Episode 3 complete!'), nl.

mission3_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: Sprint to the Finish'),
    colored_writeln(blue, 'Race against time! Type "run_fast." to advance quickly.'),
    prompt_action(run_fast, Result),
    ( Result = success ->
         colored_writeln(green, 'You dash forward with incredible speed!'),
         battle([krybot, krybot, rhinix], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Enemies defeated during your sprint!')
         ; colored_writeln(red, 'You were caught by the enemies... Game Over.'), halt )
    ; colored_writeln(red, 'You hesitated! Try again.'), nl, mission3_blue ).

mission3_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Wall Jump Challenge'),
    colored_writeln(blue, 'A series of high walls block your path. Type "wall_jump." to scale them.'),
    prompt_action(wall_jump, R),
    ( R = success ->
         colored_format(yellow, 'After your action, ~w HP is now ~w~n', [bluehead, _]),  
         colored_writeln(green, 'You gracefully leap over the walls!'),
         battle([bluehead, rhinix], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Red Ranger: Bluehead defeated!')
         ; colored_writeln(red, 'You fell in battle... Game Over.'), halt )
    ; colored_writeln(red, 'Your jump failed. Try again.'), nl, mission3_red ).

mission3_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Collect the Energy Orbs'),
    colored_writeln(blue, 'Collect 5 energy orbs by typing "collect." for each orb.'),
    collect_items(5),
    battle([krybot, rhinix], _Dummy, Outcome),
    ( Outcome = win -> colored_writeln(green, 'Green Ranger: Orbs collected and enemy defeated!')
    ; colored_writeln(red, 'An enemy ambushed you... Game Over.'), halt ).

mission3_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Forward Roll Escape'),
    colored_writeln(blue, 'Perform a forward roll to escape danger. Type "forward_roll."'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You executed a perfect roll!'),
         battle([krybot, rhinix], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Enemy defeated during your escape!')
         ; colored_writeln(red, 'You were caught... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission3_yellow ).

mission3_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Break Through Barriers'),
    colored_writeln(blue, 'Use your stone punches to break through the barrier. Type "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Barrier shattered!'),
         battle([bluehead, rhinix], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Bluehead defeated!')
         ; colored_writeln(red, 'You were overwhelmed... Game Over.'), halt )
    ; colored_writeln(red, 'Your punch did not connect. Try again.'), nl, mission3_pink ).

mission3_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Ultimate Combo Challenge'),
    colored_writeln(blue, 'Execute a combo: type "double_jump.", then "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Combo successful!'),
         battle([bluehead, krybot, rhinix], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: Foes defeated with your deadly combo!')
         ; colored_writeln(red, 'Your combo failed in battle... Game Over.'), halt )
    ; colored_writeln(red, 'Your combo did not work. Try again.'), nl, mission3_shadow ).

% --- Episode 4 ---
episode4 :-
    nl, colored_writeln(blue, 'Episode 4 - Walls'),
    current_ranger(R),
    ( R = blue   -> mission4_blue
    ; R = red    -> mission4_red
    ; R = green  -> mission4_green
    ; R = yellow -> mission4_yellow
    ; R = pink   -> mission4_pink
    ; R = shadow -> mission4_shadow
    ),
    colored_writeln(green, 'Episode 4 complete!'), nl.

mission4_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: Urban Leap'),
    colored_writeln(blue, 'Leap across rooftops by typing "jump."'),
    prompt_action(jump, R),
    ( R = success ->
         colored_writeln(green, 'You leap gracefully across the rooftops!'),
         battle([krybot, krybot, ringbah_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: You secured the urban area!')
         ; colored_writeln(red, 'You fell in battle... Game Over.'), halt )
    ; colored_writeln(red, 'Jump failed. Try again.'), nl, mission4_blue ).

mission4_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Scale the Fortress'),
    colored_writeln(blue, 'Use "wall_jump." to scale the fortress walls.'),
    prompt_action(wall_jump, R),
    ( R = success ->
         colored_writeln(green, 'You scaled the walls! Now, activate your special ability by typing "use_special."'),
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([bluehead, ringbah_robot], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: Bluehead defeated atop the fortress!')
              ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
         ; colored_writeln(red, 'Special move failed. Try again.'), nl, mission4_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission4_red ).

mission4_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Maze of Vines'),
    colored_writeln(blue, 'Collect 5 diamonds hidden in the maze by typing "collect."'),
    collect_items(5),
    battle([krybot, ringbah_robot], _Dummy, Outcome),
    ( Outcome = win -> colored_writeln(green, 'Green Ranger: Maze cleared and enemy defeated!')
    ; colored_writeln(red, 'An enemy caught you... Game Over.'), halt ).

mission4_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Quick Roll Chase'),
    colored_writeln(blue, 'Chase down the fleeing enemy using your forward roll. Type "forward_roll."'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You rolled after the enemy!'),
         battle([krybot, ringbah_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: You caught and defeated the enemy!')
         ; colored_writeln(red, 'You were overtaken... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission4_yellow ).

mission4_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Smash the Barrier'),
    colored_writeln(blue, 'Break the barrier with your stone punches. Type "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'The barrier crumbles!'),
         battle([bluehead, ringbah_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: You defeated Bluehead!')
         ; colored_writeln(red, 'Defeated in combat... Game Over.'), halt )
    ; colored_writeln(red, 'Your punch missed. Try again.'), nl, mission4_pink ).

mission4_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Shadow Run'),
    colored_writeln(blue, 'Execute a combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Your shadow moves are flawless!'),
         battle([ringbah_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: Ringbah Robot defeated!')
         ; colored_writeln(red, 'Your shadow faltered... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission4_shadow ).

% --- Episode 5 ---
episode5 :-
    nl, colored_writeln(blue, 'Episode 5 - Sam'),
    current_ranger(R),
    ( R = blue   -> mission5_blue
    ; R = red    -> mission5_red
    ; R = green  -> mission5_green
    ; R = yellow -> mission5_yellow
    ; R = pink   -> mission5_pink
    ; R = shadow -> mission5_shadow
    ),
    colored_writeln(green, 'Episode 5 complete!'), nl.

mission5_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: Aerial Recon'),
    colored_writeln(blue, 'Use "double_jump." to navigate through enemy fire.'),
    prompt_action(double_jump, R),
    ( R = success ->
         colored_format(green, 'You attack dealing ~w damage!~n', [0]),  % Damage details placeholder
         colored_writeln(green, 'You maneuver through the air!'),
         battle([krybot, krybot, scimatu_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Recon successful, enemies neutralized!')
         ; colored_writeln(red, 'You were hit... Game Over.'), halt )
    ; colored_writeln(red, 'Double jump failed. Try again.'), nl, mission5_blue ).

mission5_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Forest Pursuit'),
    colored_writeln(blue, 'Use "wall_jump." to navigate through the forest and then "use_special." to ambush your foe.'),
    prompt_action(wall_jump, R),
    ( R = success ->
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([scimatu_robot], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: rhinix ambushed and defeated!')
              ; colored_writeln(red, 'You fell in battle... Game Over.'), halt )
         ; colored_writeln(red, 'Special ability failed. Try again.'), nl, mission5_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission5_red ).

mission5_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Rescue Operation'),
    colored_writeln(blue, 'Collect 5 items to rescue civilians by typing "collect."'),
    collect_items(5),
    battle([scimatu_robot], _Dummy, Outcome),
    ( Outcome = win -> colored_writeln(green, 'Green Ranger: Civilians rescued and enemy defeated!')
    ; colored_writeln(red, 'Rescue failed... Game Over.'), halt ).

mission5_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Speed Run'),
    colored_writeln(blue, 'Use "forward_roll." to outrun your foes.'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You dash ahead swiftly!'),
         battle([scimatu_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Foe caught and defeated!')
         ; colored_writeln(red, 'You were overtaken... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission5_yellow ).

mission5_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Power Punch'),
    colored_writeln(blue, 'Break obstacles with your stone punches. Type "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Obstacle shattered!'),
         battle([orangehead], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Orangehead defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Your punch missed. Try again.'), nl, mission5_pink ).

mission5_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Stealth Strike'),
    colored_writeln(blue, 'Execute a combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Stealth strike successful!'),
         battle([scimatu_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: Detagor’s Robot neutralized!')
         ; colored_writeln(red, 'Your stealth failed... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission5_shadow ).

% --- Episode 6 ---
episode6 :-
    nl, colored_writeln(blue, 'Episode 6 - Stakeout'),
    current_ranger(R),
    ( R = blue   -> mission6_blue
    ; R = red    -> mission6_red
    ; R = green  -> mission6_green
    ; R = yellow -> mission6_yellow
    ; R = pink   -> mission6_pink
    ; R = shadow -> mission6_shadow
    ),
    colored_writeln(green, 'Episode 6 complete!'), nl.

mission6_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: Lab Infiltration'),
    colored_writeln(blue, 'Disable security systems by typing "fire_laser."'),
    prompt_action(fire_laser, R),
    ( R = success ->
         colored_writeln(green, 'Laser fired successfully!'),
         battle([krybot, krybot, drew], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Lab infiltrated and secured!')
         ; colored_writeln(red, 'Mission failed... Game Over.'), halt )
    ; colored_writeln(red, 'Laser failed. Try again.'), nl, mission6_blue ).

mission6_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Rooftop Rescue'),
    colored_writeln(blue, 'Traverse rooftops with "wall_jump."'),
    prompt_action(wall_jump, R),
    ( R = success ->
         colored_writeln(blue, 'You reached the rooftop! Activate your special ability ("use_special.") to rescue the hostage.'),
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([bluehead, drew], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: Hostage rescued, enemy defeated!')
              ; colored_writeln(red, 'Rescue failed... Game Over.'), halt )
         ; colored_writeln(red, 'Special ability failed. Try again.'), nl, mission6_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission6_red ).

mission6_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Hidden Maze'),
    colored_writeln(blue, 'Use your tracking ability ("track.") to locate hidden enemies.'),
    prompt_action(track, R),
    ( R = success ->
         colored_writeln(green, 'Enemies located!'),
         battle([krybot, krybot, drew], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Green Ranger: Maze cleared and enemies defeated!')
         ; colored_writeln(red, 'You were overwhelmed... Game Over.'), halt )
    ; colored_writeln(red, 'Tracking failed. Try again.'), nl, mission6_green ).

mission6_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Roll Out'),
    colored_writeln(blue, 'Use "forward_roll." to dodge obstacles and enemies.'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You rolled out with finesse!'),
         battle([krybot, drew], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Enemy neutralized!')
         ; colored_writeln(red, 'You got caught... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission6_yellow ).

mission6_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Beam Breaker'),
    colored_writeln(blue, 'Break through barriers with "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Barrier broken!'),
         battle([bluehead, drew], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Lab secured!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Punch missed. Try again.'), nl, mission6_pink ).

mission6_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Silent Assassination'),
    colored_writeln(blue, 'Execute a combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Your silent strike is flawless!'),
         battle([drew, drew_giant], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: rhinix eliminated!')
         ; colored_writeln(red, 'Your strike failed... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission6_shadow ).

% --- Episode 7 ---
episode7 :-
    nl, colored_writeln(blue, 'Episode 7 - Idol'),
    current_ranger(R),
    ( R = blue   -> mission7_blue
    ; R = red    -> mission7_red
    ; R = green  -> mission7_green
    ; R = yellow -> mission7_yellow
    ; R = pink   -> mission7_pink
    ; R = shadow -> mission7_shadow
    ),
    colored_writeln(green, 'Episode 7 complete!'), nl.

mission7_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: City Chase'),
    colored_writeln(blue, 'Navigate the city by jumping over obstacles. Type "jump."'),
    prompt_action(jump, R),
    ( R = success ->
         colored_writeln(green, 'You leap over urban obstacles!'),
         battle([bluehead, krybot, fake_benaag], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: City secured!')
         ; colored_writeln(red, 'You were defeated in the chase... Game Over.'), halt )
    ; colored_writeln(red, 'Jump failed. Try again.'), nl, mission7_blue ).

mission7_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Infiltration'),
    colored_writeln(blue, 'Use "wall_jump." then "use_special." inside the building to take down enemies.'),
    prompt_action(wall_jump, R),
    ( R = success ->
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([orangehead, krybot, fake_benaag], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: Building cleared!')
              ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
         ; colored_writeln(red, 'Special move failed. Try again.'), nl, mission7_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission7_red ).

mission7_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Sniper Hunt'),
    colored_writeln(blue, 'Use your tracking ability ("track.") to locate the sniper, then "catch." to apprehend him.'),
    prompt_action(track, R),
    ( R = success ->
         colored_writeln(blue, 'Sniper located! Now catch him by typing "catch."'),
         prompt_action(catch, R2),
         ( R2 = success ->
              colored_writeln(green, 'Sniper caught!'),
              battle([fake_benaag], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Green Ranger: Sniper subdued!')
              ; colored_writeln(red, 'The sniper escaped... Game Over.'), halt )
         ; colored_writeln(red, 'Failed to catch the sniper. Try again.'), nl, mission7_green )
    ; colored_writeln(red, 'Tracking failed. Try again.'), nl, mission7_green ).

mission7_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Quick Escape'),
    colored_writeln(blue, 'Use "forward_roll." to dodge enemies in the building.'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You rolled through the chaos!'),
         battle([krybot, krybot, fake_benaag], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Building cleared!')
         ; colored_writeln(red, 'Enemies overwhelmed you... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission7_yellow ).

mission7_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Break the Guard'),
    colored_writeln(blue, 'Break through the security door with your stone punches. Type "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Door shattered!'),
         battle([bluehead, fake_benaag], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Guard neutralized!')
         ; colored_writeln(red, 'You were caught... Game Over.'), halt )
    ; colored_writeln(red, 'Punch missed. Try again.'), nl, mission7_pink ).

mission7_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Final Ambush'),
    colored_writeln(blue, 'Execute a combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Ambush executed flawlessly!'),
         battle([fake_benaag], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: Drew Giant subdued!')
         ; colored_writeln(red, 'Ambush failed... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission7_shadow ).

% --- Episode 8 ---
episode8 :-
    nl, colored_writeln(blue, 'Episode 8 - Shadow'),
    current_ranger(R),
    ( R = blue   -> mission8_blue
    ; R = red    -> mission8_red
    ; R = green  -> mission8_green
    ; R = yellow -> mission8_yellow
    ; R = pink   -> mission8_pink
    ; R = shadow -> mission8_shadow
    ),
    colored_writeln(green, 'Episode 8 complete!'), nl.

mission8_blue :-
    nl, colored_writeln(white, 'Blue Ranger Mission: Rooftop Rumble'),
    colored_writeln(blue, 'Navigate rooftops by typing "jump."'),
    prompt_action(jump, R),
    ( R = success ->
         colored_writeln(green, 'You traverse the rooftops effortlessly!'),
         battle([krybot, krybot, bluehead, benaag_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Blue Ranger: Rooftop area secured!')
         ; colored_writeln(red, 'You fell... Game Over.'), halt )
    ; colored_writeln(red, 'Jump failed. Try again.'), nl, mission8_blue ).

mission8_red :-
    nl, colored_writeln(white, 'Red Ranger Mission: Chasing Shadows'),
    colored_writeln(blue, 'Use "wall_jump." then "use_special." to chase down the suspect.'),
    prompt_action(wall_jump, R),
    ( R = success ->
         prompt_action(use_special, R2),
         ( R2 = success ->
              battle([benaag_robot], _Dummy, Outcome),
              ( Outcome = win -> colored_writeln(green, 'Red Ranger: Fake General Benaag defeated!')
              ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
         ; colored_writeln(red, 'Special move failed. Try again.'), nl, mission8_red )
    ; colored_writeln(red, 'Wall jump failed. Try again.'), nl, mission8_red ).

mission8_green :-
    nl, colored_writeln(white, 'Green Ranger Mission: Maze Rescue'),
    colored_writeln(blue, 'Rescue 5 civilians by collecting them. Type "collect."'),
    collect_items(5),
    battle([krybot, benaag_robot], _Dummy, Outcome),
    ( Outcome = win -> colored_writeln(green, 'Green Ranger: Civilians rescued, enemy defeated!')
    ; colored_writeln(red, 'Rescue failed... Game Over.'), halt ).

mission8_yellow :-
    nl, colored_writeln(white, 'Yellow Ranger Mission: Multi-Story Assault'),
    colored_writeln(blue, 'Clear enemies across multiple floors. Use "forward_roll." to move between floors.'),
    prompt_action(forward_roll, R),
    ( R = success ->
         colored_writeln(green, 'You dash through the floors!'),
         battle([krybot, benaag_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Yellow Ranger: Enemies cleared!')
         ; colored_writeln(red, 'You were overwhelmed... Game Over.'), halt )
    ; colored_writeln(red, 'Roll failed. Try again.'), nl, mission8_yellow ).

mission8_pink :-
    nl, colored_writeln(white, 'Pink Ranger Mission: Heavy Hitter'),
    colored_writeln(blue, 'Break heavy barriers with "punch_beam."'),
    prompt_action(punch_beam, R),
    ( R = success ->
         colored_writeln(green, 'Barrier broken!'),
         battle([benaag_robot], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Pink Ranger: Enemy defeated!')
         ; colored_writeln(red, 'You were defeated... Game Over.'), halt )
    ; colored_writeln(red, 'Punch missed. Try again.'), nl, mission8_pink ).

mission8_shadow :-
    nl, colored_writeln(white, 'Shadow Ranger Mission: Ultimate Showdown'),
    colored_writeln(blue, 'Execute your ultimate combo: type "double_jump.", "wall_jump.", then "use_special."'),
    prompt_action(double_jump, R1),
    prompt_action(wall_jump, R2),
    prompt_action(use_special, R3),
    ( R1 = success, R2 = success, R3 = success ->
         colored_writeln(green, 'Your ultimate combo is unstoppable!'),
         battle([benaag], _Dummy, Outcome),
         ( Outcome = win -> colored_writeln(green, 'Shadow Ranger: General Benaag defeated!')
         ; colored_writeln(red, 'The ultimate showdown failed... Game Over.'), halt )
    ; colored_writeln(red, 'Combo failed. Try again.'), nl, mission8_shadow ).

% --- Battle Mechanics ---

battle(EnemyList, _Dummy, Outcome) :-
    cool_battle(EnemyList, Outcome).

cool_battle([], win).
cool_battle([EnemyName|Rest], Outcome) :-
    enemy_stats(EnemyName, EnemyHP, EnemyAttack),
    nl,
    colored_format(magenta, 'A wild ~w appears! (HP: ~w, Attack: ~w)~n', [EnemyName, EnemyHP, EnemyAttack]),
    battle_loop(EnemyName, EnemyHP, EnemyAttack, BattleOutcome),
    ( BattleOutcome = win ->
         cool_battle(Rest, Outcome)
    ; Outcome = lose ).

battle_loop(EnemyName, EnemyHP, EnemyAttack, Outcome) :-
    player_health(PlayerHP),
    ( PlayerHP =< 0 ->
         Outcome = lose,
         colored_writeln(red, 'You have been defeated!')
    ; EnemyHP =< 0 ->
         colored_format(green, 'You defeated ~w!~n', [EnemyName]),
         % Award a special attack based on the defeated enemy:
         award_special(EnemyName),
         Outcome = win
    ; 
         colored_format(yellow, 'Your HP: ~w, ~w HP: ~w~n', [PlayerHP, EnemyName, EnemyHP]),
         colored_write(white, 'Choose your action (attack/defend/special/dodge): '),
         prompt_attack(PlayerAction),
         player_turn(PlayerAction, EnemyName, EnemyHP, EnemyAttack, NewEnemyHP, Modifier),
         colored_format(yellow, 'After your action, ~w HP is now ~w~n', [EnemyName, NewEnemyHP]),
         enemy_turn(EnemyName, EnemyAttack, Modifier, DamageToPlayer),
         bonus_for_action(Modifier, Bonus),
         update_player_health(DamageToPlayer, Bonus, NewPlayerHP),
         colored_format(red, 'Enemy attacks! You take ~w damage but gain ~w bonus HP. Your HP is now ~w~n', [DamageToPlayer, Bonus, NewPlayerHP]),
         ( NewPlayerHP =< 0 ->
              Outcome = lose,
              colored_writeln(red, 'You have been defeated!'), nl
         ; 
              retract(player_health(PlayerHP)),
              assert(player_health(NewPlayerHP)),
              battle_loop(EnemyName, NewEnemyHP, EnemyAttack, Outcome)
         )
    ).

player_turn(attack, _EnemyName, EnemyHP, _EnemyAttack, NewEnemyHP, normal) :-
    random_between(15, 25, Damage),
    colored_format(green, 'You attack dealing ~w damage!~n', [Damage]),
    NewEnemyHP is EnemyHP - Damage.
player_turn(defend, _EnemyName, EnemyHP, _EnemyAttack, EnemyHP, defend) :-
    colored_writeln(white, 'You take a defensive stance.').

player_turn(special, _EnemyName, EnemyHP, _EnemyAttack, NewEnemyHP, normal) :-
    player_specials(SList),
    ( SList == [] ->
         colored_writeln(white, 'No special attacks available! You lose your turn.'),
         NewEnemyHP = EnemyHP
    ;
         unique_specials(SList, UniqueList),
         list_unique_specials(UniqueList, 1),
         colored_write(white, 'Choose your special attack by number: '),
         prompt_attack(Index),
         ( integer(Index),
           nth1(Index, UniqueList, special(SpecialName, Bonus, _Count)) ->
               select(special(SpecialName, Bonus), SList, NewSList),
               retract(player_specials(_)),
               assert(player_specials(NewSList)),
               BonusNew is Bonus + 10,
               random_between(Bonus, BonusNew, Damage),
               colored_format(green, 'You unleash your special attack ~w dealing ~w damage!~n', [SpecialName, Damage]),
               NewEnemyHP is EnemyHP - Damage,
               colored_writeln(green, 'You have used your special attack!')
         ; 
               colored_writeln(red, 'Invalid choice! You lose your turn.'),
               NewEnemyHP = EnemyHP
         )
    ).

player_turn(dodge, _EnemyName, EnemyHP, _EnemyAttack, EnemyHP, Modifier) :-
    random_between(0, 1, X),
    ( X =:= 1 ->
         Modifier = dodge_success,
         colored_writeln(green, 'You successfully dodged this turn!')
    ;  Modifier = dodge_fail,
         colored_writeln(red, 'Your dodge failed!')
    ).
player_turn(_, _EnemyName, EnemyHP, _EnemyAttack, EnemyHP, none) :-
    colored_format(red, 'Invalid action, you lose your turn.', []).

unique_specials(SList, UniqueList) :-
    sort(SList, Sorted),  
    findall(special(Name, Bonus, Count),
            ( member(special(Name, Bonus), Sorted),
              include(==(special(Name, Bonus)), SList, Occurrences),
              length(Occurrences, Count)
            ),
            UniqueList).

list_unique_specials([], _).
list_unique_specials([special(Name, Bonus, Count)|Rest], Index) :-
    ( Count > 1 ->
         colored_format(cyan, '~w. ~w (Bonus: ~w) x ~w~n', [Index, Name, Bonus, Count])
    ; 
         colored_format(cyan, '~w. ~w (Bonus: ~w)~n', [Index, Name, Bonus])
    ),
    NextIndex is Index + 1,
    list_unique_specials(Rest, NextIndex).

enemy_turn(_EnemyName, EnemyAttack, Modifier, Damage) :-
    MinDamage is EnemyAttack - 5,
    MaxDamage is EnemyAttack + 5,
    random_between(MinDamage, MaxDamage, BaseDamage),
    ( Modifier = defend ->
         Damage is BaseDamage // 2
    ; Modifier = dodge_success ->
         Damage = 0
    ; Modifier = dodge_fail ->
         Damage = BaseDamage
    ; Damage = BaseDamage
    ).

update_player_health(Damage, Bonus, NewHP) :-
    player_health(CurrentHP),
    TempHP is CurrentHP - Damage + Bonus,
    ( TempHP > 100 ->
         NewHP = 100
    ; NewHP = TempHP
    ).

bonus_for_action(defend, 25).
bonus_for_action(dodge_success, 10).
bonus_for_action(_, 0).

prompt_action(Expected, Outcome) :-
    colored_write(white, 'Enter action (help. for instructions/quit. to end the game): '),
    read(Action),
    ( Action == help ->
         help,
         prompt_action(Expected, Outcome)
    ; Action == quit -> 
         quit
    ; Action == Expected ->
         Outcome = success
    ; Outcome = fail
    ).

prompt_attack(Action) :-
    read(Input),
    ( Input = help ->
         help, prompt_attack(Action)
    ; Input = quit ->
         quit
    ; Action = Input
    ).

collect_items(0) :-
    colored_writeln(green, 'All items collected!'), nl.
collect_items(N) :-
    N > 0,
    format('Collect an item (~w remaining). Type "collect."~n', [N]),
    prompt_action(collect, Result),
    ( Result = success ->
         N1 is N - 1,
         collect_items(N1)
    ; colored_writeln(red, 'Item collection failed. Try again.'), nl,
      collect_items(N)
    ).


award_special(EnemyName) :-
    enemy_stats(EnemyName, EHP, EAttack),
    Total is EHP + EAttack,
    determine_special(Total, SpecialName, Bonus),
    colored_format(cyan, 'Defeated enemy dropped a special attack: ~w (Bonus: ~w)!~n', [SpecialName, Bonus]),
    pick_special(SpecialName, Bonus).

pick_special(SpecialName, Bonus) :-
    player_specials(CurrentList),
    NewList = [special(SpecialName, Bonus)|CurrentList],
    retract(player_specials(CurrentList)),
    assert(player_specials(NewList)),
    colored_format(green, 'Special attack ~w (Bonus: ~w) added to your inventory!~n', [SpecialName, Bonus]),
    retractall(player_special(_,_)),
    assert(player_special(SpecialName, Bonus)),
    colored_format(green, 'Your new special attack ~w (Bonus: ~w) is now equipped automatically!~n', [SpecialName, Bonus]).

pick_special(SpecialName) :-
    special_bonus(SpecialName, Bonus),
    pick_special(SpecialName, Bonus).


determine_special(Total, SpecialName, Bonus) :-
    player_ranger(Ranger),
    ( Ranger = red    -> determine_special_red(Total, SpecialName, Bonus)
    ; Ranger = blue   -> determine_special_blue(Total, SpecialName, Bonus)
    ; Ranger = green  -> determine_special_green(Total, SpecialName, Bonus)
    ; Ranger = yellow -> determine_special_yellow(Total, SpecialName, Bonus)
    ; Ranger = pink   -> determine_special_pink(Total, SpecialName, Bonus)
    ; Ranger = shadow -> determine_special_shadow(Total, SpecialName, Bonus)
    ;  % Fallback in case no ranger is set:
       determine_special_default(Total, SpecialName, Bonus)
    ).

% --- Red Ranger Specials ---
determine_special_red(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Crimson Spark',     Bonus = 40
    ; Total < 80  -> SpecialName = 'Invisible Slash',    Bonus = 80
    ; Total < 120 -> SpecialName = 'Ruby Burst',         Bonus = 120
    ; Total < 160 -> SpecialName = 'Crimson Fury',       Bonus = 160
    ; Total < 200 -> SpecialName = 'Bloodstorm',         Bonus = 200
    ; Total < 250 -> SpecialName = 'Red Vortex',         Bonus = 250
    ; Total < 300 -> SpecialName = 'Inferno Overdrive',  Bonus = 300
    ; Total < 350 -> SpecialName = 'Scarlet Apocalypse', Bonus = 350
    ;             SpecialName = 'Ultimate Red Fury',    Bonus = 400
    ).

% --- Blue Ranger Specials ---
determine_special_blue(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Azure Spark',           Bonus = 40
    ; Total < 80  -> SpecialName = 'Forcefield Flash',        Bonus = 80
    ; Total < 120 -> SpecialName = 'Sapphire Strike',         Bonus = 120
    ; Total < 160 -> SpecialName = 'Aqua Barrier',            Bonus = 160
    ; Total < 200 -> SpecialName = 'Blue Nova',               Bonus = 200
    ; Total < 250 -> SpecialName = 'Tidal Wave',              Bonus = 250
    ; Total < 300 -> SpecialName = 'Oceanic Overdrive',       Bonus = 300
    ; Total < 350 -> SpecialName = 'Neptune\'s Fury',         Bonus = 350
    ;             SpecialName = 'Ultimate Blue Tsunami',     Bonus = 400
    ).

% --- Green Ranger Specials ---
determine_special_green(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Emerald Glimmer',       Bonus = 40
    ; Total < 80  -> SpecialName = 'Nature\'s Grasp',       Bonus = 80
    ; Total < 120 -> SpecialName = 'Verdant Spear',         Bonus = 120
    ; Total < 160 -> SpecialName = 'Jade Strike',           Bonus = 160
    ; Total < 200 -> SpecialName = 'Green Tempest',         Bonus = 200
    ; Total < 250 -> SpecialName = 'Earthquake',            Bonus = 250
    ; Total < 300 -> SpecialName = 'Forest Rampage',        Bonus = 300
    ; Total < 350 -> SpecialName = 'Gaia\'s Wrath',           Bonus = 350
    ;             SpecialName = 'Ultimate Earthquake',     Bonus = 400
    ).

% --- Yellow Ranger Specials ---
determine_special_yellow(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Solar Flicker',         Bonus = 40
    ; Total < 80  -> SpecialName = 'Golden Flash',          Bonus = 80
    ; Total < 120 -> SpecialName = 'Sunburst Kick',         Bonus = 120
    ; Total < 160 -> SpecialName = 'Radiant Rush',          Bonus = 160
    ; Total < 200 -> SpecialName = 'Yellow Comet',          Bonus = 200
    ; Total < 250 -> SpecialName = 'Solar Flare',           Bonus = 250
    ; Total < 300 -> SpecialName = 'Luminous Overdrive',    Bonus = 300
    ; Total < 350 -> SpecialName = 'Helios\' Fury',          Bonus = 350
    ;             SpecialName = 'Ultimate Solar Storm',    Bonus = 400
    ).

% --- Pink Ranger Specials ---
determine_special_pink(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Rose Spark',            Bonus = 40
    ; Total < 80  -> SpecialName = 'Blush Burst',           Bonus = 80
    ; Total < 120 -> SpecialName = 'Pink Impact',           Bonus = 120
    ; Total < 160 -> SpecialName = 'Fuchsia Fury',          Bonus = 160
    ; Total < 200 -> SpecialName = 'Magenta Mayhem',        Bonus = 200
    ; Total < 250 -> SpecialName = 'Crimson Crush',         Bonus = 250
    ; Total < 300 -> SpecialName = 'Petal Overdrive',       Bonus = 300
    ; Total < 350 -> SpecialName = 'Rosy Rampage',          Bonus = 350
    ;             SpecialName = 'Ultimate Pink Power',     Bonus = 400
    ).

% --- Shadow Ranger Specials ---
determine_special_shadow(Total, SpecialName, Bonus) :-
    ( Total < 40  -> SpecialName = 'Shadow Flicker',        Bonus = 40
    ; Total < 80  -> SpecialName = 'Night Slash',           Bonus = 80
    ; Total < 120 -> SpecialName = 'Dark Strike',           Bonus = 120
    ; Total < 160 -> SpecialName = 'Phantom Edge',          Bonus = 160
    ; Total < 200 -> SpecialName = 'Eclipse Assault',       Bonus = 200
    ; Total < 250 -> SpecialName = 'Void Reaver',           Bonus = 250
    ; Total < 300 -> SpecialName = 'Spectral Overdrive',    Bonus = 300
    ; Total < 350 -> SpecialName = 'Obsidian Onslaught',      Bonus = 350
    ;             SpecialName = 'Ultimate Shadow Annihilation', Bonus = 400
    ).

% --- Default (Global) Specials ---
determine_special_default(Total, SpecialName, Bonus) :-
    ( Total < 50  -> SpecialName = 'Spark Surge',              Bonus = 50
    ; Total < 80  -> SpecialName = 'Thunder Punch',            Bonus = 60
    ; Total < 120 -> SpecialName = 'Lightning Edge',           Bonus = 65
    ; Total < 160 -> SpecialName = 'Meteor Smash',             Bonus = 70
    ; Total < 200 -> SpecialName = 'Stellar Kick',             Bonus = 100
    ; Total < 240 -> SpecialName = 'Galactic Fury',            Bonus = 200
    ; Total < 280 -> SpecialName = 'Nebula Strike',            Bonus = 300
    ; Total < 320 -> SpecialName = 'Cosmic Blast',             Bonus = 350
    ;             SpecialName = 'Universal Annihilator',      Bonus = 400
    ).


% --- Red Ranger Specials ---
special_bonus('Crimson Spark',     40).
special_bonus('Invisible Slash',    80).
special_bonus('Ruby Burst',        120).
special_bonus('Crimson Fury',      160).
special_bonus('Bloodstorm',        200).
special_bonus('Red Vortex',        250).
special_bonus('Inferno Overdrive', 300).
special_bonus('Scarlet Apocalypse',350).
special_bonus('Ultimate Red Fury', 400).

% --- Blue Ranger Specials ---
special_bonus('Azure Spark',            40).
special_bonus('Forcefield Flash',         80).
special_bonus('Sapphire Strike',         120).
special_bonus('Aqua Barrier',            160).
special_bonus('Blue Nova',               200).
special_bonus('Tidal Wave',              250).
special_bonus('Oceanic Overdrive',       300).
special_bonus('Neptune\'s Fury',         350).
special_bonus('Ultimate Blue Tsunami',   400).

% --- Green Ranger Specials ---
special_bonus('Emerald Glimmer',        40).
special_bonus('Nature\'s Grasp',        80).
special_bonus('Verdant Spear',         120).
special_bonus('Jade Strike',           160).
special_bonus('Green Tempest',         200).
special_bonus('Earthquake',            250).
special_bonus('Forest Rampage',        300).
special_bonus('Gaia\'s Wrath',          350).
special_bonus('Ultimate Earthquake',   400).

% --- Yellow Ranger Specials ---
special_bonus('Solar Flicker',         40).
special_bonus('Golden Flash',          80).
special_bonus('Sunburst Kick',         120).
special_bonus('Radiant Rush',          160).
special_bonus('Yellow Comet',          200).
special_bonus('Solar Flare',           250).
special_bonus('Luminous Overdrive',    300).
special_bonus('Helios\' Fury',          350).
special_bonus('Ultimate Solar Storm',  400).

% --- Pink Ranger Specials ---
special_bonus('Rose Spark',            40).
special_bonus('Blush Burst',           80).
special_bonus('Pink Impact',           120).
special_bonus('Fuchsia Fury',          160).
special_bonus('Magenta Mayhem',        200).
special_bonus('Crimson Crush',         250).
special_bonus('Petal Overdrive',       300).
special_bonus('Rosy Rampage',          350).
special_bonus('Ultimate Pink Power',   400).

% --- Shadow Ranger Specials ---
special_bonus('Shadow Flicker',        40).
special_bonus('Night Slash',           80).
special_bonus('Dark Strike',           120).
special_bonus('Phantom Edge',          160).
special_bonus('Eclipse Assault',       200).
special_bonus('Void Reaver',           250).
special_bonus('Spectral Overdrive',    300).
special_bonus('Obsidian Onslaught',      350).
special_bonus('Ultimate Shadow Annihilation',400).

% --- Default (Global) Specials ---
special_bonus('Spark Surge',             50).
special_bonus('Thunder Punch',           60).
special_bonus('Lightning Edge',          65).
special_bonus('Meteor Smash',            70).
special_bonus('Stellar Kick',           100).
special_bonus('Galactic Fury',          200).
special_bonus('Nebula Strike',          300).
special_bonus('Cosmic Blast',           350).
special_bonus('Universal Annihilator',  400).

