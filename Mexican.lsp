
;Function Name: runGame
;Purpose: Runs the game
;Parameters:
;           roundNum, int passed by value
;           compScore, int passed by value
;           humanScore int passed by value
;Return Value: none
;Algorithm:
;          1. If round num is one (first round), check if player wants to resume
;          2. If so, call that function, and create new round from the gameState
;          3. Otherwise, create a fresh round.
;          4. Upon round exit, ask if player wants to play another round.
;          5. If so, update scores and increment round number, and recursively call function.
;          6. Repeat steps 3-5 for each successive round.
;          6. If player chooses not to continue, display game end and return. Exit the program.
;Assistance Received: none
;
(defun runGame(roundNum compScore humanScore)
  (print "Welcome to Mexican Train!")
  ;Only done upon first calling the function
  (cond ( (equal roundNum 1)
         ;Prompt user for resuming game or not
         (let* ((resumeDecision (resumeChoice)))
           ;Player chooses to resume
           (cond( (equal resumeDecision 'y)
                 ;Create a round with the loaded state
                 ;Then prompt user to play another round
                 (let* ((gameStatus (roundLoop(loadGame)))
                        (roundDecision (playNextRound)))
                   ;Player chooses to continue 
                   (cond( (equal roundDecision 'y)
                         ;Update round num, and player scores
                         (runGame (nth 0 gameStatus)
                                   (nth 1 gameStatus)
                                  (nth 2 gameStatus)))
                         ;Player chooses to end
                         (t 
                          (gameEnd gameStatus)))))
                 ;Player chooses not to resume game
                 (t
                  ;Create fresh round
                  ;Then prompt user to play another round
                  (let* ((gameStatus (roundLoop(setUpTiles roundNum compScore humanScore)))
                         (roundDecision (playNextRound)))
                    ;Player chooses to continue
                    (cond( (equal roundDecision 'y)
                         ;Update round num, and player scores
                         (runGame (nth 0 gameStatus)
                                   (nth 1 gameStatus)
                                  (nth 2 gameStatus)))
                         ;Player chooses to end
                         (t 
                          (gameEnd gameStatus))))))))
           ;All other round numbers
           ;Create fresh round
           ;Then prompt user to play another round
           (t (let* ((gameStatus (roundLoop (setUpTiles roundNum compScore humanScore)))
                     (roundDecision (playNextRound)))
                ;Player chooses to continue
                (cond( (equal roundDecision 'y)
                         (runGame (nth 0 gameStatus)
                                   (nth 1 gameStatus)
                                  (nth 2 gameStatus)))
                         ;Player chooses to end
                         (t 
                          (gameEnd gameStatus)))))))


;Function Name: gameEnd
;Purpose: Displays game end result
;Parameters:
;           gameStatus, list passed by value
;Return Value: none
;Algorithm:
;Assistance Received: none
;
(defun gameEnd(gameStatus)
  (let*( (compScore (nth 1 gameStatus))
        (humanScore (nth 2 gameStatus)))
    ;Human wins
    (cond( (> compScore humanScore)
          (princ "Human won game with ")
          (princ humanScore)
          (princ " points to computers ")
          (princ compScore))
          ;Computer wins
          ( (< compScore humanScore)
            (princ "Computer won game with ")
            (princ  compScore)
            (princ " points to humans ")
           (princ  humanScore))
          ;Draw
          (t (princ "Game ended in draw with human scoring ")
             (princ humanScore)
             (princ " points to computers ")
             (princ compScore)))))


;Function Name: roundEnd
;Purpose: Displays round end result
;Parameters:
;           gameStatus, list passed by value
;Return Value: none
;Algorithm:
;Assistance Received: none
;
(defun roundEnd(gameStatus)
  (let*( (compScore (nth 0 gameStatus))
        (humanScore (nth 1 gameStatus)))
    ;Human wins
    (cond( (> compScore humanScore)
          (princ "Human won round with ")
          (princ humanScore)
          (princ " points to computers ")
          (princ compScore))
          ;Computer wins
          ( (< compScore humanScore)
            (princ "Computer won round with ")
            (princ  compScore)
            (princ " points to humans ")
           (princ  humanScore))
          ;Draw
          (t (princ "Round ended in draw with human scoring ")
             (princ humanScore)
             (princ " points to computers ")
             (princ compScore)))))


;Function Name: playNextRound
;Purpose: Prompts user to play another round or not and returns
;Parameters: none
;Return Value: call to validateNextRound with (read)
;Algorithm:
;Assistance Received: none
;
(defun playNextRound()
  (princ "Would you like to play another round? (y/n)")
  (validateNextRound(read)))  


;Function Name: validateNextRound
;Purpose: Validate and return user choice on next round
;Parameters:
;           choice, symbol passed by value
;Return Value: symbol, or call to playNextRound
;Algorithm:
;Assistance Received: none
;
(defun validateNextRound(choice)
  (cond( (or(equal choice 'y) (equal choice 'n))
        choice)
        (t (playNextRound))))


;Function Name: resumeChoice
;Purpose: Prompts user to resume game or not and returns
;Parameters: none
;Return Value: call to validateResume with (read)
;Algorithm:
;Assistance Received: none
;
(defun resumeChoice()
  (princ "Would you like to resume from a previous game? (y/n)")
  (validateResume(read)))


;Function Name: validateResume
;Purpose: Validate and return user choice on resuming
;Parameters:
;           choice, symbol passed by value
;Return Value: symbol, or call to resumeChoice
;Algorithm:
;Assistance Received: none
;
(defun validateResume(choice)
  (cond( (or(equal choice 'y) (equal choice 'n))
        choice)
        (t (resumeChoice))))


;Function Name: setUpTiles
;Purpose: Generates the gameState list for a fresh round
;Parameters:
;           roundNum, int passed by value
;           compScore, int passed by value
;           humanScore, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun setUpTiles(roundNum compScore humanScore)
  (let* ((tiles (shuffleTiles(loadtiles 0 0) 57))
         (engine (determineEngine roundNum tiles))
         (remainTiles (remove engine tiles :test #'equal))
         (humanHand (getList remainTiles 0 0 16 38))
         (compHand (getList remainTiles 0 16 16 22))
         (boneyard (getList remainTiles 0 32 22 0))
         (playerTrain (cons engine '()))
         (compTrain (cons engine '()))
         (mexTrain (cons engine '()))
         (nextPlayer (findNextPlayer compScore humanScore))
         (npList (cons nextPlayer '()))
         (list1 (list roundNum compScore))
         (list2 (cons humanScore '()))
         )
    ;Add all the elements in the list one by one
    (concatenate 'list list1 (list compHand) (list compTrain) list2 (list humanHand) (list playerTrain) (list mexTrain) (list boneyard) npList)
  )
  )


;Function Name: determineEngine
;Purpose: Returns engine given the round
;Parameters:
;           roundNum, int passed by value
;           tiles, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun determineEngine(roundNum tiles)
  (let* ( (roundModulo (mod roundNum 10))
         )
    ;Every ten rounds, engine is 0-0
    (cond ( (eq roundModulo 0)
           (getElement tiles '(0 0)))
          ;Otherwise, calculate 
          (t 
           (getElement tiles (list (- 10 roundModulo) (- 10 roundModulo)))))))


;Function Name: playerMenu
;Purpose: Displays menu and gets player choice
;Parameters: none
;Return Value: int
;Algorithm:
;Assistance Received: none
;
(defun playerMenu()
  (print "What will you do?")
  (print "1. Save game")
  (print "2. Make a move")
  (print "3. Ask for help")
  (print "4. Quit")
  (menuChoice(read))
  )


;Function Name: humanTurn
;Purpose: Runs human turn
;Parameters:
;           gameState, list passed by value
;Return Value: none
;Algorithm:
;          1. If no eligible tiles at all, go to boneyard function.
;          2. Otherwise, if neither Mexican or opponent are eligible to be played on,
;          play on personal train.
;          3. Otherwise, if opponent train isn't marked and mexican and personal aren't eligible,
;          go to boneyard function.
;          4. If mexican or personal are eligible, play on one of them.
;          5. Otherwise, if opponent is marked, play on any of 3 trains.
;          6. If at any point tile chosen is double, play again using recursive call with updated list.
;Assistance Received: none
;
(defun humanTurn(gameState)
  (let* ((personalTrain (nth 6 gameState))
         (opponentTrain (nth 3 gameState))
         (mexicanTrain (nth 7 gameState))
         (playerHand (nth 5 gameState))
         (personalEligibles (getEligiblesBack playerHand (reverse personalTrain)))
         (mexEligibles (getEligiblesBack playerHand (reverse mexicanTrain)))
         (oppoEligibles (getEligiblesFront playerHand opponentTrain))
         (allEligibles (append personalEligibles mexEligibles oppoEligibles))
         )
            ;No eligibles
    (cond ( (null allEligibles)
           (boneYardHuman gameState))
            ;Only personal eligible
          ( (and (null mexEligibles) (null oppoEligibles))
           (let* ( (tileChosen (askTile allEligibles))
                  (trainChosen (askTrain tileChosen personalEligibles mexEligibles oppoEligibles)))
             ;Double played
             (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                    (print "Double played! Play another tile.")
                    (humanTurn (updateGSHuman gameState tileChosen trainChosen)))
                   (t (updateNextPlayer (updateGSHuman gameState tileChosen trainChosen)
                                        'COMPUTER 0)))))
          ;Opponent not marked, not eligible
          ( (not (equal (first opponentTrain) 'M))
                     ;None of rest are eligible
           (cond (  (and (null mexEligibles) (null personalEligibles))
                  (boneYardHuman gameState))
                 ;Others are eligible
                 (t (let* ( (tileChosen (askTile allEligibles))
                           ;oppoEligibles is passed as a list of completely inelgible to signify opponent
                           ;cannot be played on, even if eligible tiles are found for it.
                           (trainChosen (askTrain tileChosen personalEligibles mexEligibles (list '(10 10))) ))
                      ;Double played
                      (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                              (print "Double played! Play another tile.")
                              (humanTurn (updateGSHuman gameState tileChosen trainChosen)))
                            (t (updateNextPlayer (updateGSHuman gameState tileChosen trainChosen)
                                                 'COMPUTER 0)))))))
          ;All are eligible
          (t 
           (let* ( (tileChosen (askTile allEligibles))
                  (trainChosen (askTrain tileChosen personalEligibles mexEligibles oppoEligibles)))
              ;Double played
              (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                              (print "Double played! Play another tile.")
                     (humanTurn (updateGSHuman gameState tileChosen trainChosen)))
                    (t (updateNextPlayer (updateGSHuman gameState tileChosen trainChosen)
                                         'COMPUTER 0))))))))


;Function Name: compTurn
;Purpose: Runs computer turn
;Parameters:
;           gameState, list passed by value
;Return Value: none
;Algorithm:
;          1. If no eligible tiles at all, go to boneyard function.
;          2. Otherwise, if neither Mexican or opponent are eligible to be played on,
;          play on personal train.
;          3. Otherwise, if opponent train isn't marked and mexican and personal aren't eligible,
;          go to boneyard function.
;          4. If mexican or personal are eligible, play on one of them.
;          5. Otherwise, if opponent is marked, play on any of 3 trains.
;          6. If at any point tile chosen is double, play again using recursive call with updated list.
;Assistance Received: none
;
(defun compTurn(gameState)
  (let* ((personalTrain (nth 3 gameState))
         (opponentTrain (nth 6 gameState))
         (mexicanTrain (nth 7 gameState))
         (playerHand (nth 2 gameState))
         (personalEligibles (getEligiblesFront playerHand personalTrain))
         (mexEligibles (getEligiblesBack playerHand (reverse mexicanTrain)))
         (oppoEligibles (getEligiblesBack playerHand (reverse opponentTrain)))
         (allEligibles (append personalEligibles mexEligibles oppoEligibles))
         )
             ;No eligibles
     (cond ( (null allEligibles)
            (boneYardComp gameState))
           ;Only personal eligible
          ( (and (null mexEligibles) (null oppoEligibles))
           (let* ( (tileChosen (askTile allEligibles))
                  (trainChosen (askTrain tileChosen personalEligibles mexEligibles oppoEligibles)))
              ;Double played
              (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                              (print "Double played! Play another tile.")
                              (compTurn (updateGSComp gameState tileChosen trainChosen)))
                    (t (updateNextPlayer (updateGSComp gameState tileChosen trainChosen)
                                         'HUMAN 0)))))
           ;Opponent not marked, not eligible
           ( (not (equal (last opponentTrain) 'M))
                     ;None of rest are eligible
            (cond (  (and (null mexEligibles) (null personalEligibles))
                   (boneYardComp gameState))
                  ;Others are eligible
                 (t (let* ( (tileChosen (askTile allEligibles))
                           (trainChosen (askTrain tileChosen personalEligibles mexEligibles (list '(10 10))) ))
                      ;Double played
                       (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                              (print "Double played! Play another tile.")
                              (compTurn (updateGSComp gameState tileChosen trainChosen)))
                             (t (updateNextPlayer (updateGSComp gameState tileChosen trainChosen)
                                                  'HUMAN 0)))))))
          ;All are eligible
          (t 
           (let* ( (tileChosen (askTile allEligibles))
                  (trainChosen (askTrain tileChosen personalEligibles mexEligibles oppoEligibles)))
               ;Double played
               (cond ( (equal (nth 0 tileChosen) (nth 1 tileChosen))
                              (print "Double played! Play another tile.")
                              (compTurn (updateGSComp gameState tileChosen trainChosen)))
                    (t (updateNextPlayer (updateGSComp gameState tileChosen trainChosen)
                                         'HUMAN 0))))))))

;Function Name: askTile
;Purpose: Asks player for tile to place
;Parameters:
;          eligibleTiles, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun askTile (eligibleTiles)
  (print "Which tile would you like to place?")
  (validateTiles (read) eligibleTiles)
  )


;Function Name: validateTiles
;Purpose: Validates and returns tile chosen by player
;Parameters:
;          tileChosen, atom passed by value
;          eligibleTiles, list passed by value
;Return Value: list or call to askTile
;Algorithm:
;Assistance Received: none
;
(defun validateTiles (tileChosen eligibleTiles)
  (cond ( (not(equal (getElement eligibleTiles tileChosen) nil))
         tileChosen)
        (t 
         (print "Tile is invalid. Please select different tile.")
         (askTile eligibleTiles))))


;Function Name: askTrain
;Purpose: Asks player for train to place tile on
;Parameters:
;          tileChosen, list passed by value
;          personalEligibles, list passed by value
;          mexEligibles, list passed by value
;          oppoEligibles, list passed by value
;Return Value: symbol
;Algorithm:
;Assistance Received: none
;
(defun askTrain(tileChosen personalEligibles mexEligibles oppoEligibles)
  (print "Which train would you like to place it on?")
  (validateTrain (read) tileChosen personalEligibles mexEligibles oppoEligibles)
  )


;Function Name: validateTrain
;Purpose: Validates and returns train chosen by player
;Parameters:
;          trainChosen, symbol passed by value
;          tileChosen, list passed by value
;          personalEligibles, list passed by value
;          mexEligibles, list passed by value
;          oppoEligibles, list passed by value
;Return Value: list or call to askTile
;Algorithm:
;Assistance Received: none
;
(defun validateTrain (trainChosen tileChosen personalEligibles mexEligibles oppoEligibles)
  (cond ( (equal trainChosen 'MEXICAN)
         (cond ( (not (equal (getelement mexEligibles tileChosen) nil))
                trainChosen)
               (t (askTrain tileChosen personalEligibles mexEligibles oppoEligibles))))
        ( (equal trainChosen 'PERSONAL)
        (cond ( (not (equal (getelement personalEligibles tileChosen) nil))
                trainChosen)
               (t (askTrain tileChosen personalEligibles mexEligibles oppoEligibles))))
        ( (equal trainChosen 'OPPONENT)
         (cond ( (not (equal (getelement oppoEligibles tileChosen) nil))
                trainChosen)
               (t (askTrain tileChosen personalEligibles mexEligibles oppoEligibles))))
        (t (askTrain tileChosen personalEligibles mexEligibles oppoEligibles))))



;Function Name: updateGSHuman
;Purpose: Updates gamestate from human turn
;Parameters:
;           gameState, list passed by value
;           tileChosen, list passed by value
;           trainChosen, list passed by value
;Return Value: list
;Algorithm:
;          1. Check for train chosen by player and update that train.
;          2. If personal is marked, remove marker.
;          3. If opponent is marked, retain marker.
;Assistance Received: none
;
(defun updateGSHuman (gameState tileChosen trainChosen)
  (let* (  (personalTrain (nth 6 gameState))
           (opponentTrain (nth 3 gameState))
           (mexicanTrain (nth 7 gameState))
           (playerHand (nth 5 gameState))
           (newHand  (remove tileChosen playerHand :test #'equal))
           (gameState2 (updateHumanHand gameState newHand 0)))
    (cond ( (equal trainChosen 'PERSONAL)
           (cond ( (equal (last personalTrain) 'M)
                  (updateHumanTrain gameState2 
                                    (addToBackT (remove 'M personalTrain)
                                                tileChosen) 0))
                 (t (updateHumanTrain gameState2 
                             (addToBackT personalTrain tileChosen) 0))))
          ( (equal trainChosen 'MEXICAN)
           (updateMexTrain gameState2 
                             (addToBackT mexicanTrain tileChosen) 0))
          ( (equal trainChosen 'OPPONENT)
           (cond ( (equal (first opponentTrain) 'M)
                  (updateCompTrain gameState2 (cons
                                                (first opponentTrain)
                                                 (addToFrontT (rest opponentTrain)
                                    tileChosen)) 0))
                 (t (updateCompTrain gameState2 
                                     (addToFrontT opponentTrain tileChosen) 0)))))))


;Function Name: updateGSComp
;Purpose: Updates gamestate from computer turn
;Parameters:
;           gameState, list passed by value
;           tileChosen, list passed by value
;           trainChosen, list passed by value
;Return Value: list
;Algorithm:
;          1. Check for train chosen by player and update that train.
;          2. If personal is marked, remove marker.
;          3. If opponent is marked, retain marker.
(defun updateGSComp (gameState tileChosen trainChosen)
  (let* (  (personalTrain (nth 3 gameState))
           (opponentTrain (nth 6 gameState))
           (mexicanTrain (nth 7 gameState))
           (playerHand (nth 2 gameState))
           (newHand  (remove tileChosen playerHand :test #'equal))
           (gameState2 (updateCompHand gameState newHand 0)))
    (cond ( (equal trainChosen 'PERSONAL)
           (cond ( (equal (first personalTrain) 'M)
                  (updateCompTrain gameState2 
                                    (addToFrontT (remove 'M personalTrain)
                                                tileChosen) 0))
                 (t (updateCompTrain gameState2 
                             (addToFrontT personalTrain tileChosen) 0))))
          ( (equal trainChosen 'MEXICAN)
           (updateMexTrain gameState2 
                             (addToBackT mexicanTrain tileChosen) 0))
          ( (equal trainChosen 'OPPONENT)
           (cond ( (equal (last opponentTrain) 'M)
                  (updateHumanTrain gameState2 (cons
                                                (addToBackT (butlast opponentTrain)
                                    tileChosen) 'M) 0))
                 (t (updateHumanTrain gameState2 
                                     (addToBackT opponentTrain tileChosen) 0)))))))


;Function Name: boneYardHuman
;Purpose: Simulates boneyard draw
;Parameters:
;           gameState, list passed by value
;Return Value: list
;Algorithm:
;          1. Check if boneyard is empty
;          2. If empty, passed gamestate back.
;          3. If no marker placed, place marker.
;          4. If boneyard not empty, draw tile.
;          5. If tile is eligible, return and play.
;          6. If tile is not, see steps 2-3.
(defun boneYardHuman(gameState)
  (let* ((personalTrain (nth 6 gameState))
         (opponentTrain (nth 3 gameState))
         (mexicanTrain (nth 7 gameState))
         (playerHand (nth 5 gameState))
         (boneyard (nth 8 gameState))
         
  )
    (cond ( (and (null boneyard) (equal (last personalTrain) 'M))
           (print "Boneyard empty! Ending turn.")
           (updateNextPlayer gameState 'COMPUTER 0))
          ( (null boneyard)
           (print "Boneyard empty! Placing marker and ending turn.")
           (updateNextPlayer
           (updateHumanTrain gameState 
                             (addtoback personalTrain 'M) 0)
           'COMPUTER 0))
          (t 
           (let* ( (boneTile (first boneyard))
                  (personalEligible (checkBackTrain boneTile 
                                                    (reverse personalTrain)))
                  (mexEligible (checkBackTrain boneTile 
                                                 (reverse mexicanTrain)))
                   (oppoEligible (checkFrontTrain boneTile opponentTrain))
                   (anyEligible (or personalEligible mexEligible oppoEligible))
                  (gameState2 (updateBoneyard gameState 
                                              (remove boneTile boneYard) 0))
                  (gameState3 (updateHumanHand gameState2 
                                               (addtoback playerHand boneTile) 0)))
                  (cond ( (and (not anyEligible) (equal (last personalTrain) 'M))
                         (print "Tile drawn not eligible! Ending turn.")
                         (updateNextPlayer gameState3 'COMPUTER 0))
                        ( (not anyEligible)
                         (print "Tile drawn not eligible! Placing marker and ending turn.")
                         (updateNextPlayer (updateHumanTrain gameState3 
                                                             (addtoback personalTrain 'M) 0)
                                           'COMPUTER 0))
                        ( (and (equal anyEligible oppoEligible) (not (equal (first opponentTrain) 'M)))
                         (cond ( (equal (last personalTrain) 'M)
                                (print "Tile drawn not eligible! Ending turn.")
                                (updateNextPlayer gameState3 'COMPUTER 0))
                               (t  (print "Tile drawn not eligible! Placing marker and ending turn.")
                                    (updateNextPlayer (updateHumanTrain gameState3 
                                                                        (addtoback personalTrain 'M) 0)
                                                      'COMPUTER 0))))
                        (t
                         (print "Tile draw is eligible. Play this tile.")
                         (humanTurn gameState3))))))))


;Function Name: boneYardComp
;Purpose: Simulates boneyard draw
;Parameters:
;           gameState, list passed by value
;Return Value: list
;Algorithm:
;          1. Check if boneyard is empty
;          2. If empty, passed gamestate back.
;          3. If no marker placed, place marker.
;          4. If boneyard not empty, draw tile.
;          5. If tile is eligible, return and play.
;          6. If tile is not, see steps 2-3.
(defun boneYardComp(gameState)
  (let* ((personalTrain (nth 3 gameState))
         (opponentTrain (nth 6 gameState))
         (mexicanTrain (nth 7 gameState))
         (playerHand (nth 2 gameState))
         (boneyard (nth 8 gameState))
         
  )
    (cond ( (and (null boneyard) (equal (first personalTrain) 'M))
           (print "Boneyard empty! Ending turn.")
           (updateNextPlayer gameState 'HUMAN 0))
          ( (null boneyard)
           (print "Boneyard empty! Placing marker and ending turn.")
           (updateNextPlayer (updateCompTrain gameState 
                             (addtofront personalTrain 'M) 0) 'HUMAN 0))
          (t 
           (let* ( (boneTile (car boneyard))
                  (personalEligible (checkFrontTrain boneTile personalTrain))
                  (mexEligible (checkBackTrain boneTile 
                                                 (reverse mexicanTrain)))
                  (oppoEligible (checkBackTrain boneTile 
                                                (reverse opponentTrain)))
                  (anyEligible (or personalEligible mexEligible oppoEligible))
                  (gameState2 (updateBoneyard gameState 
                                              (remove boneTile boneYard) 0))
                  (gameState3 (updateCompHand gameState2 
                                              (addtoback playerHand boneTile) 0)))
                  (cond ( (and (not anyEligible) (equal (first personalTrain) 'M))
                         (print "Tile drawn not eligible! Ending turn.")
                         (updateNextPlayer gameState3 'HUMAN 0))
                        ( (not anyEligible)
                         (print "Tile drawn not eligible! Placing marker and ending turn.")
                         (updateNextPlayer (updateCompTrain gameState3 
                                                            (addtofront personalTrain 'M) 0) 'HUMAN 0))
                        ( (and (equal anyEligible oppoEligible) (not (equal (first opponentTrain) 'M)))
                         (cond ( (equal (first personalTrain) 'M)
                                (print "Tile drawn not eligible! Ending turn.")
                                (updateNextPlayer gameState3 'HUMAN 0))
                               (t  (print "Tile drawn not eligible! Placing marker and ending turn.")
                                    (updateNextPlayer (updateCompTrain gameState3 
                                                                        (addtofront personalTrain 'M) 0)
                                                      'HUMAN 0))))
                        (t
                         (print "Tile draw is eligible. Play this tile.")
                         (compTurn gameState3)))))
           )))



;Function Name: roundLoop
;Purpose: Simulates round
;Parameters:
;           gameState, list passed by value
;Return Value: list
;Algorithm:
;          1. If end conditions are met, return updated roundNum, 
;          and player scores after calling roundEnd.
;          2. Otherwise, if nextPlayer is human, run human turn.
;          3. If nextPlayer is computer, run computer turn.
;          4. If round is quitted or saved, return unchanged gameStatus.
(defun roundLoop(gameState)
  (displayRoundInfo gameState)
  (let*( (humanScore (nth 4 gameState))
         (compScore (nth 1 gameState))
         (compTrain (nth 3 gameState))
         (humanTrain (nth 6 gameState))
         (humanHand (nth 5 gameState))
         (compHand (nth 2 gameState))
         (boneyard (nth 8 gameState))
         (nextPlayer (nth 9 gameState))
         (returnStatus (list
                       (nth 0 gameState)
                       (nth 1 gameState)
                        (nth 4 gameState))))
            ;End condition
    (cond ( (and (null boneyard)
                 (and (isInList humanTrain 'M)
                      (isInList compTrain 'M)))
           (let* ( (humanPoints (calculateScore humanHand))
                   (compPoints (calculateScore compHand))
                  (humanScore2 (+ humanPoints humanScore))
                  (compScore2 (+ compPoints compScore)))
             (print (roundEnd (list compPoints humanPoints)))
             (list (+ (nth 0 gameState) 1)
                   compScore2 humanScore2)
             ))
           ;End condition
           ( (or (null compHand) (null humanHand))
            (let* ( (humanPoints (calculateScore humanHand))
                   (compPoints (calculateScore compHand))
                  (humanScore2 (+ humanPoints humanScore))
                  (compScore2 (+ compPoints compScore)))
             (print (roundEnd (list compPoints humanPoints)))
             (list (+ (nth 0 gameState) 1)
                   compScore2 humanScore2)
              ))
          ;Playable conditions
          (t 
           (cond( (equal nextPlayer 'HUMAN)
                 (let* ( (playerChoice (playerMenu)))
                   (cond ( (eq playerChoice 1)
                   (saveGame gameState)
                   returnStatus)
                         ( (eq playerChoice 2)
                         (roundLoop (humanTurn gameState)))
                          ( (eq playerChoice 3)
                          returnStatus)
                          ( (eq playerChoice 4)
                          returnStatus))))
                 ( (equal nextPlayer 'COMPUTER)
                  (let* ( (playerChoice (playerMenu)))
                    (cond ( (eq playerChoice 1)
                           (saveGame gameState)
                           returnStatus)
                          ( (eq playerChoice 2)
                           (roundLoop (compTurn gameState)))
                          ( (eq playerChoice 3)
                          returnStatus)
                          ( (eq playerChoice 4)
                          returnStatus)))))))))


;Function Name: calculateScore
;Purpose: Calculates score of player hand
;Parameters:
;          list, list passed by value
;Return Value: int
;Algorithm:
;Assistance Received: none
;
(defun calculateScore(list)
  (cond  ( (null list)
          '0)
         ( (not (equal (first list) 'M))
         (+ (nth 0 (first list)) (nth 1 (first list))
            (calculateScore (rest list))))
        (t (calculateScore (rest list)))))

         
;Function Name: saveGame
;Purpose: Saves game state into text file
;Parameters:
;          gameState, list passed by value
;Return Value: none
;Algorithm:
;Assistance Received: none
;
(defun saveGame(gameState)
  (print "What will you name the file?")
  (with-open-file (stream (concatenate 'string
                            (read-line) ".txt")
                          :direction :output)
    (format stream "(")
    (terpri stream)
    (format stream "  ;Round")
    (terpri stream)
    (format stream "~A" (nth 0 gameState))
    (terpri stream)
    (format stream "  ;Computer Score")
    (terpri stream)
    (format stream  "~A" (nth 1 gameState))
    (terpri stream)
    (format stream "  ;Computer Hand")
    (terpri stream)
    (format stream  "~A" (nth 2 gameState))
    (terpri stream)
    (format stream "  ;Computer Train")
    (terpri stream)
    (format stream   "~A" (nth 3 gameState))
    (terpri stream)
    (format stream "  ;Human Score")
    (terpri stream)
    (format stream   "~A" (nth 4 gameState))
    (terpri stream)
    (format stream "  ;Human Hand")
    (terpri stream)
    (format stream   "~A" (nth 5 gameState))
    (terpri stream)
    (format stream "  ;Human Train")
    (terpri stream)
    (format stream   "~A" (nth 6 gameState))
    (terpri stream)
    (format stream "  ;Mexican Train")
    (terpri stream)
    (format stream   "~A" (nth 7 gameState))
    (terpri stream)
    (format stream "  ;Boneyard")
    (terpri stream)
    (format stream   "~A" (nth 8 gameState))
    (terpri stream) 
    (format stream "  ;Next Player")
    (terpri stream)
    (format stream   "~A" (nth 9 gameState))
    (terpri stream)
    (format stream ")")
    ))
 

;Function Name: loadGame
;Purpose: Loads game state from text file
;Parameters: none
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun loadGame()
  (print "Which file would you like to open?")
  (let* ( (fileName (read-line))
          (gameState (read(open(concatenate 'string
          fileName ".txt"))))
          
         )
    gameState
    )
  )


;Function Name: displayRoundInfo
;Purpose: Shows the state of the game 
;Parameters:
;          gameState, list passed by value
;Return Value: none
;Algorithm:
;Assistance Received: none
;
(defun displayRoundInfo(gameState)
  (princ "Round:")
  (write (nth 0 gameState))
  (terpri)
  (princ "Computer Score:")
  (write (nth 1 gameState))
  (terpri)
  (princ "Computer Hand:")
  (print (nth 2 gameState))
  (terpri)
  (princ "Computer Train:")
  (write (nth 3 gameState))
  (terpri)
  (princ "Human Score:")
  (write (nth 4 gameState))
  (terpri)
  (princ "Human Hand:")
  (write (nth 5 gameState))
  (terpri)
  (princ "Human Train:")
  (write (nth 6 gameState))
  (terpri)
  (princ "Mexican Train:")
  (write (nth 7 gameState))
  (terpri)
  (princ "Boneyard:")
  (write (nth 8 gameState))
  (terpri)
  (princ "Next Player:")
  (write (nth 9 gameState))
  (terpri)
  )


;Function Name: updateCompHand
;Purpose: Updates gameState's 3rd element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateCompHand (list element pos)
  (cond ( (/= pos 2)
         (cons (car list)
               (updateCompHand (rest list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (first list) (append
                               (list element) list)))
        )
  )


;Function Name: updateCompTrain
;Purpose: Updates gameState's 4th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateCompTrain (list element pos)
  (cond ( (/= pos 3)
         (cons (first list)
               (updateCompTrain (rest list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (first list) (append
                               (list element) list)))
        )
  )


;Function Name: updateHumanHand
;Purpose: Updates gameState's 6th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateHumanHand (list element pos)
  (cond ( (/= pos 5)
         (cons (first list)
               (updateHumanHand (rest list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (first list) (append
                               (list element) list)))
        )
  )


;Function Name: updateHumanTrain
;Purpose: Updates gameState's 7th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateHumanTrain (list element pos)
  (cond ( (/= pos 6)
         (cons (car list)
               (updateHumanTrain (cdr list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (car list) (append
                               (list element) list)))
        )
  )


;Function Name: updateMexTrain
;Purpose: Updates gameState's 8th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateMexTrain (list element pos)
  (cond ( (/= pos 7)
         (cons (first list)
               (updateMexTrain (rest list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (first list) (append
                               (list element) list)))
        )
  )


;Function Name: updateBoneyard
;Purpose: Updates gameState's 9th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateBoneyard (list element pos)
  (cond ( (/= pos 8)
         (cons (car list)
               (updateBoneyard (cdr list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (car list) (append
                               (list element) list)))
        )
  )


;Function Name: updateNextPlayer
;Purpose: Updates gameState's 10th element
;Parameters:
;          list, list passed by value
;          element, atom passed by value
;          pos, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun updateNextPlayer (list element pos)
  (cond ( (/= pos 9)
         (cons (first list)
               (updateNextPlayer (rest list) element (+ pos 1))
               ))
        ( (null list) nil)
        (t (remove (first list) (append
                               (list element) list)))
        )
  )


;Function Name: checkBackTile
;Purpose: Check if tile matches back tile
;Parameters:
;          tile1, list passed by value
;          listTile, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun checkBackTile (tile1 listTile)
  (cond ( (and 
           (/= (nth 0 tile1) (nth 1 listTile))
           (/= (nth 1 tile1) (nth 1 listTile)))
         nil)
        (t )))


;Function Name: checkFrontTile
;Purpose: Check if tile matches front tile
;Parameters:
;          tile1, list passed by value
;          listTile, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun checkFrontTile (tile1 listTile)
  (cond ( (and 
           (/= (nth 0 tile1) (nth 0 listTile))
           (/= (nth 1 tile1) (nth 0 listTile)))
         nil)
        (t )))


;Function Name: checkFrontTrain
;Purpose: Check tile against front of train
;Parameters:
;          tile, list passed by value
;          train, list passed by value
;Return Value: bool - true or NIL
;Algorithm:
;Assistance Received: none
;
(defun checkFrontTrain (tile train)
  (cond ( (equal (first train) 'M)
         (checkFrontTrain tile (rest train)))
        (t (checkFrontTile tile (first train)))))


;Function Name: checkBackTrain
;Purpose: Check tile against back of train
;Parameters:
;          tile, list passed by value
;          train, list passed by value
;Return Value: bool - true or NIL
;Algorithm:
;Assistance Received: none
;
(defun checkBackTrain (tile train)
  (cond ( (equal (first train) 'M)
         (checkBackTrain tile (rest train)))
        (t (checkBackTile tile (first train)))))


;Function Name: getEligiblesFront
;Purpose: Get list of eligible tiles based on front end tile
;Parameters:
;          hand, list passed by value
;          train, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun getEligiblesFront (hand train)
  (cond ( (null hand)
           nil)
         ((equal (first train) 'M)
         (getEligiblesFront hand (rest train)))
        ( (checkFrontTile (first hand) (first train))
         (cons (first hand)
               (getEligiblesFront (rest hand) train)))
        (t (getEligiblesFront (rest hand) train))))


;Function Name: getEligiblesBack
;Purpose: Get list of eligible tiles based on back end tile
;Parameters:
;          hand, list passed by value
;          train, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun getEligiblesBack (hand train)
  (cond ( (null hand)
           nil)
         ((equal (first train) 'M)
         (getEligiblesBack hand (rest train)))
        ( (checkBackTile (first hand) (first train))
         (cons (first hand)
               (getEligiblesBack (rest hand) train)))
        (t (getEligiblesBack (rest hand) train))))


;Function Name: menuChoice
;Purpose: Validates and returns player menu choice
;Parameters:
;          num, int passed by value
;Return Value: int
;Algorithm:
;Assistance Received: none
;
(defun menuChoice(num)
  (cond ( (< num 1)
         (playerMenu) )
        ( ( > num 4)
         (playerMenu) )
        (t num)) )


;Function Name: findNextPlayer
;Purpose: Determines next player of a round based on scores
;Parameters:
;          compScore, int passed by value
;          humanScore, int passed by value
;Return Value: symbol
;Algorithm:
;Assistance Received: none
;
(defun findNextPlayer(compScore humanScore)
  (cond ( (< compScore humanScore )
         'Computer )
        ( (< humanScore compScore )
         'Human)
        (t 
         (coinToss))))


;Function Name: coinToss
;Purpose: Simulates coin toss for player
;Parameters: none
;Return Value: symbol
;Algorithm:
;Assistance Received: none
;
(defun coinToss ()
  (print "A coin will be tossed to decide the next player. Heads or tails (h/t)?")
  (let* ( (coinFlip (coinResult (random 2)))
         (humanChoice (validateCoin(read-line))))
    (cond( (string= coinFlip humanChoice)
          (print "You won the coin toss.")
          'Human)
          ( (string/= coinFlip humanChoice)
           (print "You lost the coin toss.")
             'Computer))))


;Function Name: validateCoin
;Purpose: Validates user input for coin call
;Parameters:
;         choice, string passed by value
;Return Value: string or recursive call to itself
;Algorithm:
;Assistance Received: none
;
(defun validateCoin(choice)
  (cond ( (and ( string/= choice "h") ( string/= choice "t"))
         (print "A coin will be tossed to decide the next player. Heads or tails (h/t)?")
         (validateCoin(read-line)) )
        (t choice)))
  

;Function Name: coinResult
;Purpose: Returns result of random coin toss
;Parameters:
;          num, int passed by value
;Return Value: string
;Algorithm:
;Assistance Received: none
;
(defun coinResult (num)
  (cond ( (= num 0)
         "h")
        (t
         "t"
        )))

;Function Name: isInList
;Purpose: To determine if element is in list
;Parameters:
;          lst, a list passed by value
;          element, atom or list passed by value
;Return Value: true if element is found in list, false otherwise
;Algorithm:
;Assistance Received: none
;
(defun isInList(lst element)
  (cond ((null lst) nil)
        ((not(equal (first lst) element))
         (isInList (rest lst) element))
        (t )))

        
;Function Name: getElement
;Purpose: Gets element inside a list
;Parameters:
;          lst, a list passed by value
;          element, atom or list passed by value
;Return Value: element in the list, otherwise nil
;Algorithm:
;Assistance Received: none
;
(defun getElement (lst element)
  (cond ((null lst) nil)
   ((not(equal (first lst) element))
              (getElement (rest lst) element)
              )
        (t (first lst)))
  )


;Function Name: readanswer
;Purpose: Prompts user for answer of continuing
;Parameters: none
;Return Value: call to validate
;Algorithm:
;Assistance Received: none
;
(defun readanswer ()
  (princ "Do you want to continue?")
  (terpri)
  (validate (read-line))   )


;Function Name: validate
;Purpose: Reads in and validates answer
;Parameters:
;          answer, string passed by value
;Return Value: answer if y or n, call to (readanswer) otherwise
;Algorithm:
;Assistance Received: none
;
(defun validate (answer)
  ;Condition where user doesn't input yes or no
  (cond ( (and ( string/= answer "y") ( string/= answer "n")) 
         (readanswer) )
        ( t answer))    )


;Function Name: loadtiles
;Purpose: Generates tiles for the game board
;Parameters:
;          firstpip, atom passed by value
;          secondpip, atom passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun loadtiles(firstpip secondpip)
  ;Stop once firstpip of 10 reached
  (cond ( ( eq firstpip 10)
         ())
        (t
         (append (gentiles firstpip secondpip)
               (loadtiles (+ firstpip 1) (+ secondpip 1))
         )
        )                   
        )
  )



;Function Name: gentiles
;Purpose: Generates subset of tiles for each first pip
;Parameters:
;          firstpip, atom passed by value
;          secondpip, atom passed by value
;Return Value: list, or nil once stop condition is reached
;Algorithm:
;Assistance Received: none
;
(defun gentiles(firstpip secondpip)
  ;Stop once secondpip of 10 reached
  (cond ((eq 10 secondpip)
        ())
         (t 
          (cons
          (list firstpip secondpip)
          (gentiles firstpip (+ secondpip 1))
           )
          )
        )
  )


;Function Name: shuffleTiles
;Purpose: Shuffles list of tiles
;Parameters:
;          lst, list passed by value
;          count, atom passed by value
;Return Value: list
;Algorithm:
;Assistance Received: Nick M. suggested 
;idea of moving random values to back 
;so that no repeats occur.
;
(defun shuffleTiles(lst count)
  (cond ( (/= 1 count)
        ;Recursively call itself until all tiles have been moved to back
        (shuffleTiles
           (getElementToBack lst (random count) 0)
                   (- count 1)
         ))
        (t
         lst
         )))


;Function Name: getList
;Purpose: Pulls list from larger list
;Parameters:
;          lst, list passed by value
;          pos, int passed by value
;          startpos, int passed by value
;          count, int passed by value
;          restlst, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun getList (lst pos startpos count restlst)
  (cond ( (/= pos startpos)
          (getList (rest lst) (+ pos 1) startpos count restlst))
        ( (/= count 0)
         (cons (first lst)
               (getList (rest lst) pos startpos (- count 1) restlst)))
         (t
          (butlast lst restlst)
         )))


;Function Name: getElementToBack
;Purpose: Gets element in list and puts it at back
;Parameters:
;          lst, list passed by value
;          pos, int passed by value
;          coun, int passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun getElementToBack(lst pos coun)
  (cond ((/= pos coun)
         (cons (first lst)
          (getElementToBack (rest lst) pos (+ coun 1))))
        (t 
         (remove (first lst) 
                 (addtoback lst (first lst))
         :COUNT 1)
         )))


;Function Name: printtile
;Purpose: Prints tile in non-list format
;Parameters:
;          lst, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun printtile(lst)
  (cond (nil lst)
        (t
         (concatenate 'string
         (write-to-string (first lst))
         "-"
         (write-to-string (first (rest lst)))
         " "
         ) 
         )))


;Function Name: formattrain
;Purpose: Prints train in non list format
;Parameters:
;          lst, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun formattrain(lst)
  (cond ((null lst))
        (t
         (princ(printtile (first lst)))
         (formattrain(rest lst))
         ))
  )


;Function Name: addtoback
;Purpose: Adds element to back of list
;Parameters:
;          lst, list passed by value
;          element, atom passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun addtoback(lst element)
  (cond ((null lst))
        (t
         (reverse
          (cons element
                (reverse lst)
                )
          )
         )
        )
  )


;Function Name: addtofront
;Purpose: Adds element to front to list
;Parameters:
;          lst, list passed by value
;          element, atom passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun addtofront(lst element)
  (cond ((null lst))
        (t
         (cons element lst)
         )
        )
  )


;Function Name: addToBackT
;Purpose: Adds tile to back of train, reverses tile if need be
;Parameters:
;          lst, list passed by value
;          element, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun addToBackT(lst element)
  (let* ((updatedTrain (reverse(addtoback lst element))))
    (cond ( (equal (nth 1 element) (nth 1 (nth 1 updatedTrain)))
           (reverse
            (append 
             (list (reverse element)) (rest updatedTrain))))
          (t (reverse updatedTrain)))))


;Function Name: addToFrontT
;Purpose: Adds tile to front of train, reverses tile if need be
;Parameters:
;          list, list passed by value
;          element, list passed by value
;Return Value: list
;Algorithm:
;Assistance Received: none
;
(defun addToFrontT(lst element)
  (let* ((updatedTrain (addtofront lst element)))
    (cond ( (equal (nth 0 element) (nth 0 (nth 1 updatedTrain)))
            (append 
             (list (reverse element)) (rest updatedTrain)))
          (t updatedTrain))))
         








        
         


