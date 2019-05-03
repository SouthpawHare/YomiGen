      ******************************************************************
      * Author: Christopher Czyzewski
      * Date: 4/17/2019
      * Purpose: Generate match result text for Yomi matches
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOMI-GEN.



       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
           01 TEXT2PRINT PIC X(80).
           01 TEXT2PRINTTEMP PIC X(80).
           01 TEXT-I PIC 99.
           01 NUM2PRINT PIC 9(10).

           01 I PIC 99.
           01 J PIC 99.

           01 CurrPlayer PIC 9.
           01 CurrGame PIC 99.
           01 ScoreGoal PIC 9.
               88 ScoreGoal-Valid Value '1' thru '9'.

           01 ScoreOptionIn PIC 9.
               88 ScoreOptionInPlayer1Win Value '1' '3'.
               88 ScoreOptionInPlayer2Win Value '2' '3'.
               88 ScoreOptionInValid Value '0' thru '3'.

           01 PlayerName occurs 2 times.
               02 PlayerNameInit PIC X(1).
                   88 PlayerNameValid Value 'A' thru 'Z' 'a' thru 'z'.
               02 PlayerNameRest PIC X(19).

           01 GameData.
               02 GameDataRow occurs 20 times.
                   03 GameDataCol occurs 2 times.
                       04 GameDataCharacter PIC X(20).
                       04 GameDataScore PIC 9.

           01 Score PIC 9 occurs 2 times.




       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

           Perform varying CurrPlayer from 1 by 1 UNTIL
            CurrPlayer is greater than 2
               Perform until PlayerNameValid(CurrPlayer)
                   Display "Enter Player " CurrPlayer "'s name "
                   "(Max 20 chars):"
                   ACCEPT PlayerName(CurrPlayer)
               END-PERFORM
           END-PERFORM

           PERFORM UNTIL ScoreGoal-Valid
               Display "Enter score to win (1-9):"
               ACCEPT ScoreGoal
           END-PERFORM

           Perform TallyScores
           Perform until Score(1) Is Greater than or equal to ScoreGoal
            or Score(2) Is Greater than or equal to ScoreGoal or
            CurrGame is greater than or equal to 20
               Add 1 to CurrGame
               Perform InputGame
           END-PERFORM

           Perform ShowFinalMessage

           STOP RUN
           .

       TallyScores.
           Perform varying I from 1 by 1 until I is greater than 2
               MOVE Zero to Score(I)
               Perform varying J from 1 by 1 until J is greater than 20
                   Add GameDataScore(J, I) to Score(I)
               END-PERFORM
           END-PERFORM
           .

       InputGame.
           Display "Game " CurrGame

           Perform varying CurrPlayer from 1 by 1 UNTIL
            CurrPlayer is greater than 2
               Perform InputCharacter
           END-PERFORM

           Perform InputGameScore
           Perform TallyScores
           .

       InputGameScore.

           Move 9 to ScoreOptionIn
           Perform until ScoreOptionInValid
               Display "(1= Player 1 Win, 2= Player 2 Win"
               Display " 3= Both Win, 0= Neither win)"
               Display "Enter Result:"
               Accept ScoreOptionIn
           END-PERFORM

           If ScoreOptionInPlayer1Win THEN
              Move 1 to GameDataScore(CurrGame, 1)
           End-if

           If ScoreOptionInPlayer2Win THEN
              Move 1 to GameDataScore(CurrGame, 2)
           End-if
           .

       InputCharacter.
           Display "Enter Player " CurrPlayer "'s character"
           Accept GameDataCharacter(CurrGame, CurrPlayer)

           .
       ShowFinalMessage.
           Perform ShowHeader
           Display SPACE
           Display SPACE
           Perform ShowGameLog
           Perform ShowFooter

           .

       ShowHeader.
           Display SPACE
           MOVE PlayerName(1) to TEXT2PRINT
           PERFORM SHOW-TEXT
           DISPLAY " vs " with no ADVANCING
           MOVE PlayerName(2) to TEXT2PRINT
           PERFORM SHOW-MENTION
           .

       ShowGameLog.
           Perform varying I from 1 by 1 until I is greater than 20

               IF GameDataCharacter(I, 1)(1:1) not equal SPACES then

                   Move GameDataCharacter(I, 1) to TEXT2PRINT
                   Perform SHOW-EMOTE

                   Display " " with no advancing

                   Move GameDataScore(I, 1) to NUM2PRINT
                   Perform SHOW-VICTORY-EMOTE

                   Display " " with no advancing

                   Move GameDataScore(I, 2) to NUM2PRINT
                   Perform SHOW-VICTORY-EMOTE

                   Display " " with no advancing

                   Move GameDataCharacter(I, 1) to TEXT2PRINT
                   Perform SHOW-EMOTE

                   Display SPACE

               END-IF

           End-Perform
           .

       ShowFooter.
           Perform TallyScores
           Display SPACE
           MOVE PlayerName(1) to TEXT2PRINT
           PERFORM SHOW-TEXT
           Display " " Score(1) " - " Score(2) " " with no ADVANCING
           MOVE PlayerName(2) to TEXT2PRINT
           PERFORM SHOW-TEXT
           .

       SHOW-TEXT.
           MOVE LENGTH OF TEXT2PRINT to TEXT-I
           PERFORM UNTIL TEXT-I LESS THAN 1
            OR TEXT2PRINT(TEXT-I:1) NOT = ' '
               Subtract 1 from TEXT-I
           END-PERFORM
           IF TEXT-I > ZERO
              DISPLAY TEXT2PRINT(1:TEXT-I) with no ADVANCING
           END-IF
           .

       SHOW-EMOTE.
           Perform TOLOWER
           MOVE TEXT2PRINT to TEXT2PRINTTEMP
           String
                       ":" DELIMITED by Size
                       TEXT2PRINTTEMP delimited by space
                       ":" DELIMITED by Size
                       into TEXT2PRINT
           Perform SHOW-TEXT
           .

       SHOW-VICTORY-EMOTE.
           IF NUM2PRINT is GREATER THAN 0 THEN
               MOVE "psfist" to TEXT2PRINT
           ELSE
               MOVE "pschip" to TEXT2PRINT
           END-IF
           PERFORM SHOW-EMOTE
           .

       TOLOWER.
           Inspect TEXT2PRINT Replacing All
                          'A' by 'a'
                          'B' by 'b'
                          'C' by 'c'
                          'D' by 'd'
                          'E' by 'e'
                          'F' by 'f'
                          'G' by 'g'
                          'H' by 'h'
                          'I' by 'i'
                          'J' by 'j'
                          'K' by 'k'
                          'L' by 'l'
                          'M' by 'm'
                          'N' by 'n'
                          'O' by 'o'
                          'P' by 'p'
                          'Q' by 'q'
                          'R' by 'r'
                          'S' by 's'
                          'T' by 't'
                          'U' by 'u'
                          'V' by 'v'
                          'W' by 'w'
                          'X' by 'x'
                          'Y' by 'y'
                          'Z' by 'z'
           .

       SHOW-MENTION.
           PERFORM TOLOWER
           DISPLAY "@" with no ADVANCING
           PERFORM SHOW-TEXT
           .

       END PROGRAM YOMI-GEN.
