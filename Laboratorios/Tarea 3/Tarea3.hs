module Tarea3 where

    -- Federico Alonso 182999

    type Symb = String

    type Tape = ([Symb],Symb,[Symb])

    type State = String

    i :: State
    i = "i"

    h :: State
    h = "h"

    white :: Symb
    white = "#"

    simbolo :: Symb
    simbolo = "S"

    data Action = R | L | S Symb deriving (Show,Eq)

    type Config = (State, Tape)

    type Code = [((State, Symb), (Action, State))]

    step :: Code -> Config -> Config
    step code (state, (is,s,ds)) = case lookup (state, s) code of
        Just (action, nextState) -> (nextState, move action (is,s,ds))
        Nothing -> case lookup (state, simbolo) code of {
            Just (action, nextState) -> (nextState, move action (is,s,ds));
            Nothing -> error "No se pensaron todos los casos. Step."
        }
    
    move :: Action -> Tape -> Tape
    move R (is,s,[]) = (s:is,white,[])
    move R (is,s,d:ds) = (s:is,d,ds)
    move L ([],s,ds) = ([], white, s:ds)
    move L (i:is,s,ds) = (is,i,s:ds)
    move (S symb) (xs, y, ds) = (xs, symb, ds)

    exec :: Code -> Tape -> Tape
    exec code tape = continuarPrograma code tape i

    continuarPrograma :: Code -> Tape -> State -> Tape
    continuarPrograma code tape state = case state of {
        "h" -> tape;
        _ -> case step code (state, tape) of {
            (state2, tape) -> continuarPrograma code tape state2
        }
    }

    --  EJERCICIOS

    notMT :: Code
    notMT = [
            ((i,white),(L,"q1")),
            (("q1","T"),(S "F","q2")),
            (("q1","F"),(S "T","q2")),
            (("q2",simbolo),(R,h))
        ]
    
    tapeTrue :: Tape
    tapeTrue = (["T"],white,[])

    tapeFalse :: Tape
    tapeFalse = (["F"],white,[])

    testNotMTTrue :: Tape
    testNotMTTrue = exec notMT tapeTrue

    testNotMTFalse :: Tape
    testNotMTFalse = exec notMT tapeFalse

    rWhite :: Code
    rWhite = [
            ((i,simbolo),(R,"q1")),
            (("q1",white),(S white,h)),
            (("q1",simbolo),(R,"q1"))
        ]

    tape1 :: Tape
    tape1 = (["T",white,"X","F"],white,["T","F",white,"F","X"])

    testRWhite :: Tape
    testRWhite = exec rWhite tape1

    lWhite :: Code
    lWhite = [
            ((i,simbolo),(L,"q1")),
            (("q1",white),(S white,h)),
            (("q1",simbolo),(L,"q1"))
        ]
    
    testLWhite :: Tape
    testLWhite = exec lWhite tape1

    rX :: Code
    rX = [
            ((i,simbolo),(R,"q1")),
            (("q1","X"),(S simbolo,h)),
            (("q1",simbolo),(R,"q1"))
        ]
    
    testRX :: Tape
    testRX = exec rX tape1

    lX :: Code
    lX = [
            ((i,simbolo),(L,"q1")),
            (("q1","X"),(S simbolo,h)),
            (("q1",simbolo),(L,"q1"))
        ]
    
    testLX :: Tape
    testLX = exec lX tape1

    parMT :: Code
    parMT = [
            ((i,white),(R,"q1")),
            (("q1",white),(S "T","q2")),
            (("q2",simbolo),(L,"q3")),
            (("q3",simbolo),(L,"q4")),
            (("q4",white),(R,"q5")),
            (("q5",white),(R,"q6")),
            (("q5",simbolo),(R,"q5")),
            (("q6",simbolo),(R,h)),
            (("q4",simbolo),(S "X","q7")),
            (("q7",simbolo),(R,"q8")),
            (("q8",white),(R,"q9")),
            (("q8",simbolo),(R,"q8")),
            (("q9","T"),(S "F","q10")),
            (("q9","F"),(S "T","q10")),
            (("q10","X"),(S "1","q3")),
            (("q10",simbolo),(L,"q10"))
        ]
    
    tapePruebaDosMT :: Tape
    tapePruebaDosMT = (["1","1",white],white,[])

    tapePruebaCeroMT :: Tape
    tapePruebaCeroMT = ([],white,[])

    tapePruebaUnoMT :: Tape
    tapePruebaUnoMT = (["1"],white,[])

    testParMTCero :: Tape
    testParMTCero = exec parMT tapePruebaCeroMT

    testParMTUno :: Tape
    testParMTUno = exec parMT tapePruebaUnoMT

    testParMTDos :: Tape
    testParMTDos = exec parMT tapePruebaDosMT

    shiftRight :: Code
    shiftRight = [
            ((i,white),(L,"q1")),
            (("q1",white),(R,"q2")),
            (("q2",simbolo),(R,"q3")),
            (("q3",white),(S white,h)),
            (("q3",simbolo),(R,"q3")),
            (("q1","a1"),(S white,"q4")),
            (("q4",simbolo),(R,"q5")),
            (("q5",simbolo),(S "a1","q6")),
            (("q6",simbolo),(L,"q7")),
            (("q1","a2"),(S white,"q8")),
            (("q8",simbolo),(R,"q9")),
            (("q9",simbolo),(S "a2","q10")),
            (("q10",simbolo),(L,"q7")),
            (("q1","a3"),(S white,"q11")),
            (("q11",simbolo),(R,"q12")),
            (("q12",simbolo),(S "a3","q13")),
            (("q13",simbolo),(L,"q7")),
            (("q7",simbolo),(L,"q1"))
        ]
    
    tapeShiftRightVacia :: Tape
    tapeShiftRightVacia = ([],white,[])

    testSRVacia :: Tape
    testSRVacia = exec shiftRight tapeShiftRightVacia

    tapeShiftRightA1Uno :: Tape
    tapeShiftRightA1Uno = (["a1"],white,[])

    testSRA1Uno :: Tape
    testSRA1Uno = exec shiftRight tapeShiftRightA1Uno

    tapeShiftRightA1Dos :: Tape
    tapeShiftRightA1Dos = (["a1", "a1"],white,[])

    testSRA1Dos :: Tape
    testSRA1Dos = exec shiftRight tapeShiftRightA1Dos

    tapeShiftRightA1A2 :: Tape
    tapeShiftRightA1A2 = (["a1", "a2", "a1", "a1"],white,[])

    testSRA1A2 :: Tape
    testSRA1A2 = exec shiftRight tapeShiftRightA1A2

    tapeShiftRightA1A2A3 :: Tape
    tapeShiftRightA1A2A3 = (["a3", "a2", "a1", "a3", "a2", "a1", "a1"],white,[])

    testSRA1A2A3 :: Tape
    testSRA1A2A3 = exec shiftRight tapeShiftRightA1A2A3

    reverseMT :: Code
    reverseMT = [
            ((i,white),(L,"q1")),
            (("q1",white),(R,"q2")),
            (("q2",white),(R,"q3")),
            (("q2",simbolo),(R,"q2")),
            (("q3",white),(S white,h)),
            (("q3",simbolo),(R,"q3")),

            (("q1","a1"),(S "X","q4")),
            (("q4",simbolo),(R,"q5")),
            (("q5",white),(R,"q6")),
            (("q5",simbolo),(R,"q5")),
            (("q6",white),(S "a1","q7")),
            (("q6",simbolo),(R,"q6")),
            (("q7",simbolo),(L,"q8")),
            (("q8","X"),(S "a1","q9")),
            (("q8",simbolo),(L,"q8")),
            (("q9",simbolo),(L,"q1")),

            (("q1","a2"),(S "X","q10")),
            (("q10",simbolo),(R,"q11")),
            (("q11",white),(R,"q12")),
            (("q11",simbolo),(R,"q11")),
            (("q12",white),(S "a2","q13")),
            (("q12",simbolo),(R,"q12")),
            (("q13",simbolo),(L,"q14")),
            (("q14","X"),(S "a2","q15")),
            (("q14",simbolo),(L,"q14")),
            (("q15",simbolo),(L,"q1")),

            (("q1","a3"),(S "X","q16")),
            (("q16",simbolo),(R,"q17")),
            (("q17",white),(R,"q18")),
            (("q17",simbolo),(R,"q17")),
            (("q18",white),(S "a3","q19")),
            (("q18",simbolo),(R,"q18")),
            (("q19",simbolo),(L,"q19")),
            (("q19","X"),(S "a3","q20")),
            (("q19",simbolo),(L,"q19")),
            (("q20",simbolo),(L,"q1"))
        ]
    
    tapereverseMTVacia :: Tape
    tapereverseMTVacia = ([],white,[])

    testRVacia :: Tape
    testRVacia = exec reverseMT tapereverseMTVacia

    tapereverseMTA1Uno :: Tape
    tapereverseMTA1Uno = (["a1"],white,[])

    testRA1Uno :: Tape
    testRA1Uno = exec reverseMT tapereverseMTA1Uno

    tapereverseMTA1Dos :: Tape
    tapereverseMTA1Dos = (["a1","a1"],white,[])

    testRA1Dos :: Tape
    testRA1Dos = exec reverseMT tapereverseMTA1Dos

    tapereverseMTA1A2 :: Tape
    tapereverseMTA1A2 = (["a2","a2","a2","a1","a1"],white,[])

    testRA1A2 :: Tape
    testRA1A2 = exec reverseMT tapereverseMTA1A2

    tapereverseMTA1A2A3 :: Tape
    tapereverseMTA1A2A3 = (["a3","a3","a3","a2","a2","a1"],white,[])

    testRA1A2A3 :: Tape
    testRA1A2A3 = exec reverseMT tapereverseMTA1A2A3