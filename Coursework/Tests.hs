
--The vast majority of my testing was performed in a web based IDE (repl.it) and I simply would print out my functions.
--This allowed me to test functions easily to see what was or wasnt working without having to build complex tests
--Tests.hs is simply a log of some of the tests I did, as each main = print.. statement would be put into challenges.hs

--Challenge 1
--main = print $ calcBBInteractions 2 [(1, 1), (4, 3)]
--main = print $ calcBBInteractions 4 []
--main = print $ calcBBInteractions 4 [(2,1), (2,2)]
--main = print $ calcBBInteractions 6 [(3,4), (4, 5), (5,5)]
--main = print $ calcBBInteractions 8 [(2,3), (7,3), (4,6), (7,8)]


--Challenge 2
--main = print $ head $ filterBoards (generateBoards 4 8 (getAllAtoms 8 1 1)) [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)] 8 0
--main = print $ solveBB 4 [((North,1),Path (West,2)),((North,2),Absorb),((North,3),Path (North,6)),((North,4),Absorb),((North,5),Path (East,5)),((North,6),Path (North,3)),((North,7),Absorb),((North,8),Path (East,2)),((East,1),Path (West,1)),((East,2),Path (North,8)),((East,3),Absorb),((East,4),Path (East,7)),((East,5),Path (North,5)),((East,6),Absorb),((East,7),Path (East,4)),((East,8),Absorb),((South,1),Path (West,4)),((South,2),Absorb),((South,3),Path (West,7)),((South,4),Absorb),((South,5),Path (West,5)),((South,6),Reflect),((South,7),Absorb),((South,8),Reflect),((West,1),Path (East,1)),((West,2),Path (North,1)),((West,3),Absorb),((West,4),Path (South,1)),((West,5),Path (South,5)),((West,6),Absorb),((West,7),Path (South,3)),((West,8),Absorb)]
--main = print $ solveBB 2 (calcBBInteractions 2 [(1, 1), (4, 3)])
--main = print $ solveBB 4 (calcBBInteractions 8 [(2,3), (7,3), (4,6), (7,8)])


--Challenge 3
--main = print $ prettyPrint (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
--main = print $ prettyPrint (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
--main = print $ prettyPrint (LamApp (LamVar 2) (LamAbs 1 (LamAbs 2 (LamVar 1))))
--main = print $ prettyPrint (LamAbs 1 (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamAbs 2 (LamVar 1))))))


--Challenge 4
--main = print $ parseLet "let x1 = x2"
--main = print $ parseLet "x1 (x2 x3)"
--main = print $ parseLet "x1 x2 x3"
--main = print $ parseLet "let f1 x2 = x2 in f1 x1" 
--main = print $ parseLet "let f1 x2 = x2; f2 x1 = x1 in f1 x1" 


--Challenge 5
--main = print $ parseString (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 1 (LamVar 1)))
--main = print $ parseString (LamAbs 1 (LamApp (LamVar 1) (LamAbs 1 (LamVar 1))))
--main = print $ parseString (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1))))
--main = print $ parseString (LamAbs 0 (LamAbs 1 (LamAbs 2 (LamApp (LamVar 2) (LamVar 1)))))


--Challenge 6
--main = print $ compareInnerOuter (LamAbs 1 (LamApp (LamVar 1) (LamVar 2))) 10
--main = print $ evalCLIn $ clTransform (LamAbs 1 (LamApp (LamVar 1) (LamVar 2)))
--main = print $ compareInnerOuter (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2))) 10
--main = print $ compareInnerOuter (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1)))) 100
--main = print $ clTransform (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))
--main = print $ evalOut (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))
--main = print $ evalCLIn $ clTransform (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3)) (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)))
--main = print $ compareInnerOuter (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3)) (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4))) 4
--main = print $ evalIn $ evalIn $ evalIn (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 1))) (LamVar 3)) (LamApp (LamAbs 1 (LamVar 1)) (LamVar 4)))
--main = print $ compareInnerOuter (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))) (LamAbs 1 (LamVar 1))) 100
--main = print $ evalCLOut $ clTransform (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))) (LamAbs 1 (LamVar 1)))
--main = print $ clTransform $ evalOut $ evalOut (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))) (LamAbs 1 (LamVar 1)))
--main = print $ evalIn (LamApp (LamApp (LamAbs 1 (LamAbs 2 (LamVar 2))) (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))) (LamAbs 1 (LamVar 1)))
--main = print $ evalCLOut $ clTransform (LamApp (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))) (LamAbs 1 (LamApp (LamVar 1) (LamVar 1))))
--main = print $ clTransform (LamApp (LamAbs 1 (LamVar 1)) (LamAbs 2 (LamVar 2)))
