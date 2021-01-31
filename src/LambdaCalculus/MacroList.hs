module MacroList where

import Types

-- Identity Function
id :: LamExpr
id = LamAbs 1 (LamVar 1)

succ :: LamExpr
succ = LamAbs 1
         (LamAbs 2
          (LamAbs 3
           (LamApp
            (LamVar 2)
            (LamApp (LamApp
                     (LamVar 1)
                     (LamVar 2))
              (LamVar 3)))))

zero :: LamExpr
zero = LamAbs 1 (LamAbs 2 (LamVar 2))

one :: LamExpr
one = LamAbs 1 (LamAbs 2 (LamApp (LamVar 1) (LamVar 2)))

-- True and False primitives
t :: LamExpr
t = LamAbs 1 (LamAbs 2 (LamVar 1))

f :: LamExpr
f = LamAbs 1 (LamAbs 2 (LamVar 2))

-- If-then-else
cond :: LamExpr
cond = LamAbs 1 (LamAbs 2
                 (LamAbs 3
                  (LamApp
                   (LamApp
                    (LamVar 1)
                    (LamVar 2))
                   (LamVar 3))))

toNum :: LamExpr -> Int
toNum (LamAbs _ (LamVar _)) = 0
toNum (LamAbs _ expr) = 1 + toNum expr
toNum _ = error "toNum: Incorrect expression given"

toExpr :: Int -> LamExpr
toExpr no = LamAbs 1 (LamAbs 2 (helpExpr no))
  where
    helpExpr :: Int -> LamExpr
    helpExpr 0 = LamVar 2
    helpExpr n
      | n > 0 = LamApp (LamVar 1) (helpExpr (n-1))
      | otherwise = error "Negative Input"

