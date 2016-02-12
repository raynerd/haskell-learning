type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi h pFrom pTo pUse
  | h == 0 = []
  | h > 0  = (hanoi (h - 1) pFrom pUse pTo) ++ [(pFrom, pTo)] ++ (hanoi (h - 1) pUse pTo pFrom)
--  | h > 0 = [(pFrom, pTo)] ++ hanoi ((h - 1) pFrom pUse pTo) ++ hanoi ((h - 1) pUse pTo pFrom)
