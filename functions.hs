--options
process "help" _ = unlines [
    "Calculator Functions:",
    "  pythagorean <missing value a|b|c> <value 1> <value 2>",
    "  quadratic <a> <b> <c>",
    "  square <x>",
    "  commonDerivativesHelp (to see list of common derivative functions."
    ]

--options for derivatives
process "commonDerivativesHelp" _ = unlines [
    "Input number corresponding to derivative",
    "Common Derivative Functions:",
    "    1: (d/dx)(a)",
    "    2: (d/dx)(x)",
    "    3: (d/dx)(au)",
    "    4: (d/dx)(u + v - w)",
    "    5: (d/dx)(uv)",
    "    6: (d/dx)[u/v]",
    "    7: (d/dx)(u^n)",
    "    8: (d/dx)(sqrt(u))",
    "    9: (d/dx)[1/u]",
    "   10: (d/dx)[1/(u^n)]",
    "   11: (d/dx)[f(u)]",
    "   12: (d/dx)[ln(u)]",
    "   13: (d/dx)[log(a)u]",
    "   14: (d/dx)(e^u)",
    "   15: (d/dx)(a^u)",
    "   16: (d/dx)(u^v)",
    "   17: (d/dx)(sin(u))",
    "   18: (d/dx)(cos(u))",
    "   19: (d/dx)(tan(u))",
    "   20: (d/dx)(cot(u))",
    "   21: (d/dx)(sec(u))",
    "   22: (d/dx)(csc(u))"
    ]

process "pythagorean" (c:a:b:[]) = show $ pythagorean c' a' b'
    where c' = head c
          a' = read a :: Double
          b' = read b :: Double

process "quadratic" (a:b:c:[]) = show $ quadratic a' b' c'
    where a' = read a :: Double
          b' = read b :: Double
          c' = read c :: Double

process "square" (x:_) = show $ square x'
    where x' = read x

process "commonDerivatives" (x:_) = show $ commonDerivatives x'
    where x' = read x :: Int

process "exit" _ = "Goodbye!"

process _ _ = ""

loop = do
    putStrLn "Enter statement or \"help\""
    input <- getLine

    let (command:args) = words input
    putStrLn $ process command args

    case input of
        "exit" -> return ()
        otherwise -> loop

main = do
    loop

-- Actual Functions
-- c 3 4
pythagorean :: (Ord a, Floating a) => Char -> a -> a -> [a]
pythagorean s x y
    | s == 'c' = [x,y,sqrt(x^2 + y^2)]
    | y <= x = error "c value is incorrect"
    | s == 'a' = [sqrt(y^2 - x^2),x,y] 
    | s == 'b' = [x,sqrt(y^2 - x^2),y] 
    | otherwise = error "bad character value"


-- 1 -7 12
quadratic :: (Floating a) => a -> a -> a -> [a]
quadratic a b c = [e plus, e minus]
    where e plusOrMinus = (-b + plusOrMinus) / (2 * a)
          plus = sqrt $ (b ^ 2) - (4 * a * c)
          minus = -plus
--5
square :: (Num a) => a -> a
square x = x^2

commonDerivatives :: (Eq a, Num a) => a -> String
commonDerivatives x 
    | x == 1 = "0"
    | x == 2 = "1"
    | x == 3 = "a(du/dx)"
    | x == 4 = "(du/dx) + (dv/dx) - (dw/dx)"
    | x == 5 = "u(dv/dx) + v(du/dx)"
    | x == 6 = "(1/v)(du/dx) - (u/v^2)(dv/dx)"
    | x == 7 = "(nu^(n-1))(du/dx)"
    | x == 8 = "(1/(2sqrt(u)))(du/dx)"
    | x == 9 = "- (1/(u^2))(du/dx)"
    | x == 10 ="- (n/(u^(n+1)))(du/dx)"
    | x == 11 ="(d/du)[f(u)](du/dx)"
    | x == 12 ="(1/u)(du/dx)"
    | x == 13 ="log(a)e(1/u)(du/dx)"
    | x == 14 ="(e^u)(du/dx)"
    | x == 15 ="(a^u)(ln(a))(du/dx)"
    | x == 16 ="(vu^(v-1))(du/dx) + (ln(u))(u^v)(dv/dx)"
    | x == 17 ="(cos(u))(du/dx)"
    | x == 18 ="- (sin(u))(du/dx)"
    | x == 19 ="((sec^2)u)(du/dx)"
    | x == 20 ="- ((csc^2)u)(du/dx)"
    | x == 21 ="(sec(u))(tan(u))(du/dx)"
    | x == 22 ="- (csc(u))(cot(u))(du/dx)"
    | otherwise = error "bad number value"