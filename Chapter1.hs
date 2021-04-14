doubleMe x = x + x

doubleUs x y = x + x + y + y

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 