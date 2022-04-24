# pf2022

Anyways this is a collection of the resolution of the 2021/2022 exercises for the Functional Programming, I can assure most of this is correct

## Don't be a jackass
This is an educational repository, the point of this is to learn haskell/the ins and outs of Functional Programming as according to the classes' "progression" and to serve as a way to correct/clear up doubts on the exercises provided, don't use this to copy/cheat in tests, I will not be held responsible for y'all being lazy and not wanting to learn 😉.

In other words, I do not care what you do with this repository, but I will not be held accountable for any bad faith on the part of any user.

As such this repository will be under the protection of the MIT License, for further understanding of this License, read the License file in the root folder of the repository.

## 8-bits of Syntax help
'$' are a sort of "stand-in" for redundant parenthesis and '.' are used to chain functions, for example
show (1 + 1) is receiving a function and two integers and returning a string, thus we rewrite this as  $ show $ 1 + 1
now let us, from this return a IO (), then let us do putStrLn (show (1+1)), following what was said we can replace with putStrLn (show $ 1+1) and that with putStrLn . show $ 1+1
this isn't something that is used a lot, nor is it actually considered desirable, but sometimes it might be better for simplying your code ❤️

## Another note
Remember, reject vim, reject sublime, reject vsc, embrace emacs, replace your \ with λ (it's called λ calculus for a reason) and have fun, you'll never learn anything if you don't have fun :heart:
