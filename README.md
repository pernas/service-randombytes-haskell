# service-randombytes-haskell

Service to get random bytes

## Required tools

  - Install stack (Mac OSX)

   ```
   brew update
   brew install haskell-stack
   ```

## Build

    `stack build`

## Run

    * using repl: `stack ghci`
    * run compiled: `stack exec service-randombytes-haskell`

## Test with httpie

    `http :3000/v2/randombytes format==hex bytes==128`
