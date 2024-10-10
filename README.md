# gobblet_haskell

- ##### Author:

  Hugo Perreault Gravel

- ##### Description:
This project is a terminal-based implementation of the Gobblet game, written in Haskell. It is a 1v1 game where the opponent is intelligent/automated. Possibles moves are  drop('piece size', (position X, position Y)) and onboard((position X, position Y), (position X, position Y)). Ex: drop(B, (0,1)) or onboard((2,0), (1,3)). See gobblet rules for rules.

## Prerequisites

- Stack

## Usage

Compile:

```
stack build
```

Start the game:

```
stack run
```

Clean build

```
stack clean
```
