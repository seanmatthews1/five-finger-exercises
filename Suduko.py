          ############################################################
          # 
          # BEGIN
          # 
          # Solve Sudoku  puzzles (in  this case, some  given standard
          # 9x9  one, but  code easily  adapted/rewritten as  a proper
          # function).
          #
          ############################################################
          #

import itertools as it

# 3241368 at http://www.menneske.no/sudoku/eng/

puzzle =   [[0, 3, 0, 0, 8, 0, 4, 0, 0],
            [7, 0, 0, 0, 4, 5, 0, 0, 0],
            [0, 0, 0, 0, 0, 2, 0, 1, 0],
            [0, 4, 0, 2, 5, 0, 0, 0, 0],
            [8, 0, 0, 0, 0, 7, 0, 0, 3],
            [0, 9, 0, 0, 0, 0, 7, 0, 0],
            [1, 5, 0, 6, 0, 4, 0, 8, 0],
            [3, 0, 0, 0, 0, 0, 0, 0, 4],
            [0, 0, 0, 0, 0, 0, 0, 7, 6]]

solution = [[5, 3, 2, 7, 8, 1, 4, 6, 9],
            [7, 6, 1, 9, 4, 5, 2, 3, 8],
            [9, 8, 4, 3, 6, 2, 5, 1, 7],
            [6, 4, 7, 2, 5, 3, 8, 9, 1],
            [8, 1, 5, 4, 9, 7, 6, 2, 3],
            [2, 9, 3, 8, 1, 6, 7, 4, 5],
            [1, 5, 9, 6, 7, 4, 3, 8, 2],
            [3, 7, 6, 1, 2, 8, 9, 5, 4],
            [4, 2, 8, 5, 3, 9, 1, 7, 6]]

          # 
          ############################################################
          # 
          # note the cells that have to be filled


points = [(x,y) for (x,y) in it.product(range(9), repeat=2)
           if puzzle[y][x] == 0]

          # 
          ############################################################
          # 
          # Construct  the  initial  constraint  sets  -  there  is  a
          # constraint  for  reach row,  column,  and  square -  these
          # contraints are  _shared_ across the various  points in the
          # puzzle.

c1 = [[True] * 9 for _ in range(9)]
c2 = [[True] * 9 for _ in range(9)]
c3 = [[True] * 9 for _ in range(9)]

cons = [[(c1[y], c2[x], c3[(y // 3) * 3 + (x // 3)]) for x in range(9)] for y in range(9)]

for (x,y) in it.product(range(9), repeat=2):
    if puzzle[y][x] != 0:
        r,c,s = cons[y][x]
        v     = puzzle[y][x] - 1
        r[v]  = c[v] = s[v] = False

          #
          ############################################################
          #
          # The set of  candidates for a square is the  set of numbers
          # that are  available in  all the  relevant column,  row and
          # square constraints (returned as a generator).

def candidates(x,y):
    r,c,s = cons[y][x]
    return(i+1 for i in range(9) if r[i] & c[i] & s[i])

          #
          ############################################################
          #
          # Now we simply take the standard (somewhat mindless) method
          # of solving problems like this, of depth-first search.

l = len(points)

def do_solve(i):
    if i == l:
        return(True)
    else:
        x, y    = points[i]
        r, c, s = cons[y][x]
        for j in candidates(x, y):
            puzzle[y][x] = j
            r[j-1] = c[j-1] = s[j-1] = False
            if do_solve(i+1):
                return(True)
            else:
                r[j-1] = c[j-1] = s[j-1] = True
                puzzle[y][x] = 0
        return(False)

def solve():
    do_solve(0)

          #
          # END
          #
          ############################################################          
