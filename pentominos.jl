          ############################################################
          #
          # BEGIN
          #
          # Code  to  find  pentomino  solutions.   Implementation  of
          # Knuth's 'dancing links' method for the exact cover problem
          # Here  for  the standard  6x10  pentomino  problem. Can  be
          # trivially  reconfigured for  other  similar problems  (see
          # Knuth's original paper).
          #
          ############################################################

          ############################################################
          #
          # First,  generate all  the  possible plays  (in the  format
          # needed for  Knuth's algorithm).   In the  end, we  want to
          # construct  indicator  vectors   for  each  possible  piece
          # placement, where this consists of the piece (one of 1:12),
          # and the squares covered: five of 1:60).
          #
          # For  each  piece in  turn,  we  take one  prototype  form,
          # generate  all  the  possible  transformations  (apply  all
          # transformations, then eliminate duplicates), and from this
          # generate all sets of covered squares.
          #
          # Note  that for  one  piece  (number 2)  we  take only  the
          # minimal   subset   of    transformations,   to   eliminate
          # solutions symmetric by reflection or rotation.
          #
          ############################################################

          ############################################################
          # 
          # Given a prototype piece, apply a set of transformations to
          # generate the complete set:

Apply_Transforms(p, t) = (unique ∘ map)(f -> f(p), t)

all_transforms =
    let
        i  = identity
        p1 = (x -> reverse(x, dims=1))
        p2 = (x -> reverse(x, dims=2))
        t  = transpose

        [i, p1, p2, p1 ∘ p2, t, p1 ∘ t, p2 ∘ t, p1 ∘ p2 ∘ t]
    end

symmetry_breaking_transforms = [identity, transpose]

board = zeros(Int, 6, 10)

rows_b, cols_b = size(board)

function List_of_Plays(p_l)

          # given a  list of  possible piece configurations  (which in
          # practice   will   be   an   exhaustive   list   of   (all)
          # configurations  for one  piece),  return a  vector of  the
          # complete list of possible plays in the format we need.

    function Plays_for_config(p)

          # Given  a piece  configuration, return  a generator  of all
          # possible board positions for it

        r, c = size(p)
        r -= 1
        c -= 1

        function Make_play(x, y)

          # given a piece  configuration (p) and an  upper left corner
          # [x,y],  return a  play  recording the  (five) squares  the
          # piece covers.

            A = copy(board)
            view(A, x:x+r, y:y+c)[:,:] = p[:,:]
            return [i for (i, j) in enumerate(A) if j == 1]
        end

        (Make_play(x, y) for x in 1:rows_b - r for y in 1:cols_b - c)
        
    end

    return [v for v in Iterators.flatten(Plays_for_config.(p_l))]
    
end

          # Build a list of possible plays for each piece

plays =
    let
        F(n, p, t) = (n, (List_of_Plays ∘ Apply_Transforms)(p, t))

        [F( 1, [1 1 1 1 1], all_transforms)

         F( 2, [1 1 1 1;
                1 0 0 0]  , symmetry_breaking_transforms)

         F( 3, [1 1 1 1;
                0 1 0 0]  , all_transforms)

         F( 4, [0 1 1 1;
                1 1 0 0]  , all_transforms)

         F( 5, [1 1 1;
                1 0 0;
                1 0 0]    , all_transforms)

         F( 6, [1 1 1;
                1 1 0]    , all_transforms)

         F( 7, [1 1 1;
                1 0 1]    , all_transforms)

         F( 8, [0 0 1;
                1 1 1;
                1 0 0]    , all_transforms)

         F( 9, [0 1 0;
                1 1 1;
                1 0 0]    , all_transforms)

         F(10, [1 0 0;
                1 1 1;
                1 0 0]    , all_transforms)

         F(11, [0 1 0;
                1 1 1;
                0 1 0]    , all_transforms)

         F(12, [1 1 0;
                0 1 1;
                0 0 1]    , all_transforms) ]
    end

          #
          ############################################################

          ############################################################
          #
          # Set up the Link graph of indicators
          #
          # The link  object includes an  integer value, which  can be
          # used to store various bits of information (such as the row
          # number, or  the column  count) as appropriate.   There are
          # also pointers to the frame links.

mutable struct Link
    cell  :: Bool
    i     :: Int64
    
    up    :: Link
    down  :: Link
    left  :: Link
    right :: Link

    col   :: Link
    row   :: Link
    Link(i, c) = begin
        l      = new()
        l.i    = i
        l.cell = c
        return l
    end
end

Table = Array{Link}(undef, (sum ∘ map)(length ∘ last, plays), 60 + 12)
r = 1
for (i, p) in plays
    global r
    for j in p
        Table[r, 60 + i] = Link(0, true)
        for k in j
            Table[r, k]  = Link(0, true)
        end
        r += 1
    end
end

          # and the frame for the table  - to gather the boundaries of
          # the graph

rows_Table, cols_Table = size(Table)

Rows = [Link(i, false) for i in 1:rows_Table]
for (i, l) in enumerate(Rows)
    ll = l
    for j in 1:cols_Table
        if isassigned(Table, i, j)
            ll.right = Table[i, j]
            Table[i, j].left = ll
            Table[i, j].row  = l
            ll = Table[i, j]
        end
    end
    ll.right = l
    l.left = ll
end

Cols = [Link(i, false) for i in 1:cols_Table]
for (j, u) in enumerate(Cols)
    uu = u
    for i in 1:rows_Table
        if isassigned(Table, i, j)
            uu.down = Table[i, j]
            Table[i, j].up  = uu
            Table[i, j].col = u
            uu = Table[i, j]
        end
    end
    uu.down = u
    u.up = uu
end

Corner = Link(-1, false)

for i in 1:rows_Table
    Rows[i].up   = (i == 1          ? Corner : Rows[i-1])
    Rows[i].down = (i == rows_Table ? Corner : Rows[i+1])
end
Corner.down = Rows[         1];
Corner.up   = Rows[rows_Table];

for i in 1:cols_Table
    Cols[i].left  = (i == 1          ? Corner : Cols[i-1])
    Cols[i].right = (i == cols_Table ? Corner : Cols[i+1])
end
Corner.right = Cols[         1];
Corner.left  = Cols[cols_Table];

          #
          ############################################################

          ############################################################
          #
          # Preliminaries done,  we can define the  core functions for
          # the search.

          # First, count the number of active rows for each column

for c in Cols
    F(c) = begin
        a = 0
        cc = c.down
        while cc != c
            a += 1
            cc = cc.down
        end
        return a
    end

    c.i = F(c)
end

Is_Cell(L) = L.cell

function Get_Min_Column()
    M = Corner.left
    C = M.left
    while C != Corner
        if C.i < M.i
            M = C
        end
        C = C.left
    end
    return M
end

Corresponding_Col(L) = L.col
Corresponding_Row(L) = L.row

Decrement_Count!(L) = Corresponding_Col(L).i -= 1
Increment_Count!(L) = Corresponding_Col(L).i += 1

function Remove_Link_Vertical!(L)
    Decrement_Count!(L)
    L.down.up = L.up
    L.up.down = L.down
end

function Restore_Link_Vertical!(L)
    L.down.up = L
    L.up.down = L
    Increment_Count!(L)
end

function Remove_Link_Horizontal!(L)
    Decrement_Count!(L)
    L.left.right = L.right
    L.right.left = L.left
end

function Restore_Link_Horizontal!(L)
    L.left.right = L
    L.right.left = L
    Increment_Count!(L)
end

function Remove_Row!(L)
    Remove_Link_Horizontal!(L)
    LL = Corresponding_Row(L).left
    while Is_Cell(LL)
        Remove_Link_Vertical!(LL)
        LL = LL.left
    end
end

function Restore_Row!(L)
    LL = Corresponding_Row(L).right
    while Is_Cell(LL)
        Restore_Link_Vertical!(LL)
        LL = LL.right
    end
    Restore_Link_Horizontal!(L)
end

function Remove_Column!(L)
    LL = Corresponding_Col(L).down
    while Is_Cell(LL)
        Remove_Row!(LL)
        LL = LL.down
    end
    LL.right.left = LL.left
    LL.left.right = LL.right
end

function Restore_Column!(L)
    LL = Corresponding_Col(L).up
    while Is_Cell(LL)
        Restore_Row!(LL)
        LL = LL.up
    end
    LL.right.left = LL
    LL.left.right = LL
end

function Remove_Intersecting_Plays!(L)
    LL = Corresponding_Row(L).right
    while Is_Cell(LL)
        Remove_Link_Vertical!(LL)
        Remove_Column!(LL)
        LL = LL.right
    end
end

function Restore_Intersecting_Plays!(L)
    LL = Corresponding_Row(L).left
    while Is_Cell(LL)
        Restore_Column!(LL)
        Restore_Link_Vertical!(LL)
        LL = LL.left
    end
end

          #
          ############################################################

          ############################################################
          #
          # So, does  it work? - Following  code is a bit  patched for
          # basic testing purposes - just counts the solutions as they
          # are found.

C = 0

function solve(n)
    global C
    if n == 13
        C += 1 # found one!
        return false
    else
        L = Get_Min_Column()
        if L.i == 0
            return false
        else
            LL = L.down
            while Is_Cell(LL)
                Remove_Intersecting_Plays!(LL)
                solve(n + 1)
                Restore_Intersecting_Plays!(LL)
                LL = LL.down
            end
            return false
        end
    end
end

# C = 0; solve(1); println(C)

          # And, indeed, finds 2339 solutions. Takes <15 seconds on my
          # machine. If we  change the board shape,  finds the correct
          # number of solutions for 12x5, 15x4 and 20x3.
          # 
          #
          # END
          #
          ############################################################
