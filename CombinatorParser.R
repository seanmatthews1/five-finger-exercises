          #############################################################
          #
          # BEGIN
          #
          # Monadic combinator recursive descent parsing - very basic,
          # (esp. in R.)  Don't recommend  trying to use this with big
          # inputs (If nothing else, it will blow the stack). But very
          # useful  for small-scale  use -  and a  starting point  for
          # larger-scale use.
          #
          # Author: Sean Matthews
          # Version: see version control
          #
          # COMMENTS: For  this to scale well (which, to  be honest it
          # probably  does   not  really  currently  do),  a  sensible
          # modification  would  be  to  have the  monad  work with  a
          # constant vector  of tokens and maintain  pointers into it,
          # rather than moving along by actually taking the cdr of the
          # list of  tokens.  This way we would not  have to check the
          # length  each time we call  tok.P, and we could  also store
          # the token seqeuence in a much more efficient form.
          #
          ############################################################
          #
          # We have an additive monad
          #
          #     M po == is -> [(po, is)]
          #
          # where po = parse object and is = input token sequence.
          #
          # I.e.   a parser  is a  function that  takes a  sequence of
          # tokens, parses possible objects from the front of it, then
          # returns a list of  pairs,consisting of the possible parsed
          # objects  together with the corresponding  remainder of the
          # sequence.  If no parse is possible, then the returned list
          # is empty.
          #
          ############################################################
          #
          # The monad
          #
          #     return.P: p -> M p

return.P <-
    function (p) {
        function (l) list(list(p, l))
    }

          #     (>>=) : M p x (p -> M p1) -> M p1

'%>>=%' <-
    function(p, f) {
        function (l) {
            unlist(lapply(p(l), function (r) {(f(r[[1]]))(r[[2]])}),
                   recursive = FALSE)
        }}

          # We  also need a function  to convert a string  into parser
          # input.

as.parser.input <- function (str) {strsplit(str, split="")[[1]]}

          # Finally, given we  have applied the monad to  a string, we
          # can  extract  he results  (and  throw  away any  remaining
          # unparsed component)

extract.parses <-
    function (x) mapply(function(y) y[[1]], x)

          #
          ############################################################
          #
          # additive enrichment

          # plus (choice): M p x M p -> M p
          #
          # Take left and right as alternatives
          #
          # a %++%(b %++% c) == (a %++% b) %++% c
          # a %++% b         ~~ b %++% a               (i.e. up to
          #                                             permutation)
          #
          # We even have(!):
          #
          # (a ++ b) %>>=% c == (a %>>=% c) ++ (b %>>=% c)

'%++%' <-
    function (p1, p2) {
        function (l) c(p1(l), p2(l))
    }

          # zero: M p
          # 
          #     f      %++% zero.P           == f;
          #     zero.P %++% f                == f;
          #     m %>>=% function (d) zero.P  ~~ zero.P (i.e. up to
          #                                             termination)
          #     zero.P %>>=% f               == zero.P

zero.P <- function(l) list()

          #
          ############################################################
          #
          # So far we have  the algebraic structure, but no components
          # with  which  to  build   actual  parsers.  We  still  need
          # something that will actually  lift something off the token
          # sequence, thus...
          # 
          # Parse the next token in  the sequence, if any, whatever it
          # is.
            
tok.P <-
    function (l) {
        if (length(l) > 0) {
            list(list(l[1], l[-1]))
        } else {
            list()
        }
    }

          ############################################################
          # 
          # Finally, it's useful to  have a _deterministic_ choice: if
          # p1 works,  take the first p1 parse, else  if p2 works take
          # the  first p2  parse, otherwise zero.  Strive to  use this
          # whereever possible instead of %++%!

'%+++%' <-
    function (p1, p2) {
        function (l) {
            try1 <- p1(l)
            if (length(try1) > 0) {
                list(try1[[1]])
            } else {
                try2 <- p2(l)
                if (length(try2) > 0) {
                    list(try2[[1]])
                } else {
                    list()
                }
            }
        }
    }

          #
          ############################################################
          ############################################################
          #
          # derived parsers and combinators

          #    degenerate bind: M p x M p1 -> M p1

'%>>%' <-
    function (p1, p2) p1 %>>=% (function (dummy) p2)

          # parse none or more  similar objects off the input stream
          # and return these as a list.

many.F  <- function(p) many1.F(p) %+++% return.P(list())

          # parse one or more similar ...

many1.F <- function(p) p         %>>=% function (o1) {
                       many.F(p) %>>=% function (o2) {
                       return.P(c(o1, o2))}}

          # parse one possible object, else return default value.

possible.F <-
    function(p, default)  p %+++% return.P(default)

          # Parse the  initial token off the sequence  if it satisfies
          # predicate p

sat.P <-
    function(p) tok.P %>>=% function (x) {
                if (p(x)) {return.P(x)} else {zero.P}
                }

          # Parse the provided token off the sequence.

quote.P <-
    function(t) sat.P(function (y) y == t)

          #
          # END
          #
          ############################################################
