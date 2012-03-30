#Philosophy

the 2 main ideas behind this speedup attempt are the following.

1. instead of testing a scope against every single rule, only test against those that can possibly match.
2. When possible use binary representation with masks and bitwise-AND/OR instead of vector/string comparsions.

lets say we have the following theme, with scopes based on weekdays

1. mon.tue.wed
2. mon.thu.*
3. mon.thu.sat
4. fri.sun
N. X.X.X
we make 3 observations
A. if we check each rule, even if we fail quickly by comparing last part first, it takes at least N tries.
B. Most grammars are structured with common first part. e.g. [meta, meta.{group, class}, ...] it would be nice if we only had to parse 'meta' once.
C. rule 3 is a subset of rule 2

Something like:

class Compressed
  path = {string => Compressed}
  rule
  
could be used to express this:

   {mon=> rule = nil 
          path = { tue => rule = nil
                          path = {wed => rule = 1
                                         path = {}
                                 },          
                   thu => rule = nil
                          path = {*   => rule = 2
                                         path = {}
                                  sat => rule=>2, 3
                                         path = {}
                                 },
                 },
    fri=> rule = nil
           path = {sun => rule = 4
                         path = {}
                  }
   }
   
However that only let us express simple rules consisting of only 1 scope.
some rules are on the format a1.a2 b1.b2.

One way of doing this is to keep track of all possible rules a scope name is part of.
so in addition to 'path' and 'rule' we add 'multi_part'. In the case of multi_part scopes, we use the old mechanism for determining if a scope match. Using the bnf described in parser.cc. However instead of using strings, we use the path through the Compressed structure.

That way path prefix matching becomes

path1 == (path2 | mask)

The binary representation is calculated by giving each path key a number. the same key-name can have different numbers depending on where in the key it is located. if * is present a 1 is always added.

To not waste bits each path use the minimum needed, The minimum needed calculated by iterating all nodes on a level, and counting the number of path keys without *, and checking how many bits are needed to express that, if * is present one bit is added.


so. foo.{bar,foo} would become foo.bar 101, foo.foo 110. Obviously 1 bit is wasted here

if instead we had foo.{bar,foo, *}, we would get foo.bar=1011, foo.foo=1101, foo.*=1001,
we call this value 'hash'

but we also need to calculate a mask otherwise only complete matches and not prefixes would match.
the mask is calculated by 2^bits - 1, except in the case of * where it is simply 1.

Now we have a structure that looks like:

   class Compressed
     path = {string => Compressed}
     rule
     multi_rule = {rule}
     hash
     mask
     
With this structure we can do two things. 
1. We can use it to compress a theme
2. We can compress a scope that we want to test against the compressed scope.
  
     
