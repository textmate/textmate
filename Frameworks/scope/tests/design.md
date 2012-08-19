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
B. Most grammars are structured with common first part. e.g. [meta, meta.{group, class}, ..., mon.{...} in the example] it would be nice if we only had to parse 'mon' once.
C. rule 3 is a subset of rule 2

#Implementation
Transfering the above ideas into a data structure could look like:

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
some rules are on the format 'a1.a2 b1.b2'.

#Multipart scopes
One way to expand the single rule representation above to something that allows multi part rules, is to store which 'paths' are members of multipart rules:


   class Compressed
     path = {string => Compressed}
     rule
     multi_rule = {rule}

An example grammar:

1. mon
2. mon.thu 
3. fri.sun mon

   { mon=> rule = 1
		    multi_rule = (3) 
            path = { thu => rule = 2
                          path = {}
						  #multi_rule = (3)
					  },
	 fri=> rule = nil
	       multi_rule = ()
			 path = { sun => rule = 1
			                 multi_rule = (3)
							     path = {}}
   }


In the case of multi_part scopes, we don't get the matching 'for free'. We need to check if the multipart rules evaluated to a real match. The reason this extra evaluation is needed, is that just because a scope is part of a multi part rule, doesn't mean that that rule will match.
Matching is done using the old scheme with an ast matcher, though using binary representation instead of lists of strings. 

so if a 'mon' and/or 'fri.sun' are encountered, we need to evaluate rule 3 to see if it matches. Ofcourse we have a match only if both 'mon' and 'fri.sun' are present and appear in the right order.

## Multi Rule Representation
In the example above rules and multi_rules are represented using (lists of) integers. In the actual implementaton only simple rules are. Instead the multi_part rules are stored as a bitset, which indexes into an array containing the rules. Using a bitset the union operation is very cheap. Also x86 has a special index-of-left-most-bit instructions.

the multi_rule prefixed with # shows an optimisation. if 'mon' is part of a multi_rule 'mon.thu' will be too. So only edges will need to bit or:ed. 

The grammar above would need a bitset of length 1, since we only have one multi_rule.
it would also need an array of length one to store the actual rule, which will be used for matching.

# Binary representation
By using binary representation matching becomes as simple as

hash1 == (hash2 | mask)

To see how this is the case. We look at how a path can be represented in binary instead of say an array of strings. The binary representation (hash1 and hash2) is calculated by giving each path key a number. the same key-name can have different numbers depending on where in the key it is located. It only needs to be unique on a certain node. So the same key can appear on different levels with different binary representation, but also on the same level but on different nodes.

If the wildcard (*) is present a 1 is always shifted in.

To not waste bits each path use the minimum needed, The minimum needed is calculated by iterating all nodes on a level, and counting the number of path keys without *, and checking how many bits are needed to express that, if * is present, one extra bit is needed.

so. foo.{bar,foo} would become foo.bar 101, foo.foo 110. Obviously 1 bit is wasted here

if instead we had foo.{bar,foo, *}, we would get foo.bar=1011, foo.foo=1101, foo.*=1001,
we call this value 'hash'

but we also need to calculate a mask otherwise only complete matches and not prefixes would match.
the mask is calculated by 2^bits - 1, except in the case of * where it is simply 1.

##Example
An example tree. [symbol: hash/mask, bits dot separated by # bits per level]
{
 foo:01.00/11.000 => { bar:01.01/11.111
                       baz:01.10/11.111
				  	   zed:10.11/11.111} 
 baz:10.00/11.000 => { baz:10.01/11.111}
}
baz happens to be the same across both levels (10), but baz are not the same across nodes on same level (10 != 01),

#Summary
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
  
#Future Thoughts
Taking another look at:

1. mon
2. mon.thu 
3. fri.sun mon

detailed above.
Using a bit set for the multi_rules, (the possible candidates). There will be a lot of evaluation that can never match with this method. Everytime we encounter 'mon' which might be popular we need to test if rule 3 matches. Even if we have not encountered a 'fri.sun'.

There are ways to minimize this. One way is to not make 'mon' a multi_rule node.
Another is to expand the bitset to contain more information.
e.g.
by using 3 bits instead of one. Bits represented as follows

bit 1: result bit, starts at 0
bit 2: 'mon' bit, set if 'mon' is present
bit 3: 'fri.sun' bit, set if 'fri.sun' is present

We get a field, which will set the result bit to 1, if added by 1 and both bit 2 and bit 3 are set.

001
011 +
---
100     
