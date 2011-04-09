This is a bf interpreter written in Haskell. 

Haskell is a lazy, purely functional programming 
language, see http://www.haskell.org/

The code was written to be simple to understand, and not 
optmized. However, I believe it's reasonably efficient,
considering it's a Haskell implementation of a highly
imperative language.

The code uses the Data.Array.IO module from ghc's 
hierarchical libraries, currently present in ghc
and hugs (nhc doesn't seem to support it as of
June/2004). A previous version didn't depend
on this module, and can be obtained from CVS. 

Binary versions were compiled with ghc 6.2.1. See

http://www.haskell.org/ghc

In case you got only the binaries, sources can be 
obtained from 

http://extfun.sourceforge.net/bf

Have fun,

Andrei de A. Formiga 
ktulu@users.sourceforge.net
