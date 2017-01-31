% Example application of Gemia to specifying a data server
%
% Copyright (C) 2015-2017 Sascha Fendrich
%
% This file is part of Gemia.
%
% Gemia is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License, or
% (at your option) any later version.
%
% Gemia is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with Gemia.  If not, see <http://www.gnu.org/licenses/>.

\documentclass[a4paper]{article}

\usepackage{geometry}
\usepackage{graphicx}
\usepackage{hyperref}

\usepackage{verbatim}
\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}
\newenvironment{spec}{\footnotesize\verbatim}{\endverbatim\normalsize}

\newcommand{\haskell}[1]{\texttt{{#1}}}
\newcommand{\parop}{\mathop{||}}

\title{An Example Application of the Gemia Module}
\author{Sascha Fendrich}

\begin{document}
\maketitle

\noindent
Gemia is a Haskell%
\footnote{The Haskell programming language: \url{http://www.haskell.org}}
 module that implements some operations on Modal Interface
Automata (MIA).  This document describes how the Gemia module is used to
describe concurrent systems on basis of MIA along the dataserver example
reported in the following article:
\begin{quote}
F. Bujtor, S. Fendrich, G. L\"uttgen and W.  Vogler.
\emph{Nondeterministic Modal Interfaces}. Theoretical Computer Science, 642,
pp. 24-53, Elsevier, 2016.
\end{quote}
The reader is expected to have a basic understanding of MIA and functional
programming. One may try the examples given in this document at the command
line shell respectively the interactive Haskell interpreter GHCi%
\footnote{The Glasgow Haskell Compiler: \url{http://www.haskell.org/ghc/}}
by typing the commands shown right of the respective prompt. 
We indicate the prompts as \texttt{shell>} 
for the command line shell and as \texttt{ghci>} for the Haskell 
interpreter. E.g.,
to start our example with ghci from the shell, enter 
\texttt{ghci ex-dataserver-gemia.lhs} at the command line. We write
this as 

\begin{spec}shell> ghci ex-dataserver-gemia.lhs\end{spec}

Note that Gemia is work in progress and, thus, incomplete. 
However, the module is already in a usable state.

\section{The Data Server Example}

We demonstrate the usage of the Gemia module by going through
the source code of an example specifying a data server.
First, we have to import some modules.

\begin{code}
import Gemia
import Data.List
import System.Environment
import System.IO

import Aux
import Graphviz
\end{code}

Module \haskell{Gemia} is needed for obvious reasons. The modules
\haskell{Data.List}, \haskell{System.Environment} and 
\haskell{System.IO} are from Haskell's standard library.
They are used for the output of specified systems. 
Module \haskell{Aux} delivers some auxilary functions
and data types needed by \haskell{Gemia}, and \haskell{Graphviz} 
allows one to transform a MIA into the dot-language, such that 
they can be processed by Graphviz%
\footnote{The Graphviz graph visualization software: 
\url{http://www.graphviz.org}}
to render graphical representations of the systems.

A MIA can be specified by giving an initial state, and a list
of required and optional transitions. This is done with the function
\haskell{makeGemia}.\bigskip

\noindent
\begin{minipage}{0.5\textwidth}
\begin{code}
-- Global server specification.
global :: Gemia Int
global = makeGemia (Leaf 0) 
  [req 0 Input  "rqst" [1], 
   opt 1 Output "fail" 0,
   opt 1 Output "resp" 0]
\end{code}
\end{minipage}%
\begin{minipage}{0.5\textwidth}\centering
\includegraphics[height=6em]{global.pdf}
\end{minipage}\bigskip

We define a specification named `global' and show a
graphical representation of the corresponding MIA. 
In the graphical representation, the initial state 
is drawn with a rectangular shape, other states with elliptic shapes. 
Required transitions are drawn with solid lines, optional transitions
with dashed lines.

 In \haskell{Gemia}, specification \haskell{global} is of
type \haskell{Gemia Int}, i.e., it corresponds to a MIA where the
basic type for representing states is \haskell{Int}. The actual
type for node representation, however, is a tree of \haskell{Int},
so that the compositional structure resulting from applying
MIA-operations is still visible in a composed system. This
is the reason why the initial state is given as \haskell{Leaf 0}.

The transitions are given as a list of calls to \haskell{req} and
\haskell{opt}. For both functions, the first argument is the
source node of the transition, the second is the IO-type,
namely \haskell{Input} or \haskell{Output}, and the third argument
is a string representing the action label of the transition.
Target nodes are given as the fourth argument in a slightly different
manner for \haskell{req} and \haskell{opt}. 
As MIA supports disjunctive must-transitions, \haskell{req} expects
a list of nodes as a target, while the target for \haskell{opt} is just
a single node. All nodes, source and target, are given as the basic
node type, \haskell{Int} in our example.%
\footnote{In case one would like to use other types as base type
for nodes, it is necessary to ensure that the chosen type is
an instance of the \haskell{Dotable} type class 
(see \haskell{Graphviz} module).}
Note that `required' and `optional' have a slightly different
meaning than `must' and `may'. A required transition is a
must-transition that automatically includes the may-transitions
required by syntactic consistency, while an optional transition
represents a may-transition that is not required by any must-transition.
This way, it is impossible to specify syntactically inconsistent
systems in \haskell{Gemia}.

To get the shown graphic, one just needs
to call \haskell{toDot global} and to feed the resulting string
into Graphviz's dot-tool. In detail, calling
\haskell{putStr \$ toDot global} results in the following dot-representation:

\begin{spec}
ghci> putStr $ toDot global
digraph {
"0"[shape=box,style=rounded]
"0"->"1" [label="rqst?"]
"1"->"0" [label="resp!",style=dashed]
"1"->"0" [label="fail!",style=dashed]
}
\end{spec}

If we save this output in the file \texttt{global.dot}, we can
call the dot-tool to get the graphic as a pdf-file: 

\begin{spec}
shell> dot -Tpdf global.dot >global.pdf
\end{spec}

The following MIAs are specified the same way and, thus, should not
need further explanation.\bigskip

\noindent
\begin{minipage}{0.5\textwidth}
\begin{code}
-- back-end 1: local cache
cache :: Gemia Int
cache = makeGemia (Leaf 0)
  [req 0 Input  "rqst1" [1], 
   req 1 Output "resp1" [0], 
   opt 1 Output "miss" 0]
        
-- back-end 2: remote database
database :: Gemia Int
database = makeGemia (Leaf 0)
  [req 0 Input "rqst2" [1],
   req 1 Output "resp2" [0]]

-- R1: forward to one of the back-ends
r1 :: Gemia Int
r1 = makeGemia (Leaf 0) 
  [opt 0 Input  "rqst" 1, 
   opt 2 Output "rqst1" 0, 
   opt 2 Output "rqst2" 0,
   req 1 Output "sel" [2],
   opt 0 Output "resp" 0, 
   opt 0 Output "rqst1" 0, 
   opt 0 Output "rqst2" 0]

-- R2: one back-end or both 
r2 :: Gemia Int
r2 = makeGemia (Leaf 0) 
  [opt 0 Output "sel" 1,
   opt 0 Output "sel" 2,
   req 1 Output "rqst1" [0],
   req 2 Output "rqst2" [0],
   opt 0 Output "resp" 0, 
   opt 0 Output "rqst1" 0, 
   opt 0 Output "rqst2" 0]
\end{code}
\end{minipage}
%
\begin{minipage}{0.5\textwidth}
\begin{code}
-- R3: after rqst1, first wait for resp1, 
--     then respond to client
r3 :: Gemia Int
r3 = makeGemia (Leaf 0) 
  [opt 0 Output "rqst1" 1, 
   req 1 Input  "resp1" [2], 
   req 1 Input  "miss" [0],
   req 2 Output "resp" [0], 
   opt 0 Output "resp" 0]

-- R4: after rqst2, first wait for resp2, 
--     then respond to client
r4 :: Gemia Int
r4 = makeGemia (Leaf 0) 
  [opt 0 Output "rqst2" 1, 
   req 1 Input  "resp2" [2], 
   req 2 Output "resp" [0], 
   opt 0 Output "resp" 0]

-- R5: after a miss redirect to database
r5 :: Gemia Int
r5 = makeGemia (Leaf 0) 
  [opt 0 Input "miss" 1, 
   opt 1 Output "fbck" 2, 
   opt 1 Output "fail" 0, 
   req 2 Output "rqst2" [0], 
   opt 0 Output "rqst2" 0, 
   opt 0 Output "resp" 0]

-- Closed system
one :: Gemia Int
one = makeGemia (Leaf 0) 
  (map (\x->opt 0 Output x 0) 
        ["rqst","resp","fail"])
\end{code}
\end{minipage}\bigskip

Now that we have specified the basic building blocks of the
data server application manually, we proceed with some automatic 
constructions.
\bigskip

\noindent
\begin{minipage}[t]{0.5\textwidth}
\begin{code}
-- Conjunction of requirements,
-- unpruned and pruned.
ru, r :: Gemia Int
ru = foldl1 cprod [r1,r2,r3,r4,r5]
r  = pruneUnreachable (pruneIncons ru)

-- Conjunction of R1 and R2.
r12 :: Gemia Int
r12 = cprod r1 r2
\end{code}
\end{minipage}%
\begin{minipage}[t]{0.5\textwidth}\vspace{0pt}
\includegraphics[width=\textwidth]{r12.pdf}
\end{minipage}\bigskip

Folding \haskell{cprod} over the requirements $R_1$ through $R_5$ 
constructs their conjunctive product \haskell{ru} 
including inconsistent and unreachable states. These can be
removed by applying \haskell{pruneIncons} and \haskell{pruneUnreachable}.
It is important to prune inconsistent states first, because removing them
can lead to further unreachable states.

In the visualization, inconsistent states are shown in grey
with target-less transitions for inconsistently specified actions.
Since \haskell{ru} is too large to fit on a single page we demonstrate
this by showing the smaller conjunction of $R_1$ and $R_2$.
The node names are (nested) tuples according to the compositional
structure of the resulting MIA.

Next, we use the parallel product \haskell{pprod} and the incompatible
quotient \haskell{iquot}. Currently, the quotient does not
consider all types of incompatibilities, hence the `i' in \haskell{iquot}.
The back tics \haskell{`} around \haskell{iquot}, \haskell{pprod}
and \haskell{cprod} are Haskell's syntactic sugar which allows us to use
functions as infix operators, the dollar sign \haskell{\$} is a
low precedence function application operator.

\begin{code}
-- foreign actions
fa = map (Action Optional Output) ["rqst1", "rqst2", "resp1", "resp2", "miss"]

-- extended global specification
global' = addLoops global fa

-- upper bound for frontend
uf :: Gemia Int
uf = global' `iquot` (cache `pprod` database)

-- final front-end specification
f :: Gemia Int
f = pruneUnreachable $ pruneIncons $ uf `cprod` r

-- server
s :: Gemia Int
s = pruneUnreachable $ f `pprod` cache `pprod` database
\end{code}

A hiding operator as well as refinement are currently not implemented.
Thus, we specify the next MIA manually, from which we derive
the client specification and, finally, the full client-server application
via the parallel product $\haskell{pprod}$.

\begin{code}
-- F' hides back-end communication in F
f' = makeGemia (Leaf 0)
  [req 0 Input  "rqst" [1],
   req 1 Output "resp" [0],
   opt  1 Output "fail" 0
  ]

-- Client
client :: Gemia Int
client =  one `iquot` f'

-- Full system
sys :: Gemia Int
sys = pruneUnreachable $ client `pprod` s
\end{code}

\section{Generating Visualizations}

The following functions are only helpers for producing dot-files 
from the specifications.  They are not needed for specification,
but simplify the generation of the graphical visualizations.

\begin{code}
-- Output helper functions
writeToFile :: (Dotable n,Show n) => String -> Gemia n -> IO ()
writeToFile path g = do
  h <- openFile (path ++ ".dot") WriteMode
  putStr $ "Writing file '" ++ path ++ ".dot'\n"
  hPutStr h (toDot g)
  hClose h
\end{code}

The function \haskell{writeToFile} accepts a base name
and a MIA and writes a dot-file representing the same
MIA for Graphviz. The suffix \texttt{.dot} is appended
automatically.
  
\begin{code}
-- List of available MIAs
miaList = 
  [("global",global),
   ("globalext",global'),
   ("one", one),
   ("cache", cache),
   ("database",database),
   ("client",client),
   ("cachePPdatabase", cache `pprod` database),
   ("uf", uf),
   ("r1",r1),
   ("r2",r2),
   ("r3",r3),
   ("r4",r4),
   ("r5",r5),
   ("r12", r12),
   ("r1ANDr2", pruneIncons $ pruneUnreachable r12),
   ("ru",ru),
   ("r", r),
   ("ufCPr", pruneUnreachable (uf `cprod` r)),
   ("f", f),
   ("fprime", f'),
   ("s", s),
   ("oneIQfprime", client),
   ("sys", sys),
   ("r123", cprod r12 r3),
   ("r123p", pruneUnreachable $ pruneIncons (cprod r12 r3)),
   ("r1234", cprod (cprod r12 r3) r4),
   ("r1234p", pruneUnreachable $ pruneIncons (cprod (cprod r12 r3) r4)),
   ("r12345", cprod (cprod (cprod r12 r3) r4) r5),
   ("r12345p", pruneUnreachable $ pruneIncons (cprod (cprod (cprod r12 r3) r4) r5)),
   ("refinement", rprod s global')
  ]
\end{code}

\haskell{miaList} is the list of specifications available for transformation
into the dot-language. It is a list of pairs each of which
consists of a name and a MIA.

\begin{code}
-- Main entry point for running the example
main = do
  args <- getArgs
  mapM (uncurry writeToFile) 
    (if null args then miaList else filter (flip elem args . fst) miaList)
\end{code}

The \haskell{main} function allows one to select which MIAs should be
output as dot-files by giving names from \haskell{miaList} as command
line arguments when executing \texttt{ex-dataserver-gemia.lhs}. If
no name is given, all MIAs in the list are proceeded. Call

\begin{spec}
shell> runhaskell ex-dataserver-gemia.lhs [name]...
\end{spec}

\noindent
to run the example outside of ghci, or compile it to a binary with

\begin{spec}
shell> ghc ex-dataserver-gemia.lhs
\end{spec}
\end{document}
