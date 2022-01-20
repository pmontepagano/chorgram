--
-- Author: Emilio Tuosto <emilio.tuosto@gssi.it>
--

{-
  A very basic grammar and parser for the textual editing of global
  graphs. The grammar is a revised version of the one used in the
  ICE16 paper with the extensions for reversibility-enabled graphs of
  DAIS 18

     G ::= (o)
        |  P -> P : M
        |  P => P, ..., P : M
	|  G | G
        |  sel { Brc }
        |  sel P { Brc }
        |  branch { Brc }
        |  branch P { Brc }
        |  G ; G
        |  * G @ P
        |  repeat { G unless guard }
        |  repeat P { G unless guard }
        |  { G }
        |  ( G )                        -- this is deprecated. For backward compatibility only

     Brc   ::= G | G unless guard | B + B

     guard ::= P % str | P % str, guard

  where '(o)' has a special role: it marks a point where the selector
  of a loop may exit the iteration. Guards are used only for the
  reversible semantics and the string in them is supposed to be some
  valid Erlang code. Likewise for the 'sel' construct, which
  generalises the choice for the reversible semantics. Notice that
  the 'sel' and the 'branch' constructs have the same semantics and
  allow to specify the selector of the branch (to simplify the
  realisation of projections on Erlang, the selector is mandatory for
  REGs and optional otherwise). The clause 'unless guard' is optional
  in branching and iteration.

  The parser assumes the following equalities

     sel P { G1 unless g1 + ... + Gn unless gn } = G1 + ... + Gn      for all guards g1, ..., gn 
     repeat P {G unless g}                       = * G @ P

  The binary operators _ | _, _ + _, and _ ; _ are given in ascending order of
  precedence.

  Note: strings are made of the following characters

     0123456789
     \_$#&~
     ABCDEFGHIJKLMNOPQRSTUVWXYZ
     abcdefghijklmnopqrstuvwxyz

  and must start with a letter when specifying the identity of a
  participant.

  Reserved characters not allowed in strings are:

     @ . , ; : ( ) [ ] { } | + * ! ? - % §

  Text enclosed by '[[' and ']]' is treated as multi-line comment
  and, after '..', so is the rest of a line.

  Basic syntactic checks are made during the parsing (e.g, (i) that
  sender and receiver of interactions have to be different and (2)
  that the participant controlling a loop is active in the
  loop). More checks are planned together with improved error
  handling.


  Extensions

  - A rudimentary mechanism of definition of g-choreography constants
    and contexts is provided by the following syntactic construct

       let X_1 |-> Ctx_1 & ... & X_n |-> Ctx_n in G_1 | ... | G_m

    where X_1, ..., X_n are pairwise different and the set of mappings
    should not yield recursive definitions. To use a constant the
    productions g-choreographies are extended as follows:

       G ::= ...
          |  do X
          |  do X[G]

    where 'do X = do X[(o)].

    The parser flags uses in g-choreography of undefined constants;
    likewise, the parser checks that Ctx_i uses only constants X_j
    with j < i.
-}

{
module GCParser where
import SyntacticGlobalChoreographies
import ErlanGC
import Data.Set as S (empty, singleton, intersection, null, union, unions, difference, fromList, difference, toList, member, foldr, Set)
import Data.List as L
import qualified Data.Map as M (keys, empty, insert, union, (!), intersection, null, elems, member, map, fromList, Map)
import Misc
import CFSM
}

%name gcgrammar
%tokentype { Token }
%monad { Ptype } { thenPtype } { returnPtype }
%lexer { lexer } { TokenEof }
%error { parseError }

%token
  str	        { TokenStr $$   }
  '(o)'         { TokenEmp      }
  '->'	     	{ TokenArr      }
  '=>'	        { TokenMAr      }
  '|->'	     	{ TokenMap      }
  '|'	        { TokenPar      }
  '+'	        { TokenBra      }
  '%'	        { TokenGrd      }
  '*'	        { TokenSta      }
  ';'	        { TokenSeq      }
  '@'   	{ TokenUnt      }
  ':'	        { TokenSec      }
  '('	        { TokenBro      }
  ')'	        { TokenBrc      }
  ','	        { TokenCom      }
  '{'	        { TokenCurlyo   }
  '}'	        { TokenCurlyc   }
  '&'	        { TokenAnd      }
  'sel'         { TokenSel 3    }
  'branch'      { TokenSel 6    }
  'repeat'      { TokenRep      }
  'unless'      { TokenUnl      }
  'let'         { TokenLet      }
  'in'          { TokenIn       }
  'do'          { TokenDo       }

%right '|'
%right '+'
%right '%'
%right ';'
%right ','
%left '&'

%%

G :: { (GC, Set Ptp) }
G : GE  { $1 M.empty }
  
GE :: { GCEnv -> (GC, Set Ptp) }
GE : E B
     { \env ->
         let
           join = compEnv env $1
         in
           $2 join
     }
   | E B '|' GE
     { \env ->
         let
           join = compEnv env $1
           (g, ptps) = $2 join
         in
           (Par ((checkToken TokenPar (g, ptps))
                 ++ (checkToken TokenPar ($4 join))),
             S.union ptps (snd ($4 join))
           )
     }


E :: { GCEnv }
E : 'let' A 'in' { $2 M.empty }
  | {- empty -}  { M.empty }

                 
A :: { GCEnv -> GCEnv }
A : str '|->' B
  { \env ->
      let
        (g, ptps) = $3 env
      in
        if (M.member $1 env)
        then myErr ("Double definition of constant " ++ $1)
        else M.insert $1 (g, ptps) env
  }
  | A '&' str '|->' B
    { \env ->
        let
          old = M.intersection env ($1 M.empty)
          join = M.union env ($1 M.empty)
          (g, ptps) = $5 join
        in
          case (M.null old, M.member $3 join) of
            (False, _) -> myErr ("Double definition of constants: " ++ (mkSep (M.keys old) ", "))
            (_, True)  -> myErr ("Constant " ++ $3 ++ " already defined")
            _ -> (M.insert $3 (g, ptps) join)
    }


B :: { GCEnv -> (GC, Set Ptp) }
B : S  { $1 }
  | choiceop '{' Br '+' Bs '}'
    { \env ->
        (let
            branches = L.map fst ([$3 env] ++ ($5 env))
            aux g l = l ++ (checkToken TokenBra g)
            tmp = L.foldr aux [] branches
            gcs = M.fromList $ L.zip [0 .. length tmp] tmp
          in
           Bra gcs,
          ptpsBranches ([$3 env] ++ ($5 env))
        )
    }
  | choiceop str '{' Br '+' Bs '}'
    { \env ->
        (let
            branches = L.map fst ([$4 env] ++ ($6 env))
            aux g l = l ++ (checkToken TokenBra g)
            tmp = L.foldr aux [] branches
            gcs = M.fromList $ L.zip [0 .. length tmp] tmp
          in
           Bra gcs,
          ptpsBranches ([$4 env] ++ ($6 env))
        )
    }


choiceop : 'sel'     {}
         | 'branch'  {}


Bs :: { GCEnv -> [((GC, Set Ptp), M.Map String String)] }
Bs : Br         { \env -> [$1 env] }
   | Br '+' Bs  { \env -> [$1 env] ++ ($3 env) }


Br :: { GCEnv -> ((GC, Set Ptp), M.Map String String) }
Br : S { \env -> ($1 env, M.empty) }
   | S 'unless' guard { \env -> checkGuard ($1 env) $3 }


S :: { GCEnv -> (GC, Set Ptp) }
S : '(o)'  { \_ -> (Emp, S.empty) }
  | Blk
    { $1 }
  | B ';' B
    { \env ->
        let
          (b1, ptps1) = ($1 env)
          (b2, ptps2) = ($3 env)
        in
          (Seq ((checkToken TokenSeq (b1, ptps1) )
                ++ (checkToken TokenSeq (b2, ptps2))),
            S.union ptps1 ptps2
          )
    }

    
Blk :: { GCEnv -> (GC, Set Ptp) }
Blk : str '->' str ':' str
  { \_ ->
      case ((isPtp $1), (isPtp $3), ($1 == $3)) of
        (True, True, False)  -> ((Act ($1 , $3) $5), S.fromList [$1,$3])
        (False, False, _)    -> myErr ("Bad names " ++ $1 ++ " and " ++ $3)
        (False, True, True)  -> myErr ("Bad name " ++ $1 ++ " and sender and receiver must be different")
        (False, True, False) -> myErr ("Bad name " ++ $1)
        (True, False, False) -> myErr ("Bad name " ++ $3)
        (_, _, True)         -> myErr ("Sender " ++ $1 ++ " cannot also be receiver in the same interaction")
  }
  | str '=>' ptps ':' str
    { \_ ->
        if (L.elem $1 $3)
        then myErr ($1 ++ " must NOT be one of the receivers " ++ (L.foldl (\x y -> if x == "" then y else x ++ ", " ++ y) "" $3))
        else case (isPtp $1, $3) of
          (False, _)   -> myErr ("Bad name " ++ $1)
          (True, [])   -> myErr ($1 ++ " cannot be empty") -- ($1 ++ " => " ++ "[]")
          (True, s:[]) -> ((Act ($1 , s) $5), S.fromList([$1,s]))
          _            -> (Par (L.map (\s -> (Act ($1 , s) $5)) $3),S.fromList($1:$3))
    }
  | 'do' str
    { \env ->
        if M.member $2 env
        then env M.! $2
        else myErr ("Constant " ++ $2 ++ " is undefined")
    }
  | '*' GE '@' str
    { \env ->
        let
          (g, ptps) = $2 env
        in
          case ((isPtp $4), (S.member $4 ptps)) of
            (True, True)  -> (Rep g $4 , S.union (S.singleton $4) ptps)
            (False, _)    -> myErr ("Bad name " ++ $4)
            (True, False) -> myErr ("Participant " ++ $4 ++ " is not among the loop's participants: " ++ (show $ toList ptps))
    }
  | 'repeat' str '{' GE '}'
    { \env ->
        let
          (g, ptps) = $4 env
        in
          case ((isPtp $2), (S.member $2 ptps)) of
            (True, True)  -> (Rep g $2 , S.union (S.singleton $2) ptps)
            (False, _)    -> myErr ("Bad name " ++ $2)
            (True, False) -> myErr ("Participant " ++ $2 ++ " is not among the loop's participants: " ++ (show $ toList ptps))
    }
  | 'repeat' str '{' GE 'unless' guard '}'
    { \env ->
        let
          (g, ptps) = $4 env
        in
          case ((isPtp $2), (S.member $2 ptps)) of
            (True, True)  -> (Rep g $2 , S.union (S.singleton $2) ptps)
            (False, _)    -> myErr ("Bad name " ++ $2)
            (True, False) -> myErr ("Participant " ++ $2 ++ " is not among the loop's participants: " ++ (show $ toList ptps))
    }
  | '{' GE '}'
    { \env -> $2 env }
  | '(' GE ')'               -- this is for backward compatibility and it is deprecated
    { \env -> $2 env }


guard :: { M.Map String String }
guard : str '%' str             { M.insert $1 $3 M.empty }
      | str '%' str ',' guard   { M.insert $1 $3 $5 }


ptps :: { [String] }
ptps : str                      { if (isPtp $1) then [$1] else myErr ("Bad name " ++ $1) }
     | str ',' ptps             { if (isPtp $1)
                                  then (case $3 of
                                        [] ->  [$1]
                                        (s:l) -> ($1:s:l))
                                  else myErr ("Bad name " ++ $1)
                                }

{
data Token =
  TokenStr String
  | TokenEmp
  | TokenArr
  | TokenPar
  | TokenBra
  | TokenSel Int
  | TokenGrd
  | TokenSeq
  | TokenRep
  | TokenSta
  | TokenUnt
  | TokenSec
  | TokenBro
  | TokenBrc
  | TokenCom
  | TokenMAr
  | TokenUnl
  | TokenCurlyo
  | TokenCurlyc
  | TokenLet
  | TokenAnd
  | TokenIn
  | TokenDo
  | TokenMap
  | TokenEof
  deriving (Show)

lexer :: (Token -> Ptype a) -> Ptype a
lexer cont s (l, c) (l',c') =
  -- (l,c) is the currently reached position in the parsing
  -- (l',c') is the position of the last accepted token
  case s of
    'b':'r':'a':'n':'c':'h':x:r ->
      case x of
        ' '  -> cont (TokenSel 6) r (l, (c+7)) (l, c)
        '\t' -> cont (TokenSel 6) r (l, (c+7)) (l, c)
        '{'  -> cont (TokenSel 6) ('{':r) (l, (c+6)) (l, c)
        '\n' -> cont (TokenSel 6) r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    'r':'e':'p':'e':'a':'t':x:r ->
      case x of
        ' '  -> cont TokenRep r (l, (c+7)) (l, c)
        '\t' -> cont TokenRep r (l, (c+7)) (l, c)
        '{'  -> cont TokenRep ('{':r) (l, (c+6)) (l, c)
        '\n' -> cont TokenRep r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    'u':'n':'l':'e':'s':'s':x:r ->
      case x of
        ' '  -> cont TokenUnl r (l, (c+7)) (l, c)
        '\t' -> cont TokenUnl r (l, (c+7)) (l, c)
        '{'  -> cont TokenUnl ('{':r) (l, (c+6)) (l, c)
        '\n' -> cont TokenUnl r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    'l':'e':'t':x:r ->
      case x of
        ' '  -> cont TokenLet r (l, (c+4)) (l, c)
        '\t' -> cont TokenLet r (l, (c+4)) (l, c)
        '{'  -> cont TokenLet ('{':r) (l, (c+3)) (l, c)
        '\n' -> cont TokenLet r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    's':'e':'l':x:r ->
      case x of
        ' '  -> cont (TokenSel 3) r (l, (c+4)) (l, c)
        '\t' -> cont (TokenSel 3) r (l, (c+4)) (l, c)
        '{'  -> cont (TokenSel 3) ('{':r) (l, (c+3)) (l, c)
        '\n' -> cont (TokenSel 3) r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    '|':'-':'>':x:r ->
      case x of
        ' '  -> cont TokenMap r (l, (c+4)) (l, c)
        '\t' -> cont TokenMap r (l, (c+4)) (l, c)
        '{'  -> cont TokenMap ('{':r) (l, (c+3)) (l, c)
        '\n' -> cont TokenMap r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    'i':'n':x:r ->
      case x of
        ' '  -> cont TokenIn r (l, (c+3)) (l, c)
        '\t' -> cont TokenIn r (l, (c+3)) (l, c)
        '{'  -> cont TokenIn ('{':r) (l, (c+2)) (l, c)
        '\n' -> cont TokenIn r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    'd':'o':x:r ->
      case x of
        ' '  -> cont TokenDo r (l, (c+3)) (l, c)
        '\t' -> cont TokenDo r (l, (c+3)) (l, c)
        '{'  -> cont TokenDo ('{':r) (l, (c+2)) (l, c)
        '\n' -> cont TokenDo r ((l+1), 0) (l, c)
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    '(':'o':')':r ->
      cont TokenEmp r (l, (c+3)) (l, c)
    '.':'.':r ->
      (lexer cont) (dropWhile (\c->c/='\n') r) (l, 0) (l',c')
    '-':'>':r ->
      cont TokenArr r (l, (c+2)) (l, c)
    '=':'>':r ->
      cont TokenMAr r (l, (c+2)) (l, c)
    '[':'[':r ->
      let
        takeComment acc s =
          case s of
            ']':']':_ -> acc
            _ -> takeComment (acc ++ [head s]) (tail s)
        tmp = takeComment "" r
        lskip = l + L.length (L.filter (\c -> c == '\n') tmp)
        cskip = 0 -- c + if lskip==0 then (length tmp) else 0
      in
        if tmp == r
        then Er ("Syntax error at <" ++ (show $ l+1) ++ "," ++ (show $ c) ++ ">: " ++ "multiline comment not closed")
        else lexer cont (tail $ tail (r \\ tmp)) (lskip, cskip) (l', c')
    x:r ->
      case x of
        '&' -> cont TokenAnd r (l, (c+1)) (l, c)
        '*' -> cont TokenSta r (l, (c+1)) (l, c)
        '%' -> cont TokenGrd r (l, (c+1)) (l, c)
        '@' -> cont TokenUnt r (l, (c+1)) (l, c)
        ':' -> cont TokenSec r (l, (c+1)) (l, c)
        ';' -> cont TokenSeq r (l, (c+1)) (l, c)
        '|' -> cont TokenPar r (l, (c+1)) (l, c)
        '+' -> cont TokenBra r (l, (c+1)) (l, c)
        ',' -> cont TokenCom r (l, (c+1)) (l, c)
        '(' -> cont TokenBro r (l, (c+1)) (l, c)
        ')' -> cont TokenBrc r (l, (c+1)) (l, c)
        '{' -> cont TokenCurlyo r (l, (c+1)) (l, c)
        '}' -> cont TokenCurlyc r (l, (c+1)) (l, c)
        ' ' -> (lexer cont) r (l, (c+1)) (l', c')
        '\t' -> (lexer cont) r (l, (c+1)) (l', c')
        '\n' -> (lexer cont) r ((l+1), 0) (l', c')
        _ -> (cont (TokenStr (fst s'))) (snd s') (l, (c + (length s'))) (l, c)
    [] ->
      (cont TokenEof) "" (l, c) (l',c')
  where s' = span isAlpha s

data ParseResult a =
  Ok a
  | Er String
  deriving (Show)

type Ptype a = String -> (Int, Int) -> (Int, Int) -> ParseResult a

parseError token =
  \ _ _ (l, c) ->
    Er (synErr l c token)

thenPtype :: Ptype a -> (a -> Ptype b) -> Ptype b
m `thenPtype` k = \s (l, c) (l', c') ->
  case m s (l, c) (l', c') of
    Ok v -> k v s (l, c) (l',c')
    Er e -> Er e

returnPtype :: a -> Ptype a
returnPtype a = \s _ _ -> Ok a

failPtype :: String -> Ptype a
failPtype err = \_ _ _ -> Er err

-- GC specific functions

synErr :: Int -> Int -> Token -> String
synErr l c token =
  "Syntax error at <" ++ (show (l+1)) ++ "," ++ (show $ c+1) ++ ">: " ++ err
  where
    err =
      case token of
        TokenStr s  ->  "unexpected string or malformed string: " ++ s ++ "\n\t the following characters are forbidden: \'@\' \'.\' \',\' \';\' \':\' \'(\' \')\' \'[\' \']\' \'{\' \'}\' \'|\' \'+\' \'*\' \'!\' \'?\' \'-\' \'%\' \'§\'"
        TokenEmp    ->  "unexpected \'(o)\'"
        TokenArr    ->  "unexpected \'->\'"
        TokenPar    ->  "unexpected \'|\'"
        TokenBra    ->  "unexpected \'+\'"
        TokenSel o  ->  "unexpected branching start"
        TokenGrd    ->  "unexpected guard"
        TokenSeq    ->  "unexpected \';\'"
        TokenRep    ->  "unexpected loop"
        TokenSta    ->  "unexpected loop"
        TokenUnt    ->  "unexpected \'@\'"
        TokenSec    ->  "unexpected \':\'"
        TokenBro    ->  "unexpected \'(\'"
        TokenBrc    ->  "unexpected \')\'"
        TokenCom    ->  "unexpected \',\'"
        TokenMAr    ->  "unexpected =>"
        TokenUnl    ->  "unexpected \'unless\' clause"
        TokenCurlyo ->  "unexpected \'{'"
        TokenCurlyc ->  "unexpected \'}'"
        TokenLet    ->  "unexpected \'let\'"
        TokenAnd    ->  "unexpected \'&\'"
        TokenIn     ->  "unexpected \'in\'"
        TokenDo     ->  "unexpected \'do\'"
        TokenMap    ->  "unexpected \'|->\'"
        TokenEof    ->  "Parse error; perhaps an unexpected trailing symbol"


myErr :: String -> a
myErr err = error ("gcparser: ERROR - " ++ err)

ptpsBranches :: [((GC, Set Ptp), ReversionGuard)] -> Set Ptp
-- to be revised: also participants in constants to be taken
ptpsBranches =
  \l -> L.foldr S.union S.empty (L.map (snd . fst) l)

checkGuard :: (GC, Set Ptp) -> ReversionGuard -> ((GC, Set Ptp), ReversionGuard)
checkGuard gc@(g, ptps) m =
  let
    tmp = [ x | x <- M.keys m, not (S.member x ptps) ]
  in
    if L.null tmp
    then (gc, m)
    else myErr ("Unknown participant(s): " ++ (show tmp))

-- checkToken 'flattens', parallel, branching, and sequential composition
checkToken :: Token -> (GC, Set Ptp) -> [GC]
checkToken t (g,_) =
  case t of
    TokenPar -> case g of
      Par l -> l
      _ -> [g]
    TokenBra -> case g of
      Bra l -> M.elems l
      _ -> [g]
    TokenSeq -> case g of
      Seq l -> l
      _ -> [g]
    _        -> [g]

-- gcsptp computes the set of participants of a syntactic global graph
gcsptp :: Set Ptp -> GC -> Set Ptp
gcsptp ps g = case g of
               Emp         -> ps
               Act (s,r) _ -> S.union ps (S.fromList [s,r])
               Par gs      -> S.union ps (S.unions (L.map (gcsptp S.empty) gs))
               Bra gs      -> S.union ps (S.unions $ M.elems $ M.map (gcsptp S.empty) gs)
               Seq gs      -> S.union ps (S.unions (L.map (gcsptp S.empty) gs))
               Rep g' p    -> S.union ps (gcsptp (S.singleton p) g')

compEnv :: GCEnv -> GCEnv -> GCEnv
compEnv env env' =
  let
    common = M.intersection env env'
  in
    if (M.null common)
    then M.union env env'
    else myErr ("Double definition of constants: " ++ (mkSep (M.keys common) ", "))

}
