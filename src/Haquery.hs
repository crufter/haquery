{-# LANGUAGE OverloadedStrings #-}

{-|

Haquery is jQuery for Haskell.

Known problems:

Some selectors need explicit * to work:
":eq(0)" does not work but "*:eq(0)" does, same with "> b": "* > b", etc.

-}

module Haquery (
    -- * Types
    Attrs(), Tag(..), Child(..),
    -- * Rendering
    render,
    -- * Tag creation
    tag,
    doctype,
    html,
    head',
    body,
    div',
    text,
    form,
    input,
    article,
    a,
    title,
    link,
    script,
    -- Attributes
    at,
    (-.),
    id',
    class',
    src,
    href,
    type',
    style,
    -- * Nested tag functions
    alter,
    remove,
    select,
    -- * Single tag functions
    attrs,
    attr,
    children,
    name,
    innerText,
    append,
    prepend,
    -- * Manipulation
    addClass,
    removeClass,
    hasClass,
    toggleClass,
    -- * Parse HTML
    parseHtml,
    -- * Exported for testing purposes only
    matches,
    parseSelector,
    example
) where

import           Control.Monad
import           Control.Monad.Loops
import qualified Control.Monad.Trans.Except         as X
import qualified Data.Char                          as C
import qualified Data.Functor.Identity              as I
import qualified Data.List                          as L
import qualified Data.List.Split                    as Spl
import qualified Data.Map                           as M
import qualified Data.Text                          as T
import qualified Text.HTML.TagSoup                  as TS
import qualified Text.HTML.TagSoup.Tree             as TT
import qualified Text.Parsec.Prim                   as Pr
import qualified Text.ParserCombinators.Parsec      as P
import qualified Text.ParserCombinators.Parsec.Expr as E
import qualified Text.ParserCombinators.Parsec.Pos  as Pos

-- This package contains some questionable temporary names now to avoid clash with prelude.

{--------------------------------------------------------------------
  Types.
--------------------------------------------------------------------}

-- For simplicity reasons, currently only attributes exist and no properties.
data Attrs = Attrs (M.Map T.Text T.Text) deriving (Eq)

toAttrs :: [(T.Text, T.Text)] -> Attrs
toAttrs l = Attrs $ M.fromList l

attrsToMap :: Attrs -> M.Map T.Text T.Text
attrsToMap (Attrs at) = at

index :: Tag -> Int
index t = case t of
    Doctype i _ -> f i
    Text i _    -> f i
    Tag i _ _ _ -> f i
    where
    f x = if length x == 0
        then -1
        else last x

indexes :: Tag -> [Int]
indexes t = case t of
    Doctype i _ -> i
    Text i _    -> i
    Tag i _ _ _ -> i

-- Proof of concept currently, looks quadratic, I suspect lazyness
-- makes it a bit more effective than a strict version would be, that is
-- a not too educated guess though.
setIndex :: Int -> Tag -> Tag
setIndex i t = case t of
    Doctype ind te -> Doctype (i:ind) te
    Text ind te    -> Text (i:ind) te
    Tag ind te a c -> Tag (i:ind) te a $ map (setIndex i) c

type Child = Tag

data Tag =
        Doctype [Int]       T.Text
    |   Text    [Int]       T.Text
    --          Ind         Name        Attributes      Children
    |   Tag     [Int]       T.Text      Attrs           [Child]
    deriving (Eq)

-- | Create any tag.
tag :: T.Text -> [(T.Text, T.Text)] -> [Tag] -> Tag
tag n a c   = tag' n (toAttrs a) c

tag' :: T.Text -> Attrs -> [Tag] -> Tag
tag' n a c = Tag [] n a $ indexify c
    where
    indexify cs = map (\(a, b) -> setIndex a b) $ zip [0..] cs

-- Some frequently used tags here
doctype a   = Doctype [] a
html a c    = tag "html" a c
head' a c   = tag "head" a c
body a c    = tag "body" a c
text t      = Text [] t
div' a c    = tag "div" a c
form a c    = tag "form" a c
input a c   = tag "input" a c
a a1 c      = tag "a" a1 c
article a c = tag "article" a c
title a c   = tag "title" a c
link a c    = tag "link" a c
script a c  = tag "script" a c
img a c     = tag "img" a c

-- Frequently used attributes
id' a       = at "id" a
class' a    = at "class" a
src a       = at "src" a
style a     = at "style" a
href a      = at "href" a
type' a     = at "type" a

-- | Create any attribute.
at :: T.Text -> T.Text -> (T.Text, T.Text)
at a b = (a, b)

infix 0 -.
-- | Same as cat
(-.) :: T.Text -> T.Text -> (T.Text, T.Text)
(-.) a b = at a b

example :: Tag
example =
    html [] [
        head' [] [],
        body [at "style" "background: #ccc;"] [
            text "Hello world.",
            div' [at "class" "just-a-div"] [],
            div' [] [
                text "Hello again."
            ]
        ]
    ]

{--------------------------------------------------------------------
  Rendering.
--------------------------------------------------------------------}

voidElements :: M.Map T.Text ()
voidElements = M.fromList $ map (\x -> (x, ()))
    ["area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link", "menuitem", "meta", "param", "source", "track", "wbr"]

instance Show Attrs where
    show (Attrs at) = L.intercalate " " $ map (\(a, b) -> T.unpack a ++ "=" ++ show b) $ M.toList at

-- Show attributes
sa :: Attrs -> String
sa a@(Attrs at) = if M.size at > 0
        then " " ++ show a
        else ""

-- Show children.
sc :: [Child] -> String
sc x = L.intercalate "" $ map (\y -> tabLines $ show y) x
    where tabLines x = unlines $ map (\y -> "    " ++ y) $ lines x

-- | Show is used for pretty printing, for actual rendering/serialization
-- use render.
instance Show Tag where
    show (Doctype i a)  = "<!DOCTYPE " ++ T.unpack a ++ ">"
    show (Tag i n a c)  = case M.lookup n voidElements of
        Just ()     -> "<" ++ T.unpack n ++ sa a ++ "/>"
        Nothing     -> if length c > 0
            then "<" ++ T.unpack n ++ sa a ++ ">\n" ++ sc c ++ "</" ++ T.unpack n ++ ">"
            else "<" ++ T.unpack n ++ sa a ++ "></" ++ T.unpack n ++ ">"
    show (Text i a)     = T.unpack a

-- | Render tag to text.
render :: Tag -> T.Text
render (Doctype i a)  = T.concat ["<!DOCTYPE ", a, ">"]
render (Tag i n a c)  = case M.lookup n voidElements of
    Just ()     -> T.concat ["<", n, T.pack $ sa a, "/>"]
    Nothing     -> if length c > 0
        then T.concat $ ["<", n, T.pack $ sa a, ">"] ++ map render c ++ ["</", n, ">"]
        else T.concat $ ["<", n, T.pack $ sa a, "></", n, ">"]
render (Text i a)     = a

{--------------------------------------------------------------------
  Single tag functions.
--------------------------------------------------------------------}

-- | Returns the attributes of a tag.
attrs :: Tag ->         Attrs
attrs (Doctype _ _) = toAttrs []
attrs (Text _ _)    = toAttrs []
attrs (Tag _ _ a _) = a

-- | Returns an attribute of a tag
-- specified by the first argument.
attr :: T.Text -> Tag -> Maybe T.Text
attr attrName tag = case M.lookup attrName $ attrsToMap $ attrs tag of
    Just v  -> Just v
    Nothing -> Nothing

-- | Sets the attribute of a tag
-- specified by the first argument.
setAttr :: T.Text -> T.Text -> Tag -> Tag
setAttr key val tag = case tag of
    Doctype _ _         -> tag
    Text _ _            -> tag
    Tag i n (Attrs a) c -> Tag i n (Attrs $ M.insert key val a) c

-- | Returns the (direct) children of a tag.
children :: Tag ->          [Child]
children (Doctype _ _) = []
children (Text _ _)    = []
children (Tag _ _ _ c) = c

-- | Returns the name of a tag.
name :: Tag ->      T.Text
name (Doctype _ _) = "doctype"
name (Text _ _)    = "text"
name (Tag _ n _ _) = n

-- | Returns text contents of a tag
innerText :: Tag -> T.Text
innerText (Doctype _ text)     = text
innerText (Text _ text)        = text
innerText (Tag _ _ _ children) = T.concat $ map innerText children

{--------------------------------------------------------------------
  Manipulation.
--------------------------------------------------------------------}

-- | Adds the class specified by the first argument to the tag.
addClass :: T.Text -> Tag -> Tag
addClass clas tag = case attr "class" tag of
    Nothing     -> setAttr "class" clas tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then tag
        else setAttr "class" (T.intercalate " " $ clas:spl) tag

-- | Returns true if the tag already has the given class.
hasClass :: T.Text -> Tag -> Bool
hasClass clas tag = case attr "class" tag of
    Nothing -> False
    Just c  -> let spl = T.splitOn " " c in elem clas spl

removeClass :: T.Text -> Tag -> Tag
removeClass clas tag = case attr "class" tag of
    Nothing     -> tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then setAttr "class" (T.intercalate " " $ filter (/= clas) spl) tag
        else tag

toggleClass :: T.Text -> Tag -> Tag
toggleClass clas tag = case attr "class" tag of
    Nothing     -> setAttr "class" clas tag
    Just c      -> let spl = T.splitOn " " c in if elem clas spl
        then setAttr "class" (T.intercalate " " $ filter (/= clas) spl) tag
        else setAttr "class" (T.intercalate " " $ clas:spl) tag

-- | Insert the first argument as the last child of the second.
append :: Tag -> Tag -> Tag
append what to = case to of
        Tag ind n a c -> Tag ind n a (children to ++ [what])
        otherwise     -> to

-- | Inserts the first argument as the first child of the second.
prepend :: Tag -> Tag -> Tag
prepend what to = case to of
        Tag ind n a c -> Tag ind n a (what:c)
        otherwise     -> to

{--------------------------------------------------------------------
  Selector implementation.
--------------------------------------------------------------------}

-- | Just for quick and ugly testing.
-- Tests if a (top level) tag satisfies a given selector
matches :: T.Text -> Tag -> Either String Bool
matches sel tag = matches' [] (parseSelector sel) tag

-- Returns true of the tag or its descendants match the selector.
has :: Selector -> Tag -> Either String Bool
has sel t = do
    b <- matches' [] sel t
    if b
        then return True
        else anyM (has sel) $ children t

calcRoot :: [Tag] -> Tag -> Tag
calcRoot parents tag = if length parents == 0
    then tag
    else parents!!0

filtIndex :: (Int -> Bool) -> [a] -> [a]
filtIndex pred xs =
    let indexes = [0 ..(length xs) - 1]
        iterable = zip indexes xs
    in map (\(_, b) -> b) $ filter (\(a, _) -> pred a) iterable

-- Returns true if given tag matches the selector provided.
matches' :: [Tag] -> Selector -> Tag -> Either String Bool
matches' parents s tag = case s of
    Type t  -> Right $ name tag == t
    Id  id  -> Right $ case attr "id" tag of
        Just x  -> x == id
        Nothing -> False
    Class c -> Right $ hasClass c tag
    Attribute reg attrName attrVal -> Right $ case attr attrName tag of
        Nothing -> False
        Just x  -> case reg of
            StartsWith -> T.isPrefixOf attrVal x
            EndsWith   -> T.isSuffixOf attrVal x
            Contains   -> T.isInfixOf attrVal x
            Equals     -> x == attrVal
            Anything   -> True
    And selectors   -> allM (\x -> matches' parents x tag) selectors
    Or selectors    -> anyM (\x -> matches' parents x tag) selectors
    Not selector    -> not <$> matches' parents selector tag
    Has selector    -> anyM (has selector) $ children tag
    AncestorIs sel  -> if length parents == 0
        then Right False
        else let parinits = zip (L.inits parents) parents in
            anyM (\(pars, subj) -> matches' pars sel subj) parinits
    ParentIs sel    -> if length parents == 0
        then Right False
        else matches' (init parents) sel $ last parents
    FirstChild      -> Right $ index tag == 0
    LastChild       -> Right $
        let pl = length parents
            pcl = length . children $ last parents
        in if pl == 0 || pcl < 2    -- A children can not be the first and last too at the same time.
            then False
            else pcl == index tag + 1
    Eq selector n   -> do
        let root = calcRoot parents tag
        tm <- take (n+1) <$> select' selector root
        Right $ if length tm <= n
            then False
            else (tm!!n) == tag
    -- Revisit this.
    LesserThan s n  -> do
        let root = calcRoot parents tag
        tm <- take n <$> select' s root
        Right $ any (== tag) tm
    GreaterThan s n -> do
        let root = calcRoot parents tag
        tm <- drop (n + 1) <$> select' s root
        Right $ any (== tag) tm
    First selector  -> do
        let root = calcRoot parents tag
        tm <- take 1 <$> select' selector root
        Right $ any (== tag) tm
    Last selector   -> do
        let root = calcRoot parents tag
        tm <- select' selector root
        Right $ if length tm == 0
            then False
            else last tm == tag
    Even selector   -> do
        let root = calcRoot parents tag
        tm <- filtIndex (\x -> x `mod` 2 == 0) <$> select' selector root
        Right $ any (== tag) tm
    Odd selector    -> do
        let root = calcRoot parents tag
        tm <- filtIndex (\x -> x `mod` 2 == 1) <$> select' selector root
        Right $ any (== tag) tm
    NthChild n      -> Right $ index tag + 1 == n   -- Note that this selector is 1-indexed.
    NthLastChild n  -> Right $
        let pl = length parents
            pcl = length . children $ last parents
        in if pl == 0 || pcl < n
            then False
            else pcl - n - 1 == index tag
    Empty           -> Right $ length (children tag) == 0
    Parent          -> Right $ length (children tag) /= 0
    NextAdj s       -> if length parents == 0
        then Right False
        else let sibs = take (index tag) . children $ last parents
            in if length sibs > 0
                then matches' parents s (last sibs)
                else Right False
    NextSibl s      -> if length parents == 0
        then Right $ False
        else let sibs = take (index tag) . children $ last parents
            in if length sibs > 0
                then anyM (matches' parents s) sibs
                else Right $ False
    Any             -> Right $ True
    --
    Descendant      -> Left "matches': bug: Descendant"
    DirectChild     -> Left "matches': bug: DirectChild"
    Placeholder     -> Left "matches': bug: Placeholder"
    Comma           -> Left "matches': bug: Comma"
    otherwise       -> Left $ "matches': bug: " ++ show s

{--------------------------------------------------------------------
  Nested tag functions.
--------------------------------------------------------------------}

-- | Apply function on elements matching the selector.
alter :: T.Text -> (Tag -> Tag) -> Tag -> Either String Tag
alter sel f t =
    let sels = parseSelector sel
        alterRec :: [Tag] -> Tag -> Either String Tag
        alterRec parents tag = case tag of
            Tag i n a c     -> appif $ Tag i n a <$> mapM (alterRec $ parents ++ [tag]) c
            _               -> appif (return tag)
            where
                appif t = do
                    b <- matches' parents sels tag
                    if b
                        then f <$> t
                        else t
    in alterRec [] t

-- | Remove tags matching the selector.
-- Does not remove the provided tag itself.
remove :: T.Text -> Tag -> Either String Tag
remove sel t =
    let sels = parseSelector sel
        removeRec :: [Tag] -> Tag -> Either String Tag
        removeRec parents t = case t of
            Tag i n a c -> tag' n a <$> (filterM (matches' parents sels) =<< mapM (removeRec $ parents ++ [t]) c)
            _           -> return t
    in removeRec [] t

-- | Returns tags matching the selector.
-- Obiously not too useful if you want to alter the given elements, because
-- of Haskell's purity. See alter and remove instead.
select :: T.Text -> Tag -> Either String [Tag]
select sel t = let sels = parseSelector sel in select' sels t

select' :: Selector -> Tag -> Either String [Tag]
select' sel t =
    let selectRec :: [Tag] -> Tag -> Either String [Tag]
        selectRec parents tag = case tag of
            Tag _ _ _ c -> (++) <$> retif tag <*> (concat <$> mapM (selectRec $ parents ++ [tag]) c)
            _           -> retif tag
            where
                retif t = do
                    b <- matches' parents sel tag
                    if b
                        then return [t]
                        else return []
    in selectRec [] t

{--------------------------------------------------------------------
  Selectors.
--------------------------------------------------------------------}

-- Selectors planned.

-- Implemented      Example                     Name
-- Y                *                           - All selector
-- Y                #id                         - Id selector
-- Y                .class                      - Class selector
-- Y                selector1 selector2         - Descendant selector
-- Y                type                        - Type selector
-- Y                selector1 > selector2       - Direct child selector
-- Y                [attrName]                  - Has attribute selector
-- Y                [attrName="val"]            - Attribute equals selector
-- Y                [attrName*="val"]           - Attribute contains selector
-- Y                [attrName^="val"]           - Attribute starts with selector
-- Y                [attrName$="val"]           - Attribute ends with selector
-- Y                [attrName~="val"]           - Attribute contains word selector
-- Y                [attrName!="val"]           - Attribute not equals selector
-- Y                selector1, selector2        - Multiple selectors selector
-- Y                prev + next                 - Next adjacent selector
-- Y                prev ~ siblings             - Next siblings selector
-- Y                :not(selector)              - :not() selector
-- Y                :has(selector)              - :has() selector
-- Y                :eq(3)                      - :eq() selector
-- Y                :lt(3)                      - :lt() selector
-- Y                :gt(3)                      - :gt() selector
-- Y                :even                       - :even selector
-- Y                :odd                        - :odd selector
-- Y                :first                      - :first selector
-- Y                :last                       - :last selector
-- Y                :first-child                - :first-child selector
-- Y                :last-child                 - :last-child selector
-- Y                :nth-child(3)               - :nth-child() selector
-- Y                :nth-last-child(3)          - :nth-last-child() selector
-- Y                :empty                      - :empty selector
-- Y                :parent                     - :parent selector

data Regexy =
        StartsWith
    |   EndsWith
    |   Contains
    |   Equals
    |   Anything
    |   ContainsWord
    |   NotEquals
    deriving (Eq, Show)

data Selector =
        Any
    |   Type        T.Text
    |   Id          T.Text
    |   Class       T.Text
    |   Eq Selector Int | Even Selector | Odd Selector
    |   LesserThan Selector Int | GreaterThan Selector Int
    |   First Selector | Last Selector
    |   Parent | Empty
    |   FirstChild | LastChild | NthChild Int | NthLastChild Int
    -- Currently you can only apply flat selectors in an and.
    -- (eg: no descendant or direct child)
    |   And         [Selector]
    |   Or          [Selector]
    |   Not         Selector
    |   Has         Selector
    |   ParentIs    Selector
    |   AncestorIs  Selector
    |   Attribute   Regexy T.Text T.Text     -- Regex type, tag name, attrname, attrval
    |   NextAdj     Selector
    |   NextSibl    Selector
    -- Operators
    |   Comma
    |   Descendant
    |   DirectChild
    |   IndSep
    |   AndSep
    |   Plus
    |   Tilde
    -- Placeholder
    |   Placeholder
    deriving (Eq, Show)

{--------------------------------------------------------------------
  Parsing selectors.
--------------------------------------------------------------------}

l :: a -> [a]
l x = [x]

setCrit x v = case x of
    Eq s  i         -> Eq v i
    LesserThan s i  -> LesserThan v i
    GreaterThan s i -> GreaterThan v i
    Even s          -> Even v
    Odd s           -> Odd v
    First s         -> First v
    Last s          -> Last v
    otherwise       -> error $ "bug: can't set crit of " ++ show x

isInd x = case x of
    Eq _  _         -> True
    Even _          -> True
    Odd _           -> True
    First _         -> True
    Last _          -> True
    LesserThan _ _  -> True
    GreaterThan _ _ -> True
    otherwise       -> False

isOp :: Selector -> Bool
isOp s = case s of
    Comma       -> True
    Descendant  -> True
    DirectChild -> True
    IndSep      -> True
    AndSep      -> True
    Plus        -> True
    Tilde       -> True
    otherwise   -> False

-- Inserts e between every two elements of a list if both satisfies the predicate.
lace :: (a -> a -> Bool) -> a -> [a] -> [a]
lace pred e l
    | length l < 2      = l
    | otherwise         = if pred (l!!0) (l!!1)
        then (l!!0):e:(lace pred e $ tail l)
        else (l!!0):(lace pred e $ tail l)

simple s = (not $ isInd s) && (not $ isOp s)

-- This is ugly.
laceAnd :: [Selector] -> [Selector]
laceAnd ss = lace f AndSep ss
    where
    f a b   | not (isOp a) && simple b     = True
            | otherwise                                 = False

operatorTable ::[[E.Operator Selector () Selector]]
operatorTable = [
        [
            binary (== AndSep) (\a b -> And [a, b]) E.AssocLeft,
            binary (== IndSep) (\a b -> setCrit b a) E.AssocLeft
        ],
        [
            binary (== Tilde) (\a b -> And [b, NextSibl a]) E.AssocLeft,
            binary (== Plus) (\a b -> And [b, NextAdj a]) E.AssocLeft,
            binary (== Descendant) (\a b -> And [b, AncestorIs a]) E.AssocLeft,
            binary (== DirectChild) (\a b -> And [b, ParentIs a]) E.AssocLeft
        ],
        [binary (== Comma) (\a b -> Or [a, b]) E.AssocRight]
    ]

nextPos pos x xs  = Pos.incSourceColumn pos 1

predM :: (a -> Bool) -> (a -> Maybe a)
predM x = \y -> if x y
    then Just y
    else Nothing

binary pred fun assoc = E.Infix (do
    P.try $ Pr.tokenPrim show nextPos $ predM pred
    return fun)
    assoc

parseSelector :: T.Text -> Selector
parseSelector t =
    let errMsg = "parseSelector: can't parse selector: " ++ show t
        sels = P.parse parseExpr errMsg $ T.unpack t
    in case sels of
        Left e   -> error $ show e
        Right ss -> parseSelPrec $ concat ss

term :: Pr.ParsecT [Selector] () I.Identity Selector
term = do
    x <- P.anyToken
    return x

parseSelPrec :: [Selector] -> Selector
parseSelPrec ss =
    let errMsg = "parseSelector: can't parse expression: " ++ show ss
        prec = E.buildExpressionParser operatorTable term P.<?> "expression"
        laced = laceAnd ss
        sels = P.parse prec errMsg laced
    in case sels of
        Left e    -> error $ show e
        Right sel -> sel

parseString :: P.Parser T.Text
parseString = do
    P.char '"'
    x <- P.many (P.noneOf "\"")
    P.char '"'
    return $ T.pack x

symbol :: P.Parser Char
symbol = P.oneOf "-_"

parseNonquoted :: P.Parser T.Text
parseNonquoted = do
    first <- P.letter P.<|> symbol
    rest <- P.many (P.letter P.<|> P.digit P.<|> symbol)
    return $ T.pack $ first:rest

parseDescendant :: P.Parser [Selector]
parseDescendant = do
    P.space
    return $ l Descendant

parseId :: P.Parser [Selector]
parseId = do
    P.char '#'
    id <- parseNonquoted
    return $ l $ Id id

parseClass :: P.Parser [Selector]
parseClass = do
    P.char '.'
    clas <- parseNonquoted
    return $ l $ Class clas

parseTyp :: P.Parser [Selector]
parseTyp = do
    typ <- parseNonquoted
    return $ l $ Type typ

ts x = P.try $ P.string x

parseCons :: P.Parser [Selector]
parseCons = do
    c <- P.char '*' P.<|> P.char ':'
    case c of
        '*'     -> return $ l Any
        ':'     -> do
            cons <- ts "empty" P.<|> P.string "parent"
                P.<|> ts "first-child" P.<|> ts "last-child"
                P.<|> P.string "first" P.<|> P.string "last"
                P.<|> P.string "even" P.<|> P.string "odd"
            return $ case cons of
                "parent"      -> [Parent]
                "empty"       -> [Empty]
                "last"        -> [IndSep, Last Placeholder]
                "first"       -> [IndSep, First Placeholder]
                "first-child" -> [FirstChild]
                "last-child"  -> [LastChild]
                "even"        -> [IndSep, Even Placeholder]
                "odd"         -> [IndSep, Odd Placeholder]

parseNthChildEq :: P.Parser [Selector]
parseNthChildEq = do
    a <- ts ":nth-child(" P.<|> ts ":nth-last-child(" P.<|> ts ":eq("
        P.<|> ts ":lt(" P.<|> ts ":gt("
    num <- P.many1 P.digit
    P.char ')'
    return $ let n = (read num)::Int in case a of
        ":nth-child("      -> [NthChild     n]
        ":nth-last-child(" -> [NthLastChild n]
        ":eq("             -> [IndSep, Eq Placeholder n]
        ":lt("             -> [IndSep, LesserThan   Placeholder n]
        ":gt("             -> [IndSep, GreaterThan  Placeholder n]

parseNotHas :: P.Parser [Selector]
parseNotHas = do
    a <- ts ":not(" P.<|> ts ":has("
    sels <- parseExpr
    P.string ")"
    return $ l $ case a of
        ":not(" -> Not $ parseSelPrec $ concat sels
        ":has(" -> Has $ parseSelPrec $ concat sels

parseCommaDCSiblings :: P.Parser [Selector]
parseCommaDCSiblings = do
    P.many P.space
    v <- P.char ',' P.<|> P.char '~' P.<|> P.char '+' P.<|> P.char '>'
    P.many P.space
    return $ l $ case v of
        ',' -> Comma
        '~' -> Tilde
        '+' -> Plus
        '>' -> DirectChild

parseAttr :: P.Parser [Selector]
parseAttr = do
    P.char '['
    attrName <- parseNonquoted
    mode <- P.many $ P.string "*=" P.<|> P.string "^="
        P.<|> P.string "$=" P.<|> P.string "="
        P.<|> P.string "~=" P.<|> P.string "!="
    val <- P.many $ parseNonquoted P.<|> parseString
    P.char ']'
    return $ l $ case mode of
        []     -> Attribute Anything       attrName    ""
        ["*="] -> Attribute Contains       attrName    (val!!0)
        ["^="] -> Attribute StartsWith     attrName    (val!!0)
        ["$="] -> Attribute EndsWith       attrName    (val!!0)
        ["~="] -> Attribute ContainsWord   attrName    (val!!0)
        ["!="] -> Attribute NotEquals      attrName    (val!!0)
        ["="]  -> Attribute Equals         attrName    (f val)
    where
        f :: [T.Text] -> T.Text
        f x = if length x == 0
            then ""
            else x!!0

-- While try is not required everywhere,
-- they are there for simplicity and easier extendability.
parseExpr :: P.Parser [[Selector]]
parseExpr = P.many1 $ P.try parseId
    P.<|> P.try parseClass
    P.<|> P.try parseAttr
    P.<|> P.try parseTyp
    P.<|> P.try parseCommaDCSiblings
    P.<|> P.try parseDescendant
    P.<|> P.try parseCons
    P.<|> P.try parseNthChildEq
    P.<|> P.try parseNotHas

{--------------------------------------------------------------------
  Parse HTML.
--------------------------------------------------------------------}

-- | Turns text into tags.
parseHtml :: T.Text -> Either String [Tag] --  Either String [Tag]
parseHtml t =
    let
        tgs = (TS.parseTags t) :: [TS.Tag T.Text]
        whiteSpace :: TS.Tag T.Text -> Bool
        whiteSpace t = case t of
            TS.TagText s -> T.length (T.filter (not . C.isSpace) s) == 0
            _            -> False
        -- Filter text nodes only containing whitespace.
        tgs'  = filter (not . whiteSpace) tgs
        trees = TT.tagTree tgs'
        convert :: TT.TagTree T.Text -> Either String Tag -- Either String Tag
        convert tr = case tr of
            TT.TagBranch tname atts childr ->
                tag tname atts <$> mapM convert childr
            TT.TagLeaf tg -> case tg of
                TS.TagOpen tname' atts' -> Right $ tag tname' atts' []
                TS.TagText    str       -> Right $ text str
                TS.TagComment str       -> Right $ text str
                _                       -> Left $ "Unexpected tag type: " ++ show tg
    in
        mapM convert trees

