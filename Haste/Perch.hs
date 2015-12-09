-----------------------------------------------------------------------------
--
-- Module      :  Builder
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | Monad and Monoid instances for a builder that hang DOM elements from the
-- current parent element. It uses Haste.DOM from the haste-compiler
--
-----------------------------------------------------------------------------
{-#LANGUAGE CPP, ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances
            , OverloadedStrings, DeriveDataTypeable, UndecidableInstances
            , OverlappingInstances, InstanceSigs #-}
module Haste.Perch where
import Data.Typeable
import Haste hiding (Attribute)
import Haste.DOM hiding (Attribute, attr)
import Haste.Events
import Haste.Foreign
import Data.Maybe
import Data.Monoid
import Unsafe.Coerce
import Data.String
import Control.Monad.IO.Class
import Control.Applicative

import Prelude hiding (div, span)


newtype PerchM m a = Perch {build :: Elem -> m Elem} deriving Typeable

type Perch m = PerchM m ()

instance Monad m => Monoid (PerchM m a) where
    mappend mx my= Perch $ \e -> do
         build mx e
         build my e
         return e
    mempty  = Perch return

(<**) :: Monad m => Perch m -> Perch m -> Perch m
(<**) parp childp = Perch $ \e -> do
    par <- build parp e
    build childp par

instance Functor (PerchM m)
instance Applicative (PerchM m)

instance Monad m => Monad (PerchM m) where
   (>>) x y= mappend (unsafeCoerce x) y
   (>>=) = error "bind (>>=) invocation in the Perch monad creating DOM elements"
   return  = mempty

instance MonadIO m => MonadIO (PerchM m) where
  liftIO mx= Perch $ \e -> liftIO mx >> return e

instance MonadIO m => IsString (Perch m) where
  fromString = toElem

class ToElem a where
  toElem :: MonadIO m => a -> Perch m

instance ToElem String where
   toElem s= Perch $ \e -> do
        e' <- newTextElem s
        addChild e' e
        return e'

instance Show a => ToElem a where toElem = toElem . show

instance (MonadIO m) => ToElem (PerchM m a) where toElem e = unsafeCoerce e

attr :: MonadIO m => PerchM m a1 -> (PropID, String) -> PerchM m a
attr tag (n, v)=Perch $ \e -> do
        tag' <- build tag e
        setAttr tag' n v
        return tag'

nelem :: MonadIO m => String -> Perch m
nelem s= Perch $ \e ->do
        e' <- liftIO $ (newElem s :: IO Elem)
        liftIO $ (addChild e' e :: IO ())
        return e'

child :: (MonadIO m, ToElem a) => Perch m -> a -> Perch m
child me ch= Perch $ \e' -> do
        e <- build me e'
        let t = toElem ch
        r <- build t e
        return e

setHtml :: MonadIO m => Perch m -> String -> Perch m
setHtml me text= Perch $ \e' -> do
    e <- build me e'
    inner e text
    return e'
  where
  --inner :: Elem -> String -> m ()
  inner e txt = setProp e "innerHTML" txt

-- | create an element and add a Haste event handler to it.
addEvent :: (MonadEvent m, Event evt) => Perch m -> evt -> (EventData evt -> m ()) -> Perch m
addEvent be event action= Perch $ \e -> do
     e' <- build be e
     let atr= fromJSStr $ eventName event
     has <- getAttr e'  atr
     case has of
       "true" -> return e'
       _ -> do
        onEvent e' event  action
        setAttr e' atr "true"
        return e'





--listen :: JSType event => Elem -> event -> a -> IO Bool
--listen e event f= jsSetCB e (toJSString event) (mkCallback $! f)
--
--
--foreign import ccall jsSetCB :: Elem -> JSString -> JSFun a -> IO Bool


-- Leaf DOM nodes
--
area :: MonadIO m => Perch m
area = nelem "area"

base :: MonadIO m => Perch m
base = nelem "base"

br :: MonadIO m => Perch m
br = nelem "br"

col :: MonadIO m => Perch m
col = nelem "col"

embed :: MonadIO m => Perch m
embed = nelem "embed"

hr :: MonadIO m => Perch m
hr = nelem "hr"

img :: MonadIO m => Perch m
img = nelem "img"

input :: MonadIO m => Perch m
input = nelem "input"

keygen :: MonadIO m => Perch m
keygen = nelem "keygen"

link :: MonadIO m => Perch m
link = nelem "link"

menuitem :: MonadIO m => Perch m
menuitem = nelem "menuitem"

meta :: MonadIO m => Perch m
meta = nelem "meta"

param :: MonadIO m => Perch m
param = nelem "param"

source :: MonadIO m => Perch m
source = nelem "source"

track :: MonadIO m => Perch m
track = nelem "track"

wbr :: MonadIO m => Perch m
wbr = nelem "wbr"

-- Parent DOM nodes
--

a cont = nelem  "a" `child` cont
abbr cont = nelem  "abbr" `child` cont
address cont = nelem  "address" `child` cont
article cont = nelem  "article" `child` cont
aside cont = nelem  "aside" `child` cont
audio cont = nelem  "audio" `child` cont
b cont = nelem  "b" `child` cont
bdo cont = nelem  "bdo" `child` cont
blockquote cont = nelem  "blockquote" `child` cont
body cont = nelem  "body" `child` cont
button cont = nelem  "button" `child` cont
canvas cont = nelem  "canvas" `child` cont
caption cont = nelem  "caption" `child` cont
cite cont = nelem  "cite" `child` cont
code cont = nelem  "code" `child` cont
colgroup cont = nelem  "colgroup" `child` cont
command cont = nelem  "command" `child` cont
datalist cont = nelem  "datalist" `child` cont
dd cont = nelem  "dd" `child` cont
del cont = nelem  "del" `child` cont
details cont = nelem  "details" `child` cont
dfn cont = nelem  "dfn" `child` cont

div :: (MonadIO m, ToElem a) => a -> Perch m
div cont = nelem  "div" `child` cont

div_ :: (MonadIO m) => Perch m
div_ = div (mempty :: String)

dl cont = nelem  "dl" `child` cont
dt cont = nelem  "dt" `child` cont
em cont = nelem  "em" `child` cont
fieldset cont = nelem  "fieldset" `child` cont
figcaption cont = nelem  "figcaption" `child` cont
figure cont = nelem  "figure" `child` cont
footer cont = nelem  "footer" `child` cont
form cont = nelem  "form" `child` cont
h1 cont = nelem  "h1" `child` cont
h2 cont = nelem  "h2" `child` cont
h3 cont = nelem  "h3" `child` cont
h4 cont = nelem  "h4" `child` cont
h5 cont = nelem  "h5" `child` cont
h6 cont = nelem  "h6" `child` cont
head cont = nelem  "head" `child` cont
header cont = nelem  "header" `child` cont
hgroup cont = nelem  "hgroup" `child` cont
html cont = nelem  "html" `child` cont
i cont = nelem  "i" `child` cont
iframe cont = nelem  "iframe" `child` cont
ins cont = nelem  "ins" `child` cont
kbd cont = nelem  "kbd" `child` cont
label cont = nelem  "label" `child` cont
legend cont = nelem  "legend" `child` cont

li :: (MonadIO m, ToElem a) => a -> Perch m
li cont = nelem  "li" `child` cont

li_ :: (MonadIO m) => Perch m
li_ = li ("" :: String)

map cont = nelem  "map" `child` cont
mark cont = nelem  "mark" `child` cont
menu cont = nelem  "menu" `child` cont
meter cont = nelem  "meter" `child` cont
nav cont = nelem  "nav" `child` cont
noscript cont = nelem  "noscript" `child` cont
object cont = nelem  "object" `child` cont
ol cont = nelem  "ol" `child` cont
optgroup cont = nelem  "optgroup" `child` cont
option cont = nelem  "option" `child` cont
output cont = nelem  "output" `child` cont
p cont = nelem  "p" `child` cont
pre cont = nelem  "pre" `child` cont
progress cont = nelem  "progress" `child` cont
q cont = nelem  "q" `child` cont
rp cont = nelem  "rp" `child` cont
rt cont = nelem  "rt" `child` cont
ruby cont = nelem  "ruby" `child` cont
samp cont = nelem  "samp" `child` cont
script cont = nelem  "script" `child` cont
section cont = nelem  "section" `child` cont
select cont = nelem  "select" `child` cont
small cont = nelem  "small" `child` cont

span :: (MonadIO m, ToElem a) => a -> Perch m
span cont = nelem  "span" `child` cont

span_ :: (MonadIO m) => Perch m
span_ = span ("" :: String)

strong cont = nelem  "strong" `child` cont
{-style cont = nelem  "style" `child` cont-}
sub cont = nelem  "sub" `child` cont
summary cont = nelem  "summary" `child` cont
sup cont = nelem  "sup" `child` cont
table cont = nelem  "table" `child` cont
tbody cont = nelem  "tbody" `child` cont
td cont = nelem  "td" `child` cont
textarea cont = nelem  "textarea" `child` cont
tfoot cont = nelem  "tfoot" `child` cont
th cont = nelem  "th" `child` cont
thead cont = nelem  "thead" `child` cont
time cont = nelem  "time" `child` cont
title cont = nelem  "title" `child` cont
tr cont = nelem  "tr" `child` cont
ul cont = nelem  "ul" `child` cont
var cont = nelem  "var" `child` cont
video cont = nelem  "video" `child` cont


ctag tag cont= nelem tag `child` cont

-- HTML4 support
center cont= nelem "center" `child` cont

noHtml :: Monad m => Perch m
noHtml = mempty 

type Attribute = (String,String)

class Attributable h where
 (!) :: h -> Attribute -> h

instance (MonadIO m, ToElem a) => Attributable (a -> Perch m) where
 (!) :: (MonadIO m, ToElem a) => (a -> Perch m) -> Attribute -> (a -> Perch m)
 (!) pe atrib = \e -> pe e `attr` atrib

instance MonadIO m => Attributable (Perch m) where
 (!) = attr


atr n v= (n,v)

style= atr "style"

id = atr "id"

width= atr "width"

height= atr "height"

href= atr "href"

src= atr "src"


---------------- DOM Tree navigation and edition

-- | return the current node
this :: MonadIO m => Perch m
this= Perch $ \e -> return e

-- | goes to the parent node of the first and execute the second
goParent :: MonadIO m => Perch m -> Perch m -> Perch m
goParent pe pe'= Perch $ \e' -> do
  e <- build pe e'
  p <- parent e
  e2 <- build pe' p
  return e2

-- | delete the current node. Return the parent
delete :: MonadIO m => Perch m
delete= Perch $ \e -> do
             p <- parent e
             removeChild e p
             return p

-- | delete the children of the current node.
clear :: MonadIO m => Perch m
clear= Perch $ \e -> clearChildren e >> return e

-- | replace the current node with a new one
outer ::  MonadIO m => Perch m -> Perch m -> Perch m
outer olde  newe= Perch $ \e'' -> do
   e  <- build olde e''
   e' <- build newe e''
   replace e e'

replace :: MonadIO m => Elem -> Elem -> m Elem
replace el = liftIO . (ffi  "(function(e,e1){var par=  e.parentNode;par.replaceChild(e1,e);return e1;})" el :: Elem -> IO Elem)



parent :: MonadIO m => Elem -> m Elem
parent= liftIO . (ffi "(function(e){return e.parentNode;})" :: Elem -> IO Elem)


getBody :: MonadIO m => m Elem
getBody= liftIO $ (ffi "(function(){return document.body;})" :: IO Elem)

getDocument :: MonadIO m => m Elem
getDocument= liftIO $ (ffi  "(function(){return document;})" :: IO Elem)



-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
--
-- Example
--
-- > main= do
-- >  body <- getBody
-- >  (flip build) body $ pre $ do
-- >      div ! atr "class" "modify" $ "click"
-- >      div $ "not changed"
-- >      div ! atr "class" "modify" $ "here"
-- >
-- >      addEvent this OnClick $ \_ _ -> do
-- >          forElems' ".modify" $  this ! style "color:red"
forElems' :: MonadIO m => String -> Perch m -> m ()
forElems' for doit= do
    (flip build) undefined (forElems for doit)
    return ()

-- | a more declarative synmonym of `forElems'`
withElems' :: MonadIO m => String -> Perch m -> m ()
withElems'= forElems'

-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
forElems :: MonadIO m => String -> Perch m -> Perch m
forElems selectors dosomething= Perch $ \e -> do
    es <- queryAll  selectors
    mapM (build dosomething) es
    return e
    where
    queryAll = liftIO . ((ffi "(function(sel){return document.querySelectorAll(sel);})") :: String -> IO [Elem])

-- | a more declarative synmonym of `forElems`
withElems :: MonadIO m => String -> Perch m -> Perch m
withElems= forElems
