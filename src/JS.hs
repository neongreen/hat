{-# LANGUAGE
FlexibleInstances,
GeneralizedNewtypeDeriving,
OverloadedStrings,
QuasiQuotes,
BangPatterns,
NoImplicitPrelude
  #-}


module JS where


-- General
import BasePrelude
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
-- Formatting and interpolation
import qualified Data.Text.Buildable as Format
import NeatInterpolation

-- Local
import Utils


-- | Javascript code.
newtype JS = JS {fromJS :: Text}
  deriving (Show, Format.Buildable, Monoid)

-- | A concatenation of all Javascript functions defined in this module.
allJSFunctions :: JS
allJSFunctions = JS . T.unlines . map fromJS $ [
  -- Utilities
  replaceWithData, prependData, appendData,
  moveNodeUp, moveNodeDown,
  switchSection, switchSectionsEverywhere,
  fadeIn, fadeOutAndRemove,
  -- Misc
  createAjaxIndicator,
  autosizeTextarea,
  -- Login
  tryLogin,
  logout ]

-- | A class for things that can be converted to Javascript syntax.
class ToJS a where toJS :: a -> JS

instance ToJS Bool where
  toJS True  = JS "true"
  toJS False = JS "false"
instance ToJS JS where
  toJS = id
instance ToJS Text where
  toJS = JS . escapeJSString
instance ToJS Integer where
  toJS = JS . tshow
instance ToJS Int where
  toJS = JS . tshow
instance ToJS (Uid a) where
  toJS = toJS . uidToText

-- | A helper class for calling Javascript functions.
class JSParams a where
  jsParams :: a -> [JS]

instance JSParams () where
  jsParams () = []
instance ToJS a => JSParams [a] where
  jsParams = map toJS
instance (ToJS a,ToJS b) => JSParams (a,b) where
  jsParams (a,b) = [toJS a, toJS b]
instance (ToJS a,ToJS b,ToJS c) => JSParams (a,b,c) where
  jsParams (a,b,c) = [toJS a, toJS b, toJS c]
instance (ToJS a,ToJS b,ToJS c,ToJS d) => JSParams (a,b,c,d) where
  jsParams (a,b,c,d) = [toJS a, toJS b, toJS c, toJS d]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e) => JSParams (a,b,c,d,e) where
  jsParams (a,b,c,d,e) = [toJS a, toJS b, toJS c, toJS d, toJS e]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e,ToJS f) => JSParams (a,b,c,d,e,f) where
  jsParams (a,b,c,d,e,f) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f]

{- | This hacky class lets you construct and use Javascript functions; you give 'makeJSFunction' function name, function parameters, and function body, and you get a polymorphic value of type @JSFunction a => a@, which you can use either as a complete function definition (if you set @a@ to be @JS@), or as a function that you can give some parameters and it would return a Javascript call:

> plus = makeJSFunction "plus" ["a", "b"] "return a+b;"

>>> plus :: JS
JS "function plus(a,b) {\nreturn a+b;}\n"
>>> plus (3, 5) :: JS
JS "plus(3,5);"
-}
class JSFunction a where
  makeJSFunction
    :: Text          -- ^ Name
    -> [Text]        -- ^ Parameter names
    -> Text          -- ^ Definition
    -> a

-- This generates function definition
instance JSFunction JS where
  makeJSFunction fName fParams fDef =
    JS $ format "function {}({}) {\n{}}\n"
                (fName, T.intercalate "," fParams, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance JSParams a => JSFunction (a -> JS) where
  makeJSFunction fName _fParams _fDef = \args ->
    JS $ format "{}({});"
                (fName, T.intercalate "," (map fromJS (jsParams args)))

-- This isn't a standalone function and so it doesn't have to be listed in
-- 'allJSFunctions'.
assign :: ToJS x => JS -> x -> JS
assign v x = JS $ format "{} = {};" (v, toJS x)

-- TODO: all links here shouldn't be absolute [absolute-links]

replaceWithData :: JSFunction a => a
replaceWithData =
  makeJSFunction "replaceWithData" ["node"]
  [text|
    return function(data) {$(node).replaceWith(data);};
  |]

prependData :: JSFunction a => a
prependData =
  makeJSFunction "prependData" ["node"]
  [text|
    return function(data) {$(node).prepend(data);};
  |]

appendData :: JSFunction a => a
appendData =
  makeJSFunction "appendData" ["node"]
  [text|
    return function(data) {$(node).append(data);};
  |]

-- | Move node up (in a list of sibling nodes), ignoring anchor elements
-- inserted by 'thisNode'.
moveNodeUp :: JSFunction a => a
moveNodeUp =
  makeJSFunction "moveNodeUp" ["node"]
  [text|
    var el = $(node);
    while (el.prev().is(".dummy"))
      el.prev().before(el);
    if (el.not(':first-child'))
      el.prev().before(el);
  |]

-- | Move node down (in a list of sibling nodes), ignoring anchor elements
-- inserted by 'thisNode'.
moveNodeDown :: JSFunction a => a
moveNodeDown =
  makeJSFunction "moveNodeDown" ["node"]
  [text|
    var el = $(node);
    while (el.next().is(".dummy"))
      el.next().after(el);
    if (el.not(':last-child'))
      el.next().after(el);
  |]

-- | Given something that contains section divs (or spans), show one and
-- hide the rest. The div/span with the given @class@ will be chosen.
--
-- See Note [show-hide]
switchSection :: JSFunction a => a
switchSection =
  makeJSFunction "switchSection" ["node", "section"]
  [text|
    $(node).children(".section").removeClass("shown");
    $(node).children(".section."+section).addClass("shown");
    // See Note [autosize]
    autosize($('textarea'));
    autosize.update($('textarea'));
  |]

-- | Switch sections /everywhere/ inside the container.
--
-- See Note [show-hide]
switchSectionsEverywhere :: JSFunction a => a
switchSectionsEverywhere =
  makeJSFunction "switchSectionsEverywhere" ["node", "section"]
  [text|
    $(node).find(".section").removeClass("shown");
    $(node).find(".section."+section).addClass("shown");
    // See Note [autosize]
    autosize($('textarea'));
    autosize.update($('textarea'));
  |]

-- | This function makes the node half-transparent and then animates it to
-- full opaqueness. It's useful when e.g. something has been moved and you
-- want to “flash” the item to draw user's attention to it.
fadeIn :: JSFunction a => a
fadeIn =
  makeJSFunction "fadeIn" ["node"]
  [text|
    $(node).fadeTo(0,0.2).fadeTo(600,1);
  |]

-- | This function animates the node to half-transparency and then removes it
-- completely. It's useful when you're removing something and you want to
-- draw user's attention to the fact that it's being removed.
--
-- The reason there isn't a simple @fadeOut@ utility function here is that
-- removal has to be done by passing a callback to @fadeTo@. In jQuery you
-- can't simply wait until the animation has stopped.
fadeOutAndRemove :: JSFunction a => a
fadeOutAndRemove =
  makeJSFunction "fadeOutAndRemove" ["node"]
  [text|
     $(node).fadeTo(400,0.2,function(){$(node).remove()});
  |]

createAjaxIndicator :: JSFunction a => a
createAjaxIndicator =
  makeJSFunction "createAjaxIndicator" []
  [text|
    $("body").prepend('<div id="ajax-indicator"></div>');
    $(document).ajaxStart(function() {
      $("#ajax-indicator").show();
    });
    $(document).ajaxStop(function() {
      $("#ajax-indicator").hide();
    });
    $("#ajax-indicator").hide();
  |]

autosizeTextarea :: JSFunction a => a
autosizeTextarea =
  makeJSFunction "autosizeTextarea" ["textareaNode"]
  [text|
    autosize(textareaNode);
    autosize.update(textareaNode);
  |]

tryLogin :: JSFunction a => a
tryLogin =
  makeJSFunction "tryLogin" ["errorNode", "form"]
  [text|
    $.post("/login", $(form).serialize())
     .done(function (data) {
        if (data[0]) {
          $(errorNode).hide();
          window.location.href = "/"; }
        else {
          $(errorNode).text(data[1]);
          $(errorNode).show(); }
     });
  |]

logout :: JSFunction a => a
logout =
  makeJSFunction "logout" []
  [text|
    $.post("/logout")
     .done(function () {
        location.reload();
     });
  |]

-- When adding a function, don't forget to add it to 'allJSFunctions'!

escapeJSString :: Text -> Text
escapeJSString s =
    TL.toStrict . B.toLazyText $
    B.singleton '"' <> quote s <> B.singleton '"'
  where
    quote q = case T.uncons t of
      Nothing       -> B.fromText h
      Just (!c, t') -> B.fromText h <> escape c <> quote t'
      where
        (h, t) = T.break isEscape q
    -- 'isEscape' doesn't mention \n, \r and \t because they are handled by
    -- the “< '\x20'” case; yes, later 'escape' escapes them differently,
    -- but it's irrelevant
    isEscape c = c == '\"' || c == '\\' ||
                 c == '\x2028' || c == '\x2029' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | c < '\x20' || c == '\x2028' || c == '\x2029' =
          B.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
      | otherwise =
          B.singleton c
      where
        h = showHex (fromEnum c) ""

newtype JQuerySelector = JQuerySelector Text
  deriving (ToJS, Format.Buildable)

selectId :: Text -> JQuerySelector
selectId x = JQuerySelector $ format "#{}" [x]

selectUid :: Uid Node -> JQuerySelector
selectUid x = JQuerySelector $ format "#{}" [x]

selectClass :: Text -> JQuerySelector
selectClass x = JQuerySelector $ format ".{}" [x]

selectParent :: JQuerySelector -> JQuerySelector
selectParent x = JQuerySelector $ format ":has(> {})" [x]

selectChildren :: JQuerySelector -> JQuerySelector -> JQuerySelector
selectChildren a b = JQuerySelector $ format "{} > {}" (a, b)