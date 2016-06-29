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
import qualified Data.Text.All as T
import Data.Text.All (Text)
-- Interpolation
import NeatInterpolation

-- Local
import Utils


-- | Javascript code.
newtype JS = JS {fromJS :: Text}
  deriving (Show, T.Buildable, Monoid)

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
  showFormError,
  -- Signup/login
  trySignup,
  tryLogin,
  logout,
  -- Other
  submitWords,
  showRoundEditPopup,
  addPlayerSelf, removePlayerSelf,
  endPreregistration,
  generateGroups,
  beginNextPhase,
  finishCurrentPhase,
  setAbsent,
  setWinner,
  startRound,
  cancelCurrentRound,
  finishCurrentRound,
  changeCurrentRound,
  recalcTime,
  keepTimer,
  pauseTimer,
  makeAdmin ]

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
  toJS = JS . T.show
instance ToJS Int where
  toJS = JS . T.show
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
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e,ToJS f,ToJS g) => JSParams (a,b,c,d,e,f,g) where
  jsParams (a,b,c,d,e,f,g) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f, toJS g]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e,ToJS f,ToJS g,ToJS h) => JSParams (a,b,c,d,e,f,g,h) where
  jsParams (a,b,c,d,e,f,g,h) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f, toJS g, toJS h]
instance (ToJS a,ToJS b,ToJS c,ToJS d,ToJS e,ToJS f,ToJS g,ToJS h,ToJS i) => JSParams (a,b,c,d,e,f,g,h,i) where
  jsParams (a,b,c,d,e,f,g,h,i) = [toJS a, toJS b, toJS c, toJS d, toJS e, toJS f, toJS g, toJS h, toJS i]

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
    JS $ T.format "function {}({}) {\n{}}\n"
                  (fName, T.intercalate "," fParams, fDef)

-- This generates a function that takes arguments and produces a Javascript
-- function call
instance JSParams a => JSFunction (a -> JS) where
  makeJSFunction fName _fParams _fDef = \args ->
    JS $ T.format "{}({});"
                  (fName, T.intercalate "," (map fromJS (jsParams args)))

-- This isn't a standalone function and so it doesn't have to be listed in
-- 'allJSFunctions'.
assign :: ToJS x => JS -> x -> JS
assign v x = JS $ T.format "{} = {};" (v, toJS x)

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

showFormError :: JSFunction a => a
showFormError =
  makeJSFunction "showFormError" ["form", "field", "err"]
  [text|
    $(form).find(".label-err").remove();
    label = $(form).find("[name="+field+"]").prev();
    errSpan = $("<span>", {
      "class" : "float-right label-err",
      "text"  : err })[0];
    label.append(errSpan);
  |]

trySignup :: JSFunction a => a
trySignup =
  makeJSFunction "trySignup" ["form"]
  [text|
    $.post("/signup", $(form).serialize())
     .done(function (data) {
        if (data[0]) {
          window.location.href = "/"; }
        else {
          showFormError(form, data[1], data[2]);
        }
     });
  |]

tryLogin :: JSFunction a => a
tryLogin =
  makeJSFunction "tryLogin" ["form"]
  [text|
    $.post("/login", $(form).serialize())
     .done(function (data) {
        if (data[0]) {
          window.location.href = "/"; }
        else {
          showFormError(form, data[1], data[2]);
        }
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

submitWords :: JSFunction a => a
submitWords =
  makeJSFunction "submitWords" ["gameId", "form"]
  [text|
    $.post("/game/" + gameId + "/words/submit", $(form).serialize())
     .done(function (data) {
        if (data[0])
          location.reload();
        else
          showFormError(form, data[1], data[2]);
     });
  |]

showRoundEditPopup :: JSFunction a => a
showRoundEditPopup =
  makeJSFunction "showRoundEditPopup"
                 ["gameId", "phaseNum", "roomNum", "namerId", "guesserId",
                  "score", "namerPenalty", "guesserPenalty", "discards"]
  [text|
    dialog = $("<div>", {
      "class" : "round-edit-popup"
    })[0];

    form = $("<form>")[0];
    $(form).submit(function(event) {
      event.preventDefault();
      $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
             "/round/" + namerId + "/" + guesserId + "/set",
             $(form).serialize())
       .done(function() {
          location.reload();
        });
    });

    labelScore = $("<label>", {
      "for"  : "score",
      "text" : "Words" })[0];
    inputScore = $("<input>", {
      "name"  : "score",
      "type"  : "number",
      "min"   : "0",
      "value" : score })[0];

    penalties = $("<div>", {
      "name" : "penalties" })[0];

    penalty1 = $("<div>")[0];
    labelDiscards = $("<label>", {
      "for"  : "discards",
      "text" : "Discards" })[0];
    inputDiscards = $("<input>", {
      "name"  : "discards",
      "type"  : "number",
      "min"   : "0",
      "value" : discards })[0];
    $(penalty1).append(labelDiscards, inputDiscards);

    penalty2 = $("<div>")[0];
    labelNamerPenalty = $("<label>", {
      "for"  : "namer-penalty",
      "text" : "Namer penalty" })[0];
    inputNamerPenalty = $("<input>", {
      "name"  : "namer-penalty",
      "type"  : "number",
      "min"   : "0",
      "value" : namerPenalty })[0];
    $(penalty2).append(labelNamerPenalty, inputNamerPenalty);

    penalty3 = $("<div>")[0];
    labelGuesserPenalty = $("<label>", {
      "for"  : "guesser-penalty",
      "text" : "Guesser penalty" })[0];
    inputGuesserPenalty = $("<input>", {
      "name"  : "guesser-penalty",
      "type"  : "number",
      "min"   : "0",
      "value" : guesserPenalty })[0];
    $(penalty3).append(labelGuesserPenalty, inputGuesserPenalty);

    $(penalties).append(penalty1, penalty2, penalty3);

    saveButton = $("<button>", {
      "type"  : "submit",
      "text"  : "Save" })[0];

    clearButton = $("<button>", {
      "type"  : "button",
      "name"  : "clear",
      "text"  : "Clear round" })[0];
    $(clearButton).click(function() {
      $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
             "/round/" + namerId + "/" + guesserId + "/clear")
       .done(function() {
          location.reload();
        });
    });

    cancelButton = $("<button>", {
      "type"  : "button",
      "name"  : "cancel",
      "class" : "button-outline",
      "text"  : "Cancel" })[0];
    $(cancelButton).click(function() {
      $.magnificPopup.close(); });

    $([inputScore, inputDiscards, inputNamerPenalty, inputGuesserPenalty])
      .focus(function() {this.select();});

    $(form).append(labelScore, inputScore,
                   penalties,
                   saveButton, clearButton, cancelButton);
    $(dialog).append(form);

    $.magnificPopup.open({
      modal: true,
      focus: "[name=score]",
      items: {
        src: dialog,
        type: 'inline' }
    });
  |]

makeAdmin :: JSFunction a => a
makeAdmin =
  makeJSFunction "makeAdmin" ["nick"]
  [text|
    $.post("/user/" + nick + "/make-admin")
     .done(function () {
        location.reload();
     });
  |]

addPlayerSelf :: JSFunction a => a
addPlayerSelf =
  makeJSFunction "addPlayerSelf" ["errorNode", "gameId"]
  [text|
    $.post("/game/" + gameId + "/players/add-self")
     .done(function (data) {
        if (data[0]) {
          $(errorNode).hide();
          location.reload(); }
        else {
          $(errorNode).text(data[1]);
          $(errorNode).show(); }
     });
  |]

removePlayerSelf :: JSFunction a => a
removePlayerSelf =
  makeJSFunction "removePlayerSelf" ["errorNode", "gameId"]
  [text|
    $.post("/game/" + gameId + "/players/remove-self")
     .done(function (data) {
        if (data[0]) {
          $(errorNode).hide();
          location.reload(); }
        else {
          $(errorNode).text(data[1]);
          $(errorNode).show(); }
     });
  |]

endPreregistration :: JSFunction a => a
endPreregistration =
  makeJSFunction "endPreregistration" ["gameId"]
  [text|
    $.post("/game/" + gameId + "/begin")
     .done(function () {
        location.reload();
     });
  |]

generateGroups :: JSFunction a => a
generateGroups =
  makeJSFunction "generateGroups" ["gameId", "numInput"]
  [text|
    num = parseInt($(numInput)[0].value, 10);
    $.post("/game/" + gameId + "/generate-groups", {num: num})
     .done(function () {
        location.reload();
     });
  |]

beginNextPhase :: JSFunction a => a
beginNextPhase =
  makeJSFunction "beginNextPhase" ["gameId", "timeInput"]
  [text|
    time = parseInt($(timeInput)[0].value, 10);
    $.post("/game/" + gameId + "/begin-next-phase", {"time-per-round": time})
     .done(function () {
        location.reload();
     });
  |]

finishCurrentPhase :: JSFunction a => a
finishCurrentPhase =
  makeJSFunction "finishCurrentPhase" ["gameId"]
  [text|
    $.post("/game/" + gameId + "/finish-current-phase")
     .done(function () {
        location.reload();
     });
  |]

setAbsent :: JSFunction a => a
setAbsent =
  makeJSFunction "setAbsent"
                 ["gameId", "phaseNum", "roomNum", "playerId", "val"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
           "/player/" + playerId + "/absent",
           {"val": val})
     .done(function () {
        location.reload();
     });
  |]

setWinner :: JSFunction a => a
setWinner =
  makeJSFunction "setWinner"
                 ["gameId", "phaseNum", "roomNum", "playerId", "val"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
           "/player/" + playerId + "/winner",
           {"val": val})
     .done(function () {
        location.reload();
     });
  |]

startRound :: JSFunction a => a
startRound =
  makeJSFunction "startRound" ["gameId", "phaseNum", "roomNum"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum + "/start-round")
     .done(function () {
        location.reload();
     });
  |]

cancelCurrentRound :: JSFunction a => a
cancelCurrentRound =
  makeJSFunction "cancelCurrentRound" ["gameId", "phaseNum", "roomNum"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
           "/cancel-current-round")
     .done(function () {
        location.reload();
     });
  |]

finishCurrentRound :: JSFunction a => a
finishCurrentRound =
  makeJSFunction "finishCurrentRound" ["gameId", "phaseNum", "roomNum"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
           "/finish-current-round")
     .done(function () {
        location.reload();
     });
  |]

changeCurrentRound :: JSFunction a => a
changeCurrentRound =
  makeJSFunction "changeCurrentRound"
                 ["gameId", "phaseNum", "roomNum",
                  "numNode", "parameter", "delta"]
  [text|
    obj = {
       "score": 0,
       "discards": 0,
       "namer-penalty": 0,
       "guesser-penalty": 0 };
    obj[parameter] = delta;
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum +
           "/change-current-round", obj)
     .done(function () {
        prevNum = parseInt($(numNode).text(), 10);
        $(numNode).text(Math.max(0, prevNum + delta));
     });
  |]

recalcTime :: JSFunction a => a
recalcTime =
  makeJSFunction "recalcTime" ["maxRounds", "roundInput", "timeSpan"]
  [text|
    $(roundInput).bind('input', function() {
       maxTime = maxRounds*this.value + (maxRounds-1)*15;
       $(timeSpan).html(Math.floor(maxTime/60));
    });
  |]

keepTimer :: JSFunction a => a
keepTimer =
  makeJSFunction "keepTimer" ["timerNode", "time"]
  [text|
    m = Math.floor(time/60);
    s = time % 60;
    $(timerNode).text(m+":"+("00"+s).slice(-2));
    setInterval(function() {
      if (s > 0 || m > 0) {
        s = s-1;
        if (s<0) {s = s+60; m = m-1;}
      }
      $(timerNode).text(m+":"+("00"+s).slice(-2));
    }, 1000);
  |]

pauseTimer :: JSFunction a => a
pauseTimer =
  makeJSFunction "pauseTimer" ["gameId", "phaseNum", "roomNum", "pause"]
  [text|
    $.post("/game/" + gameId + "/" + phaseNum + "/" + roomNum + "/pause-timer",
           {pause: pause})
     .done(function () {
        location.reload();
     });
  |]

-- When adding a function, don't forget to add it to 'allJSFunctions'!

escapeJSString :: Text -> Text
escapeJSString s =
    T.builderToStrict $
    T.bsingleton '"' <> quote s <> T.bsingleton '"'
  where
    quote q = case T.uncons t of
      Nothing       -> T.strictToBuilder h
      Just (!c, t') -> T.strictToBuilder h <> escape c <> quote t'
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
          T.strictToBuilder $ "\\u" <> T.replicate (4 - T.length h) "0" <> h
      | otherwise =
          T.bsingleton c
      where
        h = T.pack (showHex (fromEnum c) "")

newtype JQuerySelector = JQuerySelector Text
  deriving (ToJS, T.Buildable)

selectId :: Text -> JQuerySelector
selectId x = JQuerySelector $ T.format "#{}" [x]

selectUid :: Uid Node -> JQuerySelector
selectUid x = JQuerySelector $ T.format "#{}" [x]

selectClass :: Text -> JQuerySelector
selectClass x = JQuerySelector $ T.format ".{}" [x]

selectParent :: JQuerySelector -> JQuerySelector
selectParent x = JQuerySelector $ T.format ":has(> {})" [x]

selectChildren :: JQuerySelector -> JQuerySelector -> JQuerySelector
selectChildren a b = JQuerySelector $ T.format "{} > {}" (a, b)
