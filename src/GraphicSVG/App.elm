module GraphicSVG.App 
    exposing
        ( graphicsApp
        , GraphicsApp
        , notificationsApp
        , NotificationsApp
        , gameApp
        , GameApp
        , GetKeyState
        )


import GraphicSVG exposing(..)
import Dict
import Browser.Navigation as Nav
import Browser exposing (UrlRequest(..))
import Browser.Events exposing (onKeyUp,onKeyDown)
import Time exposing (posixToMillis,millisToPosix)
import Url
import Json.Decode as D
import Task

type alias InputHandler userMsg =   
    Float   
    -> GetKeyState  
    -> userMsg

{-| `GetKeyState` returns a triple where the first argument is of type `Keys -> KeyState`   
so you can ask if a certain key is pressed. The other two are tuples of arrow keys and  
WASD keys, respectively. They're in the form (x,y) which represents the key presses 
of each player. For example, (0,-1) represents the left arrow (or "A") key, and (1,1)   
would mean the up (or "W") and right (or "D") key are being pressed at the same time.   
-}  
type alias GetKeyState =    
    ( Keys -> KeyState, ( Float, Float ), ( Float, Float ) )

type alias KeyCode =    
    Int

type alias KeyDict =   
    Dict.Dict KeyCode ( KeyState, Bool )

type KeyState   
    = JustDown  
    | Down  
    | JustUp    
    | Up  

type KeyAction 
    = WentUp    
    | WentDown

subs : List (Sub (HiddenMsg userMsg))
subs =   
    [ onKeyUp (D.map KeyUp (D.field "keyCode" D.int))   
    , onKeyDown (D.map KeyDown (D.field "keyCode" D.int))  
    , Time.every (1000 / 30) TickTime 
    ]

insertKeyDict : KeyDict -> KeyCode -> KeyAction -> KeyDict  
insertKeyDict dict key action = 
    let 
        currState = 
            Dict.get key dict   
    in  
        case currState of   
            Just ( JustDown, False ) -> 
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( JustDown, False ) 
                        WentUp ->  
                            ( JustDown, True )  
                    )   
                    dict    
            Just ( Down, False ) ->    
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( Down, False ) 
                        WentUp ->  
                            ( JustUp, False )   
                    )   
                    dict    
            Just ( Up, False ) ->  
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( JustDown, False ) 
                        WentUp ->  
                            ( JustUp, False )   
                    )   
                    dict    
            Just ( JustUp, False ) ->  
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( JustUp, True )    
                        WentUp ->  
                            ( JustUp, False )   
                    )   
                    dict    
            Just ( state, True ) ->    
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( state, True ) 
                        WentUp ->  
                            ( state, True ) 
                    )   
                    dict    
            Nothing -> 
                Dict.insert key 
                    (case action of 
                        WentDown -> 
                            ( JustDown, False ) 
                        WentUp ->  
                            ( JustUp, False )   
                    )   
                    dict

maintainKeyDict : KeyDict -> KeyDict   
maintainKeyDict dict =  
    Dict.filter filterHelper (Dict.map maintainHelper dict) 

filterHelper : a -> ( KeyState, b ) -> Bool    
filterHelper key action =   
    case action of  
        ( Up, _ ) ->    
            False   
        _ ->   
            True  

maintainHelper : a -> ( KeyState, Bool ) -> ( KeyState, Bool ) 
maintainHelper key action = 
    case action of  
        ( JustUp, False ) ->    
            ( Up, False )   
        ( JustUp, True ) ->    
            ( JustDown, False ) 
        ( Up, False ) ->   
            ( Up, False )   
        ( Up, True ) ->    
            ( Up, False )   
        ( JustDown, False ) -> 
            ( Down, False ) 
        ( JustDown, True ) ->  
            ( JustUp, False )   
        ( Down, False ) -> 
            ( Down, False ) 
        ( Down, True ) ->  
            ( Down, False ) 
 
{-| Includes all the regular keys. Ask for letters and numbers using `Key String`, e.g. `Key "a"` or `Key "3"`.    
-}  
type Keys   
    = Key String    
    | Backspace 
    | Tab   
    | Enter 
    | Shift 
    | Ctrl  
    | Alt   
    | Caps  
    | LeftArrow 
    | UpArrow   
    | RightArrow    
    | DownArrow 
    | Delete    
    | Space 

keyCheckerFunction : Dict.Dict Int ( KeyState, a ) -> Keys -> KeyState 
keyCheckerFunction dict key =   
    let 
        state = 
            Dict.get kc dict    
        kc =   
            case key of 
                Key str ->  
                    Char.toCode <|
                        Char.toUpper <|
                            case (String.uncons str) of    
                                Just ( a, _ ) ->   
                                    a   
                                Nothing -> 
                                    'z' 
                Backspace ->   
                    8   
                Tab -> 
                    9   
                Enter ->   
                    13  
                Shift ->   
                    16  
                Ctrl ->    
                    17  
                Alt -> 
                    18  
                Caps ->    
                    20  
                Space ->   
                    32  
                LeftArrow ->   
                    37  
                UpArrow -> 
                    38  
                RightArrow ->  
                    39  
                DownArrow ->   
                    40  
                Delete ->  
                    46  
    in  
        case state of   
            Just ( JustDown, _ ) -> 
                JustDown    
            Just ( Down, _ ) ->    
                Down    
            Just ( JustUp, _ ) ->  
                JustUp  
            Just ( Up, _ ) ->  
                Up  
            Nothing -> 
                Up     

arrowChecker checker up down left right = 
    ( case ( (checker left), (checker right) ) of  
        ( Down, Up ) -> 
            -1  
        ( Down, JustUp ) ->    
            -1  
        ( JustDown, Up ) ->    
            -1  
        ( JustDown, JustUp ) ->    
            -1  
        ( Up, Down ) ->    
            1   
        ( JustUp, Down ) ->    
            1   
        ( Up, JustDown ) ->    
            1   
        ( JustUp, JustDown ) ->    
            1   
        _ ->   
            0   
    , case ( (checker down), (checker up) ) of  
        ( Down, Up ) -> 
            -1  
        ( Down, JustUp ) ->    
            -1  
        ( JustDown, Up ) ->    
            -1  
        ( JustDown, JustUp ) ->    
            -1  
        ( Up, Down ) ->    
            1   
        ( JustUp, Down ) ->    
            1   
        ( Up, JustDown ) ->    
            1   
        ( JustUp, JustDown ) ->    
            1   
        _ ->   
            0   
    )

subtractTimeSeconds : Time.Posix -> Time.Posix -> Float 
subtractTimeSeconds t1 t0 = 
    ((Basics.toFloat <| posixToMillis t1) - Basics.toFloat (posixToMillis t0)) / 1000

type HiddenMsg userMsg = 
      UserMsg userMsg
    | InitTime Time.Posix
    | TickTime Time.Posix
    | KeyDown Int
    | KeyUp Int
    | URLRequest Browser.UrlRequest
    | URLChanged Url.Url

type alias GraphicsApp =  
    App D.Value () (HiddenMsg ())

graphicsApp : 
    {
        view : Collage ()
    }
    -> GraphicsApp
graphicsApp userApp =
    GraphicSVG.app
        { init = \_ _ _ -> ( (), Task.perform InitTime Time.now ) 
        , update = hiddenGraphicsUpdate   
        , view = \userModel -> { title = "GraphicSVG Interactive App", body = mapCollage UserMsg userApp.view }    
        , subscriptions = \_ -> Sub.none
        , onUrlChange = URLChanged
        , onUrlRequest = URLRequest
        }

hiddenGraphicsUpdate : HiddenMsg userMsg
                -> ()
                -> ( () , Cmd (HiddenMsg userMsg) )
hiddenGraphicsUpdate msg key =    
    case msg of
        URLRequest urlreq ->
            case urlreq of 
                External url -> ( (), Nav.load url )
                _ -> ( (), Cmd.none)
        _ -> ( (), Cmd.none )


type alias NotificationsApp userModel userMsg =  
    App D.Value userModel (HiddenMsg userMsg)

notificationsApp : 
    { model : userModel
    , view  : userModel -> Collage userMsg
    , update : userMsg -> userModel -> userModel
    }
    -> NotificationsApp userModel userMsg
notificationsApp userApp =
    let 
        userInit = userApp.model
        userUpdate = userApp.update
        userView = userApp.view
    in
        GraphicSVG.app
            { init = \_ _ _ -> ( userInit, Task.perform InitTime Time.now ) 
            , update = hiddenNotifUpdate userUpdate   
            , view = \userModel -> { title = "GraphicSVG Interactive App", body = mapCollage UserMsg <| userView userModel }    
            , subscriptions = \_ -> Sub.none
            , onUrlChange = URLChanged
            , onUrlRequest = URLRequest
            }

hiddenNotifUpdate : (userMsg -> userModel -> userModel)
                -> HiddenMsg userMsg
                -> userModel
                -> ( userModel , Cmd (HiddenMsg userMsg) )
hiddenNotifUpdate userUpdate msg userModel =    
    case msg of 
        UserMsg userMsg -> 
            ( userUpdate userMsg userModel, Cmd.none )
        URLRequest urlreq ->
            case urlreq of 
                External url -> ( userModel, Nav.load url )
                _ -> ( userModel, Cmd.none)
        URLChanged url ->
            ( userModel, Cmd.none )
        _ -> ( userModel, Cmd.none )

type alias GameApp userModel userMsg =  
    App D.Value ( userModel, HiddenModel userMsg ) (HiddenMsg userMsg)

type alias HiddenModel userMsg = 
    {
        tick   : InputHandler userMsg
    ,   keys   : KeyDict
    ,   navKey : Nav.Key
    ,   initT  : Time.Posix
    }

gameApp :
    InputHandler userMsg 
    -> 
    { model : userModel
    , view : userModel -> Collage userMsg
    , update : userMsg -> userModel -> userModel 
    } 
    -> GameApp userModel userMsg
gameApp tickMsg userApp =
    let 
        userInit = userApp.model
        userUpdate = userApp.update
        userView = userApp.view
    in
        GraphicSVG.app
            { init = \_ _ navKey -> ( ( userInit, initHiddenModel tickMsg navKey ), Task.perform InitTime Time.now ) 
            , update = hiddenGameUpdate userUpdate   
            , view = \(userModel,_) -> { title = "GraphicSVG Game", body = mapCollage UserMsg <| userView userModel }    
            , subscriptions = \_ -> Sub.batch subs
            , onUrlChange = URLChanged
            , onUrlRequest = URLRequest
            }

initHiddenModel : InputHandler userMsg -> Nav.Key -> HiddenModel userMsg
initHiddenModel tick navKey =
    { tick = tick
    , keys = Dict.empty
    , navKey = navKey
    , initT = millisToPosix 0
    }

hiddenGameUpdate : (userMsg -> userModel -> userModel)
                -> HiddenMsg userMsg
                -> ( userModel , HiddenModel userMsg )
                -> ( ( userModel , HiddenModel userMsg ), Cmd (HiddenMsg userMsg) )
hiddenGameUpdate userUpdate msg ( userModel, hiddenModel ) =    
    let 
        updateTick =    
            hiddenModel.tick  
    in  
        case msg of 
            UserMsg userMsg -> 
                ( ( userUpdate userMsg userModel, hiddenModel ), Cmd.none )
            InitTime t -> 
                ( (userModel, { hiddenModel | initT = t } ), Cmd.none )
            TickTime t -> 
                let 
                    timeInSeconds = subtractTimeSeconds t hiddenModel.initT
                    keyChecker    = keyCheckerFunction hiddenModel.keys
                    arrowKeys     = arrowChecker keyChecker UpArrow DownArrow LeftArrow RightArrow
                    wasd          = arrowChecker keyChecker (Key "w") (Key "s") (Key "a") (Key "d")
                    newModel      = userUpdate (hiddenModel.tick timeInSeconds ( keyChecker, arrowKeys, wasd )) userModel   
                in  
                    ( ( newModel, { hiddenModel | keys = maintainKeyDict hiddenModel.keys } ), Cmd.none )
            KeyDown keyCode ->    
                ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentDown } ), Cmd.none )   
            KeyUp keyCode -> 
                ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentUp } ), Cmd.none )
            URLRequest urlreq ->
                case urlreq of 
                    External url -> ( ( userModel, hiddenModel ), Nav.load url )
                    _ -> ( ( userModel, hiddenModel ), Cmd.none)
            URLChanged url ->
                ( ( userModel, hiddenModel ), Cmd.none )

type alias AppWithTick flags userModel userMsg =  
    App flags ( userModel, HiddenModel userMsg ) (HiddenMsg userMsg)

appWithTick :
    InputHandler userMsg 
    -> 
    { init : flags -> Url.Url -> Nav.Key -> ( userModel, Cmd userMsg )
    , update : userMsg -> userModel -> ( userModel, Cmd userMsg )
    , view : userModel -> { title : String, body : Collage userMsg }
    , subscriptions : userModel -> Sub userMsg
    , onUrlRequest : UrlRequest -> userMsg
    , onUrlChange : Url.Url -> userMsg
    }
    -> AppWithTick flags userModel userMsg
appWithTick tickMsg userApp =
    let 
        userInit = userApp.init
        userUpdate = userApp.update
        userView = userApp.view
        userSubs = userApp.subscriptions
        userUrlReq = userApp.onUrlRequest
        userUrlChange = userApp.onUrlChange
    in
        GraphicSVG.app
            { init = \flags url navKey -> 
                let
                    userInitModel = Tuple.first <| userInit flags url navKey
                    userInitCmds = Tuple.second <| userInit flags url navKey
                in
                        ( ( userInitModel, initHiddenModel tickMsg navKey ), Cmd.batch [Task.perform InitTime Time.now, Cmd.map UserMsg userInitCmds] ) 
            , update = hiddenTickUpdate userUpdate   
            , view = \(userModel,_) -> 
                let
                    userViewE = userView userModel    
                in
                    { title = userViewE.title, body = mapCollage UserMsg userViewE.body }
            , subscriptions = \(userModel,_) -> Sub.batch <| (Sub.map UserMsg <| userSubs userModel)::subs
            , onUrlChange = UserMsg << userUrlChange
            , onUrlRequest = UserMsg << userUrlReq
            }

hiddenTickUpdate : ( userMsg -> userModel -> ( userModel, Cmd userMsg ) )
                -> HiddenMsg userMsg
                -> ( userModel , HiddenModel userMsg )
                -> ( ( userModel , HiddenModel userMsg ), Cmd (HiddenMsg userMsg) )
hiddenTickUpdate userUpdate msg ( userModel, hiddenModel ) =    
    let 
        updateTick =    
            hiddenModel.tick
    in  
        case msg of 
            UserMsg userMsg -> 
                let
                    (newUserModel, newUserCmds) =  userUpdate userMsg userModel
                in
                    ( ( newUserModel, hiddenModel ), Cmd.map UserMsg newUserCmds )
            InitTime t -> 
                ( (userModel, { hiddenModel | initT = t } ), Cmd.none )
            TickTime t -> 
                let 
                    timeInSeconds = subtractTimeSeconds t hiddenModel.initT
                    keyChecker    = keyCheckerFunction hiddenModel.keys
                    arrowKeys     = arrowChecker keyChecker UpArrow DownArrow LeftArrow RightArrow
                    wasd          = arrowChecker keyChecker (Key "w") (Key "s") (Key "a") (Key "d")
                    (newUserModel, newUserCmds) = userUpdate (hiddenModel.tick timeInSeconds ( keyChecker, arrowKeys, wasd )) userModel   
                in  
                    ( ( newUserModel, { hiddenModel | keys = maintainKeyDict hiddenModel.keys } ), Cmd.map UserMsg newUserCmds )
            KeyDown keyCode ->    
                ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentDown } ), Cmd.none )   
            KeyUp keyCode -> 
                ( ( userModel, { hiddenModel | keys = insertKeyDict hiddenModel.keys keyCode WentUp } ), Cmd.none )
            _ -> ( ( userModel, hiddenModel ), Cmd.none)