-- | This is a specific implementation of the Console class,
--   by using a rich textcontrol, both for input and output.
--
--   Input is defined as follows: the user-typed command that
--   has not been submitted yet.
--   Output: the other text. This is mainly tekst programmatically
--   added to control, but also previously submitted input.

module TextCtrlConsole ( module Console
                       , TextCtrlConsole (control)
                       , textCtrlConsole
                       )
where



import Console
import Control.Monad
import Data.Maybe
import Graphics.UI.WX
import Graphics.UI.WXCore
import Graphics.UI.WXCore.WxcTypes



-- | The datatype contains the control and
--   some variables (such as attributes).
data TextCtrlConsole
  = TCC { control                :: TextCtrl ()
        , beginUserInputRef      :: Var Int
        , linkTableRef           :: Var [Link]
        , inputHandlerRef        :: Var (TextCtrlConsole -> String -> IO ())
        , prevInputTableRef      :: Var [String]
        , prevInputTableIndexRef :: Var Int
        , rememberInputRef       :: Var Bool
        , promptRef              :: Var String
        , displayPromptRef       :: Var Bool
        , inputStyleRef          :: Var (TextAttr ())
        , normalStyleRef         :: Var (TextAttr ())
        , specialStyleRef        :: Var (TextAttr ())
        , errorStyleRef          :: Var (TextAttr ())
        , linkStyleRef           :: Var (TextAttr ())
        }



instance Console TextCtrlConsole where

  -- | Adds the data to the console. If a link is supplied, it is
  --   added to the link-table together with its position on the
  --   console.
  addData console style mLink text
    = do (l, r) <- appendData console style text Nothing
         ifcond_ (isJust mLink)
           $ varUpdate (linkTableRef console)
                       (Link l r (do (fromJust mLink); return ()) :)


  -- | Creates the input handler attribute.
  userInputHandler
    = newAttr "ConsoleInputHandler"
              (varGet . inputHandlerRef)
              (varSet . inputHandlerRef)


  -- | Clears the console. All output is removed, but prompt and
  --   current user-input is preserved. After clearing, the prompt
  --   will be visible.
  clear console
    = do let textctrl = control $ console
         command <- getCurrentUserInput console

         -- remove all the output (and preserve prompt and user-input)
         set console [ displayPrompt := False ]
         textCtrlClear textctrl
         varSet (beginUserInputRef console) 0
         set console [ displayPrompt := True ]
         textCtrlSetInsertionPointEnd textctrl
         ensureInputStyleIsSet console
         textCtrlAppendText textctrl command

         -- reset internal variables
         set console [ rememberFutureInput := True ]
         varSet (linkTableRef console) []
         varSet (prevInputTableRef console) []
         varSet (prevInputTableIndexRef console) (-1)

         return ()


  -- | Transfers the focus to the text control.
  setFocus
    = windowSetFocus . control


  -- | Creates the remember future input attribute.
  rememberFutureInput
    = newAttr "ConsoleRememberFutureInput"
              (varGet . rememberInputRef)
              (varSet . rememberInputRef)


  -- | Sets the fontsize and updates the attributes.
  fontsSize
    = writeAttr "ConsoleFontSize"
                (\console size ->
                    mapM_ ( \ref ->
                              do attr <- varGet $ ref console
                                 font <- textAttrGetFont attr
                                 fontSetPointSize font size
                                 textAttrSetFont attr font
                          )
                          [ inputStyleRef
                          , normalStyleRef
                          , specialStyleRef
                          , errorStyleRef
                          , linkStyleRef
                          ]
                )


  -- | Sets the prompt.
  --   The prompt is first removed, then changed and if it was
  --   visible, added again.

  prompt
    = newAttr "ConsolePrompt"
              (varGet . promptRef)
              ( \console prompt ->
                  do isDisplaying <- get console displayPrompt
                     set console [ displayPrompt := False ]
                     varSet (promptRef console) prompt
                     set console [ displayPrompt := isDisplaying ]
              )


  -- | Sets the visibility of the prompt.
  --   If True and the prompt was not visible, it is added.
  --   If False and the prompt was visible, it is removed.
  displayPrompt
    = newAttr "ConsoleDisplayPrompt"
        (varGet . displayPromptRef)
        (\console bDisplay -> do isDisplaying <- get console displayPrompt
                                 varSet (displayPromptRef console) bDisplay
                                 ifcond_ (bDisplay /= isDisplaying)
                                   $ appendData console InputStyle "" $ Just isDisplaying
        )


  -- | Get the layout of the control. Use this to layout the console according to
  --   Graphics.UI.WXCore.Layout.
  getLayout
    = widget . control


  -- | Get the links inside this component.
  linkTable
    = readAttr "ConsoleLinks"
               (varGet . linkTableRef)



-- | Appends text in the given style to the console. The text may be empty.
--   A boolean flag may be specified to override the check to remove the
--   prompt. With empty input and this check overriden, you can use this
--   function to remove only the prompt.
--
--   The text is appended after the existing output on the console and
--   before a possible prompt and user-input. This function returns the
--   area occupied by the appended text on the console. That is: the
--   position that contains the first character and the position next to
--   the last character.

appendData :: TextCtrlConsole -> DataStyle -> String -> Maybe Bool -> IO (Int, Int)
appendData console style text maybeOverrideRemovePrompt
  = do let textctrl = control console

       initCarretPos <- textCtrlGetInsertionPoint textctrl
       lastInputPos  <- textCtrlGetLastPosition textctrl
       beginInputPos <- varGet $ beginUserInputRef console
       command       <- getCurrentUserInput console
       promptStr     <- get console prompt
       displayPrompt <- get console displayPrompt
       selection     <- getSelection textctrl

       -- Remove the prompt and user-input. Only the ouput on the console remains.
       let removePrompt = maybe displayPrompt id maybeOverrideRemovePrompt
       let promptSize   = if removePrompt then length promptStr else 0
       textCtrlRemove textctrl (beginInputPos - promptSize) lastInputPos

       -- Add the text to be appended in the right style.
       attr <- styleToAttr console style
       textCtrlSetDefaultStyle textctrl attr
       beginAppendPos <- textCtrlGetLastPosition textctrl
       textCtrlAppendText textctrl text
       lastInputPos' <- textCtrlGetLastPosition textctrl

       -- Add the prompt (if visible) and user input.
       inputAttr <- styleToAttr console InputStyle
       textCtrlSetDefaultStyle textctrl inputAttr
       ifcond_ displayPrompt
         $ textCtrlAppendText textctrl promptStr
       lastInputPos'' <- textCtrlGetLastPosition textctrl
       textCtrlAppendText textctrl command

       -- Restore the carret to the right relative position.
       let translation = lastInputPos'' - beginInputPos
       ifcond_(initCarretPos >= beginInputPos)
         $ textCtrlSetInsertionPoint textctrl (initCarretPos + translation)
       varSet (beginUserInputRef console) lastInputPos''

       -- Restore the selection (if any)
       -- Restriction: a selection will not be restored if it covered the prompt.
       let selL = min (pointX selection) (pointY selection)
       let selR = max (pointX selection) (pointY selection)
       ifcond_ (selL /= selR)
         $ do -- selection in output area (-> keep)
              ifcond_ (selR <= (beginInputPos - promptSize))
                $ textCtrlSetSelection textctrl selL selR

              -- selection in input area (-> move)
              ifcond_ (selL >= beginInputPos)
                $ textCtrlSetSelection textctrl (selL+translation) (selR+translation)

       -- Return the two postions set before and after appending the text.
       return (beginAppendPos, lastInputPos')


-- | Lookups a style into an attribute.
styleToAttr :: TextCtrlConsole -> DataStyle -> IO (TextAttr ())
styleToAttr console style
  = varGet $ ( case style of
                 InputStyle   -> inputStyleRef
                 NormalStyle  -> normalStyleRef
                 SpecialStyle -> specialStyleRef
                 ErrorStyle   -> errorStyleRef
                 LinkStyle    -> linkStyleRef
             ) console


-- | Creates the console. Supply it's parent window.
--   The console is created with a visible, standard prompt.
textCtrlConsole :: Window w -> IO TextCtrlConsole
textCtrlConsole parentWnd
  = do let defaultPrompt = "# "

       -- Initialize the variables.
       beginUserInputRef      <- varCreate (-1)
       linkTableRef           <- varCreate []
       inputHandlerRef        <- varCreate $ const $ const $ return ()
       prevInputTableRef      <- varCreate []
       prevInputTableIndexRef <- varCreate (-1)
       rememberInputRef       <- varCreate True
       promptRef              <- varCreate defaultPrompt
       displayPromptRef       <- varCreate True

       -- The text styles.
       inputStyle      <- createColoredTextAttr $ rgb   0  64 128 -- blue
       normalStyle     <- createColoredTextAttr $ rgb   0   0   0 -- black
       specialStyle    <- createColoredTextAttr $ rgb   0 128 128 -- green
       errorStyle      <- createColoredTextAttr $ rgb 170   0  80 -- red
       linkStyle       <- createColoredTextAttr $ rgb  70  70  70 -- dark gray
       inputStyleRef   <- varCreate inputStyle
       normalStyleRef  <- varCreate normalStyle
       specialStyleRef <- varCreate specialStyle
       errorStyleRef   <- varCreate errorStyle
       linkStyleRef    <- varCreate linkStyle

       -- Create the text control.
       textctrl <- textCtrlRich parentWnd WrapLine []
       let console = TCC textctrl beginUserInputRef linkTableRef inputHandlerRef
                         prevInputTableRef prevInputTableIndexRef
                         rememberInputRef promptRef displayPromptRef
                         inputStyleRef normalStyleRef specialStyleRef errorStyleRef linkStyleRef

       -- Decorate it.
       textCtrlSetDefaultStyle textctrl inputStyle
       set textctrl [ text := defaultPrompt, on mouse := mouseHandler console, processEnter := True ]
       windowOnKeyDown textctrl $ keyboardHandler console
       windowOnKeyUp textctrl $ const $ return ()
       textCtrlSetInsertionPointEnd textctrl
       lastPos <- textCtrlGetLastPosition textctrl
       varSet beginUserInputRef lastPos

       -- Use the arrow cursor for this component instead of the text cursor.
       cursor <- cursorCreateFromStock wxCURSOR_ARROW
       windowSetCursor textctrl cursor

       return console;
  where
    -- | Creates a text attribute with the given color, using
    --   a font size of 10 and a sans-serif font.
    createColoredTextAttr :: Color -> IO (TextAttr ())
    createColoredTextAttr color
      = do -- Create the font.
           font <- fontCreateDefault
           fontSetFamily font wxMODERN
           fontSetPointSize font 10

           -- Create the attribute.
           attr <- textAttrCreateDefault
           textAttrSetFont attr font
           textAttrSetTextColour attr color

           return attr


-- | This monster function processes all the keyboard events.
--   It specifies what to do if a key is pressed in a certain
--   context.
keyboardHandler :: TextCtrlConsole -> EventKey -> IO ()
keyboardHandler console (EventKey key modifiers _)
  = do let textctrl = control console

       -- Page Up, Page Down
       -- Let the control handle page up and page down
       ifcond_ (isDown [KeyPageUp, KeyPageDown] ["PgUp", "PgDn"])
         propagateEvent

       -- Arrow Left, Arrow Right
       -- Let the control handle left and right
       ifcond_ (isDown [KeyLeft, KeyRight] ["Left", "Right"])
         propagateEvent

       -- Arrow Up, Arrow Down
       -- If arrow down or arrow up is pressed, replace current input with
       -- input from the previous input table (depending on wich arrow is pressed).
       ifcond_ (isDown [KeyDown, KeyUp] ["Down", "Up"])
         $ do ifcond isInsideEditableArea
                $ do textCtrlSetInsertionPointEnd textctrl

                     -- Get the previous command table and update our index.
                     table <- varGet $ prevInputTableRef console
                     index <- varGet $ prevInputTableIndexRef console
                     let index'  = if isDown [KeyDown] ["Down"] then index - 1 else index + 1
                     let index'' = max 0 $ min index' $ length table - 1

                     varSet (prevInputTableIndexRef console) index''

                     -- Replace the current command with the one pointed to by the index (if it exists)
                     ifcond_ (index' >= 0 && index' < length table)
                       $ do -- Replace the current command.
                            userInputPos <- varGet $ beginUserInputRef console
                            lastInputPos <- textCtrlGetLastPosition textctrl
                            textCtrlReplace textctrl userInputPos lastInputPos $ table !! index'

                            -- Move carret to the end of the console and select the command.
                            textCtrlSetInsertionPointEnd textctrl
                            lastInputPos' <- textCtrlGetLastPosition textctrl
                            textCtrlSetSelection textctrl userInputPos lastInputPos'

              -- If the carret is not in the editable area, let the up/down arrow behave
              -- normally.
              ifncond isInsideEditableArea
                propagateEvent



      -- Backspace
       -- If backspace is pressed and backspace would replace text in the editable
       -- area, then the event is handled by the control.
       ifcond_ (isDown [KeyBack] ["Backspace"])
         $ ifcond isInsideEditableArea
             $ ifncond isAtBeginOfEditableArea
                 propagateEvent

       -- Delete
       -- If delete is pressed and backspace would replace text in the editable
       -- area, then the event is handled by the control.
       ifcond_ (isDown [KeyDelete] ["Delete"])
         $ ifcond isInsideEditableArea
             $ ifncond isAtEndOfEditableArea
                propagateEvent

       -- Home
       -- If home key is pressed, jump to the begin of the editable area.
       ifcond_ (isDown [KeyHome] ["Home"])
         $ do ifcond isInsideEditableArea
                $ do beginUserInputPos <- varGet $ beginUserInputRef console
                     currentCarretPos  <- textCtrlGetInsertionPoint textctrl
                     sel               <- getSelection textctrl

                     -- Jump to the beginning of the editable area.
                     textCtrlSetInsertionPoint textctrl beginUserInputPos

                     -- Update a possible selection.
                     let hasSelection = pointX sel /= pointY sel
                     ifcond_ (hasSelection && modifiers == justShift)
                      $ textCtrlSetSelection textctrl (minimum [beginUserInputPos, pointX sel, pointY sel])
                                                       (maximum [pointX sel, pointY sel])
                     ifcond_ (hasSelection && modifiers /= justShift)
                       $ textCtrlSetSelection textctrl beginUserInputPos beginUserInputPos
                     ifcond_ (modifiers == justShift)
                       $ textCtrlSetSelection textctrl beginUserInputPos currentCarretPos

              -- Not inside editable area: let home behave normally.
              ifncond isInsideEditableArea
                propagateEvent

       -- End
       -- If end key is pressed, jump to the end of the editable area.
       ifcond_ (isDown [KeyEnd] ["End"])
         $ do ifcond isInsideEditableArea
                $ do lastPos          <- textCtrlGetLastPosition textctrl
                     currentCarretPos <- textCtrlGetInsertionPoint textctrl
                     sel              <- getSelection textctrl

                     -- Jump to the end of the editable area.
                     textCtrlSetInsertionPointEnd textctrl

                     -- Update a possible selection.
                     let hasSelection = pointX sel /= pointY sel
                     ifcond_ (hasSelection)
                       $ textCtrlSetSelection textctrl (minimum [pointX sel, pointY sel])
                                                       (maximum [lastPos, pointX sel, pointY sel])
                     ifcond_ (hasSelection && modifiers /= justShift)
                       $ textCtrlSetSelection textctrl lastPos lastPos
                     ifcond_ (modifiers == justShift)
                       $ textCtrlSetSelection textctrl currentCarretPos lastPos

             -- Not inside editable area: let end behave normally.
              ifncond isInsideEditableArea
                propagateEvent

       -- Enter/Return
       -- Submit input.
       ifcond_ (isDown [KeyReturn] ["Enter"])
         $ do textCtrlSetInsertionPointEnd textctrl
              command <- getCurrentUserInput console
              ifcond_ (not $ null command)
                $ do -- Since we handle the newline ourself, we have to
                     -- add the newline. And also add another prompt.
                     textCtrlAppendText textctrl "\n"
                     ifcond (get console displayPrompt)
                       $ do prompt <- get console prompt
                            textCtrlAppendText textctrl prompt

                     -- Update the administration of the editable area and
                     -- add the command to the previous command list.
                     lastInputPos <- textCtrlGetLastPosition textctrl
                     varSet (beginUserInputRef console) lastInputPos
                     ifcond (get console rememberFutureInput)
                       $ varUpdate (prevInputTableRef console) (command :)
                     varSet (prevInputTableIndexRef console) (-1)

                     -- Move carret to the end of the control
                     textCtrlSetInsertionPointEnd textctrl

                     -- Execute the command.
                     commandHandler <- varGet $ inputHandlerRef console
                     commandHandler console command

      -- 'Normal' keys
       -- Propagate if inside the editable area.
       -- Restriction: unless a dangerous selection is made, that is
       -- a selection outside the editable area. Processing the key
       -- would replace the selection and thus change text that
       -- should remain immutable.
       ifcond_ isNormalKey
         $ ifncond hasDangerousSelection
             $ do ifcond isInsideEditableArea
                    $ do ensureInputStyleIsSet console
                         propagateEvent
                  ifncond isInsideEditableArea
                    $ do -- Check for 'Ctrl' + 'c'.
                         let isCtrlCDown = modifiers == justControl && case key of
                                                                        KeyChar 'c' -> True
                                                                        KeyChar 'C' -> True
                                                                        _           -> False
                         -- Let text control handle 'Ctrl' + 'c'.
                         ifcond_ isCtrlCDown
                           propagateEvent

                         -- Other combinations result into moving the carret
                         -- to the end of the control.
                         unless isCtrlCDown
                           $ textCtrlSetInsertionPointEnd textctrl

       -- Other keys are not ignored.
       return ()
  where
    -- | Any key with value greater or equal to the ascii value of space
    --    is considered normal.
    isNormalKey :: Bool
    isNormalKey
      = case key of
          KeyChar c -> c >= ' '
          KeyTab    -> True
          KeySpace  -> True
          _         -> False

    -- | Checks if the carret is at the begin of the editable area.
    isAtBeginOfEditableArea :: IO Bool
    isAtBeginOfEditableArea
      = do beginUserInputPos <- varGet $ beginUserInputRef console
           currentInputPos   <- textCtrlGetInsertionPoint $ control console
           return $ currentInputPos == beginUserInputPos

    -- | Checks if the carret is at the end of the editable area.
    isAtEndOfEditableArea :: IO Bool
    isAtEndOfEditableArea
      = do lastInputPos    <- textCtrlGetLastPosition $ control console
           currentInputPos <- textCtrlGetInsertionPoint $ control console
           return $ currentInputPos == lastInputPos

    -- | Checks if the carret is inside the editable area.
    isInsideEditableArea :: IO Bool
    isInsideEditableArea
      = do beginUserInputPos <- varGet $ beginUserInputRef console
           currentInputPos   <- textCtrlGetInsertionPoint $ control console
           return $ currentInputPos >= beginUserInputPos

    -- | Checks if a certain key is pressed with no special modifiers
    --   except a shift. This function returns true if the (on an upper level
    --   defined) key equals one of the keys in the list, or if it's
    --   textual representation equals once of the strings in the list.
    isDown :: [Key] -> [String] -> Bool
    isDown keys txtkeys
      =  (isNoneDown modifiers || modifiers == justShift)
      && (  any (== key) keys
         || any (== show key) txtkeys
         || any (\k -> show k == show key) txtkeys
         )

    -- | Checks if a dangerous selection is made on the console.
    --   A selection is considered dangerous if it covers both
    --   the editable area and the output area.
    hasDangerousSelection :: IO Bool
    hasDangerousSelection
      = do beginUserInputPos <- varGet $ beginUserInputRef console
           selection         <- getSelection $ control console

           return $  min (pointX selection) (pointY selection) <  beginUserInputPos
                  && max (pointX selection) (pointY selection) >= beginUserInputPos


-- | Get the user input. The user input is defined by the position
--   stored into the beginUserInputRef variable and the end of
--   the console.
getCurrentUserInput :: TextCtrlConsole -> IO String
getCurrentUserInput console
  = do lastInputPos      <- textCtrlGetLastPosition $ control console
       beginUserInputPos <- varGet $ beginUserInputRef console
       textCtrlGetRange (control console) beginUserInputPos lastInputPos


-- | Ensures that the input style is set at the last position
--   of the console. Subsequent user input is appended in this
--   style.
ensureInputStyleIsSet :: TextCtrlConsole -> IO ()
ensureInputStyleIsSet console
  = do let textctrl = control console
       lastPos <- textCtrlGetLastPosition textctrl
       attr    <- varGet $ inputStyleRef console
       textCtrlSetStyle textctrl lastPos lastPos attr
       return ()


-- | Handles the mouse clicks of the console. If a
--   mouse click is made on a link, the link is
--   executed if the mouse was not dragged.
mouseHandler :: TextCtrlConsole -> EventMouse -> IO ()
mouseHandler console (MouseLeftUp _ _)
  = do let textctrl = control console

      -- Position the carret to the possible link.
       propagateEvent

       selection <- getSelection textctrl
       unless (pointX selection /= pointY selection)
         $ do currentInputPos <- textCtrlGetInsertionPoint $ textctrl
              linkTable       <- get console linkTable

              -- Execute the actions of possibly clicked links.
              let actions = [a | (Link l r a) <- linkTable, l <= currentInputPos, currentInputPos < r]
              sequence_ actions

              -- Set the carret to the end of the console after a successful click.
              unless (null actions)
                $ textCtrlSetInsertionPointEnd textctrl
mouseHandler _ _
  = -- Let the control handle other mouse clicks.
    propagateEvent


-- | Get the currently selected text on the text control.
getSelection :: TextCtrl a -> IO Point
getSelection textCtrl
  = withPointResult $ textCtrlGetSelection textCtrl



--
-- Some support functions.
--

-- | Execute the action if the guard evaluates to True.
ifcond :: IO Bool -> IO a -> IO ()
ifcond ioBool ioAction
  = do c <- ioBool
       Control.Monad.when c ( do ioAction; return ())

-- | Execute the action if the guard evaluates to False.
ifncond :: IO Bool -> IO a -> IO ()
ifncond ioBool ioAction
  = do c <- ioBool
       Control.Monad.unless c ( do ioAction; return ())

-- | Execute the action if the guard is True.
ifcond_ :: Bool -> IO a -> IO ()
ifcond_ c a = Control.Monad.when c (do a; return ())

