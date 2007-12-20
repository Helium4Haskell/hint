-- | This is a component that represents a console: it allows the
--   user to input a line of text, a command, and executes this
--   command. The output of this process is displayed as lines
--   of text.
module Console ( Console
               , addData
               , clear
               , setFocus
               , userInputHandler
               , rememberFutureInput
               , fontsSize
               , prompt
               , displayPrompt
               , getLayout
               , linkTable
               , DataStyle (InputStyle, NormalStyle, SpecialStyle, ErrorStyle, LinkStyle)
               , Link (Link)
               )
where



import Data.Maybe
import Graphics.UI.WX



-- | Determines the graphical representation of the text
--   on the console.
data DataStyle
  = InputStyle
  | NormalStyle
  | SpecialStyle
  | ErrorStyle
  | LinkStyle


-- | A link on the console. It contains the location of the
--   console and the action that is performed when the link
--   is clicked.
data Link
  = Link { beginPosition :: Int
         , endPosition   :: Int
         , action        :: IO ()
         }

instance Show Link where
  show (Link l r _)
    = "Link (" ++ show l ++ ", " ++ show r ++ ")"


-- | The console.
class Console c where
  -- | Adds data to the console window, using the given
  --   style (graphical representation), maybe an
  --   action that is performed if you click on the data and
  --   the data itself.
  addData :: c -> DataStyle -> Maybe (IO a) -> String -> IO ()
  
  -- | Clears the console. All links and output is removed.
  --   Prompt and user-input are preserved. After clearing,
  --   the prompt will be visible.
  clear :: c -> IO ()

  -- | The input handler is called when the user submits his
  --   command. It is passed to the input handler as a
  --   string.
  userInputHandler :: Attr c (c -> Maybe String -> IO ())

  -- | The console will memorise submitted commands while
  --   this attribute is set to True.
  --
  --   For example, set this attribute to True when the user
  --   is going to input a command, and to False when it is
  --   interacting with the execution of the command. Previous
  --   commands can be looked up by using the arrow keys
  --   (depending on a concrete implementation).
  rememberFutureInput :: Attr c Bool
  
  -- | Move the focus to the console.
  setFocus :: c -> IO ()

  -- | Layout the console.
  getLayout :: c -> Layout

  -- | Sets the font size of the console (in points).
  fontsSize :: WriteAttr c Int

  -- | Sets the command prompt of the console (a string).
  prompt :: Attr c String

  -- | The console displays the prompt as long as this
  --   attribute is set to True.
  displayPrompt :: Attr c Bool

  -- | The table of links added to the console.
  linkTable :: ReadAttr c [Link]

