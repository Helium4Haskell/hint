package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;

import hint.gui.*;
import hint.gui.icon.*;
import hint.interpreter.*;
import hint.util.*;


public class InterpreterGui extends AbstractInterpreterGui
{
    protected Prompt createPrompt()
    {
        return new Prompt();
    }


    protected InterpreterTextPane createTextPane()
    {
        InterpreterTextPane  textpane = new InterpreterTextPane();
        MouseAtErrorObserver observer = new MouseAtErrorObserver();
        textpane.addMouseListener(observer);
        textpane.addMouseMotionListener(observer);
        textpane.addKeyListener(new EnterOnErrorObserver());
        return textpane;
    }


    protected List createFileActions()
    {
        InterpreterAction[] actions =
            { new LoadAction()
            , new ReloadAction()
            , new TerminateAction()
            , new QuitAction()
            };

        return Arrays.asList(actions);
    }


    protected class ReloadAction extends AbstractEvalDisabledAction
    {
        public ReloadAction()
        {
            super("Reload module", "Reload the current helium module", new HintIcon("reload.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_R));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":r");
        }
    }


    protected class LoadAction extends AbstractEvalDisabledAction
    {
        public LoadAction()
        {
            super("Open module...", "Open a helium module", new HintIcon("open.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_O));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, InputEvent.CTRL_MASK));
        }


		String moduleNameError(String fileName)
		{
			// isUpper . head
			char firstChar = fileName.charAt(0);
			if (!(firstChar >= 'A' && firstChar <= 'Z')) 
			{
				return "File name must start with upper-case letter";
		    }
		    // all isAlphaNum
			for (int i = 0; i < fileName.length(); i++) 
			{
				char c = fileName.charAt(i);
				if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '.')
					;
				else
					return "File name may contain only letters, digits and dots";
			}
			if (!fileName.endsWith(".hs"))
				return "File name must end in \".hs\"";
			return null;
		}
		
        public void actionPerformed(ActionEvent event)
        {
            FileDialog dialog = new FileDialog(InterpreterGui.this, "Open Helium module", FileDialog.LOAD);
            dialog.setFilenameFilter(new ExtentionFilenameFilter(HeliumProcess.HELIUM_FILE_EXTENTION, "Helium modules"));
            dialog.show();
            if (dialog.getFile() != null && dialog.getDirectory() != null)
            {
            	String fileName = dialog.getFile();
				String error = moduleNameError(fileName);
				if (error != null) 
				{
					getOutputPane().addText("\n" + error + "\n", InterpreterTextPane.TYPE_ERROR);
					drawPrompt();
					return;
				}
                File heliumModule = new File(dialog.getDirectory(), dialog.getFile());
                getController().performCommand(":l "+heliumModule.getPath());
            }
        }
    }


    protected class TerminateAction extends AbstractInterpreterAction
    {
        public TerminateAction()
        {
            super("Stop program", "Stop execution of the interpreter", new HintIcon("stop.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_X, InputEvent.CTRL_MASK));
        }


        public void evaluationStarted()
        {
            setEnabled(true);
        }


        public void evaluationEnded()
        {
            setEnabled(false);
        }


        public void actionPerformed(ActionEvent event)
        {
            getInterpreter().cancelEvaluation();
        }
    }


    protected class QuitAction extends AbstractEvalDisabledAction
    {
        public QuitAction()
        {
            super("Exit", "Exit the interpreter", new HintIcon("quit.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_X));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_Q, InputEvent.CTRL_MASK));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":q");
        }
    }


    protected List createInterpreterActions()
    {
        ErrorCycler cycler = new ErrorCycler();

        InterpreterAction[] actions =
            { new ConfigureAction()
            , new ClearAction()
            , new EditAction()
            , new JumpToErrorAction()
            , new CycleErrorUpAction(cycler)
            , new CycleErrorDownAction(cycler)
            };

        return Arrays.asList(actions);
    }


    protected class ConfigureAction extends AbstractEvalDisabledAction
    {
        public ConfigureAction()
        {
            super("Configure...", "Configure the interpreter", new HintIcon("settings.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_C));
        }


        public void actionPerformed(ActionEvent event)
        {
            JDialog configureDialog = new ConfigureDialog(InterpreterGui.this);
            centerOnDisplay(configureDialog);
            configureDialog.setVisible(true);
        }
    }


    protected class EditAction extends AbstractEvalDisabledAction
    {
        public EditAction()
        {
            super("Edit module", "Run editor for the current module", new HintIcon("edit.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_E));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_E, InputEvent.CTRL_MASK));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":e");
        }
    }


    protected class JumpToErrorAction extends AbstractEvalDisabledAction
    {
        public JumpToErrorAction()
        {
            super("Jump to last error", "Jump editor to the location of the last error or warning", new HintIcon("erroredit.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_J));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_J, InputEvent.CTRL_MASK));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":j");
        }


        public void evaluationEnded()
        {
            super.evaluationEnded();

            if (ErrorLocation.getLastErrorLocation(getOutputPane()) == null)
                setEnabled(false);
        }
    }


    protected class CycleErrorUpAction extends AbstractEvalDisabledAction
    {
        ErrorCycler cycler;


        public CycleErrorUpAction(ErrorCycler cycler)
        {
            super("Cycle up through errors", "Cycle up through locations of errors and press enter to launch editor", new HintIcon("cycleErrorUp.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_U));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_U, KeyEvent.CTRL_MASK));

            this.cycler = cycler;
        }


        public void actionPerformed(ActionEvent event)
        {
            cycler.cycleUp();
        }


        public void evaluationEnded()
        {
            super.evaluationEnded();

            if (ErrorLocation.getLastErrorLocation(getOutputPane()) == null)
                setEnabled(false);

            cycler.reset();
        }
    }


    protected class CycleErrorDownAction extends AbstractEvalDisabledAction
    {
        ErrorCycler cycler;


        public CycleErrorDownAction(ErrorCycler cycler)
        {
            super("Cycle down through errors", "Cycle down through locations of errors and press enter to launch editor", new HintIcon("cycleErrorDown.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_D));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_D, KeyEvent.CTRL_MASK));

            this.cycler = cycler;
        }


        public void actionPerformed(ActionEvent event)
        {
            cycler.cycleDown();
        }


        public void evaluationEnded()
        {
            super.evaluationEnded();

            if (ErrorLocation.getLastErrorLocation(getOutputPane()) == null)
                setEnabled(false);

            cycler.reset();
        }
    }


    protected class ClearAction extends AbstractEvalDisabledAction
    {
        public ClearAction()
        {
            super("Clear screen", "Clears the screen", new HintIcon("clear.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_C));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":clear");
        }
    }


    protected List createHelpActions()
    {
        InterpreterAction[] actions =
            { new UserManualAction()
            , new TourOfPreludeAction()
            , new HeliumPageAction()
            , new AvailableCommandsAction()
            , new AboutAction()
            };

        return Arrays.asList(actions);
    }


    protected class UserManualAction extends AbstractInterpreterAction
    {
        public UserManualAction()
        {
            super("User manual", "Displays online user manual", new HintIcon("globe.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_M));
        }


        public void actionPerformed(ActionEvent event)
        {
            try { new BrowserProcess("http://www.cs.uu.nl/helium/docs/HintUserManual.html"); }
            catch(IOException e) { JOptionPane.showMessageDialog(InterpreterGui.this, "Failed to launch browser, reason:\n"+e.toString(), "Error", JOptionPane.ERROR_MESSAGE); }
        }
    }


    protected class TourOfPreludeAction extends AbstractEvalDisabledAction
    {
        public TourOfPreludeAction()
        {
            super("Tour of the Prelude", "Displays the Tour of the Prelude", new HintIcon("globe.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_T));
            putValue(AbstractAction.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_T, InputEvent.CTRL_MASK));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":i");
        }
    }


    protected class HeliumPageAction extends AbstractInterpreterAction
    {
        public HeliumPageAction()
        {
            super("Helium homepage", "Displays helium homepage", new HintIcon("globe.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_H));
        }


        public void actionPerformed(ActionEvent event)
        {
            try { new BrowserProcess("http://www.cs.uu.nl/helium/"); }
            catch(IOException e) { JOptionPane.showMessageDialog(InterpreterGui.this, "Failed to launch browser, reason:\n"+e.toString(), "Error", JOptionPane.ERROR_MESSAGE); }
        }
    }


    protected class AvailableCommandsAction extends AbstractEvalDisabledAction
    {
        public AvailableCommandsAction()
        {
            super("Show interpreter commands", "Shows the available interpreter commands", new HintIcon("help.gif"));
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_S));
        }


        public void actionPerformed(ActionEvent event)
        {
            getController().performCommand(":?");
        }
    }


    protected class AboutAction extends AbstractInterpreterAction
    {
        public AboutAction()
        {
            super("About...", "Displays the about dialog", null);
            putValue(AbstractAction.MNEMONIC_KEY, new Integer(KeyEvent.VK_A));
        }


        public void actionPerformed(ActionEvent event)
        {
            JOptionPane.showMessageDialog( InterpreterGui.this
                                         , "Hint - Helium Interpreter"
                                           + "\nArie Middelkoop, 2003"
                                           + "\nhttp://www.cs.uu.nl/helium/"
                                           + "\n"
                                           + "\nReport bugs and suggestions to:"
                                           + "\n  helium@cs.uu.nl"
                                           + "\n "
                                           + "\n--"
                                           + "\nputStr$map(\\y->let x=fromInt y"
                                           + "\nin chr(round(21.0/.2.0*.x^.3-.92.0"
                                           + "\n*.x^.2+.503.0/.2.0*.x-.105.0)))$[1..4]"
                                         , "About"
                                         , JOptionPane.INFORMATION_MESSAGE
                                         );
        }
    }


    protected List createCommands()
    {
        Command[] commands =
            { new LoadCommand()
            , new ReloadCommand()
            , new TypeCommand()
            , new InfoCommand()
            , new EditCommand()
            , new JumpToLastErrorCommand()
            , new QuitCommand()
            , new HelpCommand()
            , new ClearCommand()
            };

        return Arrays.asList(commands);
    }


    protected class InfoCommand extends AbstractCommand
    {
        public InfoCommand()
        {
            super(":info [function]    - view function in online documentation");

            addName(":i");
            addName(":info");
        }


        public void performCommand(String input)
        {
            String functionname = input.trim();

            if (functionname.equals("*"))
                functionname = "mul";

            try { new BrowserProcess("http://www.cs.uu.nl/helium/docs/TourOfPrelude.html#"+functionname); }
            catch(IOException e) { JOptionPane.showMessageDialog(InterpreterGui.this, "Failed to launch browser, reason:\n"+e.toString(), "Error", JOptionPane.ERROR_MESSAGE); }
        }


        protected String urlencode(String functionname)
        {
            final String hexChars = "0123456789ABCDEF";

            StringBuffer output = new StringBuffer();
            for(int i=0; i < functionname.length(); i++)
            {
                char c = functionname.charAt(i);
                if (Character.isLetterOrDigit(c))
                    output.append(c);
                else
                {
                    output.append('%');
                    output.append(hexChars.charAt(c / 16));
                    output.append(hexChars.charAt(c % 16));
                }
            }

            return output.toString();
        }
    }


    protected class EditCommand extends AbstractCommand
    {
        public EditCommand()
        {
            super(":edit [row[:col]]   - edit the currently loaded module");

            addName(":e");
            addName(":edit");
        }


        public void performCommand(String input)
        {
            int row = 1;
            int col = 1;

            String locationInfo = input.trim();
            StringTokenizer tokenizer = new StringTokenizer(locationInfo, ":");

            if (tokenizer.hasMoreTokens())
            {
                String rowStr = tokenizer.nextToken();
                try { row = Integer.parseInt(rowStr); }
                catch(NumberFormatException exception) { throw new IllegalArgumentException("row is not a number"); }

                if (tokenizer.hasMoreTokens())
                {
                    String colStr = tokenizer.nextToken();
                    try { col = Integer.parseInt(colStr); }
                    catch(NumberFormatException exception) { throw new IllegalArgumentException("col is not a number"); }
                }
            }

            File module = getModule();
            if (module == null)
            {
                module = locatePrelude();
                if (module == null)
                {
                    displayErrorMessage("Cannot find the Prelude. The LVMPATH environment setting is\nprobably not accessible.");
                    return;
                }
            }

            try { new EditorProcess(module, row, col); }
            catch(IOException e) { displayErrorMessage("Failed to run the editor, reason:\n"+e.toString()); }
        }


        protected File locatePrelude()
        {
            String libPath = ProcessEnvironment.getEnvironment().getLVMEnvironmentSetting();
            if (libPath == null || libPath.length() <= 0)
                return null;

            StringTokenizer tokenizer = new StringTokenizer(libPath, File.pathSeparator);
            while(tokenizer.hasMoreTokens())
            {
                String path = tokenizer.nextToken();
                File module = new File(path, "Prelude" + HeliumProcess.HELIUM_FILE_EXTENTION);

                if (module != null && module.exists() && module.isFile() && module.canRead())
                    return module;
            }

            return null;
        }
    }


    protected class TypeCommand extends AbstractCommand
    {
        public TypeCommand()
        {
            super(":type               - print of the expression");

            addName(":t");
            addName(":type");
        }


        public void performCommand(String input)
        {
            HeliumParameters heliumParam = new HeliumParameters();
            heliumParam.setExpression(input.trim());
            heliumParam.setEvaluateExpressionType();
            heliumParam.setCompileOnly();
            if (getModule() != null)
                heliumParam.setModule(getModule());

            getController().evaluate(heliumParam);
        }
    }


    protected class ReloadCommand extends AbstractLoadCommand
    {
        public ReloadCommand()
        {
            super(":reload             - reloads the current module");

            addName("");
            addName(":r");
            addName(":reload");
        }


        public void performCommand(String input)
        {
            if (getModule() != null)
                loadModule(getModule());
        }
    }


    protected class LoadCommand extends AbstractLoadCommand
    {
        public LoadCommand()
        {
            super(":load <modulename>  - loads the specified module");

            addName(":l");
            addName(":load");
            addName(":open");
            addName(":module");
        }


        public void performCommand(String input)
        {
            File modulePath = null;

            String moduleName = input.trim();
            if (moduleName.length() <= 0)
            {
                setModule(null);
                return;
            }
            else
            {
                if (!moduleName.toLowerCase().endsWith(HeliumProcess.HELIUM_FILE_EXTENTION))
                    moduleName += HeliumProcess.HELIUM_FILE_EXTENTION;

                modulePath = new File(moduleName);
            }

            if (!modulePath.exists())
            {
                displayErrorMessage("Module does not exist: "+modulePath.getAbsolutePath());
                return;
            }

            loadModule(modulePath);
        }
    }


    abstract protected class AbstractLoadCommand extends AbstractCommand
    {
        public AbstractLoadCommand(String description)
        {
            super(description);
        }


        public void loadModule(File modulePath)
        {
            setModule(modulePath);

            File lvmModule = HeliumProcess.getLVMModuleFile(modulePath);
            if (lvmModule.exists())
                lvmModule.delete();

            HeliumParameters heliumParam = new HeliumParameters();
            heliumParam.setCompileOnly();
            heliumParam.setModule(modulePath);

            getController().evaluate(heliumParam);
        }
    }


    protected class QuitCommand extends AbstractCommand
    {
        public QuitCommand()
        {
            super(":quit               - exit the interpreter");

            addName(":q");
            addName(":x");
            addName(":quit");
            addName(":exit");
        }


        public void performCommand(String input)
        {
            System.exit(0);
        }
    }


    protected class HelpCommand extends AbstractCommand
    {
        public HelpCommand()
        {
            super(":help               - shows available commands");

            addName(":?");
            addName(":h");
            addName(":help");
        }


        public void performCommand(String input)
        {
            getOutputPane().addText("List of commands:\n", InterpreterTextPane.TYPE_OUTPUT);

            Iterator commands = getController().getCommands().iterator();
            while(commands.hasNext())
            {
                Command command = (Command) commands.next();
                getOutputPane().addText(command.getDescription() + "\n", InterpreterTextPane.TYPE_OUTPUT);
            }
        }
    }


    protected class ClearCommand extends AbstractCommand
    {
        public ClearCommand()
        {
            super(":clear              - clears the screen");

            addName(":clear");
            addName(":cls");
            addName(":c");
        }


        public void performCommand(String input)
        {
            getOutputPane().setText("");
        }
    }


    protected class JumpToLastErrorCommand extends AbstractCommand
    {
        public JumpToLastErrorCommand()
        {
            super(":jump               - jump editor to last error location");

            addName(":j");
            addName(":jump");
        }


        public void performCommand(String input)
        {
            ErrorLocation location = ErrorLocation.getLastErrorLocation(getOutputPane());

            if (location == null)
            {
                if (getModule() != null)
                {
                    getOutputPane().addText("No last error, jumping to module instead\n", InterpreterTextPane.TYPE_OUTPUT);
                    getController().performCommand(":e");
                }
                else
                    getOutputPane().addText("No error to jump to", InterpreterTextPane.TYPE_OUTPUT);
            }
            else
                ErrorLocation.launchEditor(location, getModule());
        }
    }


    protected class EnterOnErrorObserver extends KeyAdapter
    {
        public void keyPressed(KeyEvent event)
        {
            if (isRunningEvaluation())
                return;

            if (event.getKeyCode() != KeyEvent.VK_ENTER)
                return;

            int offset = getOutputPane().getSelectionStart();
            if (offset < 0)
                return;

            if (!ErrorLocation.isErrorLocation(offset, getOutputPane()))
                return;

            ErrorLocation location = ErrorLocation.getErrorLocation(offset, getOutputPane());
            if (location != null)
            {
                event.consume();
                ErrorLocation.launchEditor(location, getModule());
            }
        }
    }


    protected class MouseAtErrorObserver extends MouseInputAdapter
    {
        private Cursor handCursor;


        public MouseAtErrorObserver()
        {
            handCursor = new Cursor(Cursor.HAND_CURSOR);
        }


        public void mousePressed(MouseEvent event)
        {
            ErrorLocation location = ErrorLocation.getErrorLocation(event.getX(), event.getY(), getOutputPane());
            if (location != null)
                ErrorLocation.launchEditor(location, getModule());
        }


        public void mouseMoved(MouseEvent event)
        {
            if (ErrorLocation.isErrorLocation(event.getX(), event.getY(), getOutputPane()))
                getOutputPane().setCursor(handCursor);
            else if (getOutputPane().getCursor() == handCursor)
                getOutputPane().setCursor(null);
        }
    }


    static protected class ErrorLocation
    {
        private String filename;
        private int    row;
        private int    col;


        public ErrorLocation(String filename, int row, int col)
        {
            this.filename = filename;
            this.row      = row;
            this.col      = col;
        }


        public static void launchEditor(ErrorLocation location, File module)
        {
            if (module != null)
            {
                File target = new File(module.getParent(), location.getFilename());
                if (target.exists())
                {
                    try { new EditorProcess(target, location.getRow(), location.getCol()); }
                    catch(IOException e) { JOptionPane.showConfirmDialog(null, "Failed to run the editor, reason:\n"+e.toString(), "Error", JOptionPane.ERROR_MESSAGE); }
                }
            }
        }


        public String getFilename()
        {
            return filename;
        }


        public int getRow()
        {
            return row;
        }


        public int getCol()
        {
            return col;
        }


        public static boolean isErrorLocation(int x, int y, InterpreterTextPane outputPane)
        {
            Position.Bias bias[] = new Position.Bias[1];
            int offset = outputPane.getUI().viewToModel(outputPane, new Point(x, y), bias);
            return isErrorLocation(offset, outputPane);
        }


        public static boolean isErrorLocation(int offset, InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();
            Element selectedElem = document.getCharacterElement(offset);
            return selectedElem != null && outputPane.isLocationAttribute(selectedElem.getAttributes());
        }


        public static ErrorLocation getErrorLocation(int x, int y, InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();

            if (!isErrorLocation(x, y, outputPane))
                return null;

            Position.Bias bias[] = new Position.Bias[1];
            int offset = outputPane.getUI().viewToModel(outputPane, new Point(x, y), bias);
            Element selectedElement = document.getCharacterElement(offset);

            return getErrorLocation(selectedElement, outputPane);
        }


        public static ErrorLocation getErrorLocation(int offset, InterpreterTextPane outputPane)
        {
            return getErrorLocation(getLocationElement(offset, outputPane), outputPane);
        }


        public static Element getLocationElementForwards(int offset, InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();

            while(offset <= document.getLength())
            {
                Element elem = document.getCharacterElement(offset);
                if (outputPane.isLocationAttribute(elem.getAttributes()))
                    return elem;

                offset = elem.getEndOffset() + 1;
            }

            return null;
        }


        public static Element getLocationElement(int offset, InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();

            while(offset >= 0)
            {
                Element elem = document.getCharacterElement(offset);
                if (outputPane.isLocationAttribute(elem.getAttributes()))
                    return elem;

                offset = elem.getStartOffset() - 1;
            }

            return null;
        }


        public static ErrorLocation getErrorLocation(Element locationElement, InterpreterTextPane outputPane)
        {
            if (locationElement == null)
                return null;

            StyledDocument document = outputPane.getStyledDocument();

            String filename = locateFilename(locationElement.getStartOffset()-1, outputPane);
            if (filename == null)
                return null;

            String errorMsg = null;
            try { errorMsg = document.getText(locationElement.getStartOffset(), locationElement.getEndOffset()-locationElement.getStartOffset()); }
            catch(BadLocationException exception) { return null; }

            int startParenthesisIndex = errorMsg.indexOf('(');
            int commaIndex            = errorMsg.indexOf(',');
            int endParenthesisIndex   = errorMsg.indexOf(')');

            if (startParenthesisIndex < 0 || commaIndex < 0 || endParenthesisIndex < 0 || commaIndex <= startParenthesisIndex || commaIndex >= endParenthesisIndex)
                return null;

            String row = errorMsg.substring(startParenthesisIndex + 1, commaIndex);
            String col = errorMsg.substring(commaIndex + 1, endParenthesisIndex);

            int rowNumber = 0;
            try { rowNumber = Integer.parseInt(row); }
            catch(NumberFormatException e) { return null; }

            int colNumber = 0;
            try { colNumber = Integer.parseInt(col); }
            catch(NumberFormatException e) { return null; }

            return new ErrorLocation(filename, rowNumber, colNumber);
        }


        public static ErrorLocation getLastErrorLocation(InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();
            return ErrorLocation.getErrorLocation(document.getLength()-1, outputPane);
        }


        protected static String locateFilename(int offset, InterpreterTextPane outputPane)
        {
            StyledDocument document = outputPane.getStyledDocument();

            final String compilingFile = "Compiling ";

            int pos = offset;
            while(pos >= 0)
            {
                Element current = document.getCharacterElement(pos);

                String errorMsg = null;
                try { errorMsg = document.getText(current.getStartOffset(), current.getEndOffset()-current.getStartOffset()-1); }
                catch(BadLocationException exception) { return null; }

                if (errorMsg.startsWith(compilingFile))
                    return errorMsg.substring(compilingFile.length()).trim();

                pos = current.getStartOffset() - 1;
            }

            return null;
        }
    }


    protected class ErrorCycler
    {
        private int     lastOffset;
        private boolean wasCyclingUp;
        private boolean wasCyclingDown;


        public ErrorCycler()
        {
            reset();
        }


        public void reset()
        {
            lastOffset     = -1;
            wasCyclingUp   = false;
            wasCyclingDown = false;
        }


        public void cycleUp()
        {
            cycle(true);
            wasCyclingUp   = true;
            wasCyclingDown = false;
        }


        public void cycleDown()
        {
            cycle(false);
            wasCyclingDown = true;
            wasCyclingUp   = false;
        }


        protected void cycle(boolean cycleUp)
        {
            if (ErrorLocation.getLastErrorLocation(getOutputPane()) == null)
                return;

            Document doc = getOutputPane().getDocument();

            boolean ignoreFirstTime = false;
            if ((cycleUp && wasCyclingDown) || (!cycleUp && wasCyclingUp))
                ignoreFirstTime = true;

            Element errorLocation = null;
            while(errorLocation == null || ignoreFirstTime)
            {
                if (errorLocation != null)
                    ignoreFirstTime = false;

                if (cycleUp)
                {
                    if (lastOffset < 0)
                        lastOffset = doc.getLength();

                    errorLocation = ErrorLocation.getLocationElement(lastOffset, getOutputPane());
                    if (errorLocation != null)
                        lastOffset = errorLocation.getStartOffset()-1;
                }
                else
                {
                    if (lastOffset < 0)
                        lastOffset = 0;

                    errorLocation = ErrorLocation.getLocationElementForwards(lastOffset, getOutputPane());
                    if (errorLocation != null)
                        lastOffset = errorLocation.getEndOffset()+1;
                }

                if (errorLocation == null)
                    lastOffset = -1;
            }

            getOutputPane().requestFocus();
            getOutputPane().select(errorLocation.getStartOffset(), errorLocation.getEndOffset());
            setIgnoreFocusOnce();
        }
    }

}
