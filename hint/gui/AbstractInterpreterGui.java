package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.util.List;
import javax.swing.*;
import javax.swing.text.*;

import hint.gui.*;
import hint.gui.icon.*;
import hint.interpreter.*;
import hint.util.*;


abstract public class AbstractInterpreterGui extends JFrame
{
    private Interpreter         interpreter;
    private Controller          controller;
    private InterpreterTextPane outputPane;
    private JTextField          inputPane;
    private JLabel              statusPane;

    private Prompt              prompt;
    private File                moduleFile;
    private boolean             runningEvaluation;
    private boolean             ignoreFocusOnce;

    private List                fileActions;
    private List                interpreterActions;
    private List                helpActions;


    public AbstractInterpreterGui()
    {
        super();

        setTitle(null);
        setIconImage(new HintIcon("heliumTaskbarIcon.gif").getImage());
        addWindowListener(new WindowCloser());

        fileActions        = createFileActions();
        interpreterActions = createInterpreterActions();
        helpActions        = createHelpActions();

        prompt = createPrompt();

        controller = createController();
        controller.addCommands(createCommands());

        interpreter = new Interpreter();
        interpreter.addObserver(controller);

        outputPane = createTextPane();
        outputPane.setRequestFocusEnabled(true); // setFocusable(true); bestaat niet in JDK 1.3
        outputPane.addMouseListener(new InputPaneSelector());
        JScrollPane scrollPane = new JScrollPane(outputPane);
        scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);

        InputAction inputAction = new InputAction();
        inputPane = new JTextField(40);
		int fontSize = ProcessEnvironment.getEnvironment().getFontSize();
        inputPane.setFont(new Font("monospaced", fontSize < 16 ? Font.PLAIN : Font.BOLD, fontSize));
        inputPane.setAction(inputAction);
        inputPane.addKeyListener(inputAction);
        String[] inputPanePopupActions = { DefaultEditorKit.cutAction, DefaultEditorKit.copyAction, DefaultEditorKit.pasteAction };
        TextPopupMenu.addPopupMenu(inputPane, inputPanePopupActions);

        statusPane = new JLabel();

        JPanel commandPane = new JPanel();
        commandPane.setLayout(new BorderLayout());
        commandPane.add(statusPane, BorderLayout.WEST);
        commandPane.add(inputPane,  BorderLayout.CENTER);

        setJMenuBar(createMenuBar());

        Container contentPane = getContentPane();
        contentPane.setLayout(new BorderLayout());
        contentPane.add(createToolBar(), BorderLayout.NORTH);
        contentPane.add(scrollPane,      BorderLayout.CENTER);
        contentPane.add(commandPane,     BorderLayout.SOUTH);

        ignoreFocusOnce = false;
        setStoppedRunningEvaluation();

        setSize(560, 500);
        centerOnDisplay(this);
    }


    public void setFocusToInputPane()
    {
        if (!ignoreFocusOnce)
            inputPane.requestFocus();

        ignoreFocusOnce = false;
    }


    public void setTitle(String moduleName)
    {
        String title = "Hint";

        if (moduleName != null && moduleName.length() > 0)
            title += " - " + moduleName;

        super.setTitle(title);
    }


    abstract protected Prompt createPrompt();


    abstract protected InterpreterTextPane createTextPane();


    abstract protected List createFileActions();


    abstract protected List createInterpreterActions();


    abstract protected List createHelpActions();


    abstract protected List createCommands();


    protected Controller createController()
    {
        return new Controller();
    }


    protected JMenuBar createMenuBar()
    {
        JMenuBar menubar = new JMenuBar();

        JMenu fileMenu = new JMenu("File");
        fileMenu.setMnemonic(KeyEvent.VK_F);
        addMenuActions(fileMenu, fileActions);
        menubar.add(fileMenu);

        JMenu interpreterMenu = new JMenu("Interpreter");
        interpreterMenu.setMnemonic(KeyEvent.VK_I);
        addMenuActions(interpreterMenu, interpreterActions);
        menubar.add(interpreterMenu);

        JMenu helpMenu = new JMenu("Help");
        helpMenu.setMnemonic(KeyEvent.VK_H);
        addMenuActions(helpMenu, helpActions);
        menubar.add(helpMenu);

        return menubar;
    }


    protected JToolBar createToolBar()
    {
        JToolBar toolbar = new JToolBar();
        toolbar.setFloatable(false);
        toolbar.setOrientation(JToolBar.HORIZONTAL);
        toolbar.setRequestFocusEnabled(false);

        addToolbarActions(toolbar, fileActions);
        toolbar.addSeparator(new Dimension(10, 1));
        addToolbarActions(toolbar, interpreterActions);

        return toolbar;
    }


    protected void setModule(File moduleFile)
    {
        this.moduleFile = moduleFile;

        if (moduleFile != null)
        {
            prompt.setModuleName(Dir.stripFileExtention(moduleFile.getName(), HeliumProcess.HELIUM_FILE_EXTENTION));
            setTitle(moduleFile.getName());
        }
        else
        {
            prompt.setModuleName(null);
            setTitle(null);
        }
    }


    protected File getModule()
    {
        return moduleFile;
    }


    protected boolean isRunningEvaluation()
    {
        return runningEvaluation;
    }


    protected void setRunningEvaluation()
    {
        runningEvaluation = true;
        updateStatusPane();
        updateStateOfAllActions();
    }


    protected void setStoppedRunningEvaluation()
    {
        runningEvaluation = false;
        drawPrompt();
        updateStatusPane();
        updateStateOfAllActions();

        setFocusToInputPane();
    }


    protected void updateStatusPane()
    {
        if (isRunningEvaluation())
            statusPane.setIcon(new HintIcon("input.gif"));
        else
            statusPane.setIcon(new HintIcon("expression.gif"));
    }


    protected void updateStateOfAllActions()
    {
        updateActionsState(fileActions);
        updateActionsState(interpreterActions);
        updateActionsState(helpActions);
    }


    protected void updateActionsState(List actions)
    {
        Iterator iterator = actions.iterator();
        while(iterator.hasNext())
        {
            InterpreterAction action = (InterpreterAction) iterator.next();
            if (isRunningEvaluation())
                action.evaluationStarted();
            else
                action.evaluationEnded();
        }
    }


    protected void drawPrompt()
    {
        getOutputPane().ensureTrailingNewline();
        getOutputPane().addText(prompt.toString(), InterpreterTextPane.TYPE_COMMAND);
    }


    protected Interpreter getInterpreter()
    {
        return interpreter;
    }


    protected Controller getController()
    {
        return controller;
    }


    protected InterpreterTextPane getOutputPane()
    {
        return outputPane;
    }


    protected void setIgnoreFocusOnce()
    {
        ignoreFocusOnce = true;
    }


    protected void centerOnDisplay(Component frame)
    {
        Dimension frameSize;
        Rectangle displayArea;

        frameSize   = frame.getSize();
        displayArea = frame.getGraphicsConfiguration().getBounds();

        frame.setLocation( displayArea.x + (displayArea.width  - frameSize.width)  / 2
                         , displayArea.y + (displayArea.height - frameSize.height) / 2
                         );
    }


    private void addMenuActions(JMenu menu, List actions)
    {
        Iterator iterator = actions.iterator();
        while(iterator.hasNext())
        {
            Action action = (Action) iterator.next();
            menu.add(action);
        }
    }


    private void addToolbarActions(JToolBar toolbar, List actions)
    {
        Iterator iterator = actions.iterator();
        while(iterator.hasNext())
        {
            Action  action = (Action) iterator.next();
            JButton button = toolbar.add(action);
            button.setRequestFocusEnabled(false);
            button.setMargin(new Insets(1, 1, 1, 1));
        }
    }


    protected class Controller extends InterpreterAdapter
    {
        private List commands;


        public Controller()
        {
            commands = new LinkedList();
        }


        public void addCommand(Command command)
        {
            if (command == null)
                throw new IllegalArgumentException("command is null");

            this.commands.add(command);
        }


        public void addCommands(List commands)
        {
            if (commands == null)
                throw new IllegalArgumentException("commands is null");

            this.commands.addAll(commands);
        }


        public List getCommands()
        {
            return Collections.unmodifiableList(commands);
        }


        public void performCommand(String input)
        {
            getOutputPane().addText(input + "\n", InterpreterTextPane.TYPE_COMMAND);

            if (isRunningEvaluation())
            {
                getInterpreter().sendData(input);
                return;
            }

            StringTokenizer tokenizer = new StringTokenizer(input);

            String commandName;
            if (tokenizer.hasMoreTokens())
                commandName = tokenizer.nextToken().toLowerCase();
            else
                commandName = "";

            String parameters = input.substring(commandName.length());

            Iterator iterator = commands.iterator();
            while(iterator.hasNext())
            {
                Command command = (Command) iterator.next();
                if (command.hasName(commandName))
                {
                    try { command.performCommand(parameters); }
                    catch(IllegalArgumentException e)
                    {
                        JOptionPane.showMessageDialog(AbstractInterpreterGui.this, "Invalid input. Please check:\n" + command.getDescription(), "Invalid input", JOptionPane.ERROR_MESSAGE);
                    }

                    if (!isRunningEvaluation())
                        setStoppedRunningEvaluation();

                    setFocusToInputPane();
                    return;
                }
            }

            if (input.length() > 0)
            {
                HeliumParameters heliumParam = new HeliumParameters();
                heliumParam.setExpression(input);

                File module = getModule();
                if (module != null)
                    heliumParam.setModule(module);

                evaluate(heliumParam);
            }
        }


        public void evaluate(HeliumParameters parameters)
        {
            setRunningEvaluation();
            getInterpreter().evaluate(parameters);
        }


        public void evaluationFinished()
        {
            SwingUtilities.invokeLater(new Runnable() { public void run() { setStoppedRunningEvaluation(); } });
        }


        public void interpreterFailure(Exception exception)
        {
            getOutputPane().addText("\n*** Interpreter failure: "+exception.toString()+"\n", InterpreterTextPane.TYPE_ERROR);

            System.err.println(exception.toString());
            exception.printStackTrace(System.err);
        }


        public void inputRecieved(String data)
        {
            getOutputPane().addText(data, InterpreterTextPane.TYPE_OUTPUT);
        }


        public void errorRecieved(String data)
        {
            getOutputPane().addText(data, InterpreterTextPane.TYPE_ERROR);
        }
    }


    abstract protected class AbstractCommand implements Command
    {
        private String      description;
        private Collection  names;


        public AbstractCommand(String description)
        {
            this.description = description;
            this.names       = new ArrayList();
        }


        public void addName(String name)
        {
            names.add(name);
        }


        public boolean hasName(String name)
        {
            return names.contains(name);
        }


        public String getDescription()
        {
            return description;
        }


        public String toString()
        {
            return description;
        }


        protected String[] getParameters(String expression)
        {
            StringTokenizer tokenizer = new StringTokenizer(expression);
            String[] parameters = new String[tokenizer.countTokens()];

            for(int i=0; i < parameters.length; i++)
                parameters[i] = tokenizer.nextToken();

            return parameters;
        }


        protected void displayErrorMessage(String message)
        {
            JOptionPane.showMessageDialog(AbstractInterpreterGui.this.inputPane, message, "Command error", JOptionPane.ERROR_MESSAGE);
        }
    }


    protected interface Command
    {
        public boolean hasName(String name);
        public void performCommand(String input);
        public String getDescription();
    }


    protected class InputAction extends AbstractAction implements KeyListener
    {
        private List history;
        private int  lastIndex;

        public InputAction()
        {
            super();

            history   = new ArrayList();
            lastIndex = 0;
        }


        public void keyPressed(KeyEvent event)
        {
            if (history.size() > 0)
            {
                switch(event.getKeyCode())
                {
                    case KeyEvent.VK_UP:
                        lastIndex = Math.max(0, lastIndex-1);
                        inputPane.setText((String) history.get(lastIndex));
                        break;

                    case KeyEvent.VK_DOWN:
                        lastIndex = Math.min(history.size(), lastIndex+1);

                        if (lastIndex == history.size())
                            inputPane.setText("");
                        else
                            inputPane.setText((String) history.get(lastIndex));

                        break;

                    default:
                        return;
                }

                inputPane.selectAll();
                event.consume();
            }
        }


        public void keyReleased(KeyEvent event)
        {
        }


        public void keyTyped(KeyEvent event)
        {
        }


        public void actionPerformed(ActionEvent event)
        {
            inputPane.selectAll();

            if (!isRunningEvaluation())
            {
                history.add(inputPane.getText());
                lastIndex = history.size()-1;
            }

            getController().performCommand(inputPane.getText());
        }
    }


    protected class WindowCloser extends WindowAdapter
    {
        public void windowClosing(WindowEvent e)
        {
            getController().performCommand(":q");
        }
    }


    abstract protected class AbstractEvalDisabledAction extends AbstractInterpreterAction
    {
        public AbstractEvalDisabledAction(String name, String description, Icon icon)
        {
            super(name, description, icon);
        }


        public void evaluationStarted()
        {
            setEnabled(false);
        }


        public void evaluationEnded()
        {
            setEnabled(true);
        }
    }


    abstract protected class AbstractInterpreterAction extends AbstractAction implements InterpreterAction
    {
        public AbstractInterpreterAction(String name, String description, Icon icon)
        {
            super();

            putValue(AbstractAction.NAME, name);
            putValue(AbstractAction.SHORT_DESCRIPTION, description);

            if (icon != null)
                putValue(AbstractAction.SMALL_ICON, icon);
        }


        public void evaluationStarted()
        {
        }


        public void evaluationEnded()
        {
        }
    }


    public interface InterpreterAction extends Action
    {
        public void evaluationStarted();
        public void evaluationEnded();
    }


    protected class InputPaneSelector extends MouseAdapter
    {
        public void mouseClicked(MouseEvent event)
        {
            setFocusToInputPane();
        }
    }
}
