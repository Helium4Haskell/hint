package hint;


import java.io.*;
import java.net.*;
import javax.swing.*;

import hint.gui.*;
import hint.interpreter.*;


public class Hint
{
    protected static void checkTempDir()
    {
        try { ProcessEnvironment.getEnvironment().getTempPath().toURL(); }
        catch(Throwable exception)
        {
            JOptionPane.showMessageDialog(null, "Temporary directory is not available", "Error", JOptionPane.ERROR_MESSAGE);
            System.exit(-1);
        }
    }


    public static void main(String[] parameters)
    {
        try { ProcessEnvironment.getEnvironment().loadSettings(); }
        catch(IOException exception) {}

        checkTempDir();

        InterpreterGui gui = new InterpreterGui();
        gui.setVisible(true);
        gui.setFocusToInputPane();
    }
}
