package hint;


import java.io.*;
import javax.swing.*;

import hint.interpreter.*;


public class HintUU
{
    public static void main(String[] parameters)
    {
        try {
        UUHeliumEnvironment environment = new UUHeliumEnvironment();
        ProcessEnvironment.setEnvironment(environment);

        Hint.checkTempDir();

        environment.createLocalInstallation(); }
        catch(Exception exception) { JOptionPane.showMessageDialog(null, "Failed to create local Helium installation:\n"+exception.toString(), "Startup failure", JOptionPane.ERROR_MESSAGE); }

        Hint.main(parameters);
    }
}
