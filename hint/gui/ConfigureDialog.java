package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

import hint.interpreter.*;

public class ConfigureDialog extends JDialog
{
    private JTextField      basePath;
    private JTextField      lvmPaths;
    private JTextField      editorCommandline;
    private JTextField      browserCommandline;
    private JTextField      additionalOptions;
    private JTextField      fontSize;
    private JCheckBox       overloading;
    private JCheckBox       loggingOn;

    private JPanel             settingsPane;
    private GridBagLayout      layout;
    private GridBagConstraints layoutConstraints;


    public ConfigureDialog(Frame parent)
    {
        super(parent, "Configure interpreter", true);
        getContentPane().setLayout(new BorderLayout());

        settingsPane = new JPanel();
        layout = new GridBagLayout();
        layoutConstraints = new GridBagConstraints();
        settingsPane.setLayout(layout);

        basePath = new JTextField(ProcessEnvironment.getEnvironment().getBasePath(), 30);
        lvmPaths  = new JTextField(ProcessEnvironment.getEnvironment().getLvmPaths(), 30);
        JTextField tempPath = new JTextField(ProcessEnvironment.getEnvironment().getTempPath().getAbsolutePath(), 30);
        // JTextField path     = new JTextField(ProcessEnvironment.getEnvironment().getPathEnvironmentSetting(), 30);
        tempPath.setEditable(false);

        editorCommandline  = new JTextField(ProcessEnvironment.getEnvironment().getEditorCommandlineTemplate(),  30);
        browserCommandline = new JTextField(ProcessEnvironment.getEnvironment().getBrowserCommandlineTemplate(), 30);
        additionalOptions  = new JTextField(ProcessEnvironment.getEnvironment().getAdditionalHeliumParameters(), 30);
        fontSize           = new JTextField(Integer.toString(ProcessEnvironment.getEnvironment().getFontSize()), 30);
        overloading        = new JCheckBox(" ", ProcessEnvironment.getEnvironment().getOverloading());
        loggingOn          = new JCheckBox(" ", ProcessEnvironment.getEnvironment().getLoggingOn());

        layoutConstraints.anchor = GridBagConstraints.WEST;

        add(" Base path ", basePath);
        add(" Additional lvm paths: ", lvmPaths);
        add(" Editor commandline: ", editorCommandline);
        add(" Browser commandline: ", browserCommandline);
        add(" Additional helium options: ", additionalOptions);
        add(" Font size: ", fontSize);
        add(" Enable overloading: ", overloading);
        add(" Enable logging: ", loggingOn);
        add(" TEMP: ", tempPath);
        // add(" PATH: ", path);

        JButton ok = new JButton("OK");
        ok.addActionListener(new OkListener());

        JButton cancel = new JButton("Cancel");
        cancel.addActionListener(new CancelListener());

        Box controlBox = Box.createHorizontalBox();
        controlBox.add(Box.createHorizontalGlue());
        controlBox.add(ok);
        controlBox.add(cancel);

        getContentPane().add(settingsPane, BorderLayout.CENTER);
        getContentPane().add(controlBox,   BorderLayout.SOUTH);

        setResizable(false);
        pack();
    }

    protected void add(String labelText, JComponent textfield)
    {
        JLabel label = new JLabel(labelText);
        label.setHorizontalAlignment(JTextField.LEFT);
        layoutConstraints.gridwidth = GridBagConstraints.RELATIVE;
        layout.setConstraints(label, layoutConstraints);
        settingsPane.add(label);

        layoutConstraints.gridwidth = GridBagConstraints.REMAINDER;
        layout.setConstraints(textfield, layoutConstraints);
        settingsPane.add(textfield);
    }


    protected class OkListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
            ProcessEnvironment environment = ProcessEnvironment.getEnvironment();
            
            environment.setBasePath(basePath.getText());
            environment.setLvmPaths(lvmPaths.getText());
            environment.setEditorCommandlineTemplate(editorCommandline.getText());
            environment.setBrowserCommandlineTemplate(browserCommandline.getText());
            environment.setAdditionalHeliumParameters(additionalOptions.getText());

            if (environment.getOverloading() != overloading.isSelected())
                JOptionPane.showMessageDialog(ConfigureDialog.this, "You changed the overloading option. Please recompile all your modules.\n\nYou can do this by removing the .lvm files produced by helium.\nWatch out that you don\'t remove your .hs files by accident!", "Overloading changed notice", JOptionPane.INFORMATION_MESSAGE);

            environment.setOverloading(overloading.isSelected());
            environment.setLoggingOn(loggingOn.isSelected());
            try {
            	environment.setFontSize(Integer.parseInt(fontSize.getText()));
            } catch (NumberFormatException e) {}

            try
            {
                environment.saveSettings();
                ConfigureDialog.this.dispose();
            }
            catch(IOException exception) { JOptionPane.showMessageDialog(ConfigureDialog.this, "Unable to save settings, reason:\n"+exception.toString(), "Error", JOptionPane.ERROR_MESSAGE); }
        }
    }


    protected class CancelListener implements ActionListener
    {
        public void actionPerformed(ActionEvent event)
        {
            ConfigureDialog.this.dispose();
        }
    }
}
