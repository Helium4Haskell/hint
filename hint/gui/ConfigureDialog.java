package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;

import hint.interpreter.*;


public class ConfigureDialog extends JDialog
{
    private JTextField      editorCommandline;
    private JTextField      browserCommandline;
    private JTextField      additionalOptions;
    private JTextField      fontSize;

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

        editorCommandline  = new JTextField(ProcessEnvironment.getEnvironment().getEditorCommandlineTemplate(),  30);
        browserCommandline = new JTextField(ProcessEnvironment.getEnvironment().getBrowserCommandlineTemplate(), 30);
        additionalOptions  = new JTextField(ProcessEnvironment.getEnvironment().getAdditionalHeliumParameters(), 30);
        fontSize           = new JTextField(Integer.toString(ProcessEnvironment.getEnvironment().getFontSize()), 30);

        layoutConstraints.anchor = GridBagConstraints.WEST;

        add(" Editor commandline: ", editorCommandline);
        add(" Browser commandline: ", browserCommandline);
        add(" Additional helium options: ", additionalOptions);
        add(" Font size: ", fontSize);

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


    protected void add(String labelText, JTextField textfield)
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
            environment.setEditorCommandlineTemplate(editorCommandline.getText());
            environment.setBrowserCommandlineTemplate(browserCommandline.getText());
            environment.setAdditionalHeliumParameters(additionalOptions.getText());
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
