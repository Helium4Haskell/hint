package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;
import javax.swing.text.*;

import hint.gui.icon.*;
import hint.util.*;
import hint.interpreter.*;


public class InterpreterTextPane extends JTextPane
{
    private DefaultStyledDocument   document;
    private SimpleAttributeSet      errorAttr;
    private SimpleAttributeSet      commandAttr;
    private SimpleAttributeSet      outputAttr;
    private SimpleAttributeSet      locationAttr;
    private SimpleAttributeSet      specialLocationAttr;
    private SimpleAttributeSet      specialOkAttr;
    private JPopupMenu              popupMenu;
    private boolean                 useSpecialColor;

    public static final int  TYPE_ERROR   = 1;
    public static final int  TYPE_COMMAND = 2;
    public static final int  TYPE_OUTPUT  = 3;


    public InterpreterTextPane()
    {
        super();

        setFont(new Font("Monospaced", Font.PLAIN, ProcessEnvironment.getEnvironment().getFontSize()));
        setPreferredSize(new Dimension(600, 400));

        StyleContext styleContext = new StyleContext();
        document = new DefaultStyledDocument(styleContext);
        setStyledDocument(document);

        String[] actionNames = { DefaultEditorKit.copyAction };
        TextPopupMenu.addPopupMenu(this, actionNames);

        insertIcon(new HintIcon("heliumlogo.jpg"));
        setEditable(false);
        useSpecialColor = false;

        errorAttr = new SimpleAttributeSet();
        StyleConstants.setForeground(errorAttr, new Color(174, 2, 50));

        outputAttr = new SimpleAttributeSet();
        StyleConstants.setForeground(outputAttr, Color.black);

        commandAttr = new SimpleAttributeSet();
        StyleConstants.setForeground(commandAttr, new Color(0, 64, 128));


        locationAttr = new SimpleAttributeSet();
        locationAttr.addAttributes(errorAttr);
        StyleConstants.setBold(locationAttr, true);
        //StyleConstants.setUnderline(locationAttr, true);
        //StyleConstants.setItalic(locationAttr, true);


        specialOkAttr = new SimpleAttributeSet();
        specialLocationAttr = new SimpleAttributeSet();
        specialLocationAttr.addAttributes(locationAttr);

        StyleConstants.setForeground(specialOkAttr, new Color(0, 128, 128));
        StyleConstants.setForeground(specialLocationAttr, new Color(0, 128, 128));


        addText("\n", TYPE_COMMAND);
    }


    public boolean isLocationAttribute(AttributeSet set)
    {
        return set.containsAttributes(locationAttr) || set.containsAttributes(specialLocationAttr);
    }


    public void addText(String text, int type)
    {
        AttributeSet attributes = null;

        switch(type)
        {
            case TYPE_ERROR:
                if (text.equals("\0startgreen\0"))
                {
                    useSpecialColor = true;
                    return;
                }
                else if (text.startsWith("(") && text.endsWith(")") && text.indexOf(",") > 0)
                {
                    if (useSpecialColor)
                        attributes = specialLocationAttr;
                    else
                        attributes = locationAttr;
                }
                else
                {
                    if (useSpecialColor)
                        attributes = specialOkAttr;
                    else
                        attributes = errorAttr;
                }

                if (text.startsWith("Compilation successful"))
                    useSpecialColor = false;
                break;

            case TYPE_COMMAND:
                attributes = commandAttr;
                useSpecialColor = false;
                break;

            case TYPE_OUTPUT:
                attributes = outputAttr;
                useSpecialColor = false;
                break;
        }

        try { document.insertString(document.getLength(), text, attributes); }
        catch(BadLocationException exception) { Log.getLogger().throwing(getClass().getName(), "addText", exception); }

        setCaretPosition(document.getLength());
    }


    public void ensureTrailingNewline()
    {
        try
        {
            int      lastIndex = Math.max(0, document.getLength()-1);
            String   lastChars = document.getText(lastIndex, document.getLength()-lastIndex);

            if (lastChars.length() > 0 && lastChars.charAt(0) != '\n')
                addText("\n", InterpreterTextPane.TYPE_COMMAND);
        }
        catch(BadLocationException exception) { Log.getLogger().throwing(getClass().getName(), "ensureTrailingNewline", exception); }
    }
}
