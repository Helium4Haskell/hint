package hint.gui;


import java.awt.event.*;
import javax.swing.*;
import javax.swing.text.*;


public class TextPopupMenu extends JPopupMenu
{
    private JTextComponent  textComponent;


    public static void addPopupMenu(JTextComponent textComponent, String[] actions)
    {
        new TextPopupMenu(textComponent, actions);
    }


    protected TextPopupMenu(JTextComponent textComponent, String[] actionNames)
    {
        if (textComponent == null)
            throw new IllegalArgumentException("textComponent is null");
        if (actionNames == null)
            throw new IllegalArgumentException("actionNames is null");

        this.textComponent = textComponent;

        for(int i=0; i < actionNames.length; i++)
            add(getActionByName(actionNames[i]));

        textComponent.add(this);
        textComponent.addMouseListener(new PopupListener());
    }


    protected Action getActionByName(String name)
    {
        Action[] actions = textComponent.getActions();

        for(int i=0; i < actions.length; i++)
        {
            if (actions[i].getValue(Action.NAME).equals(name))
                return actions[i];
        }

        return null;
    }


    protected class PopupListener extends MouseAdapter
    {
        public void mousePressed(MouseEvent event)
        {
            if (event.isPopupTrigger())
            {
                show(textComponent, event.getX(), event.getY());
                event.consume();
            }
        }


        public void mouseReleased(MouseEvent event)
        {
            if (event.isPopupTrigger())
            {
                show(textComponent, event.getX(), event.getY());
                event.consume();
            }
        }
    }
}
