package hint.gui;


import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.text.*;
import javax.swing.text.html.*;

import hint.gui.icon.*;


public class WebpageFrame extends JFrame implements ActionListener, HyperlinkListener
{
    private JEditorPane editorPane;

    public static final Dimension PREFERRED_SIZE = new Dimension(500, 400);


    public WebpageFrame(String title, String url)
    {
        super();

        if (title == null)
            throw new IllegalArgumentException("title is null");
        if (url == null)
            throw new IllegalArgumentException("url is null");

        setTitle(title);
        setIconImage(new HintIcon("heliumTaskbarIcon.gif").getImage());

        getContentPane().setLayout(new BorderLayout());

        editorPane = null;
        try
        {
            editorPane = new JEditorPane(new URL(url));
            editorPane.setEditable(false);
            editorPane.setPreferredSize(PREFERRED_SIZE);
            editorPane.addHyperlinkListener(this);

            JScrollPane scrollPane = new JScrollPane(editorPane, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
            getContentPane().add(scrollPane, BorderLayout.CENTER);
        }
        catch(IOException e)
        {
            getContentPane().add(new JLabel("Loading of webpage failed, reason: "+e.toString()), BorderLayout.CENTER);
        }

        JButton closeButton = new JButton("Close");
        closeButton.addActionListener(this);
        getContentPane().add(closeButton, BorderLayout.SOUTH);

        pack();
    }


    public void actionPerformed(ActionEvent event)
    {
        dispose();
    }


    public void hyperlinkUpdate(HyperlinkEvent event)
    {
        if (event.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
        {
            if (event instanceof HTMLFrameHyperlinkEvent)
            {
                HTMLFrameHyperlinkEvent evt = (HTMLFrameHyperlinkEvent) event;
                HTMLDocument            doc = (HTMLDocument) editorPane.getDocument();
                doc.processHTMLFrameHyperlinkEvent(evt);
            }
            else
            {
                try { editorPane.setPage(event.getURL()); }
                catch (IOException e)
                {
                    System.err.println(e.toString());
                }
            }
        }
    }
}
