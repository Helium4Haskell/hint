package hint.gui.icon;


import java.awt.Image;
import java.net.URL;
import javax.swing.ImageIcon;


public class HintIcon extends ImageIcon
{
    public HintIcon(String iconName)
    {
        this(iconName, null);
    }


    public HintIcon(String iconName, String description)
    {
        super();

        if (iconName == null)
            throw new IllegalArgumentException("iconName is null");

        URL url = getClass().getResource(iconName);

        ImageIcon delegate;
        if (url == null)
            delegate = new ImageIcon(iconName);
        else
            delegate = new ImageIcon(url);

        Image image = delegate.getImage();
        if (image != null)
        {
            setImage(image);
            loadImage(image);
        }

        if (description != null)
            setDescription(description);
    }
}
