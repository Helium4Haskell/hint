package hint.util;


/**
 * A logging facility. Reported messages are written to the standard error
 * stream. This class is indented as a (temporary) replacement of
 * java.util.logging.Logger (jdk1.4).
 *
 * @author Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0
 * @since HI 1.0
 */

public class Log
{
    private static Log instance = null;

    public static final String LOG_NAME = "helium.interpreter";


    public static synchronized Log getLogger()
    {
        if (instance == null)
            instance = new Log();

        return instance;
    }


    protected Log()
    {
    }


    public void throwing(String className, String methodName, Throwable e)
    {
        System.err.println("Problem in "+className+":"+methodName+": "+e.toString());
        e.printStackTrace(System.err);
    }
}
