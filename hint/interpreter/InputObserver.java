package hint.interpreter;


/**
 * Interface that allows the input of a process to be observed. Input that is
 * considered as "errors' (or less dramatic: notices and warnings) is sent to
 * the errorReceived method and all normal data to inputRecieved.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public interface InputObserver
{
    public void inputRecieved(String data);
    public void errorRecieved(String data);
}
