package hint.inputfilter;


import hint.interpreter.InputObserver;


/**
 * Input filters are used to filter the output (which is considered input with
 * respect to the interpreter) of the helium compiler. After processing, the
 * resulting data is delegated to a sub filter. It is much easier to add new
 * filters this way.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

abstract public class AbstractInputFilter implements InputObserver
{
    private InputObserver delegate;


    public AbstractInputFilter(InputObserver delegate)
    {
        this.delegate = delegate;
    }


    public void inputRecieved(String data)
    {
        delegate.inputRecieved(data);
    }


    public void errorRecieved(String data)
    {
        delegate.errorRecieved(data);
    }
}
