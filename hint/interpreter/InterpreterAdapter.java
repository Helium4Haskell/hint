package hint.interpreter;


import hint.interpreter.InterpreterObserver;


/**
 * Empty implementation of InterpreterObserver.
 *
 * @author  Arie Middelkoop
 * @version 1.0 (9 feb 2003)
 * @since   HI 1.0
 */

public class InterpreterAdapter implements InterpreterObserver
{
    public void evaluationStarted()
    {
    }


    public void evaluationFinished()
    {
    }


    public void interpreterFailure(Exception e)
    {
    }


    public void inputRecieved(String data)
    {
    }


    public void errorRecieved(String data)
    {
    }
}
