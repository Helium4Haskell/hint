package hint.interpreter;


/**
 * Interface to allow observing of the interpreter.
 *
 * @author Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0
 * @since HI 1.0
 */

public interface InterpreterObserver extends InputObserver
{
    public void evaluationStarted();
    public void evaluationFinished();
    public void interpreterFailure(Exception e);
}
