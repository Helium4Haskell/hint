package hint.interpreter;


import java.util.*;

import hint.util.*;


/**
 * A broadcaster will broadcast all observer-input that is send to it, to
 * the observers that are registered to it. The actual broadcast is done in
 * another thread.
 *
 * @author  Arie Middelkoop <amiddelk@cs.uu.nl>
 * @version 1.0 (5 feb 2003)
 * @since   HI 1.0
 */

public class MessageBroadcaster implements InterpreterObserver
{
    private Set          observers;
    private MessageQueue messageQueue;


    public MessageBroadcaster()
    {
        observers    = new HashSet();
        messageQueue = null;
    }


    public synchronized void enableBroadcasting()
    {
        disableBroadcasting();

        messageQueue = new MessageQueue();

        new Thread(new Worker(messageQueue)).start();
    }


    public synchronized void disableBroadcasting()
    {
        if (messageQueue != null)
        {
            messageQueue.setFinished();
            messageQueue = null;
        }
    }


    public synchronized void addObserver(InterpreterObserver observer)
    {
        observers.add(observer);
    }


    public synchronized void removeObserver(InterpreterObserver observer)
    {
        observers.remove(observer);
    }


    public synchronized void removeObservers()
    {
        observers.clear();
    }


    protected synchronized void transmitMessage(Message message)
    {
        Iterator iterator = observers.iterator();
        while(iterator.hasNext())
        {
            InterpreterObserver observer = (InterpreterObserver) iterator.next();
            switch(message.target)
            {
                case Message.TARGET_INPUTRECIEVED:
                    observer.inputRecieved((String) message.message);
                    break;

                case Message.TARGET_ERRORRECIEVED:
                    observer.errorRecieved((String) message.message);
                    break;

                case Message.TARGET_EVALUATIONSTARTED:
                    observer.evaluationStarted();
                    break;

                case Message.TARGET_EVALUATIONFINISHED:
                    observer.evaluationFinished();
                    break;

                case Message.TARGET_INTERPRETERFAILURE:
                    observer.interpreterFailure((Exception) message.message);
                    break;
            }
        }
    }


    public void inputRecieved(String data)
    {
        messageQueue.enqueue(new Message(Message.TARGET_INPUTRECIEVED, data));
    }


    public void errorRecieved(String data)
    {
        messageQueue.enqueue(new Message(Message.TARGET_ERRORRECIEVED, data));
    }


    public void evaluationFinished()
    {
        messageQueue.enqueue(new Message(Message.TARGET_EVALUATIONFINISHED, null));
    }


    public void interpreterFailure(Exception e)
    {
        messageQueue.enqueue(new Message(Message.TARGET_INTERPRETERFAILURE, e));
    }


    public void evaluationStarted()
    {
        messageQueue.enqueue(new Message(Message.TARGET_EVALUATIONSTARTED, null));
    }


    protected class Worker implements Runnable
    {
        private MessageQueue queue;


        public Worker(MessageQueue queue)
        {
            this.queue = queue;
        }


        public void run()
        {
            Message message;
            while((message = queue.dequeue()) != null)
                transmitMessage(message);
        }
    }


    static protected class Message
    {
        public Object message;
        public int    target;

        public static final int TARGET_INPUTRECIEVED      = 10;
        public static final int TARGET_ERRORRECIEVED      = 20;
        public static final int TARGET_EVALUATIONFINISHED = 30;
        public static final int TARGET_INTERPRETERFAILURE = 40;
        public static final int TARGET_EVALUATIONSTARTED  = 50;


        public Message(int target, Object message)
        {
            this.target  = target;
            this.message = message;
        }
    }


    static protected class MessageQueue
    {
        private LinkedList queue;
        private boolean    isFinished;


        public MessageQueue()
        {
            queue      = new LinkedList();
            isFinished = false;
        }


        public synchronized void enqueue(Message message)
        {
            queue.addLast(message);
            notify();
        }


        public synchronized boolean isEmpty()
        {
            return queue.isEmpty();
        }


        public synchronized Message dequeue()
        {
            while(isEmpty() && !isFinished)
            {
                try { wait(); }
                catch(InterruptedException e) { Log.getLogger().throwing(getClass().getName(), "dequeue", e); }
            }

            if (isEmpty())
                return null;
            else
                return (Message) queue.removeFirst();
        }


        public synchronized void setFinished()
        {
            isFinished = true;
            notifyAll();
        }
    }
}
