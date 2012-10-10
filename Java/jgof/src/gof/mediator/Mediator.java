package gof.mediator;

public class Mediator
{
    public Talker talkerA, talkerB;
    
    public void send (Talker sender, String message)
    {
        if (sender == talkerA)
            talkerB.notifyMessage(message);
        else
            talkerA.notifyMessage(message);
    }
}


