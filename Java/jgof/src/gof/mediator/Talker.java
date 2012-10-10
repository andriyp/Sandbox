package gof.mediator;

public class Talker
{
    private final String   name;
    private final Mediator mediator;
    
    public Talker (String name, Mediator mediator)
    {
        this.name     = name;
        this.mediator = mediator;
    }
    
    public void notifyMessage (String msg)
    {
        System.out.println(name + " receives message: " + msg);
    }
    
    public void send (String message)
    {
        mediator.send(this, message);
    }
}
