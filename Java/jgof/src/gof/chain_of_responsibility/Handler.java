package gof.chain_of_responsibility;

public abstract class Handler<T>
{
    protected String name;
    protected Handler<T> next;
    
    public Handler<T> setNext (Handler<T> next)
    {
        this.next = next;
        return next;
    }
    
    protected abstract boolean canHandle (T request);
    
    protected abstract void handle (T request);
    
    public void send (T request)    
    {
        if (canHandle(request)) 
        {
            handle(request);
            return;
        }
        
        System.out.println("Cannot handle " + request.toString() + " in " + name);
        
        if (next != null)
        {            
            System.out.println("Sending " + request.toString() + " to next handler!");
            next.send(request);
        }
        
        else
            throw new RuntimeException(
                "Error: no handlers left! Unhandled request!"
            );
    }
}
