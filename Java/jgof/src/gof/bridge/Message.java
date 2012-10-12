package gof.bridge;

public abstract class Message
{
    protected final Printer printer;
    protected final String  message;
    
    protected Message (String message, Printer printer)
    {
        this.message = message;
        this.printer = printer;
    }
    
    public void print ()
    {
        printer.print(message);
    }
}
