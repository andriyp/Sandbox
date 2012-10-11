package gof.chain_of_responsibility;

public class EvenIntHandler extends Handler<Integer>
{
    public EvenIntHandler (String name)
    {
        this.name = name;
    }
    
    @Override
    protected boolean canHandle (Integer x)
    {
        return x % 2 == 0;            
    }

    @Override
    protected void handle (Integer x)
    {
        System.out.println("Got even number: " + x.toString());
    }
}
