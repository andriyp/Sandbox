package gof.chain_of_responsibility;

public class OddIntHandler extends Handler<Integer>
{
    public OddIntHandler (String name)
    {
        this.name = name;
    }
    
    @Override
    protected boolean canHandle (Integer x)
    {
        return x % 2 != 0;
    }

    @Override
    protected void handle (Integer x)
    {
        System.out.println("Got odd number: " + x.toString());
    }
    
}
