package gof.memento;

public class Counter implements Mementizable<Counter.Memento>
{
    private int value;
    
    public Counter (int initialValue)
    {
        value = initialValue;
    }
    
    public int inc ()
    {
        return ++value;
    }
    
    public int get ()    
    {
        return value;
    }
     
    //
    // MEMENTO
    //    
    public class Memento
    {
        private final int state;
        
        private Memento ()
        {
            this.state = value;
        }
    }
    
    public Memento saveMemento ()
    {
        return new Memento();
    }
    
    public void loadMemento (Memento memento)
    {
        value = memento.state;   
    }   
}
