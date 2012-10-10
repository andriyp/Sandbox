import gof.memento.*;
import gof.mediator.*;

import static java.lang.System.*;

public class Main
{
    @SuppressWarnings("unused")
    private static void testMemento ()
    {
        Counter cnt = new Counter(0);
        
        out.println(cnt.inc()); // 1
        
        Counter.Memento mem = cnt.saveMemento();
        
        out.println(cnt.inc()); // 2
        out.println(cnt.inc()); // 3
        
        cnt.loadMemento(mem);
        
        out.println(cnt.inc()); // 2
        out.println(cnt.inc()); // 3
    }
    
    private static void testMediator ()
    {
        Mediator mediator = new Mediator();
        
        Talker talkerA = new Talker("A", mediator),
               talkerB = new Talker("B", mediator);

        mediator.talkerA = talkerA;
        mediator.talkerB = talkerB;
        
        talkerA.send("Hello!");
        talkerB.send("Hi there!");
    }
    
    public static void main (String[] args)
    {
        testMediator();
    }
}
