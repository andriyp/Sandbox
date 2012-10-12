import gof.memento.*;
import gof.mediator.*;
import gof.adapter.*;
import gof.bridge.*;
import gof.chain_of_responsibility.*;

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
    
    @SuppressWarnings("unused")
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
    
    @SuppressWarnings("unused")
    private static void testChainOfResponsibility ()
    {
        Handler<Integer> handler = new EvenIntHandler("A");
        
        handler.setNext(new EvenIntHandler("B"))
               .setNext(new OddIntHandler("C"));
        
        handler.send(7);
    }
    
    @SuppressWarnings("unused")
    private static void testAdapter ()
    {
        Car car = new Car("Honda", 123);
        NamedCar namedCar = new NamedCar(car);
        out.println(namedCar.getName());
    }
    
    @SuppressWarnings("unused")
    private static void testBridge ()
    {
        Printer lcPrinter = new LowercasePrinter(),
                ucPrinter = new UppercasePrinter();
        
        Message msgA = new RegularMessage("Lol a", lcPrinter),
                msgB = new RegularMessage("Lol b", ucPrinter),
                msgC = new ReversedMessage("Lol c", lcPrinter),
                msgD = new ReversedMessage("Lol d", ucPrinter);
        
        msgA.print();
        msgB.print();
        msgC.print();
        msgD.print();
    }
    
    public static void main (String[] args)
    {
        // testMemento();
        // testMediator();
        // testChainOfResponsibility();
        // testAdapter();
        // testBridge();
    }
}
