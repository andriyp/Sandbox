package gof.bridge;

public class LowercasePrinter implements Printer
{
    @Override
    public void print (String str)
    {
        System.out.println(str.toLowerCase());
    }
}
