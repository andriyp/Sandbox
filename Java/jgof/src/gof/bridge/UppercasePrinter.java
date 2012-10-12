package gof.bridge;

public class UppercasePrinter implements Printer
{
    @Override
    public void print (String str)
    {
        System.out.println(str.toUpperCase());
    }
}
