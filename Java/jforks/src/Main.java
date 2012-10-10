import java.io.*;
import jforks.io.*;

public class Main
{
    public static void main (String[] args) throws IOException, UnsupportedEncodingException
    {
        ForkableStream fs = new ForkableStream(new ByteArrayInputStream("abcd".getBytes("UTF-16BE")) , 10);
        
        ForkableStream fsA = fs.fork();
        
        DataInputStream ds = new DataInputStream(fs);
        
        System.out.println(ds.readChar()); // a
        
        ForkableStream fsB = fs.fork(),
                       fsC = fs.fork();
        
        System.out.println(ds.readChar()); // b
        System.out.println(ds.readChar()); // c
        System.out.println(ds.readChar()); // d
        
        DataInputStream dsB = new DataInputStream(fsB);
        
        System.out.println(dsB.readChar()); // b
        System.out.println(dsB.readChar()); // c
        
        DataInputStream dsC = new DataInputStream(fsC);
        
        System.out.println(dsC.readChar()); // b
        
        DataInputStream dsA = new DataInputStream(fsA);
        
        System.out.println(dsA.readChar()); // a        
    }
}
