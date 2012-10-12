package gof.bridge;

public class ReversedMessage extends Message
{  
    private static String reverse (String str)
    {
        char[] chars = str.toCharArray();
        
        int l = 0, r = chars.length - 1;
        
        while (l < r)
        {
            char tmp = chars[l];
            chars[l] = chars[r];
            chars[r] = tmp;
            
            l++; r--;
        }
        
        return new String(chars);
    }
    
    public ReversedMessage (String message, Printer printer)
    {
        super(reverse(message), printer);
    }
}
