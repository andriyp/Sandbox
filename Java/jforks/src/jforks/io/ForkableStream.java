package jforks.io;
import java.io.*;

public class ForkableStream extends InputStream
{
    private final InputStream underlyingStream;
    private final int         bufferChunkCapacity;
        
    private BufferChunk bufferChunk         = null;
    private int         bufferChunkPosition = 0;
    
    public ForkableStream (InputStream underlyingStream, int bufferChunkCapacity)
    {
        this.underlyingStream    = underlyingStream;
        this.bufferChunkCapacity = bufferChunkCapacity;
    }
    
    private ForkableStream (ForkableStream parent)
    {        
        this.underlyingStream    = parent.underlyingStream;
        this.bufferChunkCapacity = parent.bufferChunkCapacity;
        this.bufferChunk         = parent.bufferChunk;
        this.bufferChunkPosition = parent.bufferChunkPosition;
    }
    
    public ForkableStream fork ()
    {
        if (bufferChunk == null)
            bufferChunk = new BufferChunk();
        
        ForkableStream fork = new ForkableStream(this);        
        return fork;
    }
    
    @Override
    public int read () throws IOException
    {
        if (bufferChunk == null)
            return underlyingStream.read();
        
        if (bufferChunkPosition == bufferChunk.size)
        {
            if (bufferChunk.size == bufferChunkCapacity)
            {
                if (bufferChunk.nextChunk == null)
                    bufferChunk.nextChunk = new BufferChunk();
                
                bufferChunk = bufferChunk.nextChunk;
                bufferChunkPosition = 0;
                return read();
            }
            
            int newByte = underlyingStream.read();
            if (newByte == -1) return -1;
            
            bufferChunk.bytes[bufferChunk.size++] = (byte)newByte;
            bufferChunkPosition++;
            return newByte;
        }
        
        return bufferChunk.bytes[bufferChunkPosition++];
    }
    
    private class BufferChunk
    {
        public final byte[] bytes     = new byte[bufferChunkCapacity];
        public BufferChunk  nextChunk = null; 
        public int          size      = 0;
    }
}