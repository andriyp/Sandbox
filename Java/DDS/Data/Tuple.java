package Data;

public class Tuple<A,B> {
	public final A fst;
	public final B snd;
	
	public Tuple (A fst, B snd) {
		this.fst = fst;
		this.snd = snd;
	}
	
    @Override
    public String toString () {
        return "(" + fst.toString() + ", " + snd.toString() + ")";
    }
}
