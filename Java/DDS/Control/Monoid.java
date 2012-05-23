package Control;

import Data.Tuple;

public class Monoid<T> extends Semigroup<T> {   
    public final T identity;
    
    protected Monoid(Arrow<Tuple<T, T>, T> op, T id) {
        super(op);
        this.identity = id;
    }    
    
    public static Monoid<Double> doubleMul =            
        new Monoid<Double>(Semigroup.doubleMul.op, 1.);
    
    public static Monoid<Double> doubleSum =
        new Monoid<Double>(Semigroup.doubleSum.op, 0.);
}
