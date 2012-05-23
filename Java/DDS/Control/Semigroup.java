package Control;

import Data.Tuple;

public class Semigroup<T> extends Magma<T> {
    protected Semigroup(Arrow<Tuple<T, T>, T> op) {
        super(op);
    }
    
    public static Semigroup<Double> doubleMul =
        new Semigroup<Double>(Magma.doubleMul.op);
    
    public static Semigroup<Double> doubleSum =
        new Semigroup<Double>(Magma.doubleSum.op);
}
