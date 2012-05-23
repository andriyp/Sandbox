package Control;

import Data.Tuple;

public class Magma<T> {
    public final Arrow<Tuple<T,T>,T> op;
    
    protected Magma (Arrow<Tuple<T,T>,T> op) {
        this.op = op;
    }
    
    public static Magma<Double> doubleMul = new Magma<Double>(
        new Arrow<Tuple<Double,Double>,Double> () {
            public Double apply(Tuple<Double,Double> arg) {
                return arg.fst * arg.snd;
            };
        });
    
    public static Magma<Double> doubleSum = new Magma<Double>(
        new Arrow<Tuple<Double,Double>,Double> () {
            public Double apply(Tuple<Double,Double> arg) {
                return arg.fst + arg.snd;
            };
        });
}
