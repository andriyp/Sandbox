import Control.Monoid;
import Data.Tuple;

public class DDS {
    public final Matrix<Double> mxA, mxB, mxC, mxD, x;
    
    public DDS (Matrix<Double> mxA, Matrix<Double> mxB, Matrix<Double> mxC, Matrix<Double> mxD, Matrix<Double> x) {
        this.mxA = mxA;
        this.mxB = mxB;
        this.mxC = mxC;
        this.mxD = mxD;
        this.x   = x;
    }
    
    public Tuple<Matrix<Double>,DDS> Simulate (Matrix<Double> u) {
        Matrix<Double> y    = mxC.mul(x, Monoid.doubleSum, Monoid.doubleMul.op)
                                 .sum(mxD.mul(u, Monoid.doubleSum, Monoid.doubleMul.op), Monoid.doubleSum.op);
        Matrix<Double> newX = mxA.mul(x, Monoid.doubleSum, Monoid.doubleMul.op)
                                 .sum(mxB.mul(u, Monoid.doubleSum, Monoid.doubleMul.op), Monoid.doubleSum.op);
        DDS newDDS = new DDS(mxA, mxB, mxC, mxD, newX);
        return new Tuple<Matrix<Double>,DDS>(y, newDDS);        
    }
    
    public Matrix<Object> getParametersMatrix () {
        Object[][] mxs = { {mxA, mxB}, {mxC, mxD} };
        return new Matrix<Object>(mxs);
    }
    
    public Tuple<Integer,Integer> getDim () {
        return x.getDim();
    }
}
