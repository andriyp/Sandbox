import Control.Arrow;
import Control.Monoid;
import Data.Tuple;

public class Matrix<T> {
    private final T[][] contents;
    
    public final int rows, columns;    
    
    public Matrix (T[][] contents) {
        this.contents = contents;
        rows    = contents.length;
        columns = contents[0].length;
    }
    
    @SuppressWarnings("unchecked")
    public static <T> Matrix<T> replicate (T el, int rows, int columns) {
        T[][] contents = (T[][]) new Object[rows][columns];
        for (int i = 0; i < rows; ++i)
            for (int j = 0; j < columns; ++j)
                contents[i][j] = el;
        return new Matrix<T>(contents);
    }
    
    public static <T> Matrix<T> singleton (T el) {
        return replicate(el, 1, 1);
    }
    
    @SuppressWarnings("unchecked")
    public Matrix<T> sum (Matrix<T> mx, Arrow<Tuple<T,T>,T> sum) {
        T[][] newContents = (T[][]) new Object[rows][columns];        
        for (int i = 0; i < rows; ++i)
            for (int j = 0; j < mx.columns; ++j) {
                newContents[i][j] = sum.apply(
                    new Tuple<T,T>(contents[i][j], mx.contents[i][j])
                );
            }
        return new Matrix<T>(newContents);
    }
    
    @SuppressWarnings("unchecked")
    public Matrix<T> mul (Matrix<T> mx, Monoid<T> sumMonoid, Arrow<Tuple<T,T>,T> mul) {
        T[][] newContents = (T[][]) new Object[rows][mx.columns];        
        for (int i = 0; i < rows; ++i)
            for (int j = 0; j < mx.columns; ++j) {
                T x = sumMonoid.identity;
                for (int k = 0; k < columns; ++k)
                    x = sumMonoid.op.apply(new Tuple<T,T>(x,
                            mul.apply(new Tuple<T,T>(contents[i][k], mx.contents[k][j]))
                        ));
                newContents[i][j] = x;
            }
        return new Matrix<T>(newContents);
    }
    
    public Tuple<Integer,Integer> getDim () {
        return new Tuple<Integer,Integer>(rows, columns);
    }
    
    @Override
    public String toString () {
        StringBuffer result = new StringBuffer();
        
        result.append("{ ");
        for (int i = 0; i < rows; ++i) {
            result.append("{");
            for (int j = 0; j < columns; ++j) {
                result.append(" ");
                result.append(contents[i][j]);                
            }
            result.append(" } ");
        }
        result.append("}");
            
        return result.toString();
    }
}
