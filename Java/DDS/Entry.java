
public class Entry {
    public static void main (String args[]) {
        Double[][] xsA = { {1., 2.}, {3., 0.} },
                   xsB = { {7., 7.}, {7., 7.} },
                   xsC = { {4., 3.}, {2., 1.} },
                   xsD = { {1., 1.}, {2., 2.} },
                   xsX = { {0.}, {0.} },
                   xsU = { {2.}, {3.} };
        
        Matrix<Double> mxA = new Matrix<>(xsA),
                       mxB = new Matrix<>(xsB),
                       mxC = new Matrix<>(xsC),
                       mxD = new Matrix<>(xsD),
                       mxX = new Matrix<>(xsX),
                       mxU = new Matrix<>(xsU);
        
        DDS dds = new DDS(mxA, mxB, mxC, mxD, mxX);
        
        System.out.println(
            dds.Simulate(mxU).snd
               .Simulate(mxU).fst                    
        );
    }
}
