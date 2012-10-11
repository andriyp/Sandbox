package gof.adapter;

public class NamedCar implements HasName
{
    private final Car car;
    
    public NamedCar (Car car)
    {
        this.car = car;
    }

    @Override
    public String getName ()
    {
        return car.mark + " " + car.model;
    }      
}
