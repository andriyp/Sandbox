package gof.memento;

public interface Mementizable<Memento>
{
    Memento saveMemento ();
    void    loadMemento (Memento memento);
}
