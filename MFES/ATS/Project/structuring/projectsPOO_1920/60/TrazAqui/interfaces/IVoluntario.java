package interfaces;

public interface IVoluntario extends IEntregas {
    IVoluntario clone();
    void setRating(double rate);
}
