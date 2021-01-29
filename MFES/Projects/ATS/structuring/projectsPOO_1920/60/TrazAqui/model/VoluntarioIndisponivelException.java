package model;

public class VoluntarioIndisponivelException extends Exception {
    public VoluntarioIndisponivelException(String voluntário_indisponível) {
        super(voluntário_indisponível);
    }
}
