package app.interfaces;

import app.models.Localizacao;
import java.io.Serializable;
import app.exceptions.PasswordErradaException;

public interface IInfo extends Serializable {

    public String getEmail();

    public void setPassword(String password);

    public Localizacao getLocalizacao();

    public void setLocalizacao(Localizacao localizacao);

    public void setEmail(String email);

    public String getNome();

    public void setNome(String nome);

    public boolean passOK(String pass) throws PasswordErradaException;

    public IInfo clone();

}
