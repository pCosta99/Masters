package app.interfaces;

import app.models.Localizacao;

/**
 * ITransportador
 */
public interface ITransportador {

    public boolean aceitoTransporteMedicamentos();

    public void aceitaMedicamentos(boolean state);

    public boolean dentroRaioAccao(Localizacao l);

}
