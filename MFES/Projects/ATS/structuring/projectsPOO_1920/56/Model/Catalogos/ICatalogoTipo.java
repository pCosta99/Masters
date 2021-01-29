package Model.Catalogos;

import Model.Tipos.ITipo;

import java.util.Set;

public interface ICatalogoTipo {
    Set<ITipo> getCatalogo();
    void addTipo (ITipo tipo);
    boolean existsTipo(ITipo tipo);
    boolean existsID(String id);
    ITipo getTipo(String id);
}
