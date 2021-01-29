package Model.Catalogos;

import Model.Tipos.ITipo;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

public class CatalogoTipo implements ICatalogoTipo, Serializable {
    private Set<ITipo> catalogo;

    public CatalogoTipo(){
        this.catalogo = new HashSet<>();
    }

    public Set<ITipo> getCatalogo(){
        return this.catalogo;
    }

    public void addTipo (ITipo tipo){
        this.catalogo.add(tipo);
    }

    public boolean existsTipo(ITipo tipo){
        return this.catalogo.contains(tipo);
    }

    public boolean existsID(String id){
        for(ITipo tipo : catalogo){
            if(tipo.getId().equals(id)) return  true;
        }
        return false;
    }

    public ITipo getTipo(String id){
        for(ITipo tipo : catalogo){
            if(tipo.getId().equals(id)) return tipo;
        }
        return null;
    }

    public String toString() {
        return "Catalogo: " +
                catalogo;
    }
}
