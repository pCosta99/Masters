package app.controllers;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import app.enums.TiposUtilizadoresEnum;
import app.exceptions.UtilizadorJaExistenteException;
import app.exceptions.UtilizadorNaoExistenteException;
import app.interfaces.IInfo;
import app.models.EmpresaTransportadora;
import app.models.Info;
import app.models.Loja;
import app.models.Utilizador;
import app.models.Voluntario;

public class RegistosUtilizador implements Serializable {

    /**
    *
    */
    private static final long serialVersionUID = -588677261471695913L;
    // #region variables
    private Map<String, IInfo> listaRegistos;
    private static String errorMessageUtilizadorNaoExistente =
            "Email de Utilizador não existente !!\n";
    private static String errorMessageUtilizadorJaExistente =
            "Email de Utilizador já existente !!\n";
    // #endregion

    // #region Construtores

    public RegistosUtilizador() {
        this.listaRegistos = new HashMap<>();
    }

    /**
     * @param listaRegistos
     */
    public RegistosUtilizador(Map<String, IInfo> listaRegistos) {
        this.setListaRegistos(listaRegistos);
    }

    /**
     * @param r
     */
    public RegistosUtilizador(RegistosUtilizador r) {
        this.setListaRegistos(r.listaRegistos);
    }

    // #endregion

    // #region getters setter

    /**
     * @return the listaRegistos
     */
    public Map<String, IInfo> getListaRegistos() {
        return this.listaRegistos.entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, a -> a.getValue().clone()));
    }

    /**
     * @param listaRegistos the listaRegistos to set
     */
    public void setListaRegistos(Map<String, IInfo> listaRegistos) {
        this.listaRegistos = listaRegistos.entrySet().stream()
                .collect(Collectors.toMap(Entry::getKey, a -> a.getValue().clone()));
    }

    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        RegistosUtilizador registos = (RegistosUtilizador) o;
        // field comparison
        return Objects.equals(this.listaRegistos, registos.listaRegistos);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizadores: ");
        sb.append('\n');
        for (IInfo i : this.listaRegistos.values()) {
            sb.append("Utilizador: ");
            sb.append('\n');
            sb.append(i.toString());
        }
        sb.append('\n');

        return sb.toString();
    }

    public RegistosUtilizador clone() {
        return new RegistosUtilizador(this);
    }

    // #endregion

    // #region Methods

    public boolean existeUtilizador(IInfo i) {

        return this.listaRegistos.containsValue(i);

    }

    public boolean existeUtilizador(String email) {

        return this.listaRegistos.containsKey(email);

    }

    public void adicionaUtilizador(IInfo i) throws UtilizadorJaExistenteException {
        if (this.existeUtilizador(i.getEmail())) {
            throw new UtilizadorJaExistenteException(errorMessageUtilizadorJaExistente);
        } else {
            this.listaRegistos.put(i.getEmail(), i.clone());
        }
    }

    public IInfo getUtilizador(String email) throws UtilizadorNaoExistenteException {
        if (!this.existeUtilizador(email)) {
            throw new UtilizadorNaoExistenteException(errorMessageUtilizadorNaoExistente);
        }
        return this.listaRegistos.get(email).clone();
    }

    public void apagaUtilizador(String email) throws UtilizadorNaoExistenteException {

        if (!this.existeUtilizador(email)) {
            throw new UtilizadorNaoExistenteException(errorMessageUtilizadorNaoExistente);
        } else
            this.listaRegistos.remove(email);
    }

    public void apagaUtilizador(IInfo i) throws UtilizadorNaoExistenteException {

        if (!this.existeUtilizador(i)) {
            throw new UtilizadorNaoExistenteException(errorMessageUtilizadorNaoExistente);
        }

        this.apagaUtilizador(i.getEmail());
    }

    public void atualizaUtilizador(String email, IInfo i) throws UtilizadorNaoExistenteException {

        if (!existeUtilizador(email)) {
            throw new UtilizadorNaoExistenteException(errorMessageUtilizadorNaoExistente);
        } else {
            this.listaRegistos.put(email, i.clone());
        }
    }


    // Conta utilizadores

    public long contaUtilizadores(TiposUtilizadoresEnum t) {
        long count = 0;
        if (t == TiposUtilizadoresEnum.UTILIZADOR) {
            count = this.listaRegistos.values().stream().filter(u -> u instanceof Utilizador)
                    .count();
        } else if (t == TiposUtilizadoresEnum.LOJA) {
            count = this.listaRegistos.values().stream().filter(u -> u instanceof Loja).count();

        } else if (t == TiposUtilizadoresEnum.VOLUNTARIO) {
            count = this.listaRegistos.values().stream().filter(u -> u instanceof Voluntario)
                    .count();

        } else if (t == TiposUtilizadoresEnum.EMPRESA) {
            count = this.listaRegistos.values().stream()
                    .filter(u -> u instanceof EmpresaTransportadora).count();

        } else if (t == TiposUtilizadoresEnum.TODOS) {
            count = this.listaRegistos.values().stream().filter(u -> u instanceof Info).count();

        }
        return count;
    }


    public List<IInfo> listaUtilizadores(TiposUtilizadoresEnum t) {
        List<IInfo> lista = null;
        if (t == TiposUtilizadoresEnum.UTILIZADOR) {
            lista = this.listaRegistos.values().stream().filter(u -> u instanceof Utilizador)
                    .map(IInfo::clone).collect(Collectors.toList());
        } else if (t == TiposUtilizadoresEnum.LOJA) {
            lista = this.listaRegistos.values().stream().filter(u -> u instanceof Loja)
                    .map(IInfo::clone).collect(Collectors.toList());

        } else if (t == TiposUtilizadoresEnum.VOLUNTARIO) {
            lista = this.listaRegistos.values().stream().filter(u -> u instanceof Voluntario)
                    .map(IInfo::clone).collect(Collectors.toList());
        } else if (t == TiposUtilizadoresEnum.EMPRESA) {
            lista = this.listaRegistos.values().stream()
                    .filter(u -> u instanceof EmpresaTransportadora).map(IInfo::clone)
                    .collect(Collectors.toList());

        } else if (t == TiposUtilizadoresEnum.TODOS) {
            lista = this.listaRegistos.values().stream().filter(u -> u instanceof Info)
                    .map(IInfo::clone).collect(Collectors.toList());

        }
        return lista;
    }

    // #endregion

}
