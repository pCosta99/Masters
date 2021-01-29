package com.javamarlon;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Set;
import java.util.stream.Collectors;

public class TrazAqui implements Serializable {

    private HashMap<String, Utilizador> utilizadores;
    private HashMap<String, Voluntario> voluntarios;
    private HashMap<String, Transportadora> transportadoras;
    private HashMap<String, Loja> lojas;

    public TrazAqui() {
        utilizadores = new HashMap<>();
        voluntarios = new HashMap<>();
        transportadoras = new HashMap<>();
        lojas = new HashMap<>();
    }

    public void addUtilizador(Utilizador novoUtilizador) {
        utilizadores.put(novoUtilizador.getEmail(), novoUtilizador);
    }

    public boolean checkLoginUtilizador(String email, String password) {
        if (this.utilizadores.containsKey(email)) {
            Utilizador utilizador = this.utilizadores.get(email);
            return (utilizador.getPassword().equals(password));
        }
        return false;
    }

    public Utilizador getUtilizador(String email) {
        return utilizadores.get(email);
    }

    public void addVoluntario(Voluntario novoVoluntario) {
        voluntarios.put(novoVoluntario.getEmail(), novoVoluntario);
    }

    public boolean checkLoginVoluntario(String email, String password) {
        if (this.voluntarios.containsKey(email)) {
            Voluntario voluntario = this.voluntarios.get(email);
            return (voluntario.getPassword().equals(password));
        }
        return false;
    }

    public Voluntario getVoluntario(String email) {
        return voluntarios.get(email);
    }

    public void addTransportadora(Transportadora novaTransportadora) {
        transportadoras.put(novaTransportadora.getEmail(), novaTransportadora);
    }

    public boolean checkLoginTransportadora(String email, String password) {
        if (this.transportadoras.containsKey(email)) {
            Transportadora transportadora = this.transportadoras.get(email);
            return (transportadora.getPassword().equals(password));
        }
        return false;
    }

    public Transportadora getTransportadora(String email) {
        return transportadoras.get(email);
    }

    public HashMap<String, Transportadora> getTransportadoras() {
        return transportadoras;
    }

    public void addLoja(Loja novaLoja) {
        lojas.put(novaLoja.getEmail(), novaLoja);
    }

    public boolean checkLoginLoja(String email, String password) {
        if (this.lojas.containsKey(email)) {
            Loja loja = this.lojas.get(email);
            return (loja.getPassword().equals(password));
        }
        return false;
    }

    public boolean lojaExiste(String nome) {
        Set<String> keys = this.lojas.keySet();
        for (String key : keys) {
            if (lojas.get(key).getNome().equals(nome)) {
                return true;
            }
        }
        return false;
    }

    public Loja getLoja(String email) {
        return lojas.get(email);
    }

    public HashMap<String, Loja> getLojas() {
        return lojas;
    }

    public boolean emailExiste(String email) {
        return (utilizadores.containsKey(email)
                || voluntarios.containsKey(email)
                || transportadoras.containsKey(email)
                || lojas.containsKey(email));
    }

    public Loja getLojaComNome(String nome) {
        ArrayList<Loja> s = lojas.keySet()
                .stream()
                .map(key -> lojas.get(key))
                .collect(Collectors.toCollection(ArrayList::new));
        for (Loja loja : s) {
            if (loja.getNome().equals(nome)) {
                return loja;
            }
        }
        return null;
    }

    public Loja getLojaComCod(String cod) {
        ArrayList<Loja> s = lojas.keySet()
                .stream()
                .map(key -> lojas.get(key))
                .collect(Collectors.toCollection(ArrayList::new));
        for (Loja loja : s) {
            if (loja.getCod().equals(cod)) {
                return loja;
            }
        }
        return null;
    }

    public ArrayList<Encomenda> encomendasNoRaio(Prestador prestador) {
        ArrayList<Loja> lojasAoAlcance = lojas.keySet()
                .stream()
                .map(key -> lojas.get(key))
                .filter(prestador::aoAlcance)
                .collect(Collectors.toCollection(ArrayList::new));

        ArrayList<Encomenda> encomendasAoAlcance = new ArrayList<>();
        for (Loja loja : lojasAoAlcance) {
            for (Encomenda encomenda : loja.getProcessadas()) {
                if (prestador.aoAlcance(encomenda.getUtilizador())) {
                    encomendasAoAlcance.add(encomenda);
                }
            }
        }
        return encomendasAoAlcance;
    }

    public void iniciarEntrega(Encomenda encomenda) {

    }
}

