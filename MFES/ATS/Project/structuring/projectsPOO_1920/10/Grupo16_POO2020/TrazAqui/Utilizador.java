package TrazAqui;


import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
/**
 * Classe que representa um Utilizador TrazAqui!
 */
public class Utilizador extends Login implements Serializable {
    /**
     * ArraysList que guarda todas as encomendas feitas por um User.
     */
    private ArrayList<Encomenda> historico;
    /**
     *HashMap que associa o código de uma Empresa a uma Encomenda que a mesma propõe transportar.
     */
    private HashMap<String, Encomenda> entregas;
    /**
     * Construtor vazio do objeto Utilizador
     */
    public Utilizador() {
        super();
        historico = new ArrayList<>();
        entregas = new HashMap<>();
    }
    /**
     * Construtor parametrizado de um objeto Utilizador
     * @param codUser String que representa o codigo de um objeto Utilizador
     * @param username String que representa o username do Utilizador
     * @param password String que representa a password do Utilizador
     * @param ponto Ponto que representa a localização do Utilizador
     */
    public Utilizador(String codUser, String username, String password, Ponto ponto) {
        super(codUser, username, password, ponto);
        this.historico = new ArrayList<>();
        this.entregas = new HashMap<>();
    }

    /**
     * Construtor de cópia de Utilizador
     * @param u Utilizador a copiar
     */
    public Utilizador(Utilizador u) {
        super(u);
        this.historico = u.getHistorico();
        this.entregas = u.getEntregas();
    }

    /**
     * Método para obter o histórico de Encomendas sendo este um ArrayList.
     * @return lista total de encomendas de um Utilizador
     */
    public ArrayList<Encomenda> getHistorico() {
        return this.historico.stream().collect(Collectors.toCollection(ArrayList::new));
    }

    /**
     * Método para obter o histórico de Encomendas filtrado por data.
     * @param init data inicial
     * @param max data final
     * @return lista de Encomendas
     */
    public ArrayList<Encomenda> getHistorico(LocalDateTime init, LocalDateTime max){
        return this.historico.stream().filter(enc -> enc.getTempo().compareTo(init) >= 0 && enc.getTempo().compareTo(max) <= 0).collect(Collectors.toCollection(ArrayList::new));
    }
    /**
     * Método para obter as entregas, sendo este um HashMap
     * @return HashMap entregas
     */
    public HashMap<String, Encomenda> getEntregas() {
        HashMap<String, Encomenda> aux = new HashMap<>();
        for (String ent : entregas.keySet()) {
            aux.put(ent, entregas.get(ent));
        }
        return aux;
    }
    /**
     * Método clone
     */
    @Override
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Método para verificar se um certo objeto é igual a Utilizador
     * @param o Objeto a comparar
     * @return true caso sejam iguais false caso contrário
     */
    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Utilizador that = (Utilizador) o;
        return Objects.equals(historico, that.historico) &&
                Objects.equals(entregas, that.entregas);
    }
    /**
     * Método que cria uma String de um objeto em concreto (Neste caso de Utilizador)
     */
    public String toString() {
        return "UTILIZADOR\n" + super.toString() + "\n";
    }
    /**
     * Método que retorna o tamanha do historico deste utilizador
     * @return tamanho int
     */
    public int getSize() {
        return this.historico.size();
    }
    /**
     * Método que vai adicionar uma encomenda ao ArrayList historico deste utilizador
     * @param enc Encomenda
     */
    public void addEncomenda(Encomenda enc) {
        historico.add(enc);
    }
    /**
     * Método que vai adicionar uma encomenda ao HashMap entregas deste utilizador
     * @param e Encomenda
     * @param emp String
     */
    public void addEntrega(Encomenda e, String emp) {
        entregas.put(emp, e);
    }
    /**
     * Método que remove uma encomenda do HashMap entregas deste utilizador
     * @param emp String
     */
    public void removeEntrega(String emp) {
        entregas.remove(emp);
    }



}
