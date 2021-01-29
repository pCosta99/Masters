import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Cliente extends Utilizador implements Serializable {

    private String  codCliente;
    private List<String> notificacoes;
    private List<Encomenda> encomendas;

    // Construtores
    public Cliente() {
        super();
        this.codCliente = new String();
        this.notificacoes = new ArrayList<>();
        this.encomendas = new ArrayList<>();
    }

    public Cliente(String codUtilizador, String nome, String email, String password, double posX, double posY, List<String> lista, List<Encomenda> encomendas) {
        super(nome, email, password, posX, posY);
        this.codCliente = codUtilizador;
        this.setNotificacoes(lista);
        this.setEncomendas(encomendas);
    }

    public Cliente(Cliente c) {
        super(c);
        this.codCliente = c.getCodCliente();
        this.setNotificacoes(c.getNotificacoes());
        this.setEncomendas(c.getEncomendas());
    }

    public String getCodCliente() {
        return this.codCliente;
    }

    public void setCodCliente(String cod) {
        this.codCliente = cod;
    }

    public List<String> getNotificacoes() {
        List<String> res = new ArrayList<>(this.notificacoes.size());
        this.notificacoes.forEach(n -> res.add(n));
        return res;
    }

    public void setNotificacoes(List<String> notificacoes) {
        this.notificacoes = new ArrayList<>(notificacoes.size());
        notificacoes.forEach(n -> this.notificacoes.add(n));
    }
    
    public List<Encomenda> getEncomendas(){
        List<Encomenda> res = new ArrayList<>(this.encomendas.size());
        this.encomendas.forEach(e -> res.add(e));
        return res;
    }
    
    public void setEncomendas(List<Encomenda> encs) {
        this.encomendas = new ArrayList<>(encs.size());
        encs.forEach(e -> this.encomendas.add(e));
    }
    
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append(super.toString());
        s.append("\n\tIdentificador: Cliente");
        s.append("\n\tCodigo Cliente");
        s.append(codCliente);
        s.append("\n\tNotificacoes:\n");
        s.append(notificacoes.toString());
        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if (!super.equals(obj) || (getClass() != obj.getClass()))
            return false;
        Cliente c = (Cliente) obj;
        return super.equals(c) &&
                this.codCliente.equals(c.getCodCliente()) &&
                this.notificacoes.equals(c.getNotificacoes());
    }

    public Cliente clone() {
        return new Cliente(this);
    }

    

    public void adicionarNotificacao(String codEncomenda) {
        this.notificacoes.add("\nA sua encomenda n." + codEncomenda + " esta pronta para ser levantada.");
    }

    public void removerTodasNotificacoes() {
        this.notificacoes = new ArrayList<>();
    }

    public void adicionaEncomenda(Encomenda e){
        this.encomendas.add(e.clone());
    }
}