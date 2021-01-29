
/**
 * Escreva a descrição da classe ClienteViagem aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class  ClienteViagem{
    
    private Cliente cliente;
    private int nrViagens;
    
    public ClienteViagem(){
        this.cliente = new Cliente();
        this.nrViagens =0;   
    }
    
    public ClienteViagem( Cliente cliente, int nrViagens){
        this.setCliente(cliente);
        this.nrViagens=nrViagens;
    }
    
    public ClienteViagem(ClienteViagem cv){
        this.setCliente(cv.getCliente());
        this.nrViagens = cv.getNrViagens();
    }
    
    public Cliente getCliente(){
        return this.cliente.clone();
    }
    
    public int getNrViagens(){
        return this.nrViagens;
    }
    
    public void setCliente(Cliente cliente){
        this.cliente = new Cliente(cliente);
    }
    
     public ClienteViagem clone() {
        return new ClienteViagem(this);
    }
    public String toString() {
        StringBuilder s = new StringBuilder();
        s.append("\n\tCliente ");
        s.append(this.cliente.toString());
        s.append("\n\tNumero Viagens");
        s.append(this.nrViagens);
        return s.toString();
    }

    public boolean equals(Object obj) {

        if (this == obj)
            return true;
        if (!super.equals(obj) || (getClass() != obj.getClass()))
            return false;
        ClienteViagem c = (ClienteViagem) obj;
        return this.cliente.equals(c.getCliente()) &&
                this.nrViagens==c.getNrViagens();
    }
}
