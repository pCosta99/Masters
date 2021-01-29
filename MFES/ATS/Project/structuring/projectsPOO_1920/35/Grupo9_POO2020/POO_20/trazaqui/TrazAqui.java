import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;
import java.util.Collections;
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.*;

public class TrazAqui implements Serializable{
    private Map<String,Utilizador> utilizadores; //todos os utilizadores da aplicaçao
    private Map<String,Encomenda> encomendas; //encomendas da aplicacao (entregues e nao)
    private Utilizador utilizador; //utilizador registado no momento
    
    public TrazAqui(){
        this.utilizadores = new TreeMap<>();
        this.encomendas = new TreeMap<>();
        this.utilizador = null;
    }

    public TrazAqui(Map<String,Utilizador> utilizadores, Utilizador utilizador){
        this.utilizadores = new TreeMap<String,Utilizador>(utilizadores);
        this.encomendas = new TreeMap<String,Encomenda>(encomendas);
        this.utilizador = utilizador;
    }
    
    public TrazAqui(TrazAqui u){
        this.utilizadores = u.getUtilizadores();
        this.encomendas = u.getEncomendas();
        this.utilizador = u.getUtilizador();
    }
    
    public Map<String,Utilizador> getUtilizadores (){
        return this.utilizadores.entrySet()
                                .stream()
                                .collect(Collectors.toMap(e->e.getKey(),e->e.getValue().clone()));
    }
    
    public Map<String,Encomenda> getEncomendas (){
        return this.encomendas.entrySet()
                                .stream()
                                .collect(Collectors.toMap(e->e.getKey(),e->e.getValue().clone()));
    }
    public Utilizador getUtilizador(){
        return this.utilizador;
    }
  
    public int getTipoUtilizador (){
        if (this.utilizador instanceof Cliente) return 1;
        if (this.utilizador instanceof Loja) return 2;
        if (this.utilizador instanceof Voluntario) return 3;
        else if (this.utilizador instanceof EmpresaTransp) return 4;
    
        return 0;
    }
    
    /**---------------------Métodos---------------------**/

    /**Funcão que inicializa a App**/
    public static TrazAqui initApp () throws IOException, ClassNotFoundException{
        ObjectInputStream ois = new ObjectInputStream(new FileInputStream("trazaqui.data"));
        TrazAqui u = (TrazAqui) ois.readObject();
    
        ois.close();
        return u;
    }
    
    /**Função que guarda os dados num ficheiro "trazaqui.data"**/
    public void gravar() throws IOException{
        ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("trazaqui.data"));
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    /**Função que regista um Utilizador**/
    public void registaUtilizador(Utilizador utilizador) throws UtilizadorExistenteException{
        String email = utilizador.getEmail();
        
        if (utilizadores.containsKey(email)) 
            throw new UtilizadorExistenteException("Utilizador já existe!");
          
        utilizadores.put(email,utilizador.clone());
    }
  
    /**Função que valida as credenciais de início de sessão (EMAIL E PASSWORD)**/
    public void iniciarSessao(String email, String password) throws SemAutorizacaoException{
        Utilizador u = this.utilizadores.get(email);
                                    
        if (u == null)
            throw new SemAutorizacaoException("Utilizador nao existe!");
        if (!(u.getPassword().equals(password)))
            throw new SemAutorizacaoException("Password incorreta!");
    
        this.utilizador = this.utilizadores.get(email);
    } 

    /** Função que fecha a sessão atualmente iniciada*/
    public void fechaSessao () {
        this.utilizador = null;
    }
    
    /**Função de classificação de um Transportador**/
    public void classificarTransportador(Avaliacao avaliacao, String email) throws SemAutorizacaoException,
    TransportadorInexistenteException{
        if(getTipoUtilizador() != 1) 
            throw new SemAutorizacaoException("Sem autorização !\n");
        if(this.utilizadores.get(email) == null) 
            throw new TransportadorInexistenteException("Transportador Inexistente !\n");
        
        Transportador t = (Transportador) this.utilizadores.get(email);
        t.addAvaliacao(avaliacao);
    }
    
    /**Função que apresenta uma lista com as encomendas efetuadas pelo cliente, entre datas**/
    public List<Encomenda> encomendasCliente(Cliente c, LocalDate x, LocalDate y){
        return c.getHist()
                .stream()
                .filter(v -> (v.getData().isAfter(x) && v.getData().isBefore(y)) || v.getData().equals(x) || v.getData().equals(y))
                .collect(Collectors.toList());
    }
    
    /**Funcao que calcula o total facturado por uma empresa transportadora num determinado período **/
    public double totalFaturado(String id, LocalDate x, LocalDate y) throws SemAutorizacaoException{
        if(getTipoUtilizador() != 4) 
            throw new SemAutorizacaoException("Sem Autorização !\n");
        
        EmpresaTransp e= null;
        
        for( Map.Entry<String,Utilizador> aux: utilizadores.entrySet()){
            if(aux.getKey().equals(id)) e = (EmpresaTransp)aux.getValue();
        }
        
        List<Encomenda> encomendas = e.getHistorico();
        
        double total =0.0;
        for(Encomenda enc : encomendas){
            if((enc.getData().isAfter(x) && enc.getData().isBefore(y)) || enc.getData().equals(x) || enc.getData().equals(y))
            total=+ enc.valorEncomenda();
        }
        return total;
    }
    
    /**Funcao que devolve listagens dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas)**/
    public List<Cliente> top10C(){
        
         List<Cliente> aux = new ArrayList<Cliente>();
         
          for(Utilizador u : this.utilizadores.values()){
            if(u.getClass().getSimpleName().equals("Cliente")) aux.add(((Cliente)u).clone());
        }
        
        List<Cliente> top10 = aux.stream().collect(Collectors.toList());
        Collections.sort(top10, new ComparadorUtilizacao());
        Collections.reverse(top10);
        top10 = top10.stream().limit(10).collect(Collectors.toList());
        return top10;
         
    }
    
    /**Funcao que devolve listagens das 10 empresas transportadoras que mais utilizam o sistema (em número de kms percorridos)**/
    public List<EmpresaTransp> top10ET(){
         List<EmpresaTransp> aux = new ArrayList<EmpresaTransp>();
         
          for(Utilizador u : this.utilizadores.values()){
            if(u.getClass().getSimpleName().equals("EmpresaTransp")) aux.add(((EmpresaTransp)u).clone());
        }
        
        List<EmpresaTransp> top10 = aux.stream().collect(Collectors.toList());
        Collections.sort(top10, new ComparadorDistancia());
        Collections.reverse(top10);
        top10 = top10.stream().limit(10).collect(Collectors.toList());
        return top10;
        
    }
    
    public TrazAqui clone (){
        return new TrazAqui(this);
    }

}