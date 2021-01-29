import java.util.HashMap;
import java.util.*;
import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;


public class GestaoEncomenda implements Serializable
{
    // vari√°veis de inst√¢ncia - substitua o exemplo abaixo pelo seu pr√≥prio
    private HashMap<String,Encomenda> encomendas;


    public GestaoEncomenda()
    {
       this.encomendas=new HashMap();
    }

   public GestaoEncomenda(HashMap<String,Encomenda> es){
    this.encomendas=new HashMap();
    for(Encomenda a:es.values())
        this.encomendas.put(a.getId(),a.clone());
    }
    
    public GestaoEncomenda(GestaoEncomenda ges){
        this.encomendas=ges.getEncomenda();
    }
    
    //get
    
    public HashMap<String,Encomenda> getEncomenda() {
        HashMap<String,Encomenda> aux = new HashMap<>();
        for(Encomenda a: this.encomendas.values())
            aux.put(a.getId(),a.clone());
        return aux;
    }

    public void setCliente(HashMap<String,Encomenda> es) {
        this.encomendas = new HashMap<>();
        for(Encomenda a : es.values())
            this.encomendas.put(a.getId(),a.clone());
    }
    
    
     public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        GestaoEncomenda aux = (GestaoEncomenda) object;
        return aux.getEncomenda().equals(this.getEncomenda());
    }
    
      public GestaoEncomenda clone(){
        return new GestaoEncomenda(this);
    }

    public String toString(){
        return "As encomendas sao: \n" + this.getEncomenda();
    }
    
    
    //adiciona encomenda
    
    public void addEncomenda(Encomenda a){
        
        this.encomendas.put(a.getId(),a.clone());
    }
    

    // busca encomenda com respetivo id
     public Encomenda buscaEncomenda(String id){
        return this.encomendas.get(id).clone();
    }
    
    //adiciona uma encomenda da instancia pronta
     public void addPronta(Pronta p) {
        this.encomendas.put(p.getId(), p.clone());// ver o clone por causa das datas
    }
    
    //adiciona uma encomenda da instancia realizadaVoluntario
    public void addRealizadaVoluntario(RealizadaVoluntario r) {
        this.encomendas.put(r.getId(), r.clone()); // ver o clone por causa das datas
    }
    
    //adiciona uma encomenda da instancia realizadaEmpresa
    public void addRealizadaEmpresa(RealizadaEmpresa r) {
        this.encomendas.put(r.getId(), r.clone());// ver o clone por causa das datas
    }
    
    // metodo para cliente classificar o serviÁo da encomenda entregue por uma empresa com o id fornecido  
    public void classificacaoClienteEmpresa(String id, double p) {
        ((RealizadaEmpresa) (this.encomendas.get(id))).clienteClassifica(p);
    }
    
    // metodo para cliente classificar o serviÁo da encomenda entregue por um voluntario com o id fornecido
    public void classificacaoClienteVoluntario(String id, double p) {
        ((RealizadaVoluntario) (this.encomendas.get(id))).clienteClassifica(p);
    }
    
    // listagem de todas as entregues realizadas por empresas
    public List<RealizadaEmpresa> getRealizadaEmpresa() {
        List<Encomenda> list = new ArrayList<>(this.encomendas.values());
        List<RealizadaEmpresa> r = new ArrayList<>();

        for (Encomenda e : list)
            if (e instanceof RealizadaEmpresa) 
                r.add((RealizadaEmpresa) e);
              
    return r;
   }
    
    // listagem de todas as entregues realizadas por voluntarios
     public List<RealizadaVoluntario> getRealizadaVoluntario() {
        List<Encomenda> list = new ArrayList<>(this.encomendas.values());
        List<RealizadaVoluntario> r = new ArrayList<>();

        for (Encomenda e : list)
            if (e instanceof RealizadaVoluntario) {
                r.add((RealizadaVoluntario) e);
                  //System.out.println(e.getData());
            }
    return r;
   }
    
   
   
   // listagem de entregues realizadas por empresas a um cliente que faltam classificar
    public List<RealizadaEmpresa> realizadosClassificarClienteEmpresa(Cliente c) {
        List<RealizadaEmpresa> result = new ArrayList<>(this.getRealizadaEmpresa());
        List<RealizadaEmpresa> result2 = new ArrayList<>();

        for (RealizadaEmpresa r : result) {
            if (r.getCliente().getEmail().equals(c.getEmail())){
                if (r.getClassificado()==false)
                    result2.add(r);
            }
        }

        return result2;
    }

    // listagem de entregues realizadas por voluntarios a um cliente que faltam classificar
    public List<RealizadaVoluntario> realizadosClassificarClienteVoluntario(Cliente c) {
        List<RealizadaVoluntario> result = new ArrayList<>(this.getRealizadaVoluntario());
        List<RealizadaVoluntario> result2 = new ArrayList<>();

        for (RealizadaVoluntario r : result) {
            if (r.getCliente().getEmail().equals(c.getEmail())){
                if (r.getClassificado()==false)
                    result2.add(r);
            }
        }

        return result2;
    }
    
     public double faturacao(String email,LocalDate inicio,LocalDate fim){
        double result = 0;
        List<Encomenda> list = new ArrayList<Encomenda>(this.getRealizadaEmpresa());

        for (Encomenda e: list){
            if ((((RealizadaEmpresa)e).getEmpresa().getEmail()).equals(email) && ((RealizadaEmpresa)e).getData().isAfter(inicio) && ((RealizadaEmpresa)e).getData().isBefore(fim)) result = result + (((RealizadaEmpresa)e).getPreco());
    }
    return  result;
    }
}
