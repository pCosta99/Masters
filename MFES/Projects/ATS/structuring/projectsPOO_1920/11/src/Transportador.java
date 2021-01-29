import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.lang.Math;
/**
 * SuperClasse das classes transportadoras
 * 
 * @author Rui Cunha
 * @version 06/04/2020
 */
public abstract class Transportador extends User
{
    //Registo das Encomendas que trasnsportaram
    private boolean transporte;//Sinalizar disponibilidade para transporte
    private boolean transporte_medico; // aceitacao ou nao de transporte de produtos medicos
    private Map<String,Integer> classificacao;//classificacoes feitas pelos utilizadores
    private Map<String,Encomenda> encomendas;//map que mostra as encomendas a ser transportadas
    private double raio; //representa a raio no qual a transportadora pode trabalhar
    
    //Construtor por omissao
    public Transportador()
    {
        super();
        this.transporte = true;
        this.transporte_medico = false;
        this.classificacao = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.raio = 0.0;
    }
    
    //Construtor por parametros
    public Transportador(String username,String nome, String password,Localizacao posicao,double raio,
    boolean tp, boolean tp_medico, Map<String,Integer> clas, HashMap<String,Encomenda> encomendas)
    {
        super(username, nome, password,posicao);
        this.transporte = tp;
        this.transporte_medico = tp_medico;
        this.raio = raio;
        setClassificacoes(clas);
        setEncomendas(encomendas);
    }
    
    //Construtor copia
    public Transportador(Transportador trans)
    {
        super(trans);
        this.transporte = trans.getTransporte();
        this.transporte_medico = trans.getTransporteMedico();
        this.raio = trans.getRaio();
        setClassificacoes(trans.getClassificacoes());
        setEncomendas(trans.getEncomendas());
    }
    
    //Gets
    public double getRaio()
    {
        return this.raio;
    }
    
    public Map<String,Integer> getClassificacoes()
    {
        Map<String,Integer> ret = new HashMap<>();
        for(Map.Entry<String,Integer> e : this.classificacao.entrySet())
        {
            ret.put(e.getKey(),e.getValue());
        }
        return ret;
    }

    public HashMap<String, Encomenda> getEncomendas() {
        HashMap<String,Encomenda> copia = new HashMap<>();
        for(Map.Entry<String,Encomenda> edb: this.encomendas.entrySet()){
            copia.put(edb.getKey(),(edb.getValue()).clone());
        }
        return copia;
    }

    public boolean getTransporte()
    {
        return this.transporte;
    }

    public boolean getTransporteMedico()
    {
        return this.transporte_medico;
    }
    
    //Sets
    public void setRaio(double raio)
    {
        this.raio = raio;
    }
    
    public void setClassificacoes(Map<String,Integer> clas)
    {
        this.classificacao = new HashMap<>();
        clas.entrySet().forEach(e -> this.classificacao.put(e.getKey(), e.getValue()));
    }

    public void setEncomendas(HashMap<String,Encomenda> edb){
        this.encomendas = new HashMap<>();
        edb.entrySet().forEach(e -> this.encomendas.put(e.getKey(),(e.getValue()).clone()));
    }
    
    public void setTransporte(boolean tp)
    {
        this.transporte = tp;
    }
    
    public void setTransporteMedico(boolean tp_medico)
    {
        this.transporte_medico = tp_medico;
    }
    
    //Metodo que insere uma nova classificacao
    public void addClassificacao(String name, int classificacao)
    {
        this.classificacao.put(name,classificacao);
    }
    
    //Metodo que remove uma classificacao pelo nome do utilizador
    public void removeClassificacao(String name)
    {
        this.classificacao.remove(name);
    }
    
    //Metodo que mostra uma classificacao pelo nome do utilizador
    public int getClassificacao(String name)
    {
        return this.classificacao.get(name);
    }

    public void addEncomenda(Encomenda e){
        this.encomendas.put(e.getCodigo(),e.clone());
    }

    public void removeEncomenda(String cod){
        this.encomendas.remove(cod);
    }

    public double classMedia(){
        double sum = 0;
        int i = 0;
        for(Map.Entry<String,Integer> e : this.classificacao.entrySet())
        {
            sum = e.getValue() + sum;
            i += 1;
        }
        return sum/i;
    }

    public boolean aceitoTransportMedico(){
        return this.transporte_medico;
    }

    public void aceitaMedicamentos(boolean med){
        this.transporte_medico = med;
    }
    
    //Metodo que determina a distancia a que este transportador está de uma localização
    public double distanciaA(Localizacao local)
    {
        double this_x = this.getPosicao().getLatitude();
        double this_y = this.getPosicao().getLongitude();
        double x = local.getLatitude();
        double y = local.getLongitude();
        double r = Math.sqrt(Math.pow(x - this_x,2) + Math.pow(y - this_y,2));
        return r;
    }
    
    //Metodo que indica se uma encomenda está dentro do raio de ação deste transportador
    public boolean dentroRaio(Localizacao local)
    {
        double distancia = distanciaA(local);
        //System.out.println(distancia+" "+this.raio);
        if (this.raio > distancia)
            return true;
        return false;
    }
    

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
        .append("\nTransporte disponivel: ").append(this.transporte + "\n")
        .append("\nTransporte médico: ").append(this.transporte_medico + "\n")
        .append(this.classificacao.toString()+"\n")
        .append(this.encomendas.toString()+"\n")
        .append("\nRaio de trabalho: ").append(this.raio + "\n");
        return sb.toString();
    }

    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Voluntario v = (Voluntario) o;
        return this.getTransporte() == v.getTransporte() &&
                this.getTransporteMedico() == v.getTransporteMedico() &&
                this.getClassificacoes().equals(v.getClassificacoes()) &&
                this.getRaio() == v.getRaio() &&
                this.getEncomendas().equals(v.getEncomendas());
    }

    public abstract Transportador clone();
}
