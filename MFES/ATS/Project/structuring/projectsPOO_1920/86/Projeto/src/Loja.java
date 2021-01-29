import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe que representa uma loja e que contem a lista das encomendas prontas a ser entregues e a lista das encomendas em
 * fila de espera.
 */
public class Loja extends Entidade implements Serializable {
    private List<Encomenda> aceites;
    private List<Encomenda> fila;
    private Double mediaEspera;
    private Map<String,Encomenda> historico;
    private Map<String,Map.Entry<String,Double>> listaProdutos; //Cod,Nome,Preco

    public Loja(){
        super();
        this.aceites = new ArrayList<>();
        this.fila = new ArrayList<>();
        this.mediaEspera = Math.random()*3+1;
        this.listaProdutos = iniciaCatalogo();
        this.historico = new HashMap<>();
    }

    public Loja(String codLoja, String nomeLoja,GPS gps) {
        super(codLoja,nomeLoja,gps);
        this.aceites = new ArrayList<>();
        this.fila = new ArrayList<>();
        this.mediaEspera = Math.random()*3+1;
        this.listaProdutos = iniciaCatalogo();
        this.historico = new HashMap<>();
    }

    public Loja(String codLoja, String nomeLoja,GPS gps, List<Encomenda> fila, Double mediaEspera) {
        super(codLoja,nomeLoja,gps);
        setFila(fila);
        this.mediaEspera = mediaEspera;
        this.historico = new HashMap<>();
    }

    public Loja(Loja l){
        super(l.getCodigo(),l.getNome(),l.getCoordenadas());
        this.aceites = l.getAceites();
        this.fila = (l.getFila());
        this.mediaEspera = l.getMediaEspera();
        this.listaProdutos= l.getListaProdutos();
        this.historico = l.getHistorico();
    }

    public List<Encomenda> getFila() {
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.fila){
            aux.add(a);
        }
        return aux;
    }

    public List<Encomenda> getAceites() {
        List<Encomenda> aux = new ArrayList<>();
        for(Encomenda a : this.aceites){
            aux.add(a);
        }
        return aux;
    }

    public Map<String, Map.Entry<String, Double>> getListaProdutos() {
        return listaProdutos;
    }

    public void setFila(List<Encomenda> fila) {
        this.fila = new ArrayList<>();
        for(Encomenda a : fila) {
                this.fila.add(a);
        }
    }

    public void setListaProdutos(Map<String, Map.Entry<String, Double>> listaProdutos) {
        this.listaProdutos = listaProdutos;
    }

    public Double getMediaEspera() {
        return mediaEspera;
    }

    public void setMediaEspera(Double mediaEspera) {
        this.mediaEspera = mediaEspera;
    }

    @Override
    public boolean equals(Object o) {

        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja loja = (Loja) o;
        return Objects.equals(fila, loja.fila) &&
                Objects.equals(mediaEspera, loja.mediaEspera);
    }


    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Loja:");
        sb.append(getCodigo()).append(",");
        sb.append(getNome()).append(",");
        sb.append(getCoordenadas().toString());
        return sb.toString();
    }

    public Loja clone(){
        return new Loja(this);
    }


    /**
     * Método que adiciona uma encomenda à lista das encomendas aceites.
     */
    public void addToAceites(Encomenda e){
        this.aceites.add(e.clone());
    }

    /**
     * Método que adiciona uma encomenda à lista da fila de espera.
     */
    public void addToFila(Encomenda e){
        this.fila.add(e.clone());
    }

    /**
     * Método que retira uma encomenda das encomendas aceites.
     */
    public void removeAceites(Encomenda e){
        this.aceites = this.aceites.stream().filter(a->!(e.getCodEncomenda().equals(a.getCodEncomenda()))).collect(Collectors.toList());
    }

    /**
     * Método que retira uma encomenda da fila de espera e adiciona-a à lista das encomendas aceites.
     */
    public void removeFila(Encomenda e){
        this.fila = this.fila.stream().filter(a-> !(e.getCodEncomenda().equals(a.getCodEncomenda()))).collect(Collectors.toList());
        this.aceites.add(e);
    }

    public Map<String,Encomenda> getHistorico(){
        Map<String,Encomenda> res = new HashMap<>();
        for(Map.Entry<String,Encomenda> s : this.historico.entrySet()){
            res.put(s.getKey(),s.getValue().clone());
        }
        return res;
    }

    /**
     * Método que adiciona uma encomenda ao historico.
     */
    public void addToHistorico(Encomenda e){
        this.historico.put(e.getCodEncomenda(),e.clone());
    }

    /**
     * Método que retorna a lista do historico de encomendas num certo intervalo de tempo.
     */
    public List<Encomenda> historicoData(LocalDateTime date, LocalDateTime date2){
        return this.historico.values().stream().filter(a->a.getEntrega().isAfter(date) && a.getEntrega().isBefore(date2)).sorted(Comparator.comparing(Encomenda::getEntrega)).collect(Collectors.toList());
    }

    /**
     * Método que cria um catálogo com produtos e preços aleatorios.
     */
    private static Map<String,Map.Entry<String,Double>> iniciaCatalogo(){
        Map<String,Map.Entry<String,Double>> res = new HashMap<>();
        if(Math.random()<=0.5)res.put("p4",new AbstractMap.SimpleEntry<>("Detergente",Math.random()*5+10));
        if(Math.random()<=0.5)res.put("p5",new AbstractMap.SimpleEntry<>("Agua sanitaria",Math.random()+1));
        if(Math.random()<=0.5)res.put("p6",new AbstractMap.SimpleEntry<>("Esponja de aco",Math.random()+0.4));
        if(Math.random()<=0.5)res.put("p7",new AbstractMap.SimpleEntry<>("Sabao em pedra",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p8",new AbstractMap.SimpleEntry<>("Sabonete",Math.random()));
        if(Math.random()<=0.5)res.put("p9",new AbstractMap.SimpleEntry<>("Shampoo",Math.random()+1));
        if(Math.random()<=0.5)res.put("p10",new AbstractMap.SimpleEntry<>("Condicionador",Math.random()+1));
        if(Math.random()<=0.5)res.put("p11",new AbstractMap.SimpleEntry<>("Desinfetante",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p12",new AbstractMap.SimpleEntry<>("Lustra moveis",Math.random()+1));
        if(Math.random()<=0.5)res.put("p13",new AbstractMap.SimpleEntry<>("Tira manchas",Math.random()+1));
        if(Math.random()<=0.5)res.put("p14",new AbstractMap.SimpleEntry<>("Limpa vidros",Math.random()+1));
        if(Math.random()<=0.5)res.put("p15",new AbstractMap.SimpleEntry<>("Alcool",Math.random()+1));
        if(Math.random()<=0.5)res.put("p16",new AbstractMap.SimpleEntry<>("Saco de lixo 30l",Math.random()*2+1));
        if(Math.random()<=0.5)res.put("p17",new AbstractMap.SimpleEntry<>("Saco de lixo 50l",Math.random()*3+1));
        if(Math.random()<=0.5)res.put("p19",new AbstractMap.SimpleEntry<>("Sumo garrafa 1l",Math.random()+1));
        if(Math.random()<=0.5)res.put("p20",new AbstractMap.SimpleEntry<>("Sumo caixa 500ml",Math.random()+0.5));
        if(Math.random()<=0.5)res.put("p21",new AbstractMap.SimpleEntry<>("Leite integral litro",Math.random()+1));
        if(Math.random()<=0.5)res.put("p22",new AbstractMap.SimpleEntry<>("Leite desnatado litro",Math.random()+1));
        if(Math.random()<=0.5)res.put("p24",new AbstractMap.SimpleEntry<>("Feijao 2kg",Math.random()+2));
        if(Math.random()<=0.5)res.put("p25",new AbstractMap.SimpleEntry<>("Macarrao",Math.random()+1));
        if(Math.random()<=0.5)res.put("p26",new AbstractMap.SimpleEntry<>("Extrato de tomate",Math.random()+1));
        if(Math.random()<=0.5)res.put("p28",new AbstractMap.SimpleEntry<>("Sal",Math.random()+1));
     	if(Math.random()<=0.5)res.put("p29",new AbstractMap.SimpleEntry<>("Acucar",Math.random()+1));
        if(Math.random()<=0.5)res.put("p31",new AbstractMap.SimpleEntry<>("Bolacha",Math.random()+1));
        if(Math.random()<=0.5)res.put("p33",new AbstractMap.SimpleEntry<>("Farofa pronta",Math.random()+1));
        if(Math.random()<=0.5)res.put("p34",new AbstractMap.SimpleEntry<>("Farinha de trigo",Math.random()+1));
        if(Math.random()<=0.5)res.put("p35",new AbstractMap.SimpleEntry<>("Farinha de milho",Math.random()+1));
        if(Math.random()<=0.5)res.put("p36",new AbstractMap.SimpleEntry<>("Farinha de mandioca",Math.random()+1));
        if(Math.random()<=0.5)res.put("p37",new AbstractMap.SimpleEntry<>("Sardinha",Math.random()+1));
        if(Math.random()<=0.5)res.put("p38",new AbstractMap.SimpleEntry<>("Atum",Math.random()+1));
        if(Math.random()<=0.5)res.put("p40",new AbstractMap.SimpleEntry<>("Molho de pimenta",Math.random()+2));
        if(Math.random()<=0.5)res.put("p41",new AbstractMap.SimpleEntry<>("Ervilha",Math.random()+2));
        if(Math.random()<=0.5)res.put("p42",new AbstractMap.SimpleEntry<>("Milho verde",Math.random()+1));
        if(Math.random()<=0.5)res.put("p44",new AbstractMap.SimpleEntry<>("Goiabada",Math.random()+1));
        if(Math.random()<=0.5)res.put("p48",new AbstractMap.SimpleEntry<>("Creme de leite",Math.random()*2+1));
        if(Math.random()<=0.5)res.put("p49",new AbstractMap.SimpleEntry<>("Leite condensado",Math.random()*2+1));
        if(Math.random()<=0.5)res.put("p51",new AbstractMap.SimpleEntry<>("Alface",Math.random()+0.2));
        if(Math.random()<=0.5)res.put("p52",new AbstractMap.SimpleEntry<>("Couve",Math.random()+0.2));
        if(Math.random()<=0.5)res.put("p53",new AbstractMap.SimpleEntry<>("Batata",Math.random()+0.2));
        if(Math.random()<=0.5)res.put("p55",new AbstractMap.SimpleEntry<>("Cenoura",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p56",new AbstractMap.SimpleEntry<>("Beterraba",Math.random()+0.2));
        if(Math.random()<=0.5)res.put("p59",new AbstractMap.SimpleEntry<>("Espinafre",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p60",new AbstractMap.SimpleEntry<>("Banana",Math.random()+0.6));
        if(Math.random()<=0.5)res.put("p61",new AbstractMap.SimpleEntry<>("Ovos",Math.random()+1));
        if(Math.random()<=0.5)res.put("p62",new AbstractMap.SimpleEntry<>("Uva",Math.random()+1));
        if(Math.random()<=0.5)res.put("p63",new AbstractMap.SimpleEntry<>("Abacate",Math.random()+1));
        if(Math.random()<=0.5)res.put("p65",new AbstractMap.SimpleEntry<>("Melancia",Math.random()*3+3));
        if(Math.random()<=0.5)res.put("p67",new AbstractMap.SimpleEntry<>("Salsa",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p68",new AbstractMap.SimpleEntry<>("Cebola",Math.random()+0.3));
        if(Math.random()<=0.5)res.put("p69",new AbstractMap.SimpleEntry<>("Queijo",Math.random()+1));
        if(Math.random()<=0.5)res.put("p70",new AbstractMap.SimpleEntry<>("Queijo Mussarela",Math.random()*2+1));
        if(Math.random()<=0.5)res.put("p71",new AbstractMap.SimpleEntry<>("Queijo outros",Math.random()*2+1));
        if(Math.random()<=0.5)res.put("p72",new AbstractMap.SimpleEntry<>("Manteiga",Math.random()+2));
        if(Math.random()<=0.5)res.put("p73",new AbstractMap.SimpleEntry<>("Margarina",Math.random()+2));
        if(Math.random()<=0.5)res.put("p74",new AbstractMap.SimpleEntry<>("Iogurte",Math.random()+1));
        if(Math.random()<=0.5)res.put("p76",new AbstractMap.SimpleEntry<>("Peixe",Math.random()*4+2));
        if(Math.random()<=0.5)res.put("p77",new AbstractMap.SimpleEntry<>("Frango",Math.random()*3+2));
        if(Math.random()<=0.5)res.put("p79",new AbstractMap.SimpleEntry<>("Carne seca",Math.random()*3+3));
        if(Math.random()<=0.5)res.put("p80",new AbstractMap.SimpleEntry<>("Salsicha",Math.random()+2));
        return res;
    }

}
