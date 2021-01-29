import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.stream.Collectors;

public class Transportadora extends Transporta implements Serializable {
    private String NIF;
    private double preco_por_km;
    private double preco_por_kg;

    private int capacidade_total; // Corresponde à capacidade total de todos os transportes.
    private int capacidade_carrinha;
    private List<Transporte> transportes;

    // Encomendas
    private List<Encomenda> encomendasARealizar;
    private Map<Encomenda, TempoCustoEncomenda> encomendasRealizadas;

    public void PrintTrasportes(){
        this.transportes.stream().forEach(a->System.out.println(a.toString()));
        System.out.println("\n");
    }
    ///////////////// CONSTRUTORES /////////////////
    /**
     * Construtor parametrizado de objetos da classe Transportadora
     */
     
     // FALTA ACRESCENTAR PARAMETROS <----------
    public Transportadora(String username, String password, String cod, String nome, Coordenadas gps, 
                          double raio, String NIF, double preco_por_km) {

       super(username, password, cod, nome, gps, raio);
       this.NIF = NIF;
       this.preco_por_km = preco_por_km;
       this.preco_por_kg = 0.20; // É PRECISO SER PASSADO COMO PARÂMETRO
       initTransportes();
       //this.transportes_disponiveis = ThreadLocalRandom.current().nextBoolean();
       this.encomendasARealizar = new ArrayList<>();
       this.encomendasRealizadas = new HashMap<>();
    }


    /**
     * Construtor de copia de objetos da classe Transportadora
     */
    public Transportadora(Transportadora t)
    {
        super(t);
        this.NIF = t.getNIF();
        this.preco_por_km = t.getPrecoPorKm();
        this.transportes = t.getTransportes();
    }

    /**
     * Inicializa os transportes, a capacidade das carrinhas e a capacidade total da transportadora. 
     * Deve ser utilizado nos construtores da classe.
     */
    public void initTransportes(){
        Random random = new Random();
        int numeroCarrinhas = 1 + random.nextInt(3);
        int numeroBicicletas = 3 + random.nextInt(5);
        this.capacidade_carrinha = 2 + random.nextInt(10); // DEPOIS AJUSTAR VALOR DA CAP. DAS CARRINHAS

        this.transportes = new ArrayList<>();

        int i;
        for (i = 0; i<numeroCarrinhas; i++) {
            Carrinha c = new Carrinha(this.preco_por_km, this.preco_por_kg, 50);
            transportes.add(c);
        }
        for (i = 0; i<numeroBicicletas; i++) {
            Bicicleta b = new Bicicleta();
            transportes.add(b);
        }
        
        this.capacidade_total = numeroBicicletas + numeroCarrinhas*this.capacidade_carrinha;
    }

    /*
    * Getters e Setters das variáveis standard
     */

    public String getNIF() {
        return this.NIF;
    }

    public void setNIF(String NIF) {
        this.NIF = NIF;
    }

    public double getPrecoPorKm() {
        return this.preco_por_km;
    }

    public void setPrecoPorKm(float p) {
        this.preco_por_km = p;
    }


    /*
    * Getters e Setters das variáveis acrescentadas ao enunciado
     */

    public boolean isTransportesDisponiveis() {
        return super.estaDisponivel();
    }

    public void setTransportesDisponiveis(boolean transportes_disponiveis) {
        this.setDisponibilidade(transportes_disponiveis);
    }

    private List<Transporte> getTransportes() {
        return this.transportes.stream()./*map(Transporte::clone)*/collect(Collectors.toList());
    }

    private void setTransportes(List<Transporte> transp) {
        this.transportes = new ArrayList<>();
        transp.stream().forEach(a-> this.transportes.add(a));//add(a.clone()));
    }


    public String toString()
    {
        StringBuilder sb = new StringBuilder();

        sb.append("CodEmpresa: ").append(super.getCodigo()).append("\n");
        sb.append("NomeEmpresa: ").append(super.getNome()).append("\n");
        sb.append("Longitude: ").append(super.getCoordenadas()).append("\n");
        sb.append("NIF: ").append(this.NIF).append("\n");
        sb.append("Raio: ").append(super.getRaio()).append("\n");

        return sb.toString();
    }

    public Transportadora clone()
    {
        return new Transportadora(this);
    }


    // SÓ ESTÁ A FAZER A VERIFICAÇÃO DO SUPER.
    public boolean equals(Object o) {
        return super.equals(o);
    }

        
    /**
     * Adiciona uma encomenda a colecao de Encomenda 
     *
     */
    public void addEncomenda(Encomenda e) {
        this.encomendasARealizar.add(e);
        if (this.encomendasARealizar.size() == this.capacidade_total) {
            this.setDisponibilidade(false); // Já tem tantas encomendas quantas consegue transportar
        }

    }
    
    /**
     * Verifica a Transportadora aceita a Encomenda(se esta no seu raio e se esta disponivel)
     * 
     * @param enc(Encomenda) a ser entregue
     * @return boolean
     */
    public boolean aceitaEncomenda(Encomenda enc){
        // if (super.aceitaEncomenda(enc)) {
        //     Utilizador u = super.getSistema().getUtilizador(enc.getCodUtilizador());
        //     return u.aceitaEntrega(enc, this);
        // }
        // else return false;
        return (super.aceitaEncomenda(enc));
    }

    public boolean utilizadorAceitaEncomenda(Encomenda enc) {
        Utilizador u = super.getSistema().getUtilizador(enc.getCodUtilizador());
        return u.aceitaEntrega(enc, this);
    }
    
    /**
     * Calcula o preco do transporte de uma Encomenda
     * 
     * @param enc(Encomenda) a ser entregue
     * @return preco do trasporte
     */
    public double precoTransporte(Encomenda enc){
        Coordenadas coordenadasLoja = AppUser.getSistema().getLoja(enc.getCodLoja()).getCoordenadas();
        Coordenadas coordenadasUtilizador = AppUser.getSistema().getUtilizador(enc.getCodUtilizador()).getCoordenadas();

        double distanciaTotal = Coordenadas.distanciaRota(this.getCoordenadas(), coordenadasLoja, coordenadasUtilizador);

        return this.preco_por_km * distanciaTotal + this.preco_por_kg * enc.getPeso();
    }
    
    
    /**
     * Calcula o total faturado pela Trasportadora num intervalo de tempo
     * 
     * @param  inicio e fim do intervalo
     * @return total faturado
     */
    public double totalFaturadoPeriodo(LocalDateTime inicio, LocalDateTime fim){
        return this.encomendasRealizadas.entrySet().stream().
                   filter(a->(a.getValue().isAfter(inicio) && a.getValue().isBefore(fim))).
                   mapToDouble(a->precoTransporte(a.getKey())).//reduce(0.0, (ac,v)->ac+v);
                   sum();
    }


    /**
     * Calcula o total faturado pela Trasportadora a aprtir da aplicação
     * @return total faturado
     */
    public double totalFaturado(){
        return this.encomendasRealizadas.entrySet().stream().
                mapToDouble(a->precoTransporte(a.getKey())).reduce(0.0, (ac,v)->ac+v);
    }

    
    /**
     * Calcula tempo aproximado que uma encomenda vai demorar a ser entregue
     * 
     * @param enc(Encomenda) a ser entregue
     * @return tempo demorado
     */
    public double tempoDemorado(Encomenda enc) {
        //tempo demorado de entrega de uma encomenda depende da distancia percorrida, fila de espera e possivelmente da mateorologia
        //secalhar devia estar no transporte, fazer uma variavel tempoPorKm?
        
        return 0.0;
    }
    
    
    /**
     * Devolve um Map com as encomendas que foram efetuadas num intervalo(com as encomendas podemos calcular as viagens???? queremos um extrato de viagens)
     * 
     * @param inicio e fim do intervalo de tempo
     * @return Map com as encomendas feitas nesse intervalo
     */
    public Map<Encomenda, TempoCustoEncomenda> extratoViagens(LocalDateTime inicio, LocalDateTime fim){
        return this.encomendasRealizadas.entrySet().stream().
               filter(a->(a.getValue().isAfter(inicio) && a.getValue().isBefore(fim))).
               collect(Collectors.toMap(a->a.getKey(),a-> a.getValue()));
    }

    /**
     * Devolve um Map com as encomendas que foram efetuadas num intervalo(com as encomendas podemos calcular as viagens???? queremos um extrato de viagens)
     *
     * @param inicio e fim do intervalo de tempo
     * @return Map com as encomendas feitas nesse intervalo
     */
    public List<Encomenda> ListaExtratoViagens(LocalDateTime inicio, LocalDateTime fim){
        return this.encomendasRealizadas.entrySet().stream().
                filter(a->(a.getValue().isAfter(inicio) && a.getValue().isBefore(fim))).
                map(a->a.getKey()).
                collect(Collectors.toList());
    }


    /**
     * Devolve as Encomendas realizadas por um utilizador
     * @param codUt código do utilizador
     * @return Set de Encomendas
     */
    public Set<Encomenda> getEncomendasUtilizador(String codUt){
        return this.encomendasRealizadas.keySet().stream().filter(x -> x.getCodUtilizador().equals(codUt)).collect(Collectors.toSet());
    }

    /**
     * Verifica se existem carrinhas disponíveis.
     * @return true se tiver carrinhas disponíveis e false caso contrário
     */
    public boolean existeCarrinhaDisponível() {
        if (!this.estaDisponivel()) return false;
        return this.transportes.stream().filter(t -> t.isDisponivel()).anyMatch(t -> t.getClass().getSimpleName().equals("Carrinha"));
    }

    /**
     * Verifica se existem bicicletas disponíveis.
     * @return true se tiver bicicletas disponíveis e false caso contrário.
     */
    public boolean existeBicicletaDisponível() {
        if (!this.estaDisponivel()) return false;
        return this.transportes.stream().filter(t -> t.isDisponivel()).anyMatch(t -> t.getClass().getSimpleName().equals("Bicicleta"));
    }


    private List<Transporte> distribuiEncomendas() {
        List<Transporte> vaoTransportar = new ArrayList<>();
        List<Carrinha> carrinhasAvailable = this.transportes.stream().filter(t -> t instanceof Carrinha && t.isDisponivel()).map(t -> (Carrinha) t).collect(Collectors.toList());
        List<Bicicleta> bicicletasAvailable = this.transportes.stream().filter(t -> t instanceof Bicicleta && t.isDisponivel()).map(t -> (Bicicleta) t).collect(Collectors.toList());
        int numeroBicicletas = bicicletasAvailable.size(); 
        //int numeroCarrinhas = carrinhasAvailable.size();

        int numeroCarrinhasAUsar = this.encomendasARealizar.size() / this.capacidade_carrinha;
        int numeroBicicletasAUsar = numeroBicicletas;

        int restantesEncomendas = this.encomendasARealizar.size() - numeroCarrinhasAUsar*capacidade_carrinha;
        if (restantesEncomendas > numeroBicicletas) {
            numeroCarrinhasAUsar++;
            numeroBicicletasAUsar = 0;
        }

        Encomenda enc;
        for (int i = 0; i<numeroCarrinhasAUsar; i++) {
            Carrinha c = carrinhasAvailable.get(i);
            for (int n = 0; n < this.capacidade_carrinha && !this.encomendasARealizar.isEmpty(); n++) {
                enc = this.encomendasARealizar.remove(0);
                c.addEncomenda(enc);
            }
            c.setDisponibilidade(false);
            vaoTransportar.add(c);
        }

        for (int n = 0; !this.encomendasARealizar.isEmpty() && n<numeroBicicletasAUsar; n++) {
            Bicicleta b = bicicletasAvailable.get(n);
            b.addEncomenda(this.encomendasARealizar.remove(0));
            vaoTransportar.add(b);
        }

        return vaoTransportar;
    }

    // DÁ PARA FAZER ISTO SEM SEPARAR BICICLETAS E CARRINHAS, DEPOIS MELHORAR ISTO
    public void trataEncomendas() {
        List<Transporte> vaoTransportar = distribuiEncomendas();
        vaoTransportar.stream().forEach(v -> despachaTransporte(v));
    }

    private void despachaTransporte(Transporte t) {
        if (t instanceof Carrinha) despachaCarrinha((Carrinha)t);
        else despachaBicicleta((Bicicleta)t);
    }

    private void despachaBicicleta(Bicicleta b) {
        Encomenda enc = b.getEncomenda();
        Coordenadas coordenadasLoja = AppUser.getSistema().getLoja(enc.getCodLoja()).getCoordenadas();
        Coordenadas coordenadasUtilizador = AppUser.getSistema().getUtilizador(enc.getCodUtilizador()).getCoordenadas();

        double distanciaTotal = this.getCoordenadas().distancia(coordenadasLoja) + 
                                Coordenadas.distancia(coordenadasLoja,coordenadasUtilizador);

        double custo = this.preco_por_km * distanciaTotal + this.preco_por_kg * enc.getPeso();
        double tempo = b.tempoViagem(distanciaTotal);
        LocalDateTime momentoPartida = LocalDateTime.now();
        AtrasaEncomenda atraso = new AtrasaEncomenda(momentoPartida);
        tempo += atraso.tempoDeAtraso();

        TempoCustoEncomenda tce = new TempoCustoEncomenda((int)tempo, custo, momentoPartida.plusMinutes((long)tempo));

        this.encomendasRealizadas.put(enc, tce);
        AppUser.getSistema().getUtilizador(enc.getCodUtilizador()).atualizaHistorico(new EncomendaEntregue(enc, tce));
        b.libertaEncomenda();
    }

    private void despachaCarrinha(Carrinha c) {
        List<Encomenda> enc = c.getEncomendas();
        List<TempoCustoEncomenda> tces = new ArrayList<>();

        Coordenadas coordenadasAtuais, coordenadasLoja, coordenadasUtilizador = null;
        int i;
        double tempo = 0, custo, distanciaTotal;
        LocalDateTime momentoPartida = LocalDateTime.now();

        for (i=0; i<enc.size(); i++) {

            if (i == 0) {
                coordenadasAtuais = this.getCoordenadas();
            }
            else {
                coordenadasAtuais = coordenadasUtilizador;
            }
            coordenadasLoja = AppUser.getSistema().getLoja(enc.get(i).getCodLoja()).getCoordenadas();
            coordenadasUtilizador = AppUser.getSistema().getUtilizador(enc.get(i).getCodUtilizador()).getCoordenadas();

            distanciaTotal = Coordenadas.distanciaRota(coordenadasAtuais, coordenadasLoja, coordenadasUtilizador);

            custo = this.preco_por_km * distanciaTotal + this.preco_por_kg * enc.get(i).getPeso();
            tempo += c.tempoViagem(distanciaTotal);
            AtrasaEncomenda atraso = new AtrasaEncomenda(momentoPartida);
            tempo += atraso.tempoDeAtraso();
            momentoPartida = momentoPartida.plusMinutes((long)tempo);

            TempoCustoEncomenda tce = new TempoCustoEncomenda((int) tempo, custo, momentoPartida);

            tces.add(tce);
        }

        for(i=0; i<enc.size(); i++) {
            TempoCustoEncomenda tempocusto = tces.get(i);
            this.encomendasRealizadas.put(enc.get(i), tempocusto);
            AppUser.getSistema().getUtilizador(enc.get(i).getCodUtilizador()).atualizaHistorico(new EncomendaEntregue(enc.get(i),tempocusto)); 
        }

        c.libertaEncomendas();

    }

    // public double precoTransporte(Encomenda enc) {
    //     coordenadasLoja = AppUser.getSistema().getLoja(enc.getCodLoja()).getCoordenadas();
    //     coordenadasUtilizador = AppUser.getSistema().getUtilizador(enc.getCodUtilizador()).getCoordenadas();

    //     distanciaTotal = Coordenadas.distanciaRota(this.getCoordenadas(), coordenadasLoja, coordenadasUtilizador);

    //     custo = this.preco_por_km * distanciaTotal + this.preco_por_kg * enc.getPeso();
    // }
    
}
