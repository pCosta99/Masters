import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeFormatterBuilder;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe principal
 */
public class SistemaTrazAqui implements IModelo, Serializable {
    private Map<String, Utilizador> users;
    private Map<String, Loja> lojas;
    private Map<String, MeioTransporte> transportadores;
    private Collection<Encomenda> encomendas;
    private CatalogoLojas cat;
    private Contas registos;

    /**
     * Construtores
     */
    public SistemaTrazAqui() {
        this.users = new HashMap<>();
        this.lojas = new HashMap<>();
        this.transportadores = new HashMap<>();
        this.encomendas = new TreeSet<>();
        this.registos = new Contas();
        this.cat = new CatalogoLojas();
    }

    /**
     * Devolve os utilizadores existentes no sistema
     */
    public Collection<Utilizador> getUsers() {
        Collection<Utilizador> res = new TreeSet<>();

        for (Utilizador u : this.users.values()) {
            res.add(u.clone());
        }

        return res;
    }

    /**
     * Devolve um MeioTransporte através do seu código
     * @param codigo Codigo
     * @return t Meio transporte pretendido
     */
    public MeioTransporte getTransportador(String codigo) {

        for (MeioTransporte t : this.transportadores.values()) {
            if (codigo.equals(t.getCodigo())) {
                return t;
            }
        }
        return null;

    }

    /**
     * Devolve todos as lojas existentes no sistema
     */
    public Collection<Loja> getLojas() {
        return this.lojas.values()
                .stream().map(Loja::clone).collect(Collectors.toCollection(ArrayList::new));
    }

    /**
     * Devolve todos os MeioTransporte existentes no sistema
     */
    public Collection<MeioTransporte> getTransportadores() {
        return this.transportadores.values()
                .stream().map(MeioTransporte::clone).collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Devolve todos as encomendas existentes no sistema
     */
    public Collection<Encomenda> getEncomendas() {
        Collection<Encomenda> res = new TreeSet<>();

        for (Encomenda e : this.encomendas) {
            res.add(e.clone());
        }

        return res;
    }

    public void adicionaCatalogoALoja(String codigo){
        cat.adicionaInfoProdutos(codigo, cat.getInfoProdutos().entrySet().iterator().next().getValue());
    }

    /**
     * Função que carrega os dados dos Logs para o sistema
     */
    public void loadFromLogs() {
        Parsing p = new Parsing();
        List<String> linhas = p.lerFicheiro("logs_20200416.txt");
        linhas = linhas.stream().skip(43).collect(Collectors.toList()); /* Este comando dá skip nas primeiras 43 linhas do ficheiro de logs, que é informação não útil para o programa */
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = p.parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    users.put(u.getCodigo(), u.clone());
                    String codigo = u.getCodigo();
                    StringBuilder sb = new StringBuilder(codigo);
                    sb.append("@gmail.com");
                    registos.adicionarRegisto(codigo, sb.toString(), codigo);
                    break;
                case "Loja":
                    Loja l = p.parseLoja(linhaPartida[1]);
                    lojas.putIfAbsent(l.getCodigo(), l.clone());
                    String idLoja = l.getCodigo();
                    StringBuilder sbL = new StringBuilder(idLoja);
                    sbL.append("@gmail.com");
                    registos.adicionarRegisto(idLoja, sbL.toString(), idLoja);
                    break;
                case "Voluntario":
                    Voluntario v = p.parseVoluntario(linhaPartida[1]);
                    transportadores.put(v.getCodigo(), v.clone());
                    String idV = v.getCodigo();
                    StringBuilder sbV = new StringBuilder(idV);
                    sbV.append("@gmail.com");
                    registos.adicionarRegisto(idV, sbV.toString(), idV);
                    break;
                case "Transportadora":
                    MeioTransporte t = p.parseTransportadora(linhaPartida[1]);
                    transportadores.put(t.getCodigo(), t.clone());
                    String idT = t.getCodigo();
                    StringBuilder sbT = new StringBuilder(idT);
                    sbT.append("@gmail.com");
                    registos.adicionarRegisto(idT, sbT.toString(), idT);
                    break;
                case "Encomenda":
                    Encomenda e = p.parseEncomenda(linhaPartida[1]);
                    encomendas.add(e.clone());
                    List<LinhaEncomenda> listaLE = e.getLinhas();
                    for (LinhaEncomenda le : listaLE)
                        cat.insereProduto(e.getCodLoja(), le);
                    break;
                default:
                    break;
            }

        }
    }

    /**
     * Passa para String
     */
    public String toString() {
        StringBuilder sb = new StringBuilder("SistemaTrazAqui{");
        sb.append("users=").append(users).append("\n");
        sb.append(", lojas=").append(lojas).append("\n");
        sb.append(", transportadoras=").append(transportadores).append("\n");
        sb.append(", encomendas=").append(encomendas).append("\n");
        sb.append(", contas=").append(registos.toString());
        sb.append('}');
        return sb.toString();
    }

    /**
     * Verifica se é igual
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SistemaTrazAqui that = (SistemaTrazAqui) o;
        return this.users.equals(that.getUsers()) &&
                this.lojas.equals(that.getLojas()) &&
                this.transportadores.equals(that.getTransportadores()) &&
                this.encomendas.equals(that.getEncomendas());
    }

    /**
     * Verifica se existe uma conta
     * @param codigo Codigo da conta
     */
    public boolean existeConta(String codigo) {
        return registos.existeConta(codigo);
    }

    /**
     * Verifica se a password inserida está correta
     * @param codigo Codigo de login da conta
     * @param pass Password da conta
     */
    public boolean passCorreta(String codigo, String pass) {
        return registos.existePass(codigo, pass);
    }

    /**
     * Cria um novo utilizador e coloca o no conjunto de utilizadores e registos do sistema
     */
    public void novoUtilizador(String codigo, String nome, GPS gps, String email, String password) {
        Utilizador u = new Utilizador(codigo, nome, gps);
        users.put(codigo, u.clone());
        registos.adicionarRegisto(codigo, email, password);
    }

    /**
     * Cria um novo voluntário e coloca o no conjunto de transportadores e registos do sistema
     */
    public void novoVoluntario(String codigo, String nome, GPS gps, String email, String password, double raio, boolean certificado, double velocidade) {
        Voluntario v = new Voluntario(codigo, nome, gps, raio, certificado, velocidade);
        transportadores.put(codigo, v.clone());
        registos.adicionarRegisto(codigo, email, password);
    }

    /**
     * Cria um novo Meio de Transporte e coloca o no conjunto de transportadores e registos do sistema
     */
    public void novaTransportadora(String codigo, String nome, GPS gps, String email, String password, String nif,
                                   double raio, double taxaDistancia, double taxaPeso, boolean variasEncs, boolean certificado, double velocidade) {
        MeioTransporte t;
        t = new Transportadora(codigo, nome, gps, raio, certificado, velocidade, nif, taxaDistancia, taxaPeso, variasEncs);
        transportadores.put(codigo, t.clone());
        registos.adicionarRegisto(codigo, email, password);
    }

    /**
     * Cria uma nova loja e coloca o no conjunto de lojas e registos do sistema
     */
    public void novaLoja(String code, String nome, GPS gps, String email,
                         String pw, boolean infoFilas) {
        Loja l = new Loja(code, nome, gps, infoFilas);
        lojas.putIfAbsent(code, l.clone());
        registos.adicionarRegisto(code, email, pw);
    }
    /**
     * Permite ver todas as lojas existentes no sistema
     * @return String com todas as lojas e informações sobre as mesmas
     */
    public String lojasDisponiveis() {
        StringBuilder sb = new StringBuilder();
        for (Loja l : lojas.values()) {
            sb.append("\n").append(l.getNome()).append(" | ")
                    .append("Código: ")
                    .append(l.getCodigo()).append(" | ")
                    .append(l.getGps()).append("\n");

        }
        return sb.toString();
    }

    /**
     * Permite ver se uma loja dá info sobre filas de espera
     * @param cod Codigo da loja
     * @return boolean que indica se tem ou não info
     */
    public boolean lojaTemInfoFilaEspera(String cod) {
        Loja l = lojas.get(cod);
        return l.isInfoFilas();
    }

    /**
     * Permite ver se uma loja dá info sobre filas de espera
     * @param cod Codigo da loja
     * @return boolean que indica se tem ou não info sobre filas de espera
     */
    public boolean existeLoja(String cod) {
        return lojas.containsKey(cod);
    }

    /**
     * Permite ver uma página específica de um catálogo de uma loja
     * @param loja Codigo da loja
     * @param p página pretendida
     * @return String que contém uma página do catálogo de produtos da loja
     */
    public String buscarProdsAoCat(String loja, int p) {
        return cat.separaPorPaginas(loja, p);
    }


    /**
     * Verifica se existe um produto específico numa loja
     * @param codLoja Codigo da loja
     * @param codProd Codigo de produto
     * @return boolean que indica se existe ou não o tal produto
     */
    public boolean existeProdutoNaLoja(String codLoja, String codProd) {
        return cat.existeProduto(codLoja, codProd);
    }

    /**
     * Cria uma linha de encomenda corrospondente a um produto
     * @param qtd Quantidade de um produto pretendido
     * @param cod Codigo de produto
     * @param codLoja Codigo da loja
     * @return Linha encomenda já criada
     */
    public LinhaEncomenda criarLinha(double qtd, String cod, String codLoja) {
        double precoUnitario = cat.precoDeUmProduto(cod, codLoja);
        String nomeProd = cat.nomeDeUmProduto(cod, codLoja);
        double pesoProd = cat.pesoDeUmProduto(cod, codLoja);
        return new LinhaEncomenda(cod, nomeProd, precoUnitario, qtd, pesoProd);
    }

    /**
     * Recebe uma encomenda e junta tudo numa String com legenda que torna a sua leitura simples e apelativa
     * @param c Corrosponde a uma encomenda
     * @return String já criada com as informações de uma  encomenda
     */
    public String estadoEncomenda(Collection<LinhaEncomenda> c) {
        StringBuilder sb = new StringBuilder();
        int i = 1;
        double precoTotal = 0.0;
        double pesoTotal = 0.0;
        for (LinhaEncomenda le : c) {
            sb.append("Produto ").append(i++)
                    .append(": ").append(le.getDesc()).append(" | Unidades: ").append(le.getQtd())
                    .append(" | Preço total: ").append(le.calculaValorLinhaEnc())
                    .append(" € | Peso total: ").append(le.calculaPeso()).append(" kg\n");
            precoTotal += le.calculaValorLinhaEnc();
            pesoTotal += le.calculaPeso();
        }
        sb.append("Preço total da encomenda: ").append(precoTotal).append(" €\n");
        sb.append("Peso total da encomenda: ").append(pesoTotal).append(" kg\n");
        return sb.toString();
    }

    /**
     * Recebe uma encomenda e calcula o seu custo total
     * @param c Corrosponde a uma encomenda
     * @return double que indica o preço total
     */
    public double precoTotalEncomenda(Collection<LinhaEncomenda> c){
        double precoTotal = 0.0;
        for (LinhaEncomenda le : c) {
            precoTotal += le.calculaValorLinhaEnc();
        }
        return precoTotal;
    }


    /**
     * Recebe uma encomenda e calcula o seu peso total
     * @param carrinho Corrosponde a todos os produtos de uma encomenda
     * @return Um double que indica o peso do carrinho
     */
    public double calculaPesoCarrinho(Collection<LinhaEncomenda> carrinho) {
        return carrinho.stream()
                .mapToDouble(LinhaEncomenda::calculaPeso).reduce(0.0, Double::sum);
    }


    /**
     * Verifica se existe uma encomenda
     * @param cod Código da encomenda
     * @return boolean que indica se existe ou não a encomenda
     */
    public boolean existeCodEnc(String cod) {
        return this.encomendas.stream()
                .anyMatch(e -> e.getCodEnc().equals(cod));
    }


    /**
     * Gera um código para uma encomenda
     * @return String com o codigo da encomenda
     */
    public String gerarCodigoEnc() {
        String res = "";
        while (existeCodEnc(res) || res.equals("")) {
            int num = 1 + (int) (Math.random() * ((10000 - 1) + 1));
            String numstr = String.valueOf(num);
            StringBuilder sb = new StringBuilder("e");
            sb.append(numstr);
            res = sb.toString();
        }
        return res;
    }


    /**
     * Adiciona uma encomenda ao sistema
     * @param e Encomenda a adicionar
     */
    public void novaEncomenda(Encomenda e) {
        this.encomendas.add(e);
    }


    /**
     * Vê quantas pessoas estão na fila de espera de uma loja
     * @param cod Codigo da loja
     * @return String Indica a fila de espera
     */
    public String pessoasEmEspera(String cod) {
        StringBuilder sb = new StringBuilder();
        Loja l = lojas.get(cod);
        sb.append(l.getPessoasEmEspera());
        return sb.toString();
    }


    /**
     * Calcula o tempo de atendimento por pessoa de uma loja para minutos
     * @param cod Codigo da loja
     * @return String que indica o tempo por pessoa
     */
    public String tempoAtendimentoPorPessoa(String cod) {
        StringBuilder sb = new StringBuilder();
        Loja l = lojas.get(cod);
        double d = (double) l.getTempoAtendimentoPorPessoa() / 60;
        String result = String.format("%.2f", d);
        sb.append(result).append(" minutos");
        return sb.toString();
    }


    /**
     * Calcula tempo de espera(min) com nº pessoas em espera e tempo de atendimento por pessoa
     * @param cod Codigo da loja
     * @return String que indica o tempo estimado de espera de uma loja
     */
    public String tempoEstimadoDeEspera(String cod) {
        StringBuilder sb = new StringBuilder();
        Loja l = lojas.get(cod);
        int i = l.getPessoasEmEspera() * l.getTempoAtendimentoPorPessoa();
        double d = (double) i / 60;
        String result = String.format("%.2f", d);
        sb.append(result).append(" minutos");
        return sb.toString();
    }


    /**
     * Altera o numero de pessoas em espera numa loja
     * @param cod Codigo da loja
     * @param num Número de pessoas em espera
     */
    public void setNumeroDePessoasEmEspera(String cod, int num) {
        lojas.get(cod).setPessoasEmEspera(num);
    }


    /**
     * Altera o tempo de atendimento por pessoa de uma loja
     * @param cod Codigo da loja
     * @param num tempo por pessoa
     */
    public void setTempoMedioAtendimentoPorPessoa(String cod, int num) {
        lojas.get(cod).setTempoAtendimentoPorPessoa(num);
    }


    /**
     * Finaliza uma encomenda
     * @param codigo Codigo do meio de transporte responsável pela encomenda
     * @return String com código de encomenda em caso de sucesso, null se a encomenda não existir
     */
    public String finalizaUmaEnc(String codigo) {
        Encomenda enc = getEncomendas()
                .stream()
                .filter(e -> e.getTransportador().equals(codigo) &&
                        e.getServicoEntrega().getEstado() == EstadoEncomenda.EM_TRANSPORTE)
                .findFirst()
                .orElse(null);
        if (enc == null) return null;
        else {
            Loja l =  getLojas().stream().filter(x -> x.getCodigo().equals(enc.getCodLoja())).findFirst().get();
            MeioTransporte t = getTransportadores().stream().filter(y -> y.getCodigo().equals(enc.getTransportador())).findFirst().get();
            Utilizador u = getUsers().stream().filter(k -> k.getCodigo().equals(enc.getCodUser())).findFirst().get();
            double tempo = (l.getTempoAtendimentoPorPessoa() * l.getPessoasEmEspera() )* 3600; // horas
            double distancia = distanciaTotal(t.getGps(), l.getGps(),u.getGps());

            enc.mudaEstado(EstadoEncomenda.ENTREGUE, enc.calculaTempoDeTransporteEncomenda(tempo, distancia, t.getVelocidade()));
            getTransportador(codigo).setDisponivel(true);
            return enc.getCodEnc();
        }

    }


    /**
     * Finaliza uma encomenda de um empresa transportadora que transporta várias encomendas
     * @param codigo Codigo da empresa transportadora responsável pela encomenda
     * @param codEnc Codigo da encomenda a finalizar
     * @return String com código de encomenda em caso de sucesso, null se a encomenda não existir
     */
    public String finalizaVariasEncomendas(String codigo, String codEnc) {
        Encomenda enc = getEncomendas()
                .stream()
                .filter(e -> e.getTransportador().equals(codigo) &&
                        e.getServicoEntrega().getEstado() == EstadoEncomenda.EM_TRANSPORTE &&
                        e.getCodEnc().equals(codEnc))
                .findFirst()
                .orElse(null);
        if (enc == null) return null;
        else {
            Loja l =  getLojas().stream().filter(x -> x.getCodigo().equals(enc.getCodLoja())).findFirst().get();
            MeioTransporte t = getTransportadores().stream().filter(y -> y.getCodigo().equals(enc.getTransportador())).findFirst().get();
            Utilizador u = getUsers().stream().filter(k -> k.getCodigo().equals(enc.getCodUser())).findFirst().get();
            double tempo = (l.getTempoAtendimentoPorPessoa() * l.getPessoasEmEspera() )* 3600; // horas
            double distancia = distanciaTotal(t.getGps(), l.getGps(),u.getGps());

            enc.mudaEstado(EstadoEncomenda.ENTREGUE, enc.calculaTempoDeTransporteEncomenda(tempo, distancia, t.getVelocidade()));
            return enc.getCodEnc();
        }

    }


    /**
     * Procura e junta numa string todas as encomendas entregues por um transportador
     * @param codigo Codigo do meio de transporte responsável pela encomenda
     * @return Histórico de encomendas
     */
    public String historicoEncTransp(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getTransportador().equals(codigo))
                .collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
           return imprimeUtility(lista, s);
        } else return "\nNão existe histórico de encomendas!\n";
    }


    /**
     * Procura e junta numa string todas as encomendas efetuadas por uma loja
     * @param codigo Codigo da loja
     * @return Histórico de encomendas
     */
    public String historicoEncLojas(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getCodLoja().equals(codigo))
                .collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            return imprimeUtility(lista,s);
        } else return "\nNão existe histórico de encomendas!\n";
    }


    /**
     * Procura e junta numa string todas as encomendas efetuadas por um utilizador
     * @param codigo Codigo do utilizador
     * @return Histórico de encomendas
     */
    public String historicoEncUtilizador(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getCodUser().equals(codigo)).collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            return imprimeUtility(lista, s);
        } else return "\nNão existe histórico de encomendas!\n";
    }


    /**
     * Procura e junta numa string todas as encomendas novas e por processar de uma loja
     * @param codigo Codigo da loja
     * @return String com as encomendas
     */
    public String imprimeEncNovasLojas(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getCodLoja().equals(codigo) &&
                        e.getServicoEntrega().getEstado() == EstadoEncomenda.NOVA)
                .collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            for (Encomenda e : lista) {
                s.append("\n").append("Cod. Encomenda: ").append(e.getCodEnc()).append(" | ")
                        .append("Loja: ").append(e.getCodLoja()).append(" | ")
                        .append("Cod. User: ").append(e.getCodUser()).append(" | ")
                        .append("Transportador: ").append(e.getTransportador()).append(" | ")
                        .append("Custo transporte: ").append(e.getServicoEntrega().getCusto()).append(" | ")
                        .append("Peso: ").append(String.format("%.2f", e.getPeso())).append(" | ")
                        .append("Preço:").append(String.format("%.2f", precoTotalEncomenda(e.getLinhas()))).append(" | ");

                if(e.isEncomendaMedica())
                    s.append("Encomenda médica: Sim | ");
                else
                    s.append("Encomenda médica: Não | ");
                s.append("Estado: ").append(e.getEstado()).append(" | ")
                        .append("Data início: ").append(e.getServicoEntrega().getDataNova()).append(" | ")
                        .append("Data fim: ").append(e.getServicoEntrega().getDataEntregue()).append(" | ")
                        .append("Produtos: [ ");

                for(LinhaEncomenda le : e.getLinhas()){
                    s.append(le.toString());
                }
                s.append("]\n");
            }
            return s.toString();
        } else return "\nNão existem encomendas novas!\n";
    }


    /**
     * Recebe uma lista de encomendas e passa-as para uma String com legendas
     * que tornem a sua leitura simples e apelativa
     * @param lista Encomendas
     * @param s StringBuilder
     * @return String com ecomendas já legendadas
     */
    public String imprimeUtility(ArrayList<Encomenda> lista, StringBuilder s) {
        for (Encomenda e : lista) {
            s.append("\n").append("Cod. Encomenda: ").append(e.getCodEnc()).append(" | ")
                    .append("Loja: ").append(e.getCodLoja()).append(" | ")
                    .append("Transportador: ").append(e.getTransportador()).append(" | ")
                    .append("Custo transporte: ").append(e.getServicoEntrega().getCusto()).append(" | ")
                    .append("Peso: ").append(String.format("%.2f", e.getPeso())).append(" | ")
                    .append("Preço:").append(String.format("%.2f", precoTotalEncomenda(e.getLinhas()))).append(" | ");

            if(e.isEncomendaMedica())
                s.append("Encomenda médica: Sim | ");
            else
                s.append("Encomenda médica: Não | ");
            s.append("Estado: ").append(e.getEstado()).append(" | ")
                    .append("Data início: ").append(e.getServicoEntrega().getDataNova()).append(" | ")
                    .append("Data fim: ").append(e.getServicoEntrega().getDataEntregue()).append(" | ")
                    .append("Produtos: [ ");

            for(LinhaEncomenda le : e.getLinhas()){
                s.append(le.toString());
            }
            s.append("]\n");
        }
        return s.toString();
    }

    /**
     * Verifica se uma string pode ou não ser transformada num int
     * @param value String a ser testada
     * @return boolean que indica se é possível ou não
     */
    public boolean tryParseInt(String value) {
        try {
            Integer.parseInt(value);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }


    /**
     * Altera a disponibilidade de um meio de transporte
     * @param codT Codigo do Meio de Transporte
     */
    public void alteraDisponibilidadeTransporte(String codT) {
        MeioTransporte t = getTransportador(codT);
        t.setDisponivel(!t.isDisponivel());
    }

    /**
     * Altera a disponibilidade para encomendas médicas de um Meio de Transporte
     * @param codTransp Codigo do Meio de Transporte
     */
    public void alteraDisponibilidadeTransporteMedico(String codTransp) {
        MeioTransporte transporte = getTransportador(codTransp);
        transporte.setAceitaMedicamentos(!transporte.aceitoTransporteMedicamentos());
    }


    /**
     * Verifica se existe uma encomenda dentro das encomendas disponíveis para um tranportador
     * @param codTransportador Codigo do Meio de Transporte
     * @param codEncomenda Codigo da Encomenda a verificar
     * @return boolean que indica se existe ou não
     */
    public boolean existeEncomendaNasEncomendasDisponiveis(String codTransportador, String codEncomenda) {
        boolean existe = encomendas.stream().anyMatch(x -> x.getCodEnc().equals(codEncomenda));

        if (existe)
            return encomendasDisponiveisAuxiliar(codTransportador).contains(
                    encomendas.stream().filter(y -> y.getCodEnc().equals(codEncomenda)).findFirst().get());
        return false;
    }


    /**
     * Aceita uma encomenda por parte de um transportador
     * @param codigoTransporte Codigo do transportador
     * @param codEncomenda Codigo da encomenda a entregar
     * @return String que indica o código da Encomenda e que foi aceite com successo
     */
    public String aceitaUmaEncomenda(String codigoTransporte, String codEncomenda) {
        StringBuilder sb = new StringBuilder();

        Encomenda e = encomendas.stream().filter(x -> x.getCodEnc().equals(codEncomenda)).findFirst().get();

        if (codigoTransporte.charAt(0) == 'v') {
            Voluntario t = (Voluntario) getTransportador(codigoTransporte);
            e.mudaEstado(EstadoEncomenda.EM_TRANSPORTE, 0);
            t.setDisponivel(false);
        }

        if (codigoTransporte.charAt(0) == 't') {
            Transportadora t = (Transportadora) getTransportador(codigoTransporte);
            e.mudaEstado(EstadoEncomenda.EM_ACEITACAO, 0);
            double distancia = distanciaTotal(t.getGps(), lojas.get(e.getCodLoja()).getGps(), users.get(e.getCodUser()).getGps());
            e.setCustoDeTransporte(distancia, e.getPeso(), t.getTaxaDistancia(), t.getTaxaPeso());
            t.setDisponivel(t.isFazVariasEnc());
        }

        e.setTransportador(codigoTransporte);

        sb.append("\nEncomenda: ").append(e.getCodEnc()).append(" Aceite com sucesso!");
        return sb.toString();
    }


    /**
     * Calcula a distância total do percurso de uma entrega
     * @param gpsTransporte Coordenadas do transportador
     * @param gpsLoja Coordenadas da loja
     * @param gpsUtilizador Coordenadas do utilizador
     * @return Double que indica a distância total do percurso
     */
    public double distanciaTotal(GPS gpsTransporte, GPS gpsLoja, GPS gpsUtilizador) {
        return GPS.dist(gpsTransporte, gpsLoja) + GPS.dist(gpsLoja, gpsUtilizador);
    }


    /**
     * Verifica se um transportador é certificado
     * @param codigo Codigo do transportador a verificar
     * @return boolean que indica se é certificado
     */
    public boolean Certificado(String codigo) {
        MeioTransporte transporte = getTransportador(codigo);
        return transporte.isCertificado();
    }


    /**
     * Verifica se uma empresa transportadora pode transportar várias encomendas
     * @param codigo Codigo da empresa transportadora a verificar
     * @return boolean que indica se é certificado
     */
    public boolean podeTransportarVariasEncomendas(String codigo) {
        Transportadora t = (Transportadora) getTransportador(codigo);
        return t.isFazVariasEnc();
    }


    /**
     * Verifica se um transportador está disponível
     * @param codigo Codigo do transportador a verificar
     * @return boolean que indica está disponível
     */
    public boolean estaDisponivel(String codigo) {
        return transportadores.get(codigo).isDisponivel();
    }


    /**
     * Verifica as disponibilidades de um transportador e coloca-as numa string
     * @param codigo Codigo do transportador a verificar
     * @param signal que indica se é ou não certificado
     * @return String com as disponibilidades
     */
    public String mostraEstados(String codigo, int signal) {
        MeioTransporte transporte = getTransportador(codigo);
        StringBuilder sb = new StringBuilder();
        if (transporte.isDisponivel()) sb.append("Estado Atual:\n-> Disponível para trabalhar\n");
        else sb.append("Estado Atual:\n-> Indisponível para trabalhar\n");
        if (signal == 1) {
            if (transporte.aceitoTransporteMedicamentos()) sb.append("-> Disponível para encomendas médicas\n");
            else sb.append("-> Indísponível para encomendas médicas\n");
        }
        return sb.toString();
    }

    /**
     * Verifica as encomendas disponíveis para um transportador
     * @param codigo Codigo do transportador
     * @return Encomendas disponíveis
     */
    public ArrayList<Encomenda> encomendasDisponiveisAuxiliar(String codigo) {
        MeioTransporte transporte = getTransportador(codigo);
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getServicoEntrega().getEstado() == EstadoEncomenda.PRONTA_A_SER_ENTREGUE &&
                        GPS.estaDentroDoRaio(transporte.getGps(), (lojas.get(e.getCodLoja()).getGps()), (users.get(e.getCodUser()).getGps()), transporte.getRaio()))
                .collect(Collectors.toCollection(ArrayList::new));
        if (lista.size() == 0) return null;
        else if (transporte.isCertificado() && transporte.aceitoTransporteMedicamentos()) return lista;

        ArrayList<Encomenda> listaNaoCertificada = lista.stream().filter(e -> !e.isEncomendaMedica()).collect(Collectors.toCollection(ArrayList::new));
        if (listaNaoCertificada.size() == 0) return null;
        else return listaNaoCertificada;

    }

    /**
     * Verifica as encomendas disponíveis para um transportador e
     * calcula e associa a distãncia total do percurso a cada uma delas
     * @param codigoMeioTransporte Codigo do transportador
     * @return Encomendas disponíveis e respetiva distância
     */
    public Map<Double, Encomenda> encomendasDisponiveis(String codigoMeioTransporte) {

        ArrayList<Encomenda> listaEncomendas = encomendasDisponiveisAuxiliar(codigoMeioTransporte);
        if (listaEncomendas == null) return null;
        double distTotal;
        Map<Double, Encomenda> lista = new TreeMap<>();
        for (Encomenda e : listaEncomendas) {
            distTotal = GPS.dist(lojas.get(e.getCodLoja()).getGps(), transportadores.get(codigoMeioTransporte).getGps())
                    + GPS.dist(lojas.get(e.getCodLoja()).getGps(), users.get(e.getCodUser()).getGps());
            lista.put(distTotal, e);
        }
        return lista;

    }


    /**
     * Recebe as encomendas disponíveis e respetivas distâncias do percurso e junta
     * tudo numa String juntamente com legendas que tornam a sua leitura apelativa
     * @param listaEncomendas Encomendas
     * @return String já completa
     */
    public String verEncomendasDisponiveis(Map<Double, Encomenda> listaEncomendas) {
        if (listaEncomendas == null) return "\nNão existem encomendas para entregar.\n";
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<Double, Encomenda> entry : listaEncomendas.entrySet()) {
            sb.append("Encomenda: ").append(entry.getValue().getCodEnc())
                    .append(" | Loja: ").append(entry.getValue().getCodLoja())
                    .append(" | Utilizador: ").append(entry.getValue().getCodUser())
                    .append(" | Distância Total do percurso: ").append(entry.getKey()).append(" km\n");
        }

        return sb.toString();

    }

    /**
     * Indica as encomendas a ser transportadas
     * @param codTransportadora Código da transportadora
     * @return String com todas as encomendas
     */
    public String encomendasASerEntreguesTransportadorasVariasEncomendas(String codTransportadora) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getServicoEntrega().getEstado() == EstadoEncomenda.EM_TRANSPORTE &&
                        e.getTransportador().equals(codTransportadora))
                .collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            return imprimeUtility(lista, s);
        } else return null;
    }


    /**
     * Sinaliza que uma encomenda está pronta para ser recolhida por uma tranportadora
     * @param codigo Código da loja
     */
    public void sinalizaEncomendaProntaParaEntrega(String codigo) {
        Encomenda e = getEncomendas().stream().filter(x -> x.getCodEnc().equals(codigo)).findFirst().get();
        e.mudaEstado(EstadoEncomenda.PRONTA_A_SER_ENTREGUE, 0);
    }


    /**
     * Verifica se o transportador já se encontra a transportar alguma encomenda
     * @param codigo Código da transportadora
     */
    public boolean estaNoMomentoATransportarOuEmAceitacao(String codigo) {
        return getEncomendas().stream().anyMatch(e -> e.getTransportador().equals(codigo) && (
                e.getEstado() == EstadoEncomenda.EM_TRANSPORTE || e.getEstado() == EstadoEncomenda.EM_ACEITACAO));
    }


    /**
     * Calcula o total faturado por uma empresa transportadora entre datas específicas
     * @param codigo Código da empresa transportadora
     * @param inicio Data de início
     * @param fim Data do fim
     * @return inteiro que indica o total faturado nesse período
     */
    public int totalFaturadoEmpTrans(String codigo, LocalDate inicio, LocalDate fim) {
        int totalFaturado = 0;
        LocalDateTime inicioT = inicio.atStartOfDay();
        LocalDateTime fimT = fim.atStartOfDay();
        for (Encomenda e : encomendas) {
            if (e.getTransportador().equals(codigo) &&
                    ((e.getServicoEntrega().getDataEntregue().isAfter(inicioT) || e.getServicoEntrega().getDataEntregue().isEqual(inicioT))
                            && (e.getServicoEntrega().getDataEntregue().isBefore(fimT) || e.getServicoEntrega().getDataEntregue().isEqual(fimT)))) {

                totalFaturado += e.getServicoEntrega().getCusto();
            }
        }
        return totalFaturado;

    }


    /**
     * DateTimeFormatter
     */
    public LocalDate dateInput(String userInput) {

        DateTimeFormatter dateFormat = new DateTimeFormatterBuilder()
                .appendOptional(DateTimeFormatter.ofPattern("dd/M/yyyy"))
                .appendOptional(DateTimeFormatter.ofPattern("d/M/yyyy"))
                .appendOptional(DateTimeFormatter.ofPattern("d/MM/yyyy"))
                .appendOptional(DateTimeFormatter.ofPattern("dd/MM/yyyy"))
                .toFormatter();


        return LocalDate.parse(userInput, dateFormat);
    }


    /**
     * Indica os 10 utilizadores mais ativos do sistema por numero de encomendas finalizadas
     * @return String com os 10 utilizadores mais ativos
     */
    public String utilizadoresMaisAtivos() {
        int[][] codUserENmrEncomendasFeitas = new int[1000][2];
        StringBuilder sb = new StringBuilder();
        int x = 0, nmrEnc, n = 999;
        Collection<Utilizador> u = getUsers();
        for (Utilizador s : u) {
            nmrEnc = 0;
            for (Encomenda e : encomendas) {
                if (e.getCodUser().equals(s.getCodigo()) && e.getServicoEntrega().getEstado() == EstadoEncomenda.ENTREGUE)
                    nmrEnc++;
            }
            codUserENmrEncomendasFeitas[x][1] = Integer.parseInt(s.getCodigo().substring(1));
            codUserENmrEncomendasFeitas[x][0] = nmrEnc;
            x++;
        }
        Arrays.sort(codUserENmrEncomendasFeitas, Comparator.comparingInt(a -> a[0]));
        x = 1;
        while (n > 989) {
            if (codUserENmrEncomendasFeitas[n][0] == 0) return sb.toString();
            else sb.append(x).append(". u").append(codUserENmrEncomendasFeitas[n][1]).append(" -> ")
                    .append(codUserENmrEncomendasFeitas[n][0]);
            if (codUserENmrEncomendasFeitas[n][0] > 1) sb.append(" Encomendas\n");
            else sb.append(" Encomenda\n");
            n--;
            x++;
        }
        return sb.toString();

    }


    /**
     * Indica as 10 empresas transportadoras mais ativas do sistema por numero de kms percorrido
     * @return String com as 10 empresas mais ativas
     */
    public String empresasTransportadorasMaisAtivas() {
        int[][] codTransENmrEncomendasFeitas = new int[1000][2];
        StringBuilder sb = new StringBuilder();
        int x = 0, distTotal, n = 999;
        Collection<MeioTransporte> u = getTransportadores();
        for (MeioTransporte s : u) {
            distTotal = 0;
            for (Encomenda e : encomendas) {
                if (e.getTransportador().equals(s.getCodigo()) && e.getServicoEntrega().getEstado() == EstadoEncomenda.ENTREGUE
                        && s.getCodigo().charAt(0) == 't')
                    distTotal += GPS.dist(lojas.get(e.getCodLoja()).getGps(), transportadores.get(s.getCodigo()).getGps())
                            + GPS.dist(lojas.get(e.getCodLoja()).getGps(), users.get(e.getCodUser()).getGps());
            }
            codTransENmrEncomendasFeitas[x][1] = Integer.parseInt(s.getCodigo().substring(1));
            codTransENmrEncomendasFeitas[x][0] = distTotal;
            x++;
        }
        Arrays.sort(codTransENmrEncomendasFeitas, Comparator.comparingInt(a -> a[0]));
        x = 1;
        while (n > 989) {
            if (codTransENmrEncomendasFeitas[n][0] == 0) return sb.toString();
            else sb.append(x).append(". t").append(codTransENmrEncomendasFeitas[n][1]).append(" -> ")
                    .append(codTransENmrEncomendasFeitas[n][0]).append(" Km");
            n--;
            x++;
        }
        return sb.toString();

    }


    /**
     * Indica as encomendas que o utilizador tem de aceitar o seu custo
     * @return String com as encomendas por aceitar
     */
    public String verEncomendasPorAceitar(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getCodUser().equals(codigo) &&
                        e.getEstado() == EstadoEncomenda.EM_ACEITACAO
                ).collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            for (Encomenda e : lista) {
                Loja l =  getLojas().stream().filter(x -> x.getCodigo().equals(e.getCodLoja())).findFirst().get();
                MeioTransporte t = getTransportadores().stream().filter(y -> y.getCodigo().equals(e.getTransportador())).findFirst().get();
                Utilizador u = getUsers().stream().filter(k -> k.getCodigo().equals(e.getCodUser())).findFirst().get();

                double tempo = (l.getTempoAtendimentoPorPessoa() * l.getPessoasEmEspera() ) / 3600; // horas
                double distancia = distanciaTotal(t.getGps(), l.getGps(),u.getGps());

                s.append("\n").append("Cod. Encomenda: ").append(e.getCodEnc()).append(" | ")
                        .append("Loja: ").append(e.getCodLoja()).append(" | ")
                        .append("Transportador: ").append(e.getTransportador()).append(" | ")
                        .append("Custo transporte: ").append(e.getServicoEntrega().getCusto()).append(" | ")
                        .append("Tempo de entrega estimado: ")
                            .append( e.calculaTempoDeTransporteEncomenda(tempo, distancia, t.getVelocidade())).append(" minutos | ")
                        .append("Peso: ").append(String.format("%.2f", e.getPeso())).append(" | ")
                        .append("Preço:").append(String.format("%.2f", precoTotalEncomenda(e.getLinhas()))).append(" | ");

                if(e.isEncomendaMedica())
                    s.append("Encomenda médica: Sim | ");
                else
                    s.append("Encomenda médica: Não | ");
                s.append("Produtos: [ ");

                for(LinhaEncomenda le : e.getLinhas()){
                    s.append(le.toString());
                }
                s.append("]\n");
            }
            return s.toString();
        } else return "\nNão existem encomendas por aceitar.\n";
    }


    /**
     * Verifica se existe uma encomenda e se está por aceitar
     * @return String com as encomendas por aceitar
     */
    public boolean existeEncomendaPorAceitarCodUser(String codigo) {
        return encomendas.stream().anyMatch(x -> x.getCodUser().equals(codigo) &&
                x.getEstado() == EstadoEncomenda.EM_ACEITACAO);
    }

    public boolean existeEncomendaPorAceitarCodEnc(String codigo) {
        return encomendas.stream().anyMatch(x -> x.getCodEnc().equals(codigo) &&
                x.getEstado() == EstadoEncomenda.EM_ACEITACAO);
    }


    /**
     * Indica que uma encomenda foi aceite por um transportador
     * @param codigo Codigo da encomenda a mudar o estado
     */
    public void sinalizaEncomendaAceite(String codigo) {
        Encomenda e = getEncomendas().stream().filter(x -> x.getCodEnc().equals(codigo)).findFirst().get();
        e.mudaEstado(EstadoEncomenda.EM_TRANSPORTE, 0);
    }


    /**
     * Indica que uma encomenda foi rejeito pelo utilizador ao aceitar os custos
     * @param codigo Codigo da encomenda a mudar o estado de volta para pronta a ser entregue
     */
    public void sinalizaEncomendaRejeitada(String codigo) {
        Encomenda e = getEncomendas().stream().filter(x -> x.getCodEnc().equals(codigo)).findFirst().get();
        e.mudaEstado(EstadoEncomenda.PRONTA_A_SER_ENTREGUE, 0);
        e.setTransportador("");
    }

    /**
     * Mostra ao utilizador as encomendas que ele fez e que ainda não classificou
     * @param codigo Codigo do utilizador
     * @return String com as encomendas
     */
    public String verEncomendasPorClassificar(String codigo) {
        ArrayList<Encomenda> lista = getEncomendas().stream()
                .filter(e -> e.getCodUser().equals(codigo) &&
                        e.getEstado() == EstadoEncomenda.ENTREGUE &&
                        e.getClassificacao() == null
                ).collect(Collectors.toCollection(ArrayList::new));

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
            return imprimeUtility(lista,s);
        } else return "\nNão existem encomendas por classificar.\n";
    }


    /**
     * Verifica se um utilizador tem alguma encomenda finalizada por classificar
     * @param codigo Codigo do utilizador
     * @return boolean que indica se tem alguma encomenda
     */
    public boolean existeEncomendaPorClassificar(String codigo) {
        return getEncomendas().stream().anyMatch(x -> x.getCodEnc().equals(codigo) &&
                x.getEstado() == EstadoEncomenda.ENTREGUE &&
                x.getClassificacao() == null);
    }

    /**
     * Classifica uma encomenda
     * @param codigo Codigo da encomenda a classificar
     */
    public void classificaEncomenda(String codigo, int classificacao) {
        Encomenda e = getEncomendas().stream().filter(x -> x.getCodEnc().equals(codigo)).findFirst().get();
        e.setClassificacaoDeTransporte(classificacao);
    }

    /**
     * Devolve um histórico de encomendas que cumpram os filtros dados á função
     * @param inicio Data inicial
     * @param fim Data final
     * @param codUser codigo de utilizador
     * @param codTransportadora codigo da transportadora
     * @return String que apresenta as encomendas pretendidas
     */
    public String historicoEncUtilizadorFiltrado(LocalDate inicio, LocalDate fim, String codUser, String codTransportadora){
        ArrayList<Encomenda> lista;
        if(codTransportadora.equals("")){
            lista = getEncomendas().stream()
                    .filter(e -> e.getCodUser().equals(codUser) &&
                            e.getServicoEntrega().getDataNova().isAfter(inicio.atStartOfDay()) &&
                            e.getServicoEntrega().getDataNova().isBefore(fim.atStartOfDay())
                    ).collect(Collectors.toCollection(ArrayList::new));
        }
        else {
            lista = getEncomendas().stream()
                    .filter(e -> e.getCodUser().equals(codUser) &&
                            e.getServicoEntrega().getDataNova().isAfter(inicio.atStartOfDay()) &&
                            e.getServicoEntrega().getDataNova().isBefore(fim.atStartOfDay()) &&
                            e.getTransportador().equals(codTransportadora)
                    ).collect(Collectors.toCollection(ArrayList::new));
        }

        StringBuilder s = new StringBuilder();
        if (lista.size() != 0) {
           return imprimeUtility(lista, s);
        } else return "\nNão existe histórico de encomendas!\n";
    }


    /**
     * Classifica uma encomenda
     * @param st codigo do tranportador
     * @return boolean que indica se existe ou não
     */
    public boolean existeCodMeioTransporte(String st){
        return transportadores.containsKey(st);
    }

}

