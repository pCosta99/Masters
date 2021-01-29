package Controller;

import Model.*;

import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Controlador responsável por ações de utilizadores
 */
public class ControllerUtilizador extends ControllerMutual implements Controller {
    private String codUtilizador;

    /**
     * Contrutor vazio
     */
    public ControllerUtilizador() {
        sys = new Sistema();
        codUtilizador = "n/d";
    }

    /**
     * Construtor parametrizado
     * @param sys Sistema para inserir no controlador
     * @param util código de um utilizador
     */
    public ControllerUtilizador(Sistema sys, String util) {
        this.sys = sys.clone();
        codUtilizador = util;
    }

    /**
     * Devolve o Sistema de um Controller
     * @return Sistema do Controller
     */
    public Sistema getSistema() {
        return sys;
    }


    /**
     * Atualiza o Sistema de um Controller
     * @param sys novo sistema para inserir
     */
    public void setSistema(Sistema sys) {
        this.sys = sys;
    }


    /**
     * Devolve o código do utilizador
     * @return string com código de utilizador
     */
    public String getCodUtilizador() {
        return codUtilizador;
    }

    /**
     * Insere o código do utilizador
     * @param codUtilizador string com código de utilizador
     */
    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    /**
     * Transforma as encomendas ativas de um utilizador numa lista de strings
     * @return lista com encomendas ativas
     */
    private List<String> ativas(){
        List<Encomenda> temp = sys.encsUtilizador(codUtilizador);
        List<String> ret = new ArrayList<>();
        ret.add("Não tem encomendas ativas.");
        if (temp.size() > 0)
            ret = temp.stream().map(Encomenda::toString).collect(Collectors.toList());

        return ret;
    }

    /**
     * Devolve uma encomenda como string
     * @param arg string com numero de encomenda
     * @return string com uma encomenda ou código de erro
     */
    private String infoEncomenda(String arg){
        String ret = "Encomenda Inválida.";
        for (Encomenda enc: sys.encsUtilizador(codUtilizador)) {
            if(enc.getNumEnc().equals(arg)) {
                ret = enc.toString();
                break;
            }
        }
        return ret;
    }

    /**
     * Devolve a morada de um utilizador como uma string
     * @param util utilizador
     * @return string com morada de um utilizador
     */
    private String morada(Utilizador util){
        return util.getGps().toString();
    }

    /**
     * Prepara uma encomenda e adiciona-a ao sistema
     * @param encomenda encomenda sob a forma de uma string
     * @return Lista de strings com o preço total e uma mensagem para o utilizador
     */
    private List<String> encomendar(List<String> encomenda){
        int nLinhasEnc = (encomenda.size() - 3) / 2;
        Random var = new Random();
        List<LinhaEncomenda> linhasEnc = new ArrayList<>();

        for (int i = 3; i < nLinhasEnc + 3 ; i+=2) {
            LinhaEncomenda aux = new LinhaEncomenda();
            aux.setDescricao(encomenda.get(i));
            aux.setQuantidade(Double.parseDouble(encomenda.get(i + 1)));
            aux.setReferencia("p" + var.nextInt());
            double rand1 = (var.nextDouble() + 0.0001) * 10;
            aux.setPreco(rand1);
            linhasEnc.add(aux);
        }
        double peso = nLinhasEnc * (var.nextDouble() + 0.0001) * 10;
        Transporte transporte;

        switch(encomenda.get(2).charAt(0)){
            case 't':
                transporte = sys.getTransportadora(encomenda.get(2));
                break;
            case 'v':
                transporte = sys.getVoluntario(encomenda.get(2));
                break;
            default:
                transporte = new Transportadora();
        }
        Encomenda enc = new Encomenda(codUtilizador, null, LocalDateTime.now(),null,peso,linhasEnc,transporte,null,Boolean.parseBoolean(encomenda.get(1)),0);
        enc = sys.novaEncomenda(enc);
        List<String> ret = new ArrayList<>();
        DecimalFormat df = new DecimalFormat("0.00");
        ret.add("Encomenda efetuada com sucesso!");
        ret.add("Preço total = " + df.format(enc.getPreco()) + "€.");
        return ret;
    }

    /**
     * Devolve uma versão alternativa do toString de transportes
     * @param t transporte
     * @return String com um transporte
     */
    private String printTransporte(Transporte t){
        StringBuilder sb = new StringBuilder();
        sb.append(t.getCode());
        sb.append(" ");
        sb.append(t.getNome());
        sb.append(" ");
        if(t instanceof Transportadora){
            sb.append(((Transportadora) t).getPrec_km() * t.getGps().distGPSKms(sys.getUtilizador(codUtilizador).getGps()));
        }else{
            sb.append(0.0);
        }
        sb.append(" ");
        try {
            sb.append(t.getClassificacoes().stream().mapToDouble(v -> v).average().orElse(5.0));
        } catch (NullPointerException e){
            sb.append("Indisponível");
        }
        return sb.toString();
    }

    /**
     * Avalia a encomenda
     * @param encomenda codigo de encomenda e classificação
     * @param util Utilizador que avalia a encomenda
     * @return String com mensagem para o utilizador
     */
    private String rankEncomenda(List<String> encomenda, Utilizador util){
        //0 -> num encomenda
        //1 -> classificacao
        String codEncomenda = encomenda.get(0);
        int classi;
        try {
            classi = Integer.parseInt(encomenda.get(1));
        }catch (Exception e){
            return "Input inválido";
        }

        Encomenda rank;
        try {
           rank = util.getEnc(codEncomenda);
           if(rank.getDataEntrega() == null)
               return "Só pode avaliar transportadoras depois de receber a encomenda.";

        }catch (NullPointerException e){
            return "Encomenda Inválida!";
        }
        if(classi < 0 || classi > 10)
            return "Classificação inválida. (0-10)";

        Transporte t = rank.getTransporte();

        sys.classificaTrans(t,classi);

        return "Classificação submetida com sucesso!";
    }

    /**
     * Executa um comando no sistema
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     */
    public List<String> execute(List<String> s) {
        List<String> ret = new ArrayList<>();
        Utilizador util = sys.getUtilizador(codUtilizador);
        List<String> aux = s.subList(1,s.size());

        switch (s.get(0)){
            case "perfil":
                ret.add(util.toString());
                break;
            case "encomendar":
                ret = encomendar(s);
                break;
            case "entregues":
                //encomendas nao recebidas
                Map<String,Encomenda> temp = util.getEncomendaMap();
                if(temp != null) {
                    if(s.size() > 1)
                        ret = temp.values().stream().filter(v -> v.getDataEntrega().isBefore(LocalDateTime.parse(s.get(1))) && v.getDataEntrega().isAfter(LocalDateTime.parse(s.get(2)))).map(Encomenda::toString).collect(Collectors.toList());
                    else
                        ret = temp.values().stream().filter(encomenda -> encomenda.getDataEntrega() != null).map(Encomenda::toString).collect(Collectors.toList());
                }
                else{
                    ret.add("Não fez nenhuma encomenda!");
                }

                if(ret.size() == 0){
                    ret.add("Não tem encomendas entregues!");
                }

                break;
            case "ativas":
                ret = ativas();
                break;
            case "rank":
                ret.add(rankEncomenda(aux, util));
                break;
            case "info":
                ret.add(infoEncomenda(s.get(1)));
                break;
            case "altera":
                util.alterarMorada();
                sys.addUser(util);
            case "morada":
                ret.add("Morada registada:");
                ret.add(morada(util));
                break;
            case "transportes":
                ret = sys.recomendaTrans(util.getGps(),Boolean.parseBoolean(s.get(1))).stream().map(this::printTransporte).collect(Collectors.toList());
                break;
            case "top":
                ret = tops(s.get(1));
                break;
            case "logout":
                ret.add("logout");
                break;
            default:
                ret.add("Comando Inválido!");

        }

        return ret;
    }


}
